use std::collections::HashSet;
use std::io;
use std::rand;
use serialize::{Encodable, Decodable, json};
use serialize::json::{Encoder, Decoder, DecoderError};
use hyper::method::Method::{Get, Post};
use hyper::client::{Request};
use hyper::status;
use hyper;

use game_data::{Color, Player};
use betrayal_server::{
    BetrayalServer,
    CreateRoomResponse,
    JoinRoomRequest, JoinRoomResponse,
    ListRoomRequest, ListRoomResponse,
    PickColorRequest, PickColorResponse,
    KickPlayerRequest, KickPlayerResponse,
};

struct TestServer {
    listen_serv : hyper::server::Listening,
    port : u16,
}

impl TestServer {
    fn new() -> TestServer {
        let host = from_str("127.0.0.1").unwrap();
        for _ in range(0, 100u) {
            let port = rand::random();
            let serv = BetrayalServer::new();
            let s = hyper::server::Server::http(host, port);
            match s.listen(serv) {
                Ok(listening) => {
                    return TestServer {
                        listen_serv: listening,
                        port: port
                    }
                },
                Err(_) => (),
            };
        }

        panic!("Could not find a port to bind the test server to!");
    }

    fn make_request(&self, method: hyper::method::Method, path: &str)
            -> Request<hyper::net::Fresh> {
        let mut url = "http://localhost:".to_string();
        url.push_str(self.port.to_string().as_slice());
        url.push_str(path);
        Request::new(
            method, hyper::Url::parse(url.as_slice()).unwrap()).unwrap()
    }

    fn do_request<T: Decodable<Decoder, DecoderError>>(&self, path: &str) -> T {
        let req = self.make_request(Get, path);

        // Start the Request, writing headers and starting streaming.
        let res = req.start().unwrap()
            // Send the Request.
            .send().unwrap()
            // Read the Response.
            .read_to_string().unwrap();

        json::decode(res.as_slice()).unwrap()
    }

    fn post_request<'a, U: Encodable<Encoder<'a>, io::IoError>,
                    D: Decodable<Decoder, DecoderError>>
                    (&self, path: &str, request: &U) -> D {
        let req = self.make_request(Post, path);
        let mut stream = req.start().unwrap();
        stream.write(json::encode(request).as_bytes()).unwrap();
        let mut result = stream.send().unwrap();
        assert_eq!(status::Ok, result.status);
        let body = result.read_to_string().unwrap();
        json::decode(body.as_slice()).unwrap()
    }

    fn create_room(&self) -> String {
        let resp : CreateRoomResponse = self.do_request("/api/create_room");
        resp.room_code
    }

    fn list_players(&self, room_code: String) -> Box<Vec<Player>> {
        let lrr : ListRoomResponse = self.post_request(
            "/api/list_room", &ListRoomRequest {
                room_code: room_code
            }
        );
        box lrr.players
    }

    fn join_room(&self, room_code: String, name: &str) {
        let _ : JoinRoomResponse = self.post_request(
            "/api/join_room", &JoinRoomRequest {
                room_code: room_code,
                name: name.to_string(),
            }
        );
    }

    fn kick_player(&self, room_code: String, to_kick: &str) {
        let _ : KickPlayerResponse = self.post_request(
            "/api/kick_player", &KickPlayerRequest {
                room_code: room_code,
                to_kick: to_kick.to_string(),
            }
        );
    }

}

impl Drop for TestServer {
    fn drop(&mut self) {
        self.listen_serv.close().unwrap();
    }
}

#[test]
fn create_room_returns_unique_codes() {
    let ts = TestServer::new();
    let mut hs = HashSet::new();
    for _ in range(0, 100u) {
        let room_code = ts.create_room();
        assert_eq!(false, hs.contains(&room_code));
        hs.insert(room_code);
    }
}

#[test]
fn join_room_works() {
    let ts = TestServer::new();

    let room_code = ts.create_room();
    assert_eq!(0, ts.list_players(room_code.clone()).len());
    ts.join_room(room_code.clone(), "Test User");

    let players = ts.list_players(room_code.clone());
    assert_eq!(1, players.len());
    assert_eq!("Test User".to_string(), players[0].name);
    assert_eq!(None, players[0].color);
}

#[test]
fn update_color() {
    let ts = TestServer::new();

    let room_code = ts.create_room();
    assert_eq!(0, ts.list_players(room_code.clone()).len());
    ts.join_room(room_code.clone(), "Test User");

    let _ : PickColorResponse = ts.post_request(
        "/api/pick_color", &PickColorRequest {
            room_code: room_code.clone(),
            name: "Test User".to_string(),
            color: Color::Red
        }
    );

    let players = ts.list_players(room_code.clone());
    assert_eq!(1, players.len());
    assert_eq!(Some(Color::Red), players[0].color);

    ts.join_room(room_code.clone(), "JoeBob");
    assert_eq!(2, ts.list_players(room_code.clone()).len());

    let r = ts.make_request(Post, "/api/pick_color");
    let mut stream = r.start().unwrap();
    let request = &PickColorRequest {
        name: "JoeBob".to_string(),
        room_code: room_code.clone(),
        color: Color::Red
    };
    stream.write(json::encode(request).as_bytes()).unwrap();
    let result = stream.send().unwrap();
    assert_eq!(status::BadRequest, result.status);
}

#[test]
fn kick_player() {
    let ts = TestServer::new();

    let room_code = ts.create_room();
    assert_eq!(0, ts.list_players(room_code.clone()).len());
    ts.join_room(room_code.clone(), "Test User");
    assert_eq!(1, ts.list_players(room_code.clone()).len());
    ts.kick_player(room_code.clone(), "Test User");
    assert_eq!(0, ts.list_players(room_code.clone()).len());
}
