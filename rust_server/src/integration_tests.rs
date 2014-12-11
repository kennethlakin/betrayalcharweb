extern crate test;

use std::collections::HashSet;
use std::io;
use std::rand;
use serialize;
use serialize::{Encodable, Decodable, json};
use serialize::json::{Encoder, Decoder, DecoderError};
use hyper::header::common::content_length::ContentLength;
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
use framework::ErrorResponse;

#[test]
fn create_room_returns_unique_codes() {
    let ts = TestServer::new();
    let mut hs = HashSet::new();
    for _ in range(0, 100u) {
        let room_code = ts.create_room().unwrap();
        assert_eq!(false, hs.contains(&room_code));
        hs.insert(room_code);
    }
}

#[test]
fn join_room_works() {
    let ts = TestServer::new();

    let room_code = ts.create_room().unwrap();
    assert_eq!(0, ts.list_players(room_code.clone()).unwrap().len());
    ts.join_room(room_code.clone(), "Test User").unwrap();

    let players = ts.list_players(room_code.clone()).unwrap();
    assert_eq!(1, players.len());
    assert_eq!("Test User".to_string(), players[0].name);
    assert_eq!(None, players[0].color);
}

#[test]
fn update_color() {
    let ts = TestServer::new();

    let room_code = ts.create_room().unwrap();
    assert_eq!(0, ts.list_players(room_code.clone()).unwrap().len());
    ts.join_room(room_code.clone(), "Test User").unwrap();

    ts.pick_color(room_code.clone(), "Test User", Color::Red).unwrap();

    let players = ts.list_players(room_code.clone()).unwrap();
    assert_eq!(1, players.len());
    assert_eq!(Some(Color::Red), players[0].color);

    ts.join_room(room_code.clone(), "JoeBob").unwrap();
    assert_eq!(2, ts.list_players(room_code.clone()).unwrap().len());

    ts.pick_color(room_code.clone(), "JoeBob", Color::Red).unwrap_err();
}

#[test]
fn kick_player() {
    let ts = TestServer::new();

    let room_code = ts.create_room().unwrap();
    assert_eq!(0, ts.list_players(room_code.clone()).unwrap().len());
    ts.join_room(room_code.clone(), "Test User").unwrap();
    assert_eq!(1, ts.list_players(room_code.clone()).unwrap().len());
    ts.kick_player(room_code.clone(), "Test User").unwrap();
    assert_eq!(0, ts.list_players(room_code.clone()).unwrap().len());
}

#[bench]
fn bench_list_players(b: &mut test::Bencher) {
    let ts = TestServer::new();
    let room_code = ts.create_room().unwrap();
    ts.join_room(room_code.clone(), "A").unwrap();
    ts.join_room(room_code.clone(), "B").unwrap();
    ts.join_room(room_code.clone(), "C").unwrap();
    ts.join_room(room_code.clone(), "D").unwrap();
    ts.join_room(room_code.clone(), "E").unwrap();
    ts.join_room(room_code.clone(), "F").unwrap();
    ts.join_room(room_code.clone(), "G").unwrap();
    b.iter(|| ts.list_players(room_code.clone()));
}

struct TestServer {
    listen_serv : hyper::server::Listening,
    port : u16,
}

type R<T> = Result<T, ErrorResponse>;

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

    fn do_request<T: Decodable<Decoder, DecoderError>>(&self, path: &str)
            -> Result<T, ErrorResponse> {
        let req = self.make_request(Get, path);

        let mut result = req.start().unwrap().send().unwrap();
        let mut decoder = Decoder::new(json::from_reader(&mut result).unwrap());
        if result.status == status::StatusCode::Ok {
            Ok(serialize::Decodable::decode(&mut decoder).unwrap())
        } else {
            Err(serialize::Decodable::decode(&mut decoder).unwrap())
        }
    }

    fn post_request<'a, U: Encodable<Encoder<'a>, io::IoError>,
                    D: Decodable<Decoder, DecoderError>>
                    (&self, path: &str, request: &U) -> R<D> {
        let mut req = self.make_request(Post, path);
        let response_json = json::encode(request);
        let bytes = response_json.as_bytes();
        req.headers_mut().set(ContentLength(bytes.len()));
        let mut stream = req.start().unwrap();
        stream.write(bytes).unwrap();
        // TODO(rictic): do something like the following to encode the json
        // directly onto the wire rather than going through an intermediate
        // string.
        // let mut encoder = Encoder::new(&mut stream);
        // request.encode(&mut encoder).unwrap();

        let mut result = stream.send().unwrap();
        let mut decoder = Decoder::new(json::from_reader(&mut result).unwrap());
        if result.status == status::StatusCode::Ok {
            Ok(serialize::Decodable::decode(&mut decoder).unwrap())
        } else {
            Err(serialize::Decodable::decode(&mut decoder).unwrap())
        }
    }

    fn create_room(&self) -> R<String> {
        let r : R<CreateRoomResponse> = self.do_request("/api/create_room");
        r.map(|resp| resp.room_code)
    }

    fn list_players(&self, room_code: String) -> R<Box<Vec<Player>>> {
        let r : R<ListRoomResponse> = self.post_request(
            "/api/list_room", &ListRoomRequest {
                room_code: room_code
            }
        );
        r.map(|lrr| box lrr.players)
    }

    fn join_room(&self, room_code: String, name: &str) ->
            R<JoinRoomResponse> {
        let r : R<JoinRoomResponse> = self.post_request(
            "/api/join_room", &JoinRoomRequest {
                room_code: room_code,
                name: name.to_string(),
            }
        );
        r
    }

    fn pick_color(&self, room_code: String, name: &str, color: Color) ->
            R<PickColorResponse> {
        let r : R<PickColorResponse> = self.post_request(
            "/api/pick_color", &PickColorRequest {
                room_code: room_code,
                name: name.to_string(),
                color: color,
            }
        );
        r
    }

    fn kick_player(&self, room_code: String, to_kick: &str)
            -> R<KickPlayerResponse> {
        let r : R<KickPlayerResponse> = self.post_request(
            "/api/kick_player", &KickPlayerRequest {
                room_code: room_code,
                to_kick: to_kick.to_string(),
            }
        );
        r
    }

}

impl Drop for TestServer {
    fn drop(&mut self) {
        self.listen_serv.close().unwrap();
    }
}
