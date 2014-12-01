use std::collections::HashSet;
use std::io;
use std::rand;
use serialize::{Encodable, Decodable, json};
use serialize::json::{Encoder, Decoder, DecoderError};
use hyper::method::Method::{Get, Post};
use hyper::client::{Request};
use hyper;

use betrayal_server::{BetrayalServer, CreateRoomResponse, JoinRoomResponse, JoinRoomRequest};

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
        let res = stream.send().unwrap().read_to_string().unwrap();
        json::decode(res.as_slice()).unwrap()
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
        let resp : CreateRoomResponse = ts.do_request("/api/create_room");
        assert_eq!(false, hs.contains(&resp.room_code));
        hs.insert(resp.room_code);
    }
}

#[test]
fn join_room_works() {
    let ts = TestServer::new();

    let resp : CreateRoomResponse = ts.do_request("/api/create_room");
    let _ : JoinRoomResponse = ts.post_request(
        "/api/join_room", &JoinRoomRequest {
            room_code: resp.room_code,
            name: "Test User".to_string(),
        }
    );
}
