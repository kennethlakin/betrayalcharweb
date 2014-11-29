extern crate hyper;
extern crate serialize;

use std::sync::{Mutex};
use std::collections::HashSet;
use serialize::json;
use std::rand;
use std::io::net::ip::Ipv4Addr;
// use hyper::{Get, Post};
use hyper::server::{Request, Response};

#[deriving(Encodable)]
pub struct CreateRoomResponse  {
    room_code: String,
}

pub struct BetrayalServer {
    active_rooms: Mutex<HashSet<String>>,
}

impl BetrayalServer {
    fn new() -> BetrayalServer {
        BetrayalServer {
            active_rooms: Mutex::new(HashSet::new()),
        }
    }
    fn handle_create_room(&self, _: Request) -> &[u8] {
        let encoded = json::encode(&CreateRoomResponse {
            room_code: self.get_unique_room_name(),
        });
        encoded.as_bytes().clone()
    }
    fn get_unique_room_name(&self) -> String {
        let mut size : uint = 1;
        let mut active_rooms = self.active_rooms.lock();

        loop {
            for _ in range(0u, 10u) {
                let candidate = generate_room_name(size);
                if !active_rooms.contains(&candidate) {
                    active_rooms.insert(candidate.clone());
                    return candidate;
                }
            }
            size += 1;
        }

    }
}

impl hyper::server::Handler for BetrayalServer {
    fn handle(&self, req: Request, mut res: Response) {
        *res.status_mut() = hyper::status::Ok;
        let r = res.start();
        if r.is_err() {
            return; // Lost the connection, no worries though.
        }
        let mut res = r.unwrap();

        let _ = res.write(self.handle_create_room(req));

        let _ = res.end();
    }
}

fn generate_room_name(len : uint) -> String {
    let letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];

    let mut s = "".to_string();
    for _ in range(0, len) {
        s.push_str(letters[rand::random::<uint>() % letters.len()]);
    }

    return s
}


fn main() {
    let server = hyper::server::Server::http(Ipv4Addr(127, 0, 0, 1), 1337);
    server.listen(BetrayalServer::new()).unwrap();
}
