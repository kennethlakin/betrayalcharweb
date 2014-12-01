extern crate url;
extern crate hyper;
extern crate serialize;
extern crate mime;

use game_data::{Player, Color, Room};
use std::sync::{Mutex};
use std::collections::HashMap;
use std::io;
use serialize::{Encodable, Decodable, json};
use serialize::json::{Encoder, Decoder, DecoderError};
use std::rand;
use std::str;
// use hyper::{Get, Post};
use hyper::header::common::content_type;
use hyper::server::{Request, Response};
use hyper::status;
use hyper::uri;
use mime::{Mime};

#[deriving(Encodable, Decodable)]
pub struct CreateRoomResponse  {
    pub room_code: String,
}

#[deriving(Decodable, Encodable)]
pub struct JoinRoomRequest {
    pub name : String,
    pub room_code: String
}

#[deriving(Encodable, Decodable)]
pub struct JoinRoomResponse;

#[deriving(Decodable)]
pub struct ListRoomRequest {
    pub room_code: String
}

#[deriving(Encodable)]
pub struct ListRoomResponse {
    pub players: Vec<Player>
}

#[deriving(Decodable)]
pub struct PickColorRequest {
    pub room_code: String,
    pub name: String,
    pub color: Color,
}

pub struct BetrayalServer {
    active_rooms: Mutex<HashMap<String, Mutex<Room>>>,
}


impl BetrayalServer {
    pub fn new() -> BetrayalServer {
        BetrayalServer {
            active_rooms: Mutex::new(HashMap::new()),
        }
    }

    fn get_unique_room_name(&self) -> String {
        let mut size : uint = 1;
        let mut active_rooms = self.active_rooms.lock();

        loop {
            for _ in range(0u, 10u) {
                let candidate = generate_room_name(size);
                if !active_rooms.contains_key(&candidate) {
                    active_rooms.insert(candidate.clone(), Mutex::new(Room::new()));
                    return candidate;
                }
            }
            size += 1;
        }
    }

    fn read_json_post<T: Decodable<Decoder, DecoderError>>(&self, mut req : Request) -> io::IoResult<T> {
        req.read_to_end().and_then(|s|
            str::from_utf8(s.as_slice()).ok_or(io::IoError {
                kind: io::InvalidInput,
                desc: "Not valid utf-8",
                detail: None
            }).and_then(|s|
                json::decode::<T>(s).map_err(|_| io::IoError {
                    kind: io::InvalidInput,
                    desc: "Invalid input",
                    detail:None
                })
            )
        )
    }

    fn create_room(&self, _ : Request, res : Response) -> Result<(), &str> {
        self.respond_with(res, &CreateRoomResponse {
            room_code: self.get_unique_room_name(),
        })
    }

    fn join_room(&self, req : Request, res : Response) -> Result<(), &str> {
        let maybe_req = self.read_json_post::<JoinRoomRequest>(req);
        let req = match maybe_req {
            Ok(ref r) => r,
            Err(ref e) => return self.write_out(e.desc, status::BadRequest, res)
        };
        let rooms = self.active_rooms.lock();
        let mut room = match rooms.get(&req.room_code) {
            Some(room) => room,
            None => return self.write_out(
                "Room not found", status::NotFound, res),
        }.lock();
        match room.get_player(req.name.as_slice()) {
            Some(_) => return self.write_out("That name is already taken",
                    status::NotFound, res),
            _ => (),
        };
        let mut player = Player::new(req.name.clone());
        player.color = rand::random();
        room.players.push(player);

        println!("Players in room {} - {}", req.room_code, room.players);
        self.respond_with(res, &JoinRoomResponse)
    }

    fn list_room(&self,
                 req : Request, res : Response) -> Result<(), &str> {
        let maybe_req = self.read_json_post::<ListRoomRequest>(req);
        let req = match maybe_req {
            Ok(ref r) => r,
            Err(ref e) => return self.write_out(e.desc, status::BadRequest, res)
        };
        let rooms = self.active_rooms.lock();
        let room = match rooms.get(&req.room_code) {
            Some(room) => room,
            None => return self.write_out(
                "Room not found", status::NotFound, res),
        }.lock();
        self.respond_with(res, &ListRoomResponse {
            players: room.players.clone(),
        })
    }

    fn pick_color(&self, req : Request, res : Response) -> Result<(), &str> {
        let maybe_req = self.read_json_post::<PickColorRequest>(req);
        let req = match maybe_req {
            Ok(ref r) => r,
            Err(ref e) => return self.write_out(e.desc, status::BadRequest, res)
        };
        let rooms = self.active_rooms.lock();
        let room = match rooms.get(&req.room_code) {
            Some(room) => room,
            None => return self.write_out(
                "Room not found", status::NotFound, res),
        }.lock();

        let player : &Player = match room.get_player(req.name.as_slice()) {
            Some(p) => p,
            None => return self.write_out(
                "Player not found", status::NotFound, res),
        };
        println!("Sure would like to set {} to {}", player.color, req.color);
        // player.color = Some(req.color);
        self.write_out(
            "Good request, not implemented yet though!",
            status::NotImplemented, res)
    }

    fn respond_with<'a, T: Encodable<Encoder<'a>, io::IoError>>(
            &self, mut res : Response, data : &T) -> Result<(), &str> {
        let mime : mime::Mime = from_str("application/json").unwrap();
        let json = content_type::ContentType(mime);
        res.headers_mut().set(json);
        self.write_out(json::encode(data).as_slice(), status::Ok, res)
    }

    fn write_out(&self, data : &str,
                 status : status::StatusCode, mut res: Response
                 ) -> Result<(), &str> {
        *res.status_mut() = status;
        let r = res.start();
        if r.is_err() {
            return Err("Unable to start response");
        }
        let mut outstream = r.unwrap();

        let maybe_err = outstream.write(data.as_bytes());
        if maybe_err.is_err() {
            return Err("Unable to write out");
        }
        let maybe_err = outstream.end();
        if maybe_err.is_err() {
            return Err("Unable to close outstream");
        }
        Ok(())
    }
}

impl hyper::server::Handler for BetrayalServer {
    fn handle(&self, req: Request, res: Response) {
        match req.uri.clone() {
            uri::RequestUri::AbsolutePath(s) => {
                let handled = match s.as_slice() {
                    "/api/create_room" => self.create_room(req, res),
                    "/api/join_room" => self.join_room(req, res),
                    "/api/list_room" => self.list_room(req, res),
                    "/api/pick_color" => self.pick_color(req, res),
                    "/api/kick_player" => self.write_out(
                        "Not implemented\n", status::NotImplemented, res),
                    "/api/set_stats" => self.write_out(
                        "Not implemented\n", status::NotImplemented, res),
                    _ => self.write_out("Not found\n", status::NotFound, res),
                };
                if handled.is_err() {
                    println!("Error handling request - {}", handled);
                }
            }
            _ => {
                let _ = self.write_out(
                    "Improper request", status::BadRequest, res);
            }
        };
    }
}

fn generate_room_name(len : uint) -> String {
    let letters = [
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

    let mut s = String::with_capacity(len);
    for _ in range(0, len) {
        s.push(letters[rand::random::<uint>() % letters.len()]);
    }

    return s
}
