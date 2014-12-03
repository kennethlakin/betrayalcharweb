extern crate url;
extern crate hyper;
extern crate serialize;
extern crate mime;

use game_data::{Player, Color, Room};
use std::sync::{Mutex, Arc};
use std::collections::HashMap;
use serialize::{Encodable};
use std::rand;
use hyper::server::{Request, Response};
use hyper::status::{BadRequest, NotFound, NotImplemented};
use hyper::status;
use hyper::uri;
use framework::{HttpResult, error, post_response, respond_with, write_out};

#[deriving(Encodable, Decodable, Show)]
pub struct CreateRoomResponse  {
    pub room_code: String,
}

#[deriving(Decodable, Encodable, Show)]
pub struct JoinRoomRequest {
    pub name : String,
    pub room_code: String
}

#[deriving(Encodable, Decodable, Show)]
pub struct JoinRoomResponse;

#[deriving(Decodable, Encodable, Show)]
pub struct ListRoomRequest {
    pub room_code: String
}

#[deriving(Encodable, Decodable, Show)]
pub struct ListRoomResponse {
    pub players: Vec<Player>
}

#[deriving(Decodable, Encodable, Show)]
pub struct PickColorRequest {
    pub room_code: String,
    pub name: String,
    pub color: Color,
}

#[deriving(Encodable, Decodable, Show)]
pub struct PickColorResponse;

#[deriving(Decodable, Encodable, Show)]
pub struct KickPlayerRequest {
    pub room_code: String,
    pub to_kick: String,
}

#[deriving(Encodable, Decodable, Show)]
pub struct KickPlayerResponse;

pub struct BetrayalServer {
    active_rooms: Mutex<HashMap<String, Arc<Mutex<Room>>>>,
}

impl BetrayalServer {
    pub fn new() -> BetrayalServer {
        BetrayalServer {
            active_rooms: Mutex::new(HashMap::new()),
        }
    }

    // request handlers

    fn create_room(&self, _ : Request, res : Response) ->
            Result<(), &'static str> {
        respond_with(res, &CreateRoomResponse {
            room_code: self.get_unique_room_name(),
        })
    }

    fn join_room(&self, j: JoinRoomRequest) -> HttpResult<JoinRoomResponse> {
        let room_lock = try!(self.get_room(j.room_code));
        let mut room = room_lock.lock();
        match room.get_player(j.name.as_slice()) {
            Some(_) => return error(NotFound, "That name is already taken"),
            _ => (),
        };
        room.players.push(Player::new(j.name.clone()));
        Ok(JoinRoomResponse)
    }

    fn list_room(&self, lrr: ListRoomRequest) ->
            HttpResult<ListRoomResponse> {
        let room_lock = try!(self.get_room(lrr.room_code));
        Ok(ListRoomResponse { players: room_lock.lock().players.clone(), })
    }

    fn pick_color(&self, pcr: PickColorRequest) ->
            HttpResult<PickColorResponse> {
        let room_lock = try!(self.get_room(pcr.room_code));
        let mut room = room_lock.lock();
        for p in room.players.iter() {
            if p.color == Some(pcr.color) {
                return error(BadRequest, "Someone else already has that color");
            }
        }
        let player = match room.get_player(pcr.name.as_slice()) {
            Some(p) => p,
            None => return error(status::NotFound, "Player not found"),
        };
        player.color = Some(pcr.color);
        Ok(PickColorResponse)
    }

    fn kick_player(&self, kpr: KickPlayerRequest) ->
            HttpResult<KickPlayerResponse> {
        let room_lock = try!(self.get_room(kpr.room_code));
        let mut room = room_lock.lock();
        let mut player_index : Option<uint> = None;
        for (idx, p) in room.players.iter().enumerate() {
            if p.name == kpr.to_kick {
                player_index = Some(idx);
                break;
            }
        }
        match player_index {
            None => return error(NotFound, "Player not found"),
            Some(idx) => room.players.remove(idx).unwrap()
        };
        Ok(KickPlayerResponse)
    }

    // private convenience functions

    fn get_room(&self, room_code: String) -> HttpResult<Arc<Mutex<Room>>> {
        Ok(match self.active_rooms.lock().get(&room_code) {
            Some(r) => r,
            None => return error(NotFound, "Room not found"),
        }.clone())
    }

    fn get_unique_room_name(&self) -> String {
        let mut size : uint = 1;
        let mut active_rooms = self.active_rooms.lock();

        loop {
            for _ in range(0u, 10u) {
                let candidate = generate_room_name(size);
                if !active_rooms.contains_key(&candidate) {
                    active_rooms.insert(candidate.clone(),
                                        Arc::new(Mutex::new(Room::new())));
                    return candidate;
                }
            }
            size += 1;
        }
    }
}


impl hyper::server::Handler for BetrayalServer {
    fn handle(&self, req: Request, res: Response) {
        match req.uri.clone() {
            uri::RequestUri::AbsolutePath(s) => {
                let handled = match s.as_slice() {
                    "/api/create_room" => self.create_room(req, res),
                    "/api/join_room" => post_response(req, res, |jrr| self.join_room(jrr)),
                    "/api/list_room" => post_response(req, res, |lrr| self.list_room(lrr)),
                    "/api/pick_color" => post_response(req, res, |pcr| self.pick_color(pcr)),
                    "/api/kick_player" => post_response(req, res, |kpr| self.kick_player(kpr)),
                    "/api/set_stats" => write_out(
                        "Not implemented\n", NotImplemented, res),
                    _ => write_out("Not found\n", NotFound, res),
                };
                if handled.is_err() {
                    println!("Error handling request - {}", handled);
                }
            }
            _ => {
                let _ = write_out(
                    "Improper request", BadRequest, res);
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
