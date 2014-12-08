use std::collections::HashSet;
use serialize::{Decodable, json};
use serialize::json::{Decoder, DecoderError};
use hyper::method::Method::{Delete, Post, Get};
use hyper::client::{Request};
use hyper::status;
use url;
use hyper;

use framework::ErrorResponse;

#[deriving(Decodable, Show)]
struct CreateGameResponse {
    room_code: String,
}

#[deriving(Decodable, Show)]
struct Player {
    gameid: String,
    playerid: String,
    name: String,
    character: Character
}

#[deriving(Decodable, Show)]
struct Character {
    color: Color,
    variant: Variant,
    stats: Stats
}

#[deriving(Decodable, Show)]
#[allow(non_camel_case_types)]
enum Color {
    purple, green, white, blue, red, orange,
}

#[deriving(Decodable, Show)]
#[allow(non_camel_case_types)]
enum Variant {
    front, back,
}

#[deriving(Decodable, Show)]
struct Game {
    gameid: String,
}

#[deriving(Decodable, Show)]
struct Stats {
    speed: int,
    might: int,
    sanity: int,
    knowledge: int,
}

#[deriving(Decodable, Show)]
struct ListPlayersResponse {
    gameid: String,
    players: Vec<BasicPlayer>,
}

#[deriving(Decodable, Show)]
struct BasicPlayer {
    name: String,
    id: String,
}

#[deriving(Decodable, Show)]
struct JoinRoomResponse {
    gameid: String,
    playerid: String,
}

// const DOMAIN : &'static str = "localhost";
const DOMAIN : &'static str = "10.0.1.76";
const PORT : u16 = 8080;

#[test]
fn test_create_room() {
    let host = url::Host::Domain(DOMAIN.to_string());
    let ec = ErlClient::new(host, PORT);

    let mut hs = HashSet::new();
    for _ in range(0, 100u) {
        let room_code = ec.create_room().unwrap();
        assert_eq!(false, hs.contains(&room_code.clone()));
        hs.insert(room_code);
    }
}


#[test]
fn join_room_works() {
    let host = url::Host::Domain(DOMAIN.to_string());
    let ec = ErlClient::new(host, PORT);

    let room_code = ec.create_room().unwrap();
    assert_eq!(0, ec.list_players(room_code.clone()).unwrap().len());
    let player_id = ec.join_room(room_code.clone(), "TestUser").unwrap();

    let players = ec.list_players(room_code.clone()).unwrap();
    assert_eq!(1, players.len());
    assert_eq!("TestUser".to_string(), players[0].name);
    assert_eq!(player_id, players[0].id);
}

#[test]
fn update_info() {
    let host = url::Host::Domain(DOMAIN.to_string());
    let ec = ErlClient::new(host, PORT);

    let room_code = ec.create_room().unwrap();
    assert_eq!(0, ec.list_players(room_code.clone()).unwrap().len());
    let player_id = ec.join_room(room_code.clone(), "TestUser").unwrap();

    ec.set_stats(room_code.clone(), player_id.clone(), [4,4,4,4]).unwrap();

    ec.pick_color(
        room_code.clone(), player_id.clone(), Color::red, Variant::front)
        .unwrap();

    let players = ec.list_players(room_code.clone()).unwrap();
    assert_eq!(1, players.len());
    // assert_eq!(Some(Color::red), players[0].color);

    ec.join_room(room_code.clone(), "JoeBob").unwrap();
    assert_eq!(2, ec.list_players(room_code.clone()).unwrap().len());

    // TODO - uncomment this
    // ec.pick_color(
    //     room_code.clone(), player_id.clone(), Color::red, Variant::back)
    //     .unwrap_err();
}

#[test]
fn kick_player() {
    let host = url::Host::Domain(DOMAIN.to_string());
    let ec = ErlClient::new(host, PORT);

    let room_code = ec.create_room().unwrap();
    assert_eq!(0, ec.list_players(room_code.clone()).unwrap().len());
    let player_id = ec.join_room(room_code.clone(), "TestUser").unwrap();
    assert_eq!(1, ec.list_players(room_code.clone()).unwrap().len());
    ec.kick_player(room_code.clone(), player_id).unwrap();
    assert_eq!(0, ec.list_players(room_code.clone()).unwrap().len());
}

type R<T> = Result<T, ErrorResponse>;

struct ErlClient {
    host: url::Host,
    port: u16,
}
impl ErlClient {
    fn new(host: url::Host, port: u16) -> ErlClient {
        ErlClient {
            host: host,
            port: port
        }
    }

    fn make_request(&self, method: hyper::method::Method, path: &str, query: &str)
            -> Request<hyper::net::Fresh> {
        let url = hyper::Url {
            scheme: "http".to_string(),
            scheme_data: url::SchemeData::Relative(url::RelativeSchemeData{
                username: "".to_string(),
                password: None,
                host: self.host.clone(),
                port: Some(self.port),
                default_port: None,
                path: vec![path.to_string()]
            }),
            query: Some(query.to_string()),
            fragment: None
        };
        Request::new(
            method, url).unwrap()
    }


    fn do_request<T: Decodable<Decoder, DecoderError>>(&self, method: hyper::method::Method, path: &str, query: &str)
            -> Result<T, ErrorResponse> {
        println!("{} request: {}", method, query);
        let req = self.make_request(method, path, query);

        let mut res = req.start().unwrap().send().unwrap();
        // Read the Response.
        let body = res.read_to_string().unwrap();

        println!("response {}: {}\n", res.status, body);
        if res.status == status::StatusCode::Ok {
            Ok(json::decode(body.as_slice()).unwrap())
        } else {
            Err(json::decode(body.as_slice()).unwrap())
        }
    }

    fn create_room(&self) -> R<String> {
        self.do_request::<CreateGameResponse>(Post,
            "character", "action=creategame").map(|resp| resp.room_code)
    }

    fn list_players(&self, room_code: String) -> R<Vec<BasicPlayer>> {
        let mut s = "action=getplayers".to_string();
        s.push_str("&gameid=");
        s.push_str(room_code.as_slice());
        self.do_request::<ListPlayersResponse>(Get, "character", s.as_slice())
            .map(|resp| resp.players)
    }

    fn join_room(&self, room_code: String, name: &str) -> R<String> {
        let mut s = "action=addplayer".to_string();
        s.push_str("&gameid=");
        s.push_str(room_code.as_slice());
        s.push_str("&playername=");
        s.push_str(name);
        self.do_request::<JoinRoomResponse>(Post, "character", s.as_slice())
            .map(|jrr| jrr.playerid)
    }

    fn pick_color(&self,
            room_code: String, player: String, color: Color, variant: Variant)
            -> R<()> {
        let mut s = "action=setcolor".to_string();
        s.push_str("&gameid=");
        s.push_str(room_code.as_slice());
        s.push_str("&playerid=");
        s.push_str(player.as_slice());
        s.push_str("&color=");
        s.push_str(color.to_string().as_slice());
        s.push_str("&variant=");
        s.push_str(variant.to_string().as_slice());
        self.do_request::<String>(Post, "character", s.as_slice()).map(|_| {})
    }

    fn kick_player(&self, room_code: String, to_kick: String) -> R<()> {
        let mut s = "action=kickplayer".to_string();
        s.push_str("&gameid=");
        s.push_str(room_code.as_slice());
        s.push_str("&playerid=");
        s.push_str(to_kick.as_slice());
        self.do_request::<String>(Delete, "character", s.as_slice())
            .map(|_| {})
    }

    fn set_stats(&self,
            room_code: String, player: String, stats: [int, ..4]) -> R<()> {
        let mut s = "action=setstats".to_string();
        s.push_str("&gameid=");
        s.push_str(room_code.as_slice());
        s.push_str("&playerid=");
        s.push_str(player.as_slice());
        s.push_str("&speed=");
        s.push_str(stats[0].to_string().as_slice());
        s.push_str("&might=");
        s.push_str(stats[1].to_string().as_slice());
        s.push_str("&sanity=");
        s.push_str(stats[2].to_string().as_slice());
        s.push_str("&knowledge=");
        s.push_str(stats[3].to_string().as_slice());
        self.do_request::<String>(Post, "character", s.as_slice()).map(|_| {})
    }
}
