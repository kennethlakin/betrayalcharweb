use std::collections::HashSet;
use serialize::{Decodable, json};
use serialize::json::Json;
use serialize::json::{Decoder, DecoderError};
use hyper::method::Method::{Post, Get};
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
enum Color {
    purple, green, white, blue, red, orange,
}

#[deriving(Decodable, Show)]
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
struct JoinRoomResponse {
    gameid: String,
    playerid: String,
}

// -record(playerrec, {id::binary(), player::player()}).
// -record(gamerec, {gameid::gameid(), players::[player()]}).


#[test]
fn test_create_room() {
    let host = url::Host::Domain("127.0.0.1".to_string());
    let ec = ErlClient::new(host, 8080);

    let mut hs = HashSet::new();
    for _ in range(0, 100u) {
        let room_code = ec.create_room().unwrap();
        assert_eq!(false, hs.contains(&room_code.clone()));
        hs.insert(room_code);
    }
}


#[test]
fn join_room_works() {
    let host = url::Host::Domain("127.0.0.1".to_string());
    let ec = ErlClient::new(host, 8080);

    let room_code = ec.create_room().unwrap();
    assert_eq!(0, ec.list_players(room_code.clone()).unwrap().len());
    let player_id = ec.join_room(room_code.clone(), "TestUser").unwrap();

    // let players = ec.list_players(room_code.clone()).unwrap();
    // assert_eq!(1, players.len());
    // assert_eq!("Test User".to_string(), players[0].name);
    // assert_eq!(None, players[0].color);
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
        let req = self.make_request(method, path, query);

        let mut res = req.start().unwrap().send().unwrap();
        // Read the Response.
        let body = res.read_to_string().unwrap();

        println!("body: {}", body);
        assert_eq!(status::StatusCode::Ok, res.status);
        if res.status == status::StatusCode::Ok {
            Ok(json::decode(body.as_slice()).unwrap())
        } else {
            Err(json::decode(body.as_slice()).unwrap())
        }
    }

//     fn post_request<'a, U: Encodable<Encoder<'a>, io::IoError>,
//                     D: Decodable<Decoder, DecoderError>>
//                     (&self, path: &str, request: &U) -> R<D> {
//         let req = self.make_request(Post, path);
//         let mut stream = req.start().unwrap();
//         stream.write(json::encode(request).as_bytes()).unwrap();
//         let mut result = stream.send().unwrap();
//         let status = result.status;
//         let body = result.read_to_string().unwrap();
//         if status == status::Ok {
//             Ok(json::decode(body.as_slice()).unwrap())
//         } else {
//             Err(json::decode(body.as_slice()).unwrap())
//         }
//     }


    fn create_room(&self) -> R<String> {
        self.do_request::<CreateGameResponse>(Post,
            "character", "action=creategame").map(|resp| resp.room_code)
    }

    fn list_players(&self, room_code: String) -> R<Vec<Player>> {
        let mut s = "action=getplayers".to_string();
        s.push_str("&gameid=");
        s.push_str(room_code.as_slice());
        self.do_request::<(Game, Vec<Player>)>(Get, "character", s.as_slice())
            .map(|resp| {let (_, players) = resp; players})
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

//     fn pick_color(&self, room_code: String, name: &str, color: Color) ->
//             R<PickColorResponse> {
//         let r : R<PickColorResponse> = self.post_request(
//             "/api/pick_color", &PickColorRequest {
//                 room_code: room_code,
//                 name: name.to_string(),
//                 color: color,
//             }
//         );
//         r
//     }

//     fn kick_player(&self, room_code: String, to_kick: &str)
//             -> R<KickPlayerResponse> {
//         let r : R<KickPlayerResponse> = self.post_request(
//             "/api/kick_player", &KickPlayerRequest {
//                 room_code: room_code,
//                 to_kick: to_kick.to_string(),
//             }
//         );
//         r
//     }

}
