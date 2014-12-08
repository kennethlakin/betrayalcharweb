use serialize::{Decodable, json};
use serialize::json::{Decoder, DecoderError};
use hyper::method::Method::{Post};
use hyper::client::{Request};
use hyper::status;
use url;
use hyper;

use framework::ErrorResponse;

#[deriving(Decodable, Show)]
struct CreateGameResponse {
    room_code: String,
}

#[test]
fn test_create_room() {
    let host = url::Host::Ipv6(url::Ipv6Address::parse("2601:9:4f00:1191:f66d:4ff:fe53:d79c").unwrap());
    let ec = ErlClient::new(host, 9909);

    let _ : CreateGameResponse = ec.do_request(Post, "character", "action=creategame").unwrap();
}

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

//     fn create_room(&self) -> R<String> {
//         let r : R<CreateRoomResponse> = self.do_request("/api/create_room");
//         r.map(|resp| resp.room_code)
//     }

//     fn list_players(&self, room_code: String) -> R<Box<Vec<Player>>> {
//         let r : R<ListRoomResponse> = self.post_request(
//             "/api/list_room", &ListRoomRequest {
//                 room_code: room_code
//             }
//         );
//         r.map(|lrr| box lrr.players)
//     }

//     fn join_room(&self, room_code: String, name: &str) ->
//             R<JoinRoomResponse> {
//         let r : R<JoinRoomResponse> = self.post_request(
//             "/api/join_room", &JoinRoomRequest {
//                 room_code: room_code,
//                 name: name.to_string(),
//             }
//         );
//         r
//     }

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
