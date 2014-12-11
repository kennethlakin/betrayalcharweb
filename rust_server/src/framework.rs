use std::io;
use serialize;
use serialize::{Encodable, Decodable, json};
use serialize::json::{Encoder, Decoder, DecoderError};
use hyper::header::common::content_type;
use hyper::server::{Request, Response};
use hyper::status::StatusCode::{BadRequest};
use hyper::status;
use hyper::header::common::content_length::ContentLength;
use mime::{Mime};
use mime;

pub struct HTTPError {
    pub status: status::StatusCode,
    pub error: String,
}

pub type HttpResult<T> = Result<T, HTTPError>;

#[deriving(Decodable, Encodable, Show)]
pub struct ErrorResponse {
    error_message: String,
}

pub fn error<X>(status: status::StatusCode, message: &'static str)-> Result<X, HTTPError> {
    Err(HTTPError{ status: status, error: message.to_string()})
}

pub fn post_response<'a, D: Encodable<Encoder<'a>, io::IoError>,
                     U: Decodable<Decoder, DecoderError>>
        (req: Request, res: Response, handler: |U| -> Result<D, HTTPError>) ->
        Result<(), &'static str> {
    let u = read_json_post::<U>(req).map_err(|e|
        HTTPError{status: BadRequest, error:e.to_string()});
    match u.and_then(handler) {
        Ok(d) => respond_with(res, &d),
        Err(HTTPError{status: st, error: er}) =>
            respond_with_code(res, st, &ErrorResponse{error_message: er})
    }
 }

fn read_json_post<T: Decodable<Decoder, DecoderError>>(mut req : Request) ->
        io::IoResult<T> {
    let json = try!(json::from_reader(&mut req).map_err(|_| {
        io::IoError {
            kind: io::InvalidInput,
            desc: "Unable to parse json in request",
            detail: None,
        }
    }));
    let mut decoder = Decoder::new(json);
    serialize::Decodable::decode(&mut decoder).map_err(|_| {
        io::IoError {
            kind: io::InvalidInput,
            desc: "Unable to map the json in the request to the expected type",
            detail: None,
        }
    })
}

fn respond_with_code<'a, T: Encodable<Encoder<'a>, io::IoError>>(
        mut res : Response, status: status::StatusCode, data : &T)
        -> Result<(), &'static str> {
    let mime : mime::Mime = from_str("application/json").unwrap();
    let json = content_type::ContentType(mime);
    res.headers_mut().set(json);
    write_out(json::encode(data).as_slice(), status, res)
}

pub fn respond_with<'a, T: Encodable<Encoder<'a>, io::IoError>>(
            res: Response, data: &T) -> Result<(), &'static str> {
    respond_with_code(res, status::StatusCode::Ok, data)
}

pub fn write_out(data: &str,
         status: status::StatusCode, mut res: Response
         ) -> Result<(), &'static str> {
    let bytes = data.as_bytes();
    *res.status_mut() = status;
    res.headers_mut().set(ContentLength(bytes.len()));
    let r = res.start();
    if r.is_err() {
        return Err("Unable to start response");
    }
    let mut outstream = r.unwrap();

    let maybe_err = outstream.write(bytes);
    if maybe_err.is_err() {
        return Err("Unable to write out");
    }
    let maybe_err = outstream.end();
    if maybe_err.is_err() {
        return Err("Unable to close outstream");
    }
    Ok(())
}
