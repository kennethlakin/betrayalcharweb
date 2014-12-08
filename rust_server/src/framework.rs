use std::io;
use serialize::{Encodable, Decodable, json};
use serialize::json::{Encoder, Decoder, DecoderError};
use std::str;
use hyper::header::common::content_type;
use hyper::server::{Request, Response};
use hyper::status::StatusCode::{BadRequest};
use hyper::status;
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
