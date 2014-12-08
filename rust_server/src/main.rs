extern crate url;
extern crate hyper;
extern crate serialize;
extern crate mime;
extern crate getopts;

mod betrayal_server;
mod game_data;
mod framework;

#[cfg(test)]
mod erl_client;
#[cfg(test)]
mod integration_tests;

#[cfg(not(test))]
mod main {
    use hyper;
    use getopts::{optflag,getopts};
    use std::os;
    use std::io;

    use betrayal_server::BetrayalServer;

    #[main]
    fn main() {
        let opts = &[
            optflag("a", "address", "set the address to listen on"),
            optflag("p", "port", "set the port to listen on")
        ];

        let matches = match getopts(os::args().tail(), opts) {
            Ok(m) => m,
            Err(f) => panic!(f.to_string()),
        };
        let addr_str = matches.opt_str("a").unwrap_or("127.0.0.1".to_string());
        let addr : io::net::ip::IpAddr = match from_str(addr_str.as_slice()) {
            Some(a) => a,
            None => panic!("Unable to parse address"),
        };
        let port = match from_str(matches.opt_str("p").unwrap_or("1337".to_string()).as_slice()) {
            Some(p) => p,
            None => panic!("Unable to parse port"),
        };

        let betrayal_server = BetrayalServer::new();
        hyper::server::Server::http(addr, port).listen(betrayal_server).unwrap();
        println!("Listening on port {}", port);
    }
}


