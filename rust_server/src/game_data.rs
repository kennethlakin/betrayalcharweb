extern crate serialize;

use serialize::json;

#[deriving(Show, Encodable, Decodable, Clone, Rand)]
pub enum Color {
    PURPLE, GREEN, WHITE, BLUE, RED, YELLOW
}

impl json::ToJson for Color {
    fn to_json(&self) -> json::Json {
        let s = match *self {
            Color::PURPLE => "purple",
            Color::GREEN => "green",
            Color::WHITE => "white",
            Color::BLUE => "blue",
            Color::RED => "red",
            Color::YELLOW => "yellow",
        };
        json::String(s.to_string())
    }
}

#[deriving(Show, Encodable, Clone)]
pub struct Player {
    pub name : String,
    pub color : Option<Color>
}

impl Player {
    pub fn new(name: String) -> Player {
        Player {
            name: name,
            color: None
        }
    }
}

pub struct Room {
    pub players : Vec<Player>
}

impl Room {
    pub fn new() -> Room {
        Room {
            players: vec![],
        }
    }

    pub fn get_player<'a>(&'a self, name: &str) -> Option<&'a Player> {
        for ref player in self.players.iter() {
            if player.name.as_slice() == name {
                return Some(*player);
            }
        }
        None
    }
}

