extern crate serialize;

#[deriving(Show, Encodable, Decodable, Clone, Rand, Eq, PartialEq)]
pub enum Color {
    Purple, Green, White, Blue, Red, Yellow
}

#[deriving(Show, Encodable, Decodable, Clone)]
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

