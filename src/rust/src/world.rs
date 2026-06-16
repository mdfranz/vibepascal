use configparser::ini::Ini;
use rand::Rng;

use crate::state::GameState;
use crate::types::{Item, Room, MAX_ITEMS, MAX_ROOMS};

pub fn load_world(s: &mut GameState, path: &str) {
    let mut cfg = Ini::new();
    if let Err(e) = cfg.load(path) {
        eprintln!("Error loading world: {}", e);
        return;
    }

    // First pass: create rooms (configparser lowercases section names)
    for i in 1..=MAX_ROOMS {
        let section = format!("room{}", i);
        if let Some(name) = cfg.get(&section, "name") {
            let description = cfg.get(&section, "description").unwrap_or_default();
            s.rooms[i] = Some(Room {
                id: i,
                name,
                description,
                north: 0,
                south: 0,
                east: 0,
                west: 0,
            });
        }
    }

    // Second pass: link exits by room ID
    for i in 1..=MAX_ROOMS {
        if s.rooms[i].is_none() {
            continue;
        }
        let section = format!("room{}", i);
        let n = parse_room_id(&cfg, &section, "north");
        let so = parse_room_id(&cfg, &section, "south");
        let e = parse_room_id(&cfg, &section, "east");
        let w = parse_room_id(&cfg, &section, "west");

        if let Some(room) = s.rooms[i].as_mut() {
            room.north = n;
            room.south = so;
            room.east = e;
            room.west = w;
        }
    }

    // Load items
    for i in 1..=MAX_ITEMS {
        let section = format!("item{}", i);
        if let Some(name) = cfg.get(&section, "name") {
            let description = cfg.get(&section, "description").unwrap_or_default();
            let details = cfg.get(&section, "details").unwrap_or_default();
            let location = cfg
                .get(&section, "location")
                .and_then(|v| v.parse::<i32>().ok())
                .unwrap_or(0);
            let is_takeable = cfg
                .get(&section, "istakeable")
                .and_then(|v| v.parse::<i32>().ok())
                .unwrap_or(0)
                == 1;
            s.items[i] = Item {
                name: name.to_uppercase(),
                description,
                details,
                location,
                is_takeable,
            };
        }
    }
}

fn parse_room_id(cfg: &Ini, section: &str, key: &str) -> usize {
    cfg.get(section, key)
        .and_then(|v| v.trim().parse::<usize>().ok())
        .filter(|&id| id >= 1 && id <= MAX_ROOMS)
        .unwrap_or(0)
}

pub fn randomize_map_location(s: &mut GameState) {
    for i in 1..=MAX_ITEMS {
        if s.items[i].name == "MAP" {
            s.items[i].location = (s.rng.gen_range(0..7) + 1) as i32;
            break;
        }
    }
}
