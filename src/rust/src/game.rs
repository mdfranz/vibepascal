use crate::{out_print, out_println};
use crate::output::{is_dark, wrap_write_ln};
use crate::state::{init_state, GameState};
use crate::types::{INV_LOCATION, MAX_ITEMS};
use crate::world::{load_world, randomize_map_location};

pub fn new_game(seed: Option<u64>, turn_limit: i32) -> GameState {
    let mut s = init_state(seed);
    s.turn_limit = turn_limit;
    load_world(&mut s, "data/world.ini");
    randomize_map_location(&mut s);
    s.current_room = 1;
    s.is_playing = true;
    look(&mut s);
    s
}

pub fn look(s: &mut GameState) {
    out_println!(s);

    if is_dark(s) {
        wrap_write_ln(s, "🌑 It is pitch black. You can't see anything.");
        return;
    }

    let cr = s.current_room;

    if s.room_burning[cr] > 0 {
        out_println!(s, "🔥 The room is lit by a growing fire.");
    }

    if s.turns >= DARK_TURN {
        out_println!(s, "🌕 [The moon hangs in the black sky]");
    } else if s.turns >= TWILIGHT_TURN {
        out_println!(s, "🌇 [The sky is purple as the sun sets]");
    }

    if s.is_riding {
        out_print!(s, "🏇 ");
    }

    let room_name = s.rooms[cr].as_ref().map(|r| r.name.clone()).unwrap_or_default();
    let room_desc = s.rooms[cr].as_ref().map(|r| r.description.clone()).unwrap_or_default();

    out_print!(s, "📍 === {} === ", room_name);
    match cr {
        1 => out_println!(s, "🏘️"),
        2 => out_println!(s, "📟"),
        3 => out_println!(s, "🐴"),
        4 => out_println!(s, "⚖️"),
        5 => out_println!(s, "🛒"),
        6 => out_println!(s, "🌵"),
        7 => out_println!(s, "👮"),
        8..=13 => out_println!(s, "🏜️"),
        _ => out_println!(s),
    }

    wrap_write_ln(s, &room_desc);

    // Show exits if player has MAP in inventory
    if find_item_in_loc("MAP", INV_LOCATION, s) > 0 {
        let room = s.rooms[cr].as_ref().unwrap();
        let mut exits = String::new();
        if room.north > 0 { exits.push_str("NORTH, "); }
        if room.south > 0 { exits.push_str("SOUTH, "); }
        if room.east  > 0 { exits.push_str("EAST, "); }
        if room.west  > 0 { exits.push_str("WEST, "); }
        if !exits.is_empty() {
            exits.truncate(exits.len() - 2);
            out_println!(s, "Exits: [{}]", exits);
        }
    }

    if s.snake_room == cr {
        out_println!(s);
        out_println!(s, "!!! A RATTLESNAKE is coiled here, buzzing its tail angrily !!!");
        out_println!(s, "One wrong move could be your last.");
    }

    if s.outlaw_room == cr {
        out_println!(s);
        out_println!(s, "!!! A DIRTY OUTLAW is leaning against the wall, hand on his holster !!!");
        out_println!(s, r#""You don't belong here, stranger," he sneers."#);
    }

    let mut found_items = false;
    for i in 1..=MAX_ITEMS {
        if s.items[i].location == cr as i32 {
            if !found_items {
                out_println!(s);
                out_println!(s, "📦 You see the following here:");
                found_items = true;
            }
            let desc = s.items[i].description.clone();
            out_println!(s, "  - {}", desc);
        }
    }
    out_println!(s);
}

pub fn find_item(name: &str, loc: i32, s: &GameState) -> usize {
    let upper = name.to_uppercase();
    for i in 1..=MAX_ITEMS {
        if s.items[i].location == loc && s.items[i].name == upper {
            return i;
        }
    }
    0
}

pub fn find_item_any(name: &str, s: &GameState) -> usize {
    let upper = name.to_uppercase();
    for i in 1..=MAX_ITEMS {
        if s.items[i].name == upper {
            return i;
        }
    }
    0
}

pub fn find_item_in_loc(name: &str, loc: i32, s: &GameState) -> usize {
    find_item(name, loc, s)
}

pub fn is_desert_room(id: usize) -> bool {
    id >= 8 && id <= 13
}

use crate::types::{DARK_TURN, TWILIGHT_TURN};
