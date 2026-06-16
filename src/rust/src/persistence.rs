use std::fs;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::out_println;

use crate::game::look;
use crate::state::GameState;
use crate::types::{MAX_ITEMS, MAX_ROOMS};

#[derive(Serialize, Deserialize)]
struct SaveItem {
    location: i32,
    description: String,
}

#[derive(Serialize, Deserialize)]
struct SaveData {
    current_room: usize,
    is_pump_fixed: bool,
    is_lamp_lit: bool,
    has_water: bool,
    is_horse_saddled: bool,
    is_riding: bool,
    is_telegraph_fixed: bool,
    is_box_open: bool,
    temp_light_turns: i32,
    canteen_drinks: i32,
    thirst: i32,
    horse_thirst: i32,
    turns: i32,
    score: i32,
    room_burning: Vec<i32>,
    items: Vec<SaveItem>,
    room_visited: Vec<bool>,
    item_scored: Vec<bool>,
    scored_pump_fix: bool,
    scored_first_fill: bool,
    scored_lamp_light: bool,
    scored_box_open: bool,
    scored_telegraph_fix: bool,
    scored_outlaw_kill: bool,
    scored_note_found: bool,
}

pub fn save_game(s: &mut GameState, path: &str) {
    save_game_internal(s, path, false);
}

fn save_game_internal(s: &mut GameState, path: &str, quiet: bool) {
    let data = SaveData {
        current_room: s.current_room,
        is_pump_fixed: s.is_pump_fixed,
        is_lamp_lit: s.is_lamp_lit,
        has_water: s.has_water,
        is_horse_saddled: s.is_horse_saddled,
        is_riding: s.is_riding,
        is_telegraph_fixed: s.is_telegraph_fixed,
        is_box_open: s.is_box_open,
        temp_light_turns: s.temp_light_turns,
        canteen_drinks: s.canteen_drinks,
        thirst: s.thirst,
        horse_thirst: s.horse_thirst,
        turns: s.turns,
        score: s.score,
        room_burning: (1..=MAX_ROOMS).map(|i| s.room_burning[i]).collect(),
        items: (1..=MAX_ITEMS)
            .map(|i| SaveItem {
                location: s.items[i].location,
                description: s.items[i].description.clone(),
            })
            .collect(),
        room_visited: (1..=MAX_ROOMS).map(|i| s.room_visited[i]).collect(),
        item_scored: (1..=MAX_ITEMS).map(|i| s.item_scored[i]).collect(),
        scored_pump_fix: s.scored_pump_fix,
        scored_first_fill: s.scored_first_fill,
        scored_lamp_light: s.scored_lamp_light,
        scored_box_open: s.scored_box_open,
        scored_telegraph_fix: s.scored_telegraph_fix,
        scored_outlaw_kill: s.scored_outlaw_kill,
        scored_note_found: s.scored_note_found,
    };

    let json = match serde_json::to_string_pretty(&data) {
        Ok(j) => j,
        Err(e) => {
            if !quiet {
                out_println!(s, "Error saving game: {}", e);
            }
            return;
        }
    };

    if let Some(parent) = Path::new(path).parent() {
        let _ = fs::create_dir_all(parent);
    }

    if let Err(e) = fs::write(path, &json) {
        if !quiet {
            out_println!(s, "Error saving game: {}", e);
        }
        return;
    }

    if !quiet {
        out_println!(s, "💾 Game saved.");
    }
}

pub fn check_autosave(s: &mut GameState) {
    if !s.autosave_enabled || s.autosave_interval <= 0 {
        return;
    }
    if s.turns > 0 && s.turns % s.autosave_interval == 0 {
        let path = s.autosave_path.clone();
        save_game_internal(s, &path, true);
    }
}

pub fn load_game(s: &mut GameState, path: &str) {
    if !Path::new(path).exists() {
        out_println!(s, "No save file found.");
        return;
    }

    let json = match fs::read_to_string(path) {
        Ok(j) => j,
        Err(e) => {
            out_println!(s, "Error loading save file: {}", e);
            return;
        }
    };

    let data: SaveData = match serde_json::from_str(&json) {
        Ok(d) => d,
        Err(e) => {
            out_println!(s, "Error parsing save file: {}", e);
            return;
        }
    };

    if data.current_room >= 1 && data.current_room <= MAX_ROOMS && s.rooms[data.current_room].is_some() {
        s.current_room = data.current_room;
    }

    s.is_pump_fixed     = data.is_pump_fixed;
    s.is_lamp_lit       = data.is_lamp_lit;
    s.has_water         = data.has_water;
    s.is_horse_saddled  = data.is_horse_saddled;
    s.is_riding         = data.is_riding;
    s.is_telegraph_fixed = data.is_telegraph_fixed;
    s.is_box_open       = data.is_box_open;
    s.temp_light_turns  = data.temp_light_turns;
    s.canteen_drinks    = data.canteen_drinks;
    s.thirst            = data.thirst;
    s.horse_thirst      = data.horse_thirst;
    s.turns             = data.turns;
    s.score             = data.score;

    for (i, val) in data.room_burning.iter().enumerate() {
        if i + 1 <= MAX_ROOMS {
            s.room_burning[i + 1] = *val;
        }
    }
    for (i, item) in data.items.iter().enumerate() {
        if i + 1 <= MAX_ITEMS {
            s.items[i + 1].location = item.location;
            if !item.description.is_empty() {
                s.items[i + 1].description = item.description.clone();
            }
        }
    }
    for (i, visited) in data.room_visited.iter().enumerate() {
        if i + 1 <= MAX_ROOMS {
            s.room_visited[i + 1] = *visited;
        }
    }
    for (i, scored) in data.item_scored.iter().enumerate() {
        if i + 1 <= MAX_ITEMS {
            s.item_scored[i + 1] = *scored;
        }
    }

    s.scored_pump_fix      = data.scored_pump_fix;
    s.scored_first_fill    = data.scored_first_fill;
    s.scored_lamp_light    = data.scored_lamp_light;
    s.scored_box_open      = data.scored_box_open;
    s.scored_telegraph_fix = data.scored_telegraph_fix;
    s.scored_outlaw_kill   = data.scored_outlaw_kill;
    s.scored_note_found    = data.scored_note_found;

    if s.is_telegraph_fixed {
        if let Some(room) = s.rooms[2].as_mut() {
            room.description = "The telegraph has been repaired. The line hums faintly with life.".to_string();
        }
    }

    out_println!(s, "📂 Game loaded. (Turns played: {})", s.turns);
    look(s);
}
