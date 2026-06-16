#![allow(dead_code)]

use serde::{Deserialize, Serialize};

use crate::output::is_dark;
use crate::state::GameState;
use crate::types::{INV_LOCATION, MAX_ITEMS};

#[derive(Serialize, Deserialize, Default, Clone)]
pub struct GameSummary {
    pub room_id: usize,
    pub room_name: String,
    pub turns: i32,
    pub score: i32,
    pub is_playing: bool,
    pub is_riding: bool,
    pub is_dark: bool,
    pub thirst: i32,
    pub horse_thirst: i32,
    pub has_water: bool,
    pub lamp_lit: bool,
    pub horse_saddled: bool,
    pub inventory: Vec<String>,
}

pub fn summarize_state(s: &GameState) -> GameSummary {
    let mut summary = GameSummary {
        room_id: s.current_room,
        room_name: s.rooms[s.current_room]
            .as_ref()
            .map(|r| r.name.clone())
            .unwrap_or_default(),
        turns: s.turns,
        score: s.score,
        is_playing: s.is_playing,
        is_riding: s.is_riding,
        is_dark: is_dark(s),
        thirst: s.thirst,
        horse_thirst: s.horse_thirst,
        has_water: s.has_water,
        lamp_lit: s.is_lamp_lit,
        horse_saddled: s.is_horse_saddled,
        inventory: Vec::new(),
    };
    for i in 1..=MAX_ITEMS {
        if s.items[i].location == INV_LOCATION {
            summary.inventory.push(s.items[i].description.clone());
        }
    }
    summary
}

pub fn describe_room(s: &GameState) -> String {
    let Some(room) = s.rooms[s.current_room].as_ref() else {
        return "You are in an unknown location.".to_string();
    };

    let mut desc = format!("{}\n{}\n", room.name, room.description);

    let items: Vec<&str> = (1..=MAX_ITEMS)
        .filter(|&i| s.items[i].location == s.current_room as i32)
        .map(|i| s.items[i].description.as_str())
        .collect();
    if !items.is_empty() {
        desc.push_str(&format!("Items: {}\n", items.join(", ")));
    }

    let mut exits = Vec::new();
    if room.north > 0 { exits.push("north"); }
    if room.south > 0 { exits.push("south"); }
    if room.east  > 0 { exits.push("east"); }
    if room.west  > 0 { exits.push("west"); }
    if !exits.is_empty() {
        desc.push_str(&format!("Exits: {}", exits.join(", ")));
    }

    desc.trim_end().to_string()
}

pub fn describe_inventory(s: &GameState) -> String {
    let items: Vec<&str> = (1..=MAX_ITEMS)
        .filter(|&i| s.items[i].location == INV_LOCATION)
        .map(|i| s.items[i].description.as_str())
        .collect();

    if items.is_empty() {
        "You are carrying nothing.".to_string()
    } else {
        format!("You are carrying: {}", items.join(", "))
    }
}

pub fn summarize_state_json(s: &GameState) -> String {
    serde_json::to_string(&summarize_state(s)).unwrap_or_else(|_| "{}".to_string())
}
