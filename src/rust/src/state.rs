use std::collections::VecDeque;

use rand::{rngs::SmallRng, SeedableRng};

use crate::types::*;

pub struct GameState {
    pub rooms: Vec<Option<Room>>,
    pub items: Vec<Item>,
    pub current_room: usize,

    // When Some, all output goes here instead of stdout (MCP capture mode).
    pub capture: Option<Vec<u8>>,

    pub is_playing: bool,
    pub is_pump_fixed: bool,
    pub is_lamp_lit: bool,
    pub has_water: bool,
    pub is_headless: bool,
    pub is_box_open: bool,
    pub is_telegraph_fixed: bool,
    pub is_horse_saddled: bool,
    pub is_riding: bool,

    pub temp_light_turns: i32,
    pub canteen_drinks: i32,
    pub snake_room: usize,
    pub outlaw_room: usize,
    pub thirst: i32,
    pub turns: i32,
    pub turn_limit: i32,
    pub horse_thirst: i32,
    pub score: i32,

    pub room_visited: [bool; MAX_ROOMS + 1],
    pub item_scored: [bool; MAX_ITEMS + 1],

    pub scored_pump_fix: bool,
    pub scored_first_fill: bool,
    pub scored_lamp_light: bool,
    pub scored_box_open: bool,
    pub scored_telegraph_fix: bool,
    pub scored_outlaw_kill: bool,
    pub scored_note_found: bool,

    pub room_burning: [i32; MAX_ROOMS + 1],

    pub history: VecDeque<String>,

    pub autosave_enabled: bool,
    pub autosave_interval: i32,
    pub autosave_path: String,

    pub rng: SmallRng,
}

pub fn init_state(seed: Option<u64>) -> GameState {
    let rng = match seed {
        Some(s) => SmallRng::seed_from_u64(s),
        None => SmallRng::from_entropy(),
    };

    let mut rooms = Vec::with_capacity(MAX_ROOMS + 1);
    for _ in 0..=MAX_ROOMS {
        rooms.push(None);
    }

    let mut items = Vec::with_capacity(MAX_ITEMS + 1);
    for _ in 0..=MAX_ITEMS {
        items.push(Item::default());
    }

    GameState {
        rooms,
        items,
        current_room: 0,
        capture: None,
        is_playing: true,
        is_pump_fixed: false,
        is_lamp_lit: false,
        has_water: false,
        is_headless: false,
        is_box_open: false,
        is_telegraph_fixed: false,
        is_horse_saddled: false,
        is_riding: false,
        temp_light_turns: 0,
        canteen_drinks: 0,
        snake_room: 0,
        outlaw_room: 0,
        thirst: 0,
        turns: 0,
        turn_limit: 0,
        horse_thirst: 0,
        score: 0,
        room_visited: [false; MAX_ROOMS + 1],
        item_scored: [false; MAX_ITEMS + 1],
        scored_pump_fix: false,
        scored_first_fill: false,
        scored_lamp_light: false,
        scored_box_open: false,
        scored_telegraph_fix: false,
        scored_outlaw_kill: false,
        scored_note_found: false,
        room_burning: [0; MAX_ROOMS + 1],
        history: VecDeque::with_capacity(MAX_HISTORY + 1),
        autosave_enabled: false,
        autosave_interval: 5,
        autosave_path: "data/autosave.json".to_string(),
        rng,
    }
}
