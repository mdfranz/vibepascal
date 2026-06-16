pub const MAX_ROOMS: usize = 20;
pub const MAX_ITEMS: usize = 24;
pub const INV_LOCATION: i32 = -1;
pub const MAX_HISTORY: usize = 10;
pub const MAX_CARRY: usize = 5;
pub const THIRST_LIMIT: i32 = 50;
pub const HORSE_THIRST_LIMIT: i32 = 40;
pub const DARK_TURN: i32 = 30;
pub const TWILIGHT_TURN: i32 = 20;
pub const SCORE_ROOM_VISIT: i32 = 5;
pub const SCORE_ITEM_PICKUP: i32 = 3;
pub const SCORE_NOTE_FOUND: i32 = 5;
pub const SCORE_PUMP_FIX: i32 = 20;
pub const SCORE_FIRST_FILL: i32 = 10;
pub const SCORE_LAMP_LIGHT: i32 = 5;
pub const SCORE_BOX_OPEN: i32 = 10;
pub const SCORE_OUTLAW_KILL: i32 = 15;
pub const SCORE_TELEGRAPH_FIX: i32 = 10;
pub const STREAM_ROOM_ID: usize = 13;
pub const DESERT_ENTRY_ROOM_ID: usize = 8;

pub struct Room {
    #[allow(dead_code)]
    pub id: usize,
    pub name: String,
    pub description: String,
    pub north: usize,
    pub south: usize,
    pub east: usize,
    pub west: usize,
}

#[derive(Default, Clone)]
pub struct Item {
    pub name: String,
    pub description: String,
    pub details: String,
    pub location: i32,
    pub is_takeable: bool,
}
