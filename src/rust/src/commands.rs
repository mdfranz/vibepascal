use rand::Rng;

use crate::out_println;
use crate::game::{find_item, find_item_any, is_desert_room, look};
use crate::output::wrap_write_ln;
use crate::persistence::check_autosave;
use crate::state::GameState;
use crate::types::*;

// ---------------------------------------------------------------------------
// Movement helpers
// ---------------------------------------------------------------------------

fn print_movement(s: &mut GameState, direction: &str, is_riding: bool) {
    let idx: usize = s.rng.gen_range(0..5);
    if is_riding {
        let msg = match idx {
            0 => format!("🏇 You ride {}.", direction),
            1 => format!("🏇 You guide your horse {}.", direction),
            2 => format!("🏇 You trot {}.", direction),
            3 => format!("🏇 You and your mare head {}.", direction),
            _ => format!("🏇 The horse carries you {}.", direction),
        };
        out_println!(s, "{}", msg);
    } else {
        let msg = match idx {
            0 => format!("🚶 You walk {}.", direction),
            1 => format!("🚶 You trek {} through the dust.", direction),
            2 => format!("🚶 You head {}.", direction),
            3 => format!("🚶 You make your way {}.", direction),
            _ => format!("🚶 You trudge {} across the dry ground.", direction),
        };
        out_println!(s, "{}", msg);
    }
}

pub fn move_to(s: &mut GameState, new_room_id: usize) {
    if new_room_id == 0 {
        out_println!(s, "You cannot go that way.");
    } else if s.is_riding && matches!(new_room_id, 2 | 4 | 5 | 7) {
        out_println!(s, "You can't bring a horse in there. Dismount first.");
    } else if s.current_room == 6 && new_room_id == DESERT_ENTRY_ROOM_ID && !s.is_riding {
        out_println!(s, "The desert is too dangerous on foot. You must be riding a saddled horse.");
    } else {
        s.current_room = new_room_id;
        if s.is_riding {
            for i in 1..=MAX_ITEMS {
                if s.items[i].name == "HORSE" {
                    s.items[i].location = INV_LOCATION;
                    break;
                }
            }
        }
        if s.current_room != 1 && !s.room_visited[s.current_room] {
            s.room_visited[s.current_room] = true;
            s.score += SCORE_ROOM_VISIT;
        }
        if s.current_room != 1 && s.rng.gen_range(0..100) < 20 {
            s.snake_room = s.current_room;
        } else {
            s.snake_room = 0;
        }
        if s.current_room != 1 && s.current_room != 7 && s.rng.gen_range(0..100) < 15 {
            s.outlaw_room = s.current_room;
        } else {
            s.outlaw_room = 0;
        }
        look(s);
    }
}

// ---------------------------------------------------------------------------
// World update (called after each turn-consuming command)
// ---------------------------------------------------------------------------

pub fn update_world(s: &mut GameState) {
    s.turns += 1;
    s.thirst += 1;
    if s.temp_light_turns > 0 && !s.is_lamp_lit {
        s.temp_light_turns -= 1;
    }
    if s.is_horse_saddled && is_desert_room(s.current_room) {
        s.horse_thirst += 1;
    }

    // Items in burning rooms get destroyed when fire dies (burning == 1)
    for i in 1..=MAX_ITEMS {
        let loc = s.items[i].location;
        if loc > 0 {
            let burning = s.room_burning[loc as usize];
            if burning == 1 {
                let desc = s.items[i].description.clone();
                s.items[i].location = 0;
                out_println!(s, "🔥 The fire destroys {}.", desc);
            }
        }
    }

    for i in 1..=MAX_ROOMS {
        if s.room_burning[i] > 0 {
            s.room_burning[i] -= 1;
        }
    }

    if s.snake_room > 0 && s.room_burning[s.snake_room] > 0 {
        s.snake_room = 0;
        out_println!(s, "🔥 The fire drives away the rattlesnake.");
    }
    if s.snake_room > 0 && s.rng.gen_range(0..100) < 30 {
        s.snake_room = 0;
    }

    if s.thirst > THIRST_LIMIT - 5 {
        out_println!(s);
        out_println!(s, "🌵 === Your throat is parched. You need water soon. ===");
    }
    if s.is_horse_saddled && is_desert_room(s.current_room) && s.horse_thirst > HORSE_THIRST_LIMIT - 5 {
        out_println!(s);
        out_println!(s, "🐎 === Your horse is showing signs of exhaustion. It needs water soon. ===");
    }

    if s.thirst >= THIRST_LIMIT {
        out_println!(s);
        wrap_write_ln(s, "💀 You have collapsed from dehydration. GAME OVER.");
        s.is_playing = false;
    }

    if is_desert_room(s.current_room) && !s.is_riding {
        out_println!(s);
        wrap_write_ln(s, "🔥 The desert heat is overwhelming on foot. You collapse into the sand. GAME OVER.");
        s.is_playing = false;
    }

    if s.is_horse_saddled && is_desert_room(s.current_room) && s.horse_thirst >= HORSE_THIRST_LIMIT {
        out_println!(s);
        wrap_write_ln(s, "💀 Your horse collapses from dehydration. You are stranded in the desert. GAME OVER.");
        s.is_playing = false;
    }

    if s.turns == TWILIGHT_TURN {
        out_println!(s, "🌇 The sun is getting low.");
    }
    if s.turns == DARK_TURN {
        out_println!(s, "🌑 It is now dark.");
    }
}

// ---------------------------------------------------------------------------
// Hazard check (called before each command)
// ---------------------------------------------------------------------------

fn is_safe_verb(verb: &str) -> bool {
    matches!(
        verb,
        "N" | "S" | "E" | "W"
            | "NORTH"
            | "SOUTH"
            | "EAST"
            | "WEST"
            | "LOOK"
            | "L"
            | "EXAMINE"
            | "X"
            | "SEARCH"
            | "INVENTORY"
            | "I"
            | "CHECK"
            | "HELP"
            | "?"
            | "H"
            | "SCORE"
            | "SAVE"
            | "LOAD"
            | "QUIT"
            | "Q"
    )
}

pub fn check_hazards(s: &mut GameState, verb: &str) -> bool {
    if s.snake_room == s.current_room && !is_safe_verb(verb) && verb != "FREEZE" && verb != "WAIT" {
        out_println!(s);
        wrap_write_ln(s, "🐍 As you reach out, the rattlesnake strikes! You feel a sharp pain in your hand.");
        out_println!(s);
        wrap_write_ln(s, "💀 The venom works quickly. GAME OVER.");
        s.is_playing = false;
        return false;
    }
    if s.outlaw_room == s.current_room && !is_safe_verb(verb) && verb != "SHOOT" && verb != "KILL" {
        out_println!(s);
        wrap_write_ln(s, "🤠 The outlaw doesn't like you poking around. He draws his gun and fires.");
        out_println!(s);
        wrap_write_ln(s, "💥 Everything goes dark. GAME OVER.");
        s.is_playing = false;
        return false;
    }
    true
}

// ---------------------------------------------------------------------------
// Command split
// ---------------------------------------------------------------------------

pub fn split_command(cmd: &str) -> (String, String) {
    let trimmed = cmd.trim();
    if let Some(pos) = trimmed.find(' ') {
        let verb = trimmed[..pos].to_uppercase();
        let noun = trimmed[pos + 1..].trim().to_string();
        (verb, noun)
    } else {
        (trimmed.to_uppercase(), String::new())
    }
}

// ---------------------------------------------------------------------------
// Individual command handlers
// ---------------------------------------------------------------------------

fn cmd_drink(s: &mut GameState, _noun: &str, _consume_turn: &mut bool) {
    if find_item("CANTEEN", INV_LOCATION, s) == 0 {
        out_println!(s, "You don't have anything to drink from.");
    } else if !s.has_water {
        out_println!(s, "Your canteen is empty.");
    } else {
        s.thirst = 0;
        if s.canteen_drinks > 0 {
            s.canteen_drinks -= 1;
        }
        if s.canteen_drinks <= 0 {
            s.has_water = false;
        }
        wrap_write_ln(s, "💧 The water is warm but refreshing.");
        out_println!(s, "Your thirst is quenched.");
    }
}

fn cmd_fill_canteen(s: &mut GameState, _noun: &str, _consume_turn: &mut bool) {
    if find_item("CANTEEN", INV_LOCATION, s) == 0 {
        out_println!(s, "You have nothing to fill.");
    } else if s.current_room == 3 && s.is_pump_fixed {
        s.has_water = true;
        s.canteen_drinks = 3;
        out_println!(s, "💧 You fill your canteen with fresh water from the pump.");
        if !s.scored_first_fill {
            s.scored_first_fill = true;
            s.score += SCORE_FIRST_FILL;
        }
    } else if s.current_room == STREAM_ROOM_ID {
        s.has_water = true;
        s.canteen_drinks = 3;
        out_println!(s, "💧 You fill your canteen with cold stream water.");
    } else {
        out_println!(s, "There is no water here.");
    }
}

fn cmd_light_lamp(s: &mut GameState, noun: &str, _consume_turn: &mut bool) {
    let noun_upper = noun.trim().to_uppercase();
    if !noun.is_empty() && noun_upper != "MATCH" && noun_upper != "MATCHES" && noun_upper != "LAMP" {
        out_println!(s, "Light what?");
        return;
    }
    if find_item("LAMP", INV_LOCATION, s) > 0 {
        s.is_lamp_lit = true;
        wrap_write_ln(s, "🔦 You light the lamp. A yellow glow illuminates the room.");
        if !s.scored_lamp_light {
            s.scored_lamp_light = true;
            s.score += SCORE_LAMP_LIGHT;
        }
    } else {
        s.temp_light_turns = 3;
        wrap_write_ln(s, "🔥 You strike a match. The room brightens for a moment.");
    }
}

fn cmd_show_help(s: &mut GameState, _noun: &str, consume_turn: &mut bool) {
    out_println!(s);
    out_println!(s, "Available Commands:");
    out_println!(s, "  🚶 N, S, E, W      - Move North, South, East, West");
    out_println!(s, "  👀 LOOK (L)        - Look around");
    out_println!(s, "  🔍 EXAMINE (X)     - Look closely at an item");
    out_println!(s, "  🖐️  TAKE (GET)      - Pick up an item");
    out_println!(s, "  ✋  DROP            - Leave an item");
    out_println!(s, "  🎒 INVENTORY (I)   - Check your gear");
    out_println!(s, "  💧 DRINK           - Drink from your canteen");
    out_println!(s, "  🚰 FILL            - Fill canteen at a water source");
    out_println!(s, "  🐎 WATER           - Water your horse at a water source");
    out_println!(s, "  🔦 LIGHT           - Light your lamp if you have matches");
    out_println!(s, "  🔧 FIX             - Repair something");
    out_println!(s, "  🏇 SADDLE          - Put a saddle on the horse");
    out_println!(s, "  ❄️  FREEZE (WAIT)   - Stay still to avoid danger");
    out_println!(s, "  🔥 BURN            - Burn a flammable item (requires matches)");
    out_println!(s, "  🔥 FIRE            - Start a fire in certain rooms (requires matches)");
    out_println!(s, "  🧗 CLIMB           - Climb a steep obstacle");
    out_println!(s, "  💾 SAVE / LOAD     - Save or load your progress");
    out_println!(s, "  🏆 SCORE           - Show current score");
    out_println!(s, "  ❓ HELP (H)        - Show this list");
    out_println!(s, "  🚪 QUIT (Q)        - Exit");
    out_println!(s);
    *consume_turn = false;
}

fn cmd_examine_item(s: &mut GameState, target_noun: &str, consume_turn: &mut bool) {
    let noun = if target_noun.to_uppercase().starts_with("AT ") {
        target_noun[3..].trim()
    } else {
        target_noun
    };

    let cr = s.current_room as i32;
    let item_id = {
        let id = find_item(noun, INV_LOCATION, s);
        if id == 0 { find_item(noun, cr, s) } else { id }
    };

    if item_id > 0 {
        let details = s.items[item_id].details.clone();
        wrap_write_ln(s, &details);

        if s.items[item_id].name == "ROCK" {
            let key_id = find_item_any("KEY", s);
            if key_id > 0 && s.items[key_id].location == 0 {
                s.items[key_id].location = s.current_room as i32;
                out_println!(s);
                out_println!(s, "You lift the rock. A small brass key is hidden beneath it.");
            }
        }
        if s.items[item_id].name == "BOOK" {
            let note_id = find_item_any("NOTE", s);
            if note_id > 0 && s.items[note_id].location == 0 {
                s.items[note_id].location = INV_LOCATION;
                out_println!(s);
                out_println!(s, "A small folded note falls out of the book.");
                if !s.scored_note_found {
                    s.scored_note_found = true;
                    s.score += SCORE_NOTE_FOUND;
                }
            }
        }
    } else if noun.is_empty() {
        look(s);
    } else {
        out_println!(s, "You don't see that here.");
    }
    *consume_turn = false;
}

fn cmd_fix_something(s: &mut GameState, target_noun: &str, _consume_turn: &mut bool) {
    let noun = target_noun.trim().to_uppercase();
    if noun == "PUMP" && s.current_room == 3 {
        if find_item("LEATHER", INV_LOCATION, s) > 0 {
            s.is_pump_fixed = true;
            out_println!(s, "You fix the pump. Water starts to flow.");
            s.items[3].description = "a working water pump".to_string();
            if !s.scored_pump_fix {
                s.scored_pump_fix = true;
                s.score += SCORE_PUMP_FIX;
            }
        } else {
            out_println!(s, "You need leather.");
        }
    } else if (noun == "WIRE" || noun == "WIRES" || noun == "TELEGRAPH") && s.current_room == 2 {
        if s.is_telegraph_fixed {
            out_println!(s, "The telegraph is already repaired.");
        } else if find_item("WIRE", INV_LOCATION, s) > 0 {
            s.is_telegraph_fixed = true;
            out_println!(s, "You splice the copper wire and restore the telegraph line.");
            if let Some(r) = s.rooms[2].as_mut() {
                r.description = "The telegraph has been repaired. The line hums faintly with life.".to_string();
            }
            if !s.scored_telegraph_fix {
                s.scored_telegraph_fix = true;
                s.score += SCORE_TELEGRAPH_FIX;
            }
            // Remove the wire from inventory
            let wire_id = find_item("WIRE", INV_LOCATION, s);
            if wire_id > 0 {
                s.items[wire_id].location = 0;
            }
        } else {
            out_println!(s, "You need copper wire.");
        }
    } else {
        out_println!(s, "Nothing to fix here.");
    }
}

fn cmd_water_horse(s: &mut GameState, target_noun: &str, _consume_turn: &mut bool) {
    let noun = target_noun.trim().to_uppercase();
    if !noun.is_empty() && noun != "HORSE" && noun != "MARE" {
        out_println!(s, "Water what?");
        return;
    }
    if !s.is_horse_saddled {
        out_println!(s, "You don't have a horse with you.");
        return;
    }
    if s.current_room != STREAM_ROOM_ID {
        out_println!(s, "There is no water here for your horse.");
        return;
    }
    s.horse_thirst = 0;
    out_println!(s, "Your horse drinks deeply from the stream.");
}

fn cmd_saddle_horse(s: &mut GameState, target_noun: &str, _consume_turn: &mut bool) {
    let noun = target_noun.trim().to_uppercase();
    if !noun.is_empty() && noun != "HORSE" && noun != "ON HORSE" && noun != "MARE" {
        out_println!(s, "Saddle what?");
        return;
    }
    let horse_id = find_item("HORSE", s.current_room as i32, s);
    if horse_id == 0 {
        out_println!(s, "There is no horse here.");
        return;
    }
    let saddle_id = find_item("SADDLE", INV_LOCATION, s);
    if saddle_id == 0 {
        out_println!(s, "You need a saddle.");
        return;
    }
    if s.is_horse_saddled {
        out_println!(s, "The horse is already saddled.");
        return;
    }
    s.is_horse_saddled = true;
    s.items[saddle_id].location = 0;
    s.items[horse_id].description = "a saddled horse".to_string();
    s.items[horse_id].details = "A calm, saddle-ready horse. It looks steady and patient.".to_string();
    out_println!(s, "You secure the saddle onto the horse. It stands quietly.");
}

fn cmd_handle_mount(s: &mut GameState, noun: &str, _consume_turn: &mut bool) {
    let noun_upper = noun.trim().to_uppercase();
    if !noun_upper.is_empty() && noun_upper != "HORSE" && noun_upper != "MARE" {
        out_println!(s, "Mount what?");
        return;
    }
    if s.is_riding {
        out_println!(s, "You are already riding.");
    } else if find_item("HORSE", s.current_room as i32, s) > 0 {
        if s.is_horse_saddled {
            s.is_riding = true;
            let horse_id = find_item("HORSE", s.current_room as i32, s);
            s.items[horse_id].location = INV_LOCATION;
            out_println!(s, "You swing yourself into the saddle. You are now riding.");
        } else {
            out_println!(s, "The horse needs a saddle before you can ride her.");
        }
    } else {
        out_println!(s, "There is no horse here.");
    }
}

fn cmd_handle_dismount(s: &mut GameState, noun: &str, _consume_turn: &mut bool) {
    let noun_upper = noun.trim().to_uppercase();
    if !noun_upper.is_empty() && noun_upper != "HORSE" && noun_upper != "MARE" {
        out_println!(s, "Dismount what?");
        return;
    }
    if !s.is_riding {
        out_println!(s, "You aren't riding anything.");
    } else {
        s.is_riding = false;
        let horse_id = find_item("HORSE", INV_LOCATION, s);
        if horse_id > 0 {
            s.items[horse_id].location = s.current_room as i32;
        }
        out_println!(s, "You dismount and stand beside your horse.");
    }
}

fn cmd_handle_open(s: &mut GameState, noun: &str, consume_turn: &mut bool) {
    if noun == "BOX" && s.current_room == 7 {
        if s.is_box_open {
            out_println!(s, "It is already open.");
        } else if find_item("KEY", INV_LOCATION, s) == 0 {
            out_println!(s, "The box is locked. You need a key.");
        } else {
            s.is_box_open = true;
            s.items[8].location = 7;
            out_println!(s, "You unlock the box. Inside lies a heavy revolver.");
            if !s.scored_box_open {
                s.scored_box_open = true;
                s.score += SCORE_BOX_OPEN;
            }
        }
    } else {
        out_println!(s, "There is nothing to open here.");
        *consume_turn = false;
    }
}

fn cmd_handle_shoot(s: &mut GameState, _noun: &str, _consume_turn: &mut bool) {
    if find_item("REVOLVER", INV_LOCATION, s) == 0 {
        out_println!(s, "You have nothing to shoot with.");
    } else if s.outlaw_room == s.current_room {
        s.outlaw_room = 0;
        wrap_write_ln(s, "💥 You draw your revolver and fire first. The outlaw falls to the ground.");
        out_println!(s, "💀 The threat is gone.");
        if !s.scored_outlaw_kill {
            s.scored_outlaw_kill = true;
            s.score += SCORE_OUTLAW_KILL;
        }
    } else {
        out_println!(s, "Nothing here to shoot.");
    }
}

fn cmd_handle_freeze(s: &mut GameState, _noun: &str, _consume_turn: &mut bool) {
    out_println!(s, "You stay perfectly still. The snake watches you...");
    if s.rng.gen_range(0..100) < 50 {
        s.snake_room = 0;
        out_println!(s, "The snake loses interest and slithers into the shadows.");
    }
}

fn cmd_handle_inventory(s: &mut GameState, _noun: &str, consume_turn: &mut bool) {
    out_println!(s, "You are carrying:");
    for i in 1..=MAX_ITEMS {
        if s.items[i].location == INV_LOCATION {
            let desc = s.items[i].description.clone();
            out_println!(s, "  - {}", desc);
        }
    }
    *consume_turn = false;
}

fn cmd_handle_score(s: &mut GameState, _noun: &str, consume_turn: &mut bool) {
    out_println!(s, "🏆 Score: {}", s.score);
    *consume_turn = false;
}

fn cmd_handle_save(s: &mut GameState, noun: &str, consume_turn: &mut bool) {
    let path = if noun.is_empty() {
        "data/save.json".to_string()
    } else {
        let base = noun.to_lowercase();
        if base.ends_with(".json") {
            format!("data/{}", base)
        } else {
            format!("data/{}.json", base)
        }
    };
    crate::persistence::save_game(s, &path);
    *consume_turn = false;
}

fn cmd_handle_load(s: &mut GameState, noun: &str, consume_turn: &mut bool) {
    let path = if noun.is_empty() {
        "data/save.json".to_string()
    } else {
        let base = noun.to_lowercase();
        if base.ends_with(".json") {
            format!("data/{}", base)
        } else {
            format!("data/{}.json", base)
        }
    };
    crate::persistence::load_game(s, &path);
    *consume_turn = false;
}

fn cmd_handle_take(s: &mut GameState, noun: &str, _consume_turn: &mut bool) {
    let item_id = find_item(noun, s.current_room as i32, s);
    if item_id > 0 {
        let carry_count = (1..=MAX_ITEMS)
            .filter(|&i| s.items[i].location == INV_LOCATION && s.items[i].is_takeable)
            .count();
        if carry_count >= MAX_CARRY {
            out_println!(s, "You can't carry any more. Drop something first.");
            return;
        }
        if !s.items[item_id].is_takeable {
            let msg = match s.items[item_id].name.as_str() {
                "PUMP"  => "The pump is fixed in place.",
                "HORSE" => "It's too big to carry.",
                "BOX"   => "It's bolted down.",
                "ROCK"  => "It's too heavy to carry.",
                _       => "You can't take that.",
            };
            out_println!(s, "{}", msg);
            return;
        }
        s.items[item_id].location = INV_LOCATION;
        let desc = s.items[item_id].description.clone();
        out_println!(s, "🎒 Taken: {}.", desc);
        if !s.item_scored[item_id] {
            s.item_scored[item_id] = true;
            s.score += SCORE_ITEM_PICKUP;
        }
    } else {
        out_println!(s, "Not here.");
    }
}

fn cmd_handle_drop(s: &mut GameState, noun: &str, _consume_turn: &mut bool) {
    let item_id = find_item(noun, INV_LOCATION, s);
    if item_id > 0 {
        s.items[item_id].location = s.current_room as i32;
        let desc = s.items[item_id].description.clone();
        out_println!(s, "✋ Dropped: {}.", desc);
    } else {
        out_println!(s, "You aren't carrying that.");
    }
}

fn cmd_handle_burn(s: &mut GameState, noun: &str, _consume_turn: &mut bool) {
    let target = noun.trim().to_uppercase();
    if target.is_empty() {
        out_println!(s, "Burn what?");
        return;
    }
    if find_item("MATCHES", INV_LOCATION, s) == 0 {
        out_println!(s, "You have nothing to burn it with.");
        return;
    }
    let item_id = {
        let id = find_item(&target, INV_LOCATION, s);
        if id == 0 { find_item(&target, s.current_room as i32, s) } else { id }
    };
    if item_id == 0 {
        out_println!(s, "You don't see that here.");
        return;
    }
    let name = s.items[item_id].name.clone();
    if !matches!(name.as_str(), "BOOK" | "LEDGER" | "LEATHER" | "MAP" | "SADDLE") {
        out_println!(s, "It doesn't burn.");
        return;
    }
    s.items[item_id].location = 0;
    out_println!(s, "You burn it to ash.");
}

fn cmd_handle_fire(s: &mut GameState, _noun: &str, _consume_turn: &mut bool) {
    if find_item("MATCHES", INV_LOCATION, s) == 0 {
        out_println!(s, "You have nothing to start a fire with.");
        return;
    }
    let id = s.current_room;
    if !matches!(id, 2 | 3 | 5) {
        out_println!(s, "There is nothing here that will catch fire.");
        return;
    }
    if s.room_burning[id] > 0 {
        out_println!(s, "A fire is already burning here.");
        return;
    }
    s.room_burning[id] = 3;
    out_println!(s, "🔥 You start a fire. The room glows with heat.");
    if s.snake_room == id {
        s.snake_room = 0;
        out_println!(s, "🔥 The rattlesnake recoils from the flames and disappears.");
    }
}

fn cmd_handle_climb(s: &mut GameState, _noun: &str, _consume_turn: &mut bool) {
    if s.current_room == 12 {
        move_to(s, STREAM_ROOM_ID);
    } else {
        out_println!(s, "There is nothing to climb here.");
    }
}

fn cmd_handle_put(s: &mut GameState, noun: &str, consume_turn: &mut bool) {
    if noun.to_uppercase().contains("SADDLE") {
        cmd_saddle_horse(s, "HORSE", consume_turn);
    }
}

fn cmd_handle_quit(s: &mut GameState, _noun: &str, _consume_turn: &mut bool) {
    s.is_playing = false;
}

fn cmd_handle_search(s: &mut GameState, _noun: &str, consume_turn: &mut bool) {
    look(s);
    *consume_turn = false;
}

fn cmd_handle_look(s: &mut GameState, noun: &str, consume_turn: &mut bool) {
    cmd_examine_item(s, noun, consume_turn);
    *consume_turn = false;
}

// ---------------------------------------------------------------------------
// Main command dispatcher
// ---------------------------------------------------------------------------

pub fn process_command(s: &mut GameState, cmd: &str) {
    let (verb, noun) = split_command(cmd);
    let mut consume_turn = true;

    if !check_hazards(s, &verb) {
        return;
    }

    match verb.as_str() {
        "N" | "NORTH" => {
            let dest = s.rooms[s.current_room].as_ref().map(|r| r.north).unwrap_or(0);
            if dest > 0 {
                print_movement(s, "NORTH", s.is_riding);
            }
            move_to(s, dest);
        }
        "S" | "SOUTH" => {
            let dest = s.rooms[s.current_room].as_ref().map(|r| r.south).unwrap_or(0);
            if dest > 0 {
                print_movement(s, "SOUTH", s.is_riding);
            }
            move_to(s, dest);
        }
        "E" | "EAST" => {
            let dest = s.rooms[s.current_room].as_ref().map(|r| r.east).unwrap_or(0);
            if dest > 0 {
                print_movement(s, "EAST", s.is_riding);
            }
            move_to(s, dest);
        }
        "W" | "WEST" => {
            let dest = s.rooms[s.current_room].as_ref().map(|r| r.west).unwrap_or(0);
            if dest > 0 {
                print_movement(s, "WEST", s.is_riding);
            }
            move_to(s, dest);
        }
        "LOOK" | "L"        => cmd_handle_look(s, &noun, &mut consume_turn),
        "EXAMINE" | "X"     => cmd_examine_item(s, &noun, &mut consume_turn),
        "SEARCH"            => cmd_handle_search(s, &noun, &mut consume_turn),
        "HELP" | "?" | "H"  => cmd_show_help(s, &noun, &mut consume_turn),
        "INVENTORY" | "I" | "INV" => cmd_handle_inventory(s, &noun, &mut consume_turn),
        "CHECK" => {
            let n = noun.to_uppercase();
            if n == "INVENTORY" || n == "INV" || n == "I" {
                cmd_handle_inventory(s, &noun, &mut consume_turn);
            }
        }
        "DRINK"             => cmd_drink(s, &noun, &mut consume_turn),
        "FILL"              => cmd_fill_canteen(s, &noun, &mut consume_turn),
        "WATER"             => cmd_water_horse(s, &noun, &mut consume_turn),
        "LIGHT"             => cmd_light_lamp(s, &noun, &mut consume_turn),
        "FIX"               => cmd_fix_something(s, &noun, &mut consume_turn),
        "SADDLE"            => cmd_saddle_horse(s, &noun, &mut consume_turn),
        "PUT"               => cmd_handle_put(s, &noun, &mut consume_turn),
        "MOUNT" | "RIDE"    => cmd_handle_mount(s, &noun, &mut consume_turn),
        "DISMOUNT"          => cmd_handle_dismount(s, &noun, &mut consume_turn),
        "OPEN"              => cmd_handle_open(s, noun.trim().to_uppercase().as_str(), &mut consume_turn),
        "SHOOT" | "KILL"    => cmd_handle_shoot(s, &noun, &mut consume_turn),
        "FREEZE" | "WAIT"   => cmd_handle_freeze(s, &noun, &mut consume_turn),
        "TAKE" | "GET"      => cmd_handle_take(s, &noun, &mut consume_turn),
        "DROP" | "D"        => cmd_handle_drop(s, &noun, &mut consume_turn),
        "BURN"              => cmd_handle_burn(s, &noun, &mut consume_turn),
        "FIRE"              => cmd_handle_fire(s, &noun, &mut consume_turn),
        "CLIMB"             => cmd_handle_climb(s, &noun, &mut consume_turn),
        "SAVE"              => cmd_handle_save(s, &noun, &mut consume_turn),
        "LOAD"              => cmd_handle_load(s, &noun, &mut consume_turn),
        "SCORE"             => cmd_handle_score(s, &noun, &mut consume_turn),
        "QUIT" | "Q"        => cmd_handle_quit(s, &noun, &mut consume_turn),
        _                   => out_println!(s, "🤷 I don't know how to do that."),
    }

    if s.is_playing && consume_turn {
        update_world(s);
        check_autosave(s);
    }
}
