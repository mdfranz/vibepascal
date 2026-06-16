use crate::state::GameState;
use crate::types::DARK_TURN;

// Write to capture buffer (MCP mode) or stdout.
#[macro_export]
macro_rules! out_print {
    ($s:expr, $($arg:tt)*) => {
        if let Some(ref mut buf) = $s.capture {
            use std::io::Write;
            let _ = write!(buf, $($arg)*);
        } else {
            print!($($arg)*);
        }
    };
}

#[macro_export]
macro_rules! out_println {
    ($s:expr) => {
        if let Some(ref mut buf) = $s.capture {
            use std::io::Write;
            let _ = writeln!(buf);
        } else {
            println!();
        }
    };
    ($s:expr, $($arg:tt)*) => {
        if let Some(ref mut buf) = $s.capture {
            use std::io::Write;
            let _ = writeln!(buf, $($arg)*);
        } else {
            println!($($arg)*);
        }
    };
}

pub fn wrap_write_ln(s: &mut GameState, text: &str) {
    const MAX_WIDTH: usize = 79;
    let mut remaining = text;
    while remaining.chars().count() > MAX_WIDTH {
        let chars: Vec<char> = remaining.chars().collect();
        let mut space_pos = MAX_WIDTH;
        while space_pos > 0 && chars[space_pos] != ' ' {
            space_pos -= 1;
        }
        if space_pos == 0 {
            space_pos = MAX_WIDTH;
        }
        let line: String = chars[..space_pos].iter().collect();
        let byte_len: usize = chars[..space_pos].iter().map(|c| c.len_utf8()).sum();
        out_println!(s, "{}", line);
        remaining = remaining[byte_len..].trim_start_matches(' ');
    }
    out_println!(s, "{}", remaining);
}

pub fn is_dark(s: &GameState) -> bool {
    s.turns >= DARK_TURN && !s.is_lamp_lit && s.temp_light_turns == 0
}
