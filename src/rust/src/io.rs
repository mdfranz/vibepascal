use std::collections::VecDeque;
use std::io::{self, BufRead, Write};

use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
    terminal::{disable_raw_mode, enable_raw_mode},
};

use crate::state::GameState;
use crate::types::MAX_HISTORY;

pub fn custom_read_ln(s: &mut GameState, prompt: &str) -> String {
    if s.is_headless {
        print!("{}", prompt);
        let _ = io::stdout().flush();
        let mut line = String::new();
        let stdin = io::stdin();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => return "QUIT".to_string(), // EOF
            Ok(_) => return line.trim_end_matches(['\r', '\n']).to_string(),
            Err(_) => return "QUIT".to_string(),
        }
    }

    // Interactive raw-mode input with history
    print!("{}", prompt);
    let _ = io::stdout().flush();

    if enable_raw_mode().is_err() {
        // Fallback to simple readline
        let mut line = String::new();
        let stdin = io::stdin();
        let _ = stdin.lock().read_line(&mut line);
        return line.trim_end_matches(['\r', '\n']).to_string();
    }

    let mut line_chars: Vec<char> = Vec::new();
    let mut hist_idx = s.history.len();

    loop {
        match event::read() {
            Ok(Event::Key(key_event)) => {
                match key_event.code {
                    KeyCode::Enter => {
                        let _ = disable_raw_mode();
                        print!("\r\n");
                        let _ = io::stdout().flush();
                        let result: String = line_chars.iter().collect();
                        if !result.is_empty() {
                            let prev = s.history.back().map(String::as_str).unwrap_or("");
                            if result != prev {
                                if s.history.len() >= MAX_HISTORY {
                                    s.history.pop_front();
                                }
                                s.history.push_back(result.clone());
                            }
                        }
                        return result;
                    }
                    KeyCode::Char('d') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                        let _ = disable_raw_mode();
                        print!("QUIT\r\n");
                        let _ = io::stdout().flush();
                        return "QUIT".to_string();
                    }
                    KeyCode::Char(c) => {
                        line_chars.push(c);
                        print!("{}", c);
                        let _ = io::stdout().flush();
                    }
                    KeyCode::Backspace => {
                        if !line_chars.is_empty() {
                            line_chars.pop();
                            print!("\x08 \x08");
                            let _ = io::stdout().flush();
                        }
                    }
                    KeyCode::Up => {
                        if hist_idx > 0 {
                            // Erase current line
                            for _ in &line_chars {
                                print!("\x08 \x08");
                            }
                            hist_idx -= 1;
                            let entry = history_get(&s.history, hist_idx);
                            line_chars = entry.chars().collect();
                            print!("{}", entry);
                            let _ = io::stdout().flush();
                        }
                    }
                    KeyCode::Down => {
                        if hist_idx < s.history.len() {
                            for _ in &line_chars {
                                print!("\x08 \x08");
                            }
                            hist_idx += 1;
                            if hist_idx < s.history.len() {
                                let entry = history_get(&s.history, hist_idx);
                                line_chars = entry.chars().collect();
                                print!("{}", entry);
                            } else {
                                line_chars.clear();
                            }
                            let _ = io::stdout().flush();
                        }
                    }
                    KeyCode::Esc => {
                        // Clear current line on Escape
                        for _ in &line_chars {
                            print!("\x08 \x08");
                        }
                        line_chars.clear();
                        let _ = io::stdout().flush();
                    }
                    _ => {}
                }
            }
            Err(_) => {
                let _ = disable_raw_mode();
                return "QUIT".to_string();
            }
            _ => {}
        }
    }
}

fn history_get(history: &VecDeque<String>, idx: usize) -> String {
    history.get(idx).cloned().unwrap_or_default()
}
