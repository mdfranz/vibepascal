use clap::Parser;

mod commands;
mod game;
mod io;
mod output;
mod persistence;
mod state;
mod summary;
mod types;
mod world;

use crate::commands::process_command;
use crate::game::new_game;
use crate::io::custom_read_ln;

#[derive(Parser, Debug)]
#[command(name = "dustwood-rs", about = "Dustwood text adventure (Rust port)")]
struct Cli {
    #[arg(long, default_value_t = false)]
    headless: bool,

    #[arg(long, default_value_t = 25)]
    turns: i32,

    #[arg(long)]
    seed: Option<u64>,

    #[arg(long, default_value_t = false)]
    autosave: bool,

    #[arg(long, default_value_t = 5)]
    autosave_interval: i32,

    #[arg(long, default_value = "data/autosave.json")]
    autosave_path: String,

    #[arg(long, default_value_t = false)]
    mcp_http: bool,

    #[arg(long, default_value_t = false)]
    mcp_stdio: bool,

    #[arg(long, default_value = "127.0.0.1:8765")]
    mcp_addr: String,

    #[arg(long, default_value = "/mcp")]
    mcp_path: String,

    #[arg(long)]
    mcp_token: Option<String>,

    #[arg(long, default_value_t = false)]
    mcp_json_response: bool,

    #[arg(long, default_value_t = false)]
    mcp_stateless: bool,

    #[arg(long, action = clap::ArgAction::Append)]
    mcp_origin: Vec<String>,
}

fn main() {
    let cli = Cli::parse();

    if cli.mcp_http || cli.mcp_stdio {
        eprintln!("MCP server mode not yet implemented in this build (Phase 3).");
        std::process::exit(1);
    }

    let mut s = new_game(cli.seed, cli.turns);
    s.is_headless = cli.headless;
    s.autosave_enabled = cli.autosave;
    s.autosave_interval = cli.autosave_interval;
    s.autosave_path = cli.autosave_path;

    while s.is_playing {
        let cmd = custom_read_ln(&mut s, "> ");
        process_command(&mut s, &cmd);
    }

    println!();
    println!("🏆 Final score: {}", s.score);
}
