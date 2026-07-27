# Dustwood Language Port Comparison: Go vs. Rust vs. .NET

This document provides a technical comparison of the **Go**, **Rust**, and **.NET (C#)** engine implementations for *Echoes of Dustwood*. All three stacks preserve identical gameplay logic parity, but differ significantly in paradigm, memory safety strategies, persistence formats, I/O streaming, and MCP (Model Context Protocol) architectures.

---

## 1. Module-by-Module Mapping

All three codebases adopt a consistent structure where each component maps directly to its counterpart in the other languages:

| Architectural Role | Go Implementation | Rust Implementation | .NET (C#) Implementation | Key Architectural Differences |
| :--- | :--- | :--- | :--- | :--- |
| **CLI Entry Point** | [main.go](golang/main.go) | [main.rs](rust/src/main.rs) | [Program.cs](dotnet/Program.cs) | Go uses stdlib `flag`; Rust uses `clap`; .NET uses a custom args parser loop. Rust exposes `--mcp-stdio`; Go and .NET focus on HTTP MCP. |
| **Constants & Schema** | [types.go](golang/types.go) | [types.rs](rust/src/types.rs) | [Types.cs](dotnet/Types.cs) | Go uses `const` blocks; Rust uses typed `pub const`; .NET uses a static `Constants` class. |
| **State Definitions** | [state.go](golang/state.go) | [state.rs](rust/src/state.rs) | [State.cs](dotnet/State.cs) | Go uses GC struct; Rust embeds `SmallRng` + `VecDeque`; .NET uses object properties with fixed arrays and `TextWriter Out`. |
| **World Loader** | [world.go](golang/world.go) | [world.rs](rust/src/world.rs) | [World.cs](dotnet/World.cs) | Go maps exits via pointers (`*Room`); Rust uses ID indices (`usize`); .NET uses direct object references (`Room?`). |
| **Core Game Loops** | [game.go](golang/game.go) | [game.rs](rust/src/game.rs) | [Game.cs](dotnet/Game.cs) | .NET wraps execution in `Game.ExecuteCommand` using `StringWriter` to capture output for MCP responses cleanly. |
| **Command Processing** | [commands.go](golang/commands.go) | [commands.rs](rust/src/commands.rs) | [Commands.cs](dotnet/Commands.cs) | Go matches struct registrations; Rust uses fast `match`; .NET uses `SafeVerbs` `HashSet<string>` and switch branches. |
| **Terminal I/O** | [io.go](golang/io.go) | [io.rs](rust/src/io.rs) | [IO.cs](dotnet/IO.cs) | Go uses `golang.org/x/term`; Rust uses `crossterm`; .NET uses `Console.ReadKey(true)` for custom arrow-key history scrolling. |
| **Output Wrapping** | [output.go](golang/output.go) | [output.rs](rust/src/output.rs) | [IO.cs](dotnet/IO.cs) | Go uses `io.Writer` interface; Rust uses `out_println!` macro; .NET uses `IO.OutPrintln(s)` checking `TextWriter Out`. |
| **Persistence** | [persistence.go](golang/persistence.go) | [persistence.rs](rust/src/persistence.rs) | [Persistence.cs](dotnet/Persistence.cs) | Go uses binary `bbolt` DB; Rust and .NET serialize intermediate state objects to pretty-printed JSON via `System.Text.Json` / `serde`. |
| **State Summarization** | [summary.go](golang/summary.go) | [summary.rs](rust/src/summary.rs) | [Summary.cs](dotnet/Summary.cs) | Go/Rust/C# marshal structured JSON state summaries using attributes (`[JsonPropertyName]` in .NET). |
| **MCP Integration** | [mcp_server.go](golang/mcp_server.go) | [mcp_server.rs](rust/src/mcp_server.rs) | [MCPServer.cs](dotnet/MCPServer.cs) | Go uses official Go SDK; Rust uses `rmcp` + `axum`; .NET uses ASP.NET Core (`WebApplication.CreateBuilder()`) minimal API POST `/mcp`. |

---

## 2. Core Differences in Detail

### A. Graph Representation & Memory Model

One of the most profound differences lies in how the room graph connectivity is represented in memory across GC, borrowing, and runtime-managed environments:

*   **Go Pointers (`*Room`)**:
    In [types.go](golang/types.go#L26-L34), Go represents spatial exits as direct pointers:
    ```go
    type Room struct {
        ID          int
        Name        string
        Description string
        North       *Room
        South       *Room
        East        *Room
        West        *Room
    }
    ```
    Relies on Go's garbage collector to manage allocations.

*   **Rust Indices (`usize`)**:
    In [types.rs](rust/src/types.rs#L22-L31), Rust represents exits as room IDs (integers corresponding to vector offsets):
    ```rust
    pub struct Room {
        pub id: usize,
        pub name: String,
        pub description: String,
        pub north: usize,
        pub south: usize,
        pub east: usize,
        pub west: usize,
    }
    ```
    Avoids graph cycle ownership issues under Rust's borrow checker via a flat lookup vector (`Vec<Option<Room>>`).

*   **.NET Direct Object References (`Room?`)**:
    In [Types.cs](dotnet/Types.cs), .NET uses nullable reference types (`Room?`):
    ```csharp
    public class Room
    {
        public int Id { get; set; }
        public string Name { get; set; } = "";
        public string Description { get; set; } = "";
        public Room? North { get; set; }
        public Room? South { get; set; }
        public Room? East { get; set; }
        public Room? West { get; set; }
    }
    ```
    Managed by the .NET CLR GC with direct object references.

---

### B. Detailed Comparison of Data Structures

#### 1. Room Representation & Registry
*   **Free Pascal**: Fixed array of pointers: `RoomRegistry: array[1..MAX_ROOMS] of PRoom` with raw pointers (`PRoom = ^TRoom`).
*   **Go**: Fixed pointer array `RoomRegistry [MaxRooms + 1]*Room` chaining direct `*Room` pointers.
*   **Rust**: Contiguous vector of optional entries `Vec<Option<Room>>` with primitive `usize` index exits.
*   **.NET (C#)**: Sized array `Room?[] RoomRegistry` indexed by room ID (1..15). Direct nullable `Room?` reference chaining for exits.

#### 2. Item Tracking & Inventory
*   **Free Pascal**: `TItem = record` stored in static array `Items: array[1..MAX_ITEMS] of TItem`.
*   **Go**: `type Item struct` stored in array `Items [MaxItems + 1]Item`.
*   **Rust**: `pub struct Item` stored in `Vec<Item>` sized to `MAX_ITEMS + 1`.
*   **.NET (C#)**: `public class Item` initialized in fixed-length array `Item[] Items = new Item[Constants.MaxItems + 1]`.

#### 3. Command History Buffer
*   **Free Pascal / Go**: Fixed-size string array with explicit `HistoryCount` shifting.
*   **Rust**: `VecDeque<String>` using `push_back()` and `pop_front()` when exceeding `MAX_HISTORY`.
*   **.NET (C#)**: Fixed string array `string[] History = new string[Constants.MaxHistory + 1]` managed via `HistoryCount`.

#### 4. Summary Table of Core Structs

| Data Structure / Field | Free Pascal Reference | Go Port | Rust Port | .NET Port |
| :--- | :--- | :--- | :--- | :--- |
| **Room Collection** | `array[1..MAX_ROOMS] of PRoom` | `[MaxRooms + 1]*Room` | `Vec<Option<Room>>` | `Room?[]` (size `MaxRooms + 1`) |
| **Room Exits** | Typed pointers (`PRoom = ^TRoom`) | Pointer references (`*Room`) | Primitive vector index (`usize`) | Nullable object references (`Room?`) |
| **Invalid Exits** | `nil` | `nil` | `0` (denotes `None`) | `null` |
| **Item Collection** | `array[1..MAX_ITEMS] of TItem` | `[MaxItems + 1]Item` | `Vec<Item>` | `Item[]` (size `MaxItems + 1`) |
| **Command History** | `array[0..MAX_HISTORY] of string` | `[MaxHistory + 1]string` | `VecDeque<String>` | `string[]` (size `MaxHistory + 1`) |
| **Memory Management** | Manual pointer heap allocation | GC-managed heap | Monolithic state block / stack | CLR GC-managed heap |

---

### C. Persistence Model (BoltDB vs. Serde JSON vs. .NET JSON)

*   **Go's BBolt Key-Value Database**: Writes to a local binary key-value store using `bbolt` across 3 buckets (`state`, `items`, `scoreflags`). Saves to `data/autosave.db`.
*   **Rust's Serde JSON**: Serializes state to intermediate `SaveData` struct via `serde_json`. Saves pretty-printed JSON to `data/autosave.json`.
*   **.NET's System.Text.Json**: Serializes `SaveStateData` DTO object using `System.Text.Json` with `WriteIndented = true`. Saves formatted JSON to `data/autosave.db` (or custom path).

---

### D. Output Capturing & Terminal I/O

*   **Go (`io.Writer` interface)**: Swaps out `s.Out` (`io.Writer`) for an in-memory `bytes.Buffer` during MCP tool calls.
*   **Rust (Conditional Macros)**: Uses macro `out_println!` checking `GameState::capture` buffer.
*   **.NET (`TextWriter` Property)**: `GameState.Out` holds a `TextWriter` reference (defaults to `Console.Out`). In `Game.ExecuteCommand`, it temporarily replaces `Out` with a `StringWriter` to capture game output cleanly for JSON-RPC tool responses.

---

### E. Model Context Protocol (MCP) Infrastructures

*   **Go Server**: Built with `github.com/modelcontextprotocol/go-sdk`. Serves HTTP SSE via `mcp.NewStreamableHTTPHandler`.
*   **Rust Server**: Built with `rmcp` and `axum`. Exposes `--mcp-stdio` CLI mode alongside HTTP SSE web routes.
*   **.NET Server**: Built with ASP.NET Core (`WebApplication.CreateBuilder()`). Maps POST `/mcp` handling JSON-RPC (`initialize`, `tools/list`, `tools/call`, `resources/list`, `resources/read`, `prompts/list`, `prompts/get`) and GET `/health`.

---

### F. RNG Design & Seed Determinism

*   **Go RNG**: Global package-level PRNG (`math/rand`).
*   **Rust RNG**: Local `SmallRng` instance attached to `GameState`.
*   **.NET RNG**: `System.Random` instance encapsulated in `Game.Rng`, ensuring isolated per-game instance seed determinism.

---

## 3. Dependency Matrix

| Capability | Go Packages | Rust Crates | .NET (C#) Libraries |
| :--- | :--- | :--- | :--- |
| **Model Context Protocol** | `github.com/modelcontextprotocol/go-sdk` | `rmcp` | Custom ASP.NET Core JSON-RPC (`MCPServer.cs`) |
| **Config File Parsing** | `gopkg.in/ini.v1` | `configparser` | Custom INI parser (`World.cs`) |
| **Raw Terminal Management** | `golang.org/x/term` | `crossterm` | `Console.ReadKey(true)` custom loop (`IO.cs`) |
| **JSON Support** | Go stdlib (`encoding/json`) | `serde`, `serde_json` | `System.Text.Json` |
| **CLI Parser** | Go stdlib (`flag`) | `clap` | Custom args parser (`Program.cs`) |
| **HTTP Framework** | Go stdlib (`net/http`) | `axum`, `tokio` | `Microsoft.AspNetCore` |
| **State Persistence** | `go.etcd.io/bbolt` | Standard file write | `System.IO` + `System.Text.Json` |
| **RNG Provider** | Go stdlib (`math/rand`) | `rand` (`small_rng`) | `System.Random` |

---

## 4. Key Performance and Architecture Summary

1.  **Memory Management Overhead**:
    *   **Go**: Garbage-collected struct pointers.
    *   **Rust**: Strict ownership model with flat index vectors (`usize`) and zero-cost abstraction macros.
    *   **.NET**: Managed runtime with clean object-oriented encapsulation and `TextWriter` abstraction.
2.  **Concurrency and Server Scaling**:
    *   **Go**: Lightweight goroutines with global PRNG seeding caveat.
    *   **Rust**: Safe async architecture via `axum` with isolated per-state `SmallRng`.
    *   **.NET**: ASP.NET Core Kestrel HTTP engine with thread-safe `MCPServer` locking and isolated `System.Random` per game instance.
3.  **Saved Game Accessibility**:
    *   **Go**: Binary `bbolt` key-value store requiring DB tools to inspect.
    *   **Rust & .NET**: Human-readable, pretty-printed JSON formats supporting quick inspection and debugging.
