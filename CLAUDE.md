# Echoes of Dustwood — Codebase Guide

## Overview

Echoes of Dustwood is a Free Pascal/Go text adventure game with AI agent integration via the Model Context Protocol (MCP). The repo compares 4 AI frameworks (Pydantic AI, Agno, Strands, ADK) on autonomous gameplay.

## Key Files

### Game Engines
- **`src/pascal/dustwood.pas`** — Main game logic (Free Pascal). Core state machine, command parser, survival mechanics.
- **`src/golang/main.go`** — Go port of the same logic (preferred for MCP due to better concurrency).
- **`data/world.ini`** — Game world definition (rooms, items, exits).
- **`Makefile`** — Build targets: `make build-go`, `make build-pascal`.

### AI Clients (Framework Packages)

Each lives in `packages/<framework>/` with its own venv:

#### Pydantic AI (`packages/pydantic/`)
- `pydantic_client.py` — Direct stdio gameplay
- `pydantic_mcp_client.py` — MCP-based gameplay (recommended)
- **Setup:** `cd packages/pydantic && uv sync --prerelease=allow`
- **Model format:** `google:gemini-3.5-flash` or `anthropic:claude-3-5-sonnet-20241022`

#### Agno (`packages/agno/`)
- `agno_client.py` — Direct stdio gameplay
- `agno_mcp_client.py` — MCP-based gameplay
- **Setup:** `cd packages/agno && uv sync --upgrade`
- **Framework:** Uses Agno's built-in MCP tools

#### Strands (`packages/strands/`)
- `strands_client.py` — Direct stdio gameplay
- `strands_mcp_client.py` — MCP-based gameplay
- **Setup:** `cd packages/strands && uv sync --upgrade`
- **Framework:** LiteLLM for broad model support

#### ADK (`packages/adk/`)
- `adk_mcp_client.py` — MCP-based gameplay
- **Setup:** `cd packages/adk && uv sync --upgrade`
- **Framework:** Google ADK with MCP toolset integration

### Shared Utilities (`packages/shared/`)
- `vibepascal_shared/guidance_loader.py` — Loads difficulty-based gameplay hints
- `vibepascal_shared/llm_observability.py` — Logging, HTTP debugging, timing
- `vibepascal_shared/mcp_command_policy.py` — Command validation

### Orchestrators
- Root-level `*-game.sh` scripts (e.g., `pydantic-mcp-game.sh`) — Wrapper scripts that invoke `uv run --project packages/<fw>` with the right arguments. These do **not** manage the MCP server themselves; they expect one already running.
- `play-mcp-game.sh` — Runs all 4 frameworks sequentially for benchmarking. Unlike the single-framework scripts, this one **does** manage its own MCP server lifecycle — see the restart note below.

## Architecture Decisions

### Why Per-Framework Venvs?
The frameworks have incompatible dependency versions (e.g., pydantic-ai 2.0.0b3 requires pydantic 2.14a1, while agno works with stable pydantic 2.13). Monolithic venv doesn't work; isolation does.

### Why MCP?
The original "stdio" clients parse game output as text. MCP clients use structured tool calls, allowing agents to:
1. Call `command(cmd="LOOK")` and get back JSON state
2. Reason about structured game state, not text
3. Avoid hallucinations from parsing

### Why Go Over Pascal?
The Go port (`bin/dustwood-go`) handles HTTP MCP better (async I/O, signal handling). Pascal version is the original but less suitable for server workloads.

## Common Tasks

### Restart-after-`GAME OVER` is off by default (server-side)
`bin/dustwood-go`'s `--allow-restart` flag (default **off**) controls whether `reset_game` /
`command(reset=true)` work once a game has ended (win, death, or day/night timeout) — see
`src/golang/mcp_server.go`. With it off (default), a model gets exactly one attempt per server
process; with it on, models can retry after `GAME OVER` (the old behavior). Because this is
per-*process*, **one server instance must not be reused across multiple separate client runs**
unless `--allow-restart` is passed — the second run's own required bootstrap `reset=true` call
would be rejected too, since the server can't distinguish "a new run starting" from "the same
model retrying." Restart the server between every individual `*-game.sh` invocation (`play-mcp-game.sh`
does this automatically for its own 4 sub-runs; the single-framework scripts below do not, so do it
yourself). Also match the server's `--turns` to the client's `max_turns` argument — they're
independent, and the *server's* turn limit governs the in-game day/night cutoff regardless of what
turn count you pass the client.

### Test One Framework
```bash
./bin/dustwood-go --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response --turns 25 &
./pydantic-mcp-game.sh google-gla:gemini-3.5-flash 25 1 full
```

### Upgrade Agno to Latest
```bash
cd packages/agno
uv sync --upgrade
```

### Benchmark All 4 Frameworks
```bash
./play-mcp-game.sh google-gla:gemini-3.5-flash 25 1 full
```
No need to start the server yourself — `play-mcp-game.sh` builds `bin/dustwood-go` and restarts it
fresh before each of the 4 clients. Pass `--allow-restart` to let models retry after `GAME OVER`
instead of getting one attempt each.

### Add a New Game Command
1. Edit `src/golang/commands.go` (preferred) or `src/pascal/u_commands.pas`
2. Rebuild: `make build-go` or `make build-pascal`
3. Test with an MCP client (it will auto-discover new tools)

### Debug an Agent Failure
1. Check wrapper script logs in `logs/` (if enabled)
2. Look at `packages/shared/vibepascal_shared/llm_observability.py` — enables HTTP/model payload logging via env vars: `HTTP_DEBUG_LOGGING=1`, `PROVIDER_PAYLOAD_LOGGING=1`
3. MCP server logs: redirect `./bin/dustwood-go --mcp-http ... > logs/mcp-server.log 2>&1`

## Git & Commits

The repo recently underwent a restructure (May 2026):
- Moved `scripts/` clients into isolated `packages/<fw>/` with per-framework pyproject.toml
- Each framework now has its own lockfile and venv
- Updated all wrapper scripts to use `uv run --project`

If you're working on a feature branch, expect `RM` and `R` entries in `git status` (git mv moves), not new files.

## Testing

No automated test suite currently. Manual testing via the MCP clients (see "Common Tasks" above).

## References

- **Documentation:** See the main [README.md](file:///home/mfranz/github/vibepascal/README.md) for the project overview and index.
- **System Architecture:** [src/ARCHITECTURE.md](file:///home/mfranz/github/vibepascal/src/ARCHITECTURE.md) — Detailed mirrored engines walkthrough.
- **Framework Setup:** [packages/README.md](file:///home/mfranz/github/vibepascal/packages/README.md) — How virtual environments are structured.
- **Game world:** `data/world.ini` — Edit to add/modify rooms and items
- **MCP spec:** Game exposes `command` tool; see `src/golang/mcp_server.go` for implementation
- **Framework docs:**
  - Pydantic AI: https://docs.pydantic.dev/latest/concepts/agents/
  - Agno: https://docs.phidata.com/
  - Strands: https://github.com/strands-ai/strands-agents-api
