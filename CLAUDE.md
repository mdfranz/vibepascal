# Echoes of Dustwood — Codebase Guide

## Overview

Echoes of Dustwood is a Free Pascal/Go text adventure game with AI agent integration via the Model Context Protocol (MCP). The repo compares 4 AI frameworks (Pydantic AI, Agno, Strands, Microsoft Agent Framework) on autonomous gameplay.

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

#### Microsoft Agent Framework (`packages/ms_agent/`)
- `ms_agent_client.py` — Direct stdio gameplay
- `ms_agent_mcp_client.py` — MCP-based gameplay
- **Setup:** `cd packages/ms_agent && uv sync --upgrade`
- **Framework:** Built-in MCP tool integration

### Shared Utilities (`packages/shared/`)
- `vibepascal_shared/guidance_loader.py` — Loads difficulty-based gameplay hints
- `vibepascal_shared/llm_observability.py` — Logging, HTTP debugging, timing
- `vibepascal_shared/mcp_command_policy.py` — Command validation

### Orchestrators
- Root-level `*-game.sh` scripts (e.g., `pydantic-mcp-game.sh`) — Wrapper scripts that invoke `uv run --project packages/<fw>` with the right arguments.
- `play-mcp-game.sh` — Runs all 4 frameworks sequentially for benchmarking.

## Architecture Decisions

### Why Per-Framework Venvs?
The 4 frameworks have incompatible dependency versions (e.g., pydantic-ai 2.0.0b3 requires pydantic 2.14a1, while agno works with stable pydantic 2.13). Monolithic venv doesn't work; isolation does.

### Why MCP?
The original "stdio" clients parse game output as text. MCP clients use structured tool calls, allowing agents to:
1. Call `command(cmd="LOOK")` and get back JSON state
2. Reason about structured game state, not text
3. Avoid hallucinations from parsing

### Why Go Over Pascal?
The Go port (`bin/dustwood-go`) handles HTTP MCP better (async I/O, signal handling). Pascal version is the original but less suitable for server workloads.

## Common Tasks

### Test One Framework
```bash
./bin/dustwood-go --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response &
./pydantic-mcp-game.sh full google:gemini-3.5-flash 1 5
```

### Upgrade Agno to Latest
```bash
cd packages/agno
uv sync --upgrade
```

### Benchmark All 4 Frameworks
```bash
./bin/dustwood-go --mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response &
./play-mcp-game.sh google:gemini-3.5-flash full 1 5
```

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

- **Game world:** `data/world.ini` — Edit to add/modify rooms and items
- **MCP spec:** Game exposes `command` tool; see `src/golang/mcp_server.go` for implementation
- **Framework docs:**
  - Pydantic AI: https://docs.pydantic.dev/latest/concepts/agents/
  - Agno: https://docs.phidata.com/
  - Strands: https://github.com/strands-ai/strands-agents-api
  - Microsoft Agent: https://github.com/microsoft/autogen

