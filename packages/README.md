# Framework Packages

Each AI framework package has isolated dependencies and its own virtual environment. This allows testing frameworks with incompatible library versions side-by-side.

## Structure

```
packages/
├── shared/              # vibepascal-shared library
│   ├── pyproject.toml
│   ├── vibepascal_shared/
│   │   ├── guidance_loader.py
│   │   ├── llm_observability.py
│   │   └── mcp_command_policy.py
│   ├── client.py        # Generic HTTP test client
│   └── mcp_benchmark.py # Multi-client benchmark runner
│
├── pydantic/            # Pydantic AI (2.0.0b3)
│   ├── pyproject.toml
│   ├── pydantic_client.py
│   └── pydantic_mcp_client.py
│
├── agno/                # Agno 2.6.9+
│   ├── pyproject.toml
│   ├── agno_client.py
│   └── agno_mcp_client.py
│
├── strands/             # Strands Agents SDK + LiteLLM
│   ├── pyproject.toml
│   ├── strands_client.py
│   └── strands_mcp_client.py
│
└── ms_agent/            # Microsoft Agent Framework
    ├── pyproject.toml
    ├── ms_agent_client.py
    └── ms_agent_mcp_client.py
```

## Setup

### First Time (One Per Package)

```bash
cd packages/<framework>
uv sync [--upgrade] [--prerelease=allow]
```

### Run

From the repo root, use the orchestrator scripts:

```bash
./pydantic-mcp-game.sh full google:gemini-3.5-flash 1 5
./agno-mcp-game.sh full gpt-4o-mini 1 5
./strands-mcp-game.sh full gemini/gemini-3.5-flash 1 5
./ms-agent-mcp-game.sh full gpt-4o-mini 1 5
```

Each script automatically invokes `uv run --project packages/<fw>`, ensuring the right venv is used.

## Shared Utilities

All frameworks depend on `packages/shared/vibepascal_shared/`:

- **guidance_loader.py** — Load difficulty-based gameplay guidance (full/medium/minimal)
- **llm_observability.py** — Structured logging, HTTP debugging, performance timing
- **mcp_command_policy.py** — Command validation and sanitization for MCP safety

The shared package is installed as an editable local dependency in each framework's `pyproject.toml`.

## Dependencies

### pydantic (2.0.0b3)
- **Note:** Pre-release version requires `uv sync --prerelease=allow`
- Models: Google Gemini, Anthropic Claude
- No MCP client library (uses raw HTTP)

### agno (2.6.9+)
- Upgraded to latest stable on `uv sync --upgrade`
- Models: Google Gemini, Anthropic Claude, OpenAI GPT, Ollama
- Built-in MCP tools support

### strands (1.33.0+)
- LiteLLM for model abstraction (broad provider support)
- mcp library for official MCP SDK
- Models: Any model supported by LiteLLM (Gemini, Claude, GPT, local Ollama, etc.)

### ms_agent
- agent-framework and agent-framework-anthropic
- Models: OpenAI, Anthropic, Google, Ollama
- Built-in tool/MCP integration

## Isolation Benefits

- **No version conflicts** — Each framework can use incompatible versions of shared deps (e.g., pydantic 2.13 vs 2.14a1)
- **Smaller venvs** — Only install what you need
- **Cleaner upgrades** — Upgrade one framework without affecting others
- **Easier debugging** — Dependency issues are scoped to one package

## Benchmarking

Run all 4 frameworks against a single model:

```bash
./play-mcp-game.sh google:gemini-3.5-flash full 1 5
```

This script invokes each `*-mcp-game.sh` sequentially and compares performance.
