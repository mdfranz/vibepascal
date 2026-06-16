# Framework Packages

Each AI framework package has isolated dependencies and its own virtual environment. This allows testing frameworks with incompatible library versions side-by-side.

For deep dives into individual clients, execution flows, and telemetry schemas, see:
- [packages/IMPL.md](file:///home/mfranz/github/vibepascal/packages/IMPL.md) — Implementation breakdown of each client.
- [packages/FLOW.md](file:///home/mfranz/github/vibepascal/packages/FLOW.md) — Control loop and logical boundary comparisons.
- [packages/shared/OBSERVABILITY.md](file:///home/mfranz/github/vibepascal/packages/shared/OBSERVABILITY.md) — Hook APIs and standardized token event schemas.

## Framework Native Capabilities

What each framework provides out-of-the-box, and how we use it:

| Capability | Pydantic AI | Agno | Strands | ADK |
| :--- | :--- | :--- | :--- | :--- |
| **Loop driver** | `agent.iter()` autonomous node loop | Explicit `async while` — one `agent.arun()` per turn | `agent(prompt)` single synchronous call | `runner.run_async()` event stream |
| **MCP integration** | `MCPToolset` (native) | `MCPTools` (native) | `MCPClient` (native, multi-transport) | `McpToolset` (native, `tool_filter` support) |
| **Observability hooks** | Usage delta from `agent_run.usage` each node | `post_hooks=[fn]` on `Agent` | 6 typed lifecycle events (`BeforeInvocationEvent` … `AfterToolCallEvent`) | `after_model_callback` on `Agent` |
| **Model routing** | `KnownModelName` strings (`google-gla:`, `anthropic:`, …) | Native model objects (`Claude`, `Gemini`, `OpenAIChat`, `Ollama`) | LiteLLM model strings — any provider | Native Gemini ID or `LiteLlm(model=…)` |
| **Context/budget control** | `UsageLimits(request_limit=N)` | Manual sliding history window | `SlidingWindowConversationManager(window_size=N)` | `RunConfig(max_llm_calls=N)` |
| **Extended thinking** | `Thinking()` capability + `anthropic_thinking` model settings | Metrics via `run_output.metrics.reasoning_tokens` | Via LiteLLM (provider-dependent) | `thoughts_token_count` in `usage_metadata` |
| **Command policy** | None — model decides | Loop-level `CommandPolicy.rewrite()` before each MCP call | `BeforeToolCall` hook rewrites `tool_input` in-place | None — model decides |
| **Session abstraction** | None | None | None | `InMemorySessionService` + `Runner` |

> **Command policy** (`vibepascal_shared.mcp_command_policy`) is our own layer — not native to any framework. Agno and Strands use it; Pydantic AI and ADK rely on the model to vary its behavior.

## Structure

```
packages/
├── shared/              # vibepascal-shared library
│   ├── pyproject.toml
│   ├── vibepascal_shared/
│   │   ├── guidance_loader.py   # load_guidance(), format_guidance_block()
│   │   ├── llm_observability.py # setup_logger(), log_kv(), Timer, …
│   │   └── mcp_command_policy.py
│   ├── client.py        # Generic HTTP test client
│   └── mcp_benchmark.py # Multi-client benchmark runner
│
├── pydantic/            # Pydantic AI (2.0.0b3)
│   ├── pyproject.toml
│   └── pydantic_mcp_client.py
│
├── agno/                # Agno 2.6.9+
│   ├── pyproject.toml
│   └── agno_mcp_client.py
│
├── strands/             # Strands Agents SDK + LiteLLM
│   ├── pyproject.toml
│   └── strands_mcp_client.py
│
└── adk/                 # Google ADK (MCP)
    ├── pyproject.toml
    └── adk_mcp_client.py
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
./adk-mcp-game.sh full gemini-3.5-flash 1 5
```

Each script automatically invokes `uv run --project packages/<fw>`, ensuring the right venv is used.

## Dependencies

### Common to all frameworks

| Dependency | Source | Purpose |
| :--- | :--- | :--- |
| `python-dotenv>=1.2.1` | declared | `.env` loading |
| `pydantic>=2.0` | declared (agno, strands, adk) | data validation |
| `vibepascal-shared` | declared (editable local) | shared logging, guidance, command policy |
| `httpx>=0.28.1` | via shared | HTTP client used by shared and most framework SDKs |
| `asyncio` | stdlib | async runtime — all MCP clients are `async def` |
| `logging`, `os`, `time` | stdlib | used in every client |

### Critical unique dependencies

| Framework | Key package | Why isolated |
| :--- | :--- | :--- |
| **pydantic** | `pydantic-ai==2.0.0b3` | Pre-release; pins `pydantic` to a pre-release version incompatible with other frameworks. Requires `uv sync --prerelease=allow`. |
| **agno** | `agno>=2.5.11` | Bundles its own provider SDKs (`anthropic`, `openai`, `google-genai`, `ollama`) at potentially different versions than other frameworks need. Also declares `mcp>=1.0.0` separately. |
| **strands** | `strands-agents>=1.33.0` + `litellm>=1.81.16` | LiteLLM is a large transitive dependency tree with many provider-specific pins that conflict with direct provider SDKs used by other frameworks. |
| **adk** | `google-adk[extensions,mcp]==2.0.0` | Pinned version; `extensions` extra pulls in LiteLLM for non-Gemini model routing. Google-specific and incompatible with agno/strands provider SDK versions. |

### MCP transport

`agno` and `strands` declare `mcp>=1.0.0` explicitly (the reference MCP SDK). `pydantic-ai` and `adk` ship their own MCP client implementations — no separate `mcp` package needed.

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

## Related Documentation

- **Overview Index:** [README.md](file:///home/mfranz/github/vibepascal/README.md)
- **Client Implementations:** [packages/IMPL.md](file:///home/mfranz/github/vibepascal/packages/IMPL.md)
- **Control Flows:** [packages/FLOW.md](file:///home/mfranz/github/vibepascal/packages/FLOW.md)
- **Observability Setup:** [packages/shared/OBSERVABILITY.md](file:///home/mfranz/github/vibepascal/packages/shared/OBSERVABILITY.md)
- **Dependency Rationale:** [PKG.md](file:///home/mfranz/github/vibepascal/PKG.md) — Detailed rationale for system dependencies.
