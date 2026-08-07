# Observability in MCP Clients

This document summarizes the current observability implementation across the active framework clients in this repository:

- `packages/adk/adk_mcp_client.py`
- `packages/agno/agno_mcp_client.py`
- `packages/pydantic/pydantic_mcp_client.py` — **now uses Logfire instead of the shared `log_kv` schema below; see [Pydantic AI](#pydantic-ai) and [Logfire (Pydantic AI only)](#logfire-pydantic-ai-only).**
- `packages/strands/strands_mcp_client.py`


## Shared Foundation: `vibepascal_shared.llm_observability`

ADK, Agno, and Strands rely on a shared utility module for structured telemetry (Pydantic AI now
uses Logfire instead — see below — but still uses `setup_logger`/`print_game` from this module for
basic operational logging and game-narrative console output):

- `setup_logger(name, log_file)`: creates the file + optional console handler; wires HTTP debug logging. Called once per client at module level — replaces ~20 lines of duplicated setup.
- `log_kv`: emits one-line `key=value` logs with JSON-safe value encoding.
- `Timer`: latency timing in milliseconds.
- `format_payload`: serialization + redaction + truncation.
- `redact_secrets`: secret scrubbing.

### Environment Controls

- `LOG_PAYLOADS` (default `True`): include full request/response/tool payloads.
- `GAME_CONSOLE` (default `True`): print game narrative to stdout.
- `LOG_CONSOLE` (default `False`): duplicate structured logs to stderr.
- `LOG_HTTP` (default `False`): enable low-level HTTP logging.
- `LOG_MAX_CHARS` (default `20000`): payload truncation bound.
- `AI_REASONING` (default `False`): enable extended thinking for Pydantic AI (Anthropic only — sets `Thinking()` capability and `anthropic_thinking` model settings).
- `LOGFIRE_ENABLED` (default `1` under `pydantic-mcp-game.sh`, `0`/unset otherwise) — **Pydantic AI only**, checked directly in `pydantic_mcp_client.py` (not part of `llm_observability.py`). Gates `logfire.configure()` + `logfire.instrument_pydantic_ai()`.
- `LOGFIRE_ENVIRONMENT` (default `development`) — **Pydantic AI only**. Sets the Logfire `deployment.environment` resource attribute.

> **Note:** `play-mcp-game.sh` exports `AI_REASONING=1` and `LOG_CONSOLE=1` unconditionally, so all benchmark runs have thinking enabled for Pydantic AI and console log output mirrored to stderr. `pydantic-mcp-game.sh` additionally exports `LOGFIRE_ENABLED=1` by default.

## Canonical Token Event Schema

> **Pydantic AI no longer emits these `log_kv` events.** It moved to Logfire (see
> [Logfire (Pydantic AI only)](#logfire-pydantic-ai-only)) — full per-model-call token usage and
> per-tool-call args/results are captured automatically by `logfire.instrument_pydantic_ai()`
> instead of the `provider_call`/`tool_call` events below. This schema now applies to ADK, Agno,
> and Strands only.

ADK, Agno, and Strands emit a `provider_call` event with the following normalized top-level fields:

```
event="provider_call"  client=<fw>  model=<id>  latency_ms=<n>
  input_tokens=<n>  output_tokens=<n>  total_tokens=<n>
  reasoning_tokens=<n>          # when available
  cache_read_tokens=<n>         # when available
  tool_calls=<n>                # when available
```

ADK, Agno, and Strands emit a `run_summary` event at session end:

```
event="run_summary"  client=<fw>  model=<id>  token_scope="run_total"
  input_tokens=<n>  output_tokens=<n>  total_tokens=<n>
  requests=<n>
  latency_ms=<n>          # ADK, Agno, Strands only
  stop_reason=<str>       # ADK, Agno, Strands only
  cache_read_tokens=<n>   # ADK, Pydantic AI only
  reasoning_tokens=<n>    # Agno only
```

## Instrumentation Pattern by Framework

### ADK

Pattern: `after_model_callback` on the `Agent` constructor.

- `_make_after_model_callback(resolved_model_id)` returns a callback registered as `after_model_callback=`.
- The callback receives `(callback_context, llm_response)` and reads `llm_response.usage_metadata`.
- Returns `None` to leave the response unmodified.

Token fields extracted from `usage_metadata`:

| ADK field | Emitted as |
| :--- | :--- |
| `prompt_token_count` | `input_tokens` |
| `candidates_token_count` | `output_tokens` |
| `total_token_count` | `total_tokens` |
| `thoughts_token_count` | `reasoning_tokens` |
| `cached_content_token_count` | `cache_read_tokens` |

### Agno

Pattern: `post_hooks=[_provider_post_hook]` on the `Agent` constructor.

- `_provider_post_hook(run_output)` is a closure registered at agent construction time.
- Latency is tracked via a `_call_timer` list: `.clear()` + `.append(Timer.start_new())` before each `arun()` call; the hook reads `_call_timer[0].elapsed_ms()` since it fires inside `arun()` before it returns.
- Token data comes from `run_output.metrics`.

Token fields:

- `input_tokens`, `output_tokens`, `total_tokens`, `reasoning_tokens`
- `tool_calls` (count of `run_output.tools`)

### Pydantic AI

**Superseded by Logfire** — see [Logfire (Pydantic AI only)](#logfire-pydantic-ai-only) below.
Historically this used the same iterator-based delta-tracking pattern described here for ADK/Agno/Strands
(reading `agent_run.usage` per node, diffing against `last_input_tokens`/`last_output_tokens`/
`last_cache_read_tokens`, emitting `provider_call`/`run_summary` via `log_kv`); that code has been
removed from `pydantic_mcp_client.py` in favor of `logfire.instrument_pydantic_ai()`, which captures
the same data (and more — full request/response spans, tool args/results) automatically.

<details>
<summary>Historical schema (pre-Logfire, kept for reference)</summary>

Token fields per step (delta):

- `input_tokens`, `output_tokens`, `total_tokens`, `cache_read_tokens`

Token fields in run summary:

- `input_tokens`, `output_tokens`, `total_tokens`, `cache_read_tokens`, `requests`

> Note: Pydantic AI 2.0.0b3 does expose a `Hooks` class, but it is located under the capabilities module (`pydantic_ai.capabilities.hooks.Hooks`) and must be registered as a capability (`capabilities=[hooks]`) rather than imported from the top-level `pydantic_ai.hooks` namespace. The iterator-based loop this client used integrated cleanly with the step-by-step console logging flow, but the native hooks capability is fully functional.

</details>

### Logfire (Pydantic AI only)

Pattern: [Logfire](https://github.com/pydantic/logfire)'s native Pydantic AI integration, gated by
`LOGFIRE_ENABLED` (see [Environment Controls](#environment-controls)).

- `logfire.configure(service_name="pydantic-mcp-client", environment=..., data_dir="~/.logfire")` runs once at
  module import time, only when `LOGFIRE_ENABLED` is truthy. `data_dir` is pinned to `~/.logfire/`
  (rather than the SDK default of a cwd-relative `.logfire/`) because this client is invoked from
  different working directories (e.g. repo root via `pydantic-mcp-game.sh`), and project write-token
  credentials need to resolve consistently regardless of cwd. Auth resolves via `LOGFIRE_TOKEN` if
  set, otherwise the cached token under `~/.logfire/` from a prior `logfire auth` / `logfire projects
  use <project> --data-dir ~/.logfire` run.
- `logfire.instrument_pydantic_ai()` auto-instruments the whole `agent.iter()` run — every model
  request/response and every MCP tool call becomes a span, with token usage attached to each model
  call automatically. This is what replaced the old `provider_call`/`tool_call` `log_kv` events.
- The client also wraps each full game session in a top-level `logfire.span("pydantic_game_run", model=...,
  level=..., max_turns=..., session_id=...)` so every span for one run (model calls, tool calls, and
  the two custom log events below) shares one `trace_id`.
- Two custom structured log events (`logfire.info(...)`), both game/run-domain data that
  `instrument_pydantic_ai()` doesn't know about on its own:
  - `game_turn` — emitted once per tool-return that carries game state: `turn`, `room`, `score`, `thirst`.
    Lets Logfire chart score-over-turns directly from the trace data (previously this required manually
    reading `[turn=... room=... score=... thirst=...]` lines out of local log files, e.g. for
    `charts/generate_charts_may27.py`'s `SCORE_TRAJ` data).
  - `run_summary` — emitted once at the end of the run: `model`, `input_tokens`, `output_tokens`,
    `total_tokens`, `cache_read_tokens`, `requests`. Nested inside the `pydantic_game_run` span, so it
    shares a `trace_id` with everything else from that run.
- When `LOGFIRE_ENABLED` is unset/false (default outside `pydantic-mcp-game.sh`), none of the above
  runs — the client falls back to only the basic operational logging described in
  [Shared Foundation](#shared-foundation-vibepascal_sharedllm_observability) (banner, warnings,
  final response dump via `setup_logger`) plus stdout game narrative via `print_game`. No per-call
  token/tool telemetry is captured locally in that case.

#### Error hardening (found via Logfire)

Running this client against OpenRouter models surfaced a gap the old local logging never caught:
a malformed tool-call argument from a weaker model (e.g. `qwen/qwen3.7-flash` sending `seed: "None"`
— a string — where the `command` tool's schema requires `null`/integer) raises
`mcp.shared.exceptions.McpError` from the MCP transport layer. `MCPToolset`'s built-in
`tool_error_behavior="retry"` only converts `fastmcp.exceptions.ToolError` into a `ModelRetry` (so
the model can see the error and self-correct); it does not cover `McpError`, so this previously
propagated uncaught and crashed the entire game run.

Fixed with two layers, both in `pydantic_mcp_client.py`:

1. `MCPToolset(..., process_tool_call=process_tool_call)` — a local `process_tool_call` wraps every
   tool call and re-raises `McpError` as `ModelRetry`, matching what `tool_error_behavior="retry"`
   already does for `ToolError`. This lets the model itself retry with corrected arguments (bounded
   by the existing `max_retries=3`); if it still can't recover, Pydantic AI raises
   `UnexpectedModelBehavior`, which was already handled gracefully.
2. A broad `except Exception` (in addition to the existing `except (UnexpectedModelBehavior,
   UsageLimitExceeded)`) around the game loop as defense in depth — any other unexpected error ends
   that run's game loop gracefully (`run_summary` still logged, `logfire.exception(...)` still
   recorded when `LOGFIRE_ENABLED`) instead of crashing the whole script, so one bad model in a
   serial multi-model benchmark run doesn't take out the rest.

### Strands

Pattern: lifecycle hooks registered via `agent.add_hook(...)`.

Registered hooks:

- `BeforeInvocationEvent` / `AfterInvocationEvent`
- `BeforeModelCallEvent` / `AfterModelCallEvent`
- `BeforeToolCallEvent` / `AfterToolCallEvent`

Token data is extracted in `_after_invocation` from `event.result.metrics.accumulated_usage`. Field names vary by provider — LiteLLM normalizes differently depending on which backend is in use:

| Provider | LiteLLM field names | Emitted as |
| :--- | :--- | :--- |
| Gemini (via LiteLLM) | `prompt_tokens`, `completion_tokens` | `input_tokens`, `output_tokens` |
| Anthropic (via LiteLLM) | `inputTokens`, `outputTokens`, `totalTokens` (camelCase) | ✅ Resolved (fallbacks added) |

> **Known bug**: Anthropic runs via Strands produce `input_tokens=None output_tokens=None` in the `provider_call` event because `accumulated_usage` uses camelCase keys (`inputTokens`/`outputTokens`) rather than the LiteLLM snake_case names the normalization code expects. The raw blob is still logged correctly. Fix: add camelCase fallbacks to the extraction in `_after_invocation`.

The raw `usage` and `metrics` blobs are still logged when `LOG_PAYLOADS` is enabled.

The `model_call` event logs per-call latency and `stop_reason` but does not emit token fields (those are accumulated at invocation level).

## Token Telemetry Status Matrix

| Framework | Hook/callback mechanism | Normalized token fields | Gemini | Anthropic |
| :--- | :--- | :--- | :--- | :--- |
| ADK | `after_model_callback` | `input_tokens`, `output_tokens`, `total_tokens`, `reasoning_tokens`, `cache_read_tokens` | ✅ | ✅ |
| Agno | `post_hooks` | `input_tokens`, `output_tokens`, `total_tokens`, `reasoning_tokens`, `tool_calls` | ✅ | ✅ Resolved (closed manually to avoid anyio error) |
| Pydantic AI | `logfire.instrument_pydantic_ai()` (opt-in via `LOGFIRE_ENABLED`) | Full spans: token usage per model call, tool args/results, `game_turn`, `run_summary` | ✅ | ✅ |
| Strands | `AfterInvocationEvent` hook | `input_tokens`, `output_tokens`, `total_tokens` | ✅ | ✅ Resolved (camelCase fallbacks added) |

## Hooks Implementation Comparison

The frameworks differ significantly in how they design and expose hooks for runtime observability, instrumentation, and execution interception:

| Framework | Hook Architecture | Lifecycles & Events Exposed | Integration Style |
| :--- | :--- | :--- | :--- |
| **ADK** | Functional Callbacks | `before_agent_callback`, `after_agent_callback`, `before_model_callback`, `after_model_callback`, `before_tool_callback`, `after_tool_callback` | Registered as keyword args (callbacks) directly on the `Agent` configuration. |
| **Agno** | Sequential Interceptors | `pre_hooks` (runs before agent execution), `post_hooks` (runs after agent execution), `tool_hooks` (runs during tool runs) | Registered as lists of callables on the `Agent`. Callables mutate context or log metrics directly. |
| **Pydantic AI** | Capability Hooks | Run-level (`before_run`/`after_run`), Node-level (per loop-turn), Model-level (`before_model_request`/`after_model_request`), Tool-level (pre/post execute and validate), Output-level (pre/post validate) | Registered via the modular capabilities system (`capabilities=[hooks]`) using `pydantic_ai.capabilities.hooks.Hooks` decorators/kwargs. |
| **Strands** | Event-Driven Hooks | `BeforeInvocationEvent`/`AfterInvocationEvent`, `BeforeModelCallEvent`/`AfterModelCallEvent`, `BeforeToolCallEvent`/`AfterToolCallEvent` | Event listeners attached dynamically using `agent.add_hook(callback, EventClass)`. |

## Observed Run Behaviour (claude-haiku-4-5, 25 turns)

| Framework | Final score | Turns used | Total tokens | Outcome |
| :--- | :--- | :--- | :--- | :--- |
| ADK | 70 | 23 | ~260k | Completed |
| Strands | 70 | 25 | 184,650 | Completed |
| Pydantic AI | 65 | 24 | 193,743 | Completed |
| Agno | ~10 | 4 | 14,613 | Crashed mid-game |

Notable: Pydantic AI with Anthropic uses individual named MCP tools (`go`, `take`, `drop`) rather than the single `command` tool — Claude Haiku discovers and prefers the more specific tools the MCP server exposes.

## Known Bugs / Fixes Needed

### 1. Strands — camelCase token fields for Anthropic (priority: high) — ✅ Fixed

`accumulated_usage` from Anthropic via LiteLLM uses camelCase keys. The normalization in `_after_invocation` has been extended to check both snake_case and camelCase fallback keys (`inputTokens`, `outputTokens`, `totalTokens`).

### 2. Agno — TaskGroup teardown crash on Anthropic (priority: medium) — ✅ Fixed

Agno previously crashed with `unhandled errors in a TaskGroup (1 sub-exception)` during async teardown because the connection lifecycle was managed via a context manager that exited outside the task context. 

This has been resolved by manually connecting the tool and calling `await mcp_tools.close()` within the `finally` block of the runner script, which safely catches and suppresses these teardown errors within the SDK's `close` method wrapper.

### 3. Pydantic AI — multi-tool MCP exposure (priority: low, investigate)

When using Anthropic models, Pydantic AI passes through all MCP tools individually (`go`, `take`, `drop`, etc.) rather than routing through the single `command` tool. This may be intentional MCP server behaviour or a schema negotiation difference. Investigate whether `MCPToolset` filters tools differently per model provider.

## Telemetry Standardization Walkthrough

A telemetry standardization effort has successfully aligned logging behavior across all active clients:

* **Standardized `token_scope`**:
  * ADK & Agno: Emitted on `provider_call` as `token_scope="call_total"`.
  * Pydantic AI: Emitted on `provider_call` as `token_scope="call_delta"`.
  * Strands: Emitted on `provider_call` as `token_scope="run_total"` (since Strands collects accumulated totals).
* **Standardized `run_summary`**:
  * All active clients now emit a `run_summary` event at session completion with `token_scope="run_total"`.
  * ADK & Agno: Track token totals across step callbacks and output them in the final log.
  * Strands: Standardized `run_summary` to include requests count and cumulative tokens.
  * Pydantic AI: Refactored to guarantee the `run_summary` logs even if the agent is aborted by a turn-limit exception.
* **LiteLLM Token Normalization**:
  * Added dict-or-object key helper in Strands to correctly extract camelCase and snake_case properties from LiteLLM's `accumulated_usage` mapping, resolving a bug where token counts originally logged as `None` for Strands runs.

## Remaining Gaps

- Strands does not emit per-model-call token deltas — only per-invocation accumulated totals. `AfterModelCallEvent` does not currently expose per-call usage.
- Strands `cache_read_tokens` and `reasoning_tokens` are not extracted (not present in LiteLLM's `accumulated_usage` for any tested provider).

## Related Documentation

- **Framework Setup & Overview:** [packages/README.md](file:///home/mfranz/github/vibepascal/packages/README.md)
- **Detailed Control Flow:** [packages/FLOW.md](file:///home/mfranz/github/vibepascal/packages/FLOW.md) — Comparison of execution loops and logical boundaries.
- **Client Implementations:** [packages/IMPL.md](file:///home/mfranz/github/vibepascal/packages/IMPL.md) — How each client functions.
- **Main Overview Index:** [README.md](file:///home/mfranz/github/vibepascal/README.md)
