# Observability in MCP Clients

This document summarizes the current observability implementation across the active framework clients in this repository:

- `packages/adk/adk_mcp_client.py`
- `packages/agno/agno_mcp_client.py`
- `packages/pydantic/pydantic_mcp_client.py` — **now uses Logfire instead of the shared `log_kv` schema below; see [Pydantic AI](#pydantic-ai) and [Logfire (Pydantic AI only)](#logfire-pydantic-ai-only).**
- `packages/strands/strands_mcp_client.py` — **now uses Logfire instead of the shared `log_kv` schema below; see [Strands](#strands) and [Logfire (Strands)](#logfire-strands).**


## Shared Foundation: `vibepascal_shared.llm_observability`

ADK and Agno rely on a shared utility module for structured telemetry (Pydantic AI and Strands now
use Logfire instead — see below — but Strands still uses `setup_logger`/`print_game` from this
module for basic operational logging and game-narrative console output, same as Pydantic AI):

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
- `LOGFIRE_ENABLED` (default `1` under `pydantic-mcp-game.sh` and `strands-mcp-game.sh`, `0`/unset otherwise) — **Pydantic AI and Strands**, checked directly in each client (not part of `llm_observability.py`). Gates `logfire.configure()` plus, for Pydantic AI, `logfire.instrument_pydantic_ai()` (Strands needs no separate instrument call — see [Logfire (Strands)](#logfire-strands)).
- `LOGFIRE_ENVIRONMENT` (default `development`) — **Pydantic AI and Strands**. Sets the Logfire `deployment.environment` resource attribute.

> **Note:** `play-mcp-game.sh` exports `AI_REASONING=1` and `LOG_CONSOLE=1` unconditionally, so all benchmark runs have thinking enabled for Pydantic AI and console log output mirrored to stderr. `pydantic-mcp-game.sh` and `strands-mcp-game.sh` additionally export `LOGFIRE_ENABLED=1` by default.

## Canonical Token Event Schema

> **Pydantic AI and Strands no longer emit these `log_kv` events.** Both moved to Logfire (see
> [Logfire (Pydantic AI only)](#logfire-pydantic-ai-only) and [Logfire (Strands)](#logfire-strands))
> — full per-model-call token usage and per-tool-call args/results are captured automatically by
> OTel spans instead of the `provider_call`/`tool_call` events below. This schema now applies to
> ADK and Agno only.

ADK and Agno emit a `provider_call` event with the following normalized top-level fields:

```
event="provider_call"  client=<fw>  model=<id>  latency_ms=<n>
  input_tokens=<n>  output_tokens=<n>  total_tokens=<n>
  reasoning_tokens=<n>          # when available
  cache_read_tokens=<n>         # when available
  tool_calls=<n>                # when available
```

ADK and Agno emit a `run_summary` event at session end:

```
event="run_summary"  client=<fw>  model=<id>  token_scope="run_total"
  input_tokens=<n>  output_tokens=<n>  total_tokens=<n>
  requests=<n>
  latency_ms=<n>          # ADK, Agno only
  stop_reason=<str>       # ADK, Agno only
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

**Superseded by Logfire** — see [Logfire (Strands)](#logfire-strands) below. Historically this used
lifecycle hooks (`BeforeInvocationEvent`/`AfterInvocationEvent`, `BeforeModelCallEvent`/
`AfterModelCallEvent`, `BeforeToolCallEvent`/`AfterToolCallEvent`) registered via
`agent.add_hook(...)` to hand-track latency and token usage and emit `provider_call`/`tool_call`/
`run_summary` via `log_kv`, the same pattern still used by ADK and Agno. That telemetry code has
been removed from `strands_mcp_client.py` in favor of Strands' native OTel instrumentation, which
Logfire picks up automatically once `logfire.configure()` runs (no hooks required).

The six lifecycle hooks are still registered (minus `BeforeInvocationEvent`/`BeforeModelCallEvent`,
which had no non-telemetry purpose), but now carry only game logic that has nothing to do with
observability: `_before_tool_call`/`_after_tool_call` track game state from MCP tool results,
enforce the turn limit and game-over cutoff, and drive `CommandPolicy` rewriting; `_after_model_call`
still extracts `THINKING` text from Gemini/Anthropic reasoning content into the local log file
(unrelated to Logfire — a local debugging aid); `_after_invocation` emits the one remaining custom
Logfire event, `run_summary`.

<details>
<summary>Historical schema (pre-Logfire, kept for reference)</summary>

Token data was extracted in `_after_invocation` from `event.result.metrics.accumulated_usage`.
Field names vary by provider — LiteLLM normalizes differently depending on which backend is in use:

| Provider | LiteLLM field names | Emitted as |
| :--- | :--- | :--- |
| Gemini (via LiteLLM) | `prompt_tokens`, `completion_tokens` | `input_tokens`, `output_tokens` |
| Anthropic (via LiteLLM) | `inputTokens`, `outputTokens`, `totalTokens` (camelCase) | ✅ Resolved (fallbacks added) |

The camelCase-fallback normalization logic itself was kept (it now feeds the `run_summary` Logfire
event's `input_tokens`/`output_tokens`/`total_tokens` fields instead of a `log_kv` line).

</details>

### Logfire (Strands)

Pattern: [Logfire](https://github.com/pydantic/logfire)'s native Strands integration, gated by
`LOGFIRE_ENABLED` (see [Environment Controls](#environment-controls)) — see
[the Logfire Strands integration guide](https://pydantic.dev/docs/logfire/integrations/llms/strands/).

- `logfire.configure(service_name="strands-mcp-client", environment=..., data_dir="~/.logfire")` runs
  once at module import time, only when `LOGFIRE_ENABLED` is truthy — same `data_dir` pinning
  rationale as Pydantic AI (this client is invoked from different working directories, e.g. repo
  root via `strands-mcp-game.sh`).
- Unlike Pydantic AI, there is no explicit `instrument_strands()` call. Strands emits OTel spans for
  agent invocations, model calls, and MCP tool executions natively (`strands.telemetry`); once
  `logfire.configure()` sets the global tracer provider, those spans flow to Logfire on their own.
  Calling `logfire.configure()` before the `Agent(...)` is constructed is the entire integration.
- `OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental,gen_ai_span_attributes_only` is set
  (via `os.environ.setdefault`, so it doesn't clobber an operator-set value) before `logfire.configure()`
  runs. Without it, Strands' spans omit `gen_ai.input.messages`/`gen_ai.output.messages` — the actual
  conversation content — so this is required to get full request/response visibility, not just
  latency/token counts.
- `Agent(..., trace_attributes={"session.id": ..., "vibepascal.model": ..., "vibepascal.level": ...,
  "vibepascal.max_turns": ...})` — a native Strands constructor kwarg, not Logfire-specific — tags
  every span the agent emits with these attributes, so runs are filterable/groupable in Logfire
  without needing a separate wrapper span to carry them.
- The client additionally wraps the whole game session in a top-level `logfire.span("strands_game_run",
  model=..., level=..., max_turns=..., session_id=...)`, mirroring Pydantic AI's `pydantic_game_run`
  span, so every span for one run shares one `trace_id`.
- Two custom structured log events (`logfire.info(...)`), both game/run-domain data that Strands'
  own instrumentation doesn't know about on its own:
  - `game_turn` — emitted once per tool-call return that carries game state (`turn`, `room`,
    `score`, `thirst`), same as Pydantic AI's.
  - `run_summary` — emitted once at the end of the run: `model`, `input_tokens`, `output_tokens`,
    `total_tokens` (from `event.result.metrics.accumulated_usage`, with the camelCase-fallback
    normalization for Anthropic still applied).
  - A `game_over` event is also emitted when the server reports the turn limit was hit before the
    agent noticed locally (see [Known Bugs](#known-bugs--fixes-needed)).
- When `LOGFIRE_ENABLED` is unset/false (default outside `strands-mcp-game.sh`), none of the above
  runs — the client falls back to only the basic operational logging described in
  [Shared Foundation](#shared-foundation-vibepascal_sharedllm_observability) (banner, `THINKING` text,
  final response dump via `setup_logger`) plus stdout game narrative via `print_game`. No per-call
  token/tool telemetry is captured locally in that case, same tradeoff as Pydantic AI.

## Token Telemetry Status Matrix

| Framework | Hook/callback mechanism | Normalized token fields | Gemini | Anthropic |
| :--- | :--- | :--- | :--- | :--- |
| ADK | `after_model_callback` | `input_tokens`, `output_tokens`, `total_tokens`, `reasoning_tokens`, `cache_read_tokens` | ✅ | ✅ |
| Agno | `post_hooks` | `input_tokens`, `output_tokens`, `total_tokens`, `reasoning_tokens`, `tool_calls` | ✅ | ✅ Resolved (closed manually to avoid anyio error) |
| Pydantic AI | `logfire.instrument_pydantic_ai()` (opt-in via `LOGFIRE_ENABLED`) | Full spans: token usage per model call, tool args/results, `game_turn`, `run_summary` | ✅ | ✅ |
| Strands | Native OTel via `strands.telemetry`, picked up by `logfire.configure()` (opt-in via `LOGFIRE_ENABLED`) | Full spans: token usage per model call, tool args/results, `game_turn`, `run_summary` | ✅ | ✅ Resolved (camelCase fallbacks still applied for the `run_summary` event) |

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

### 1. Strands — camelCase token fields for Anthropic (priority: high) — ⚠️ Misdiagnosed, see #4

`accumulated_usage` from Anthropic via LiteLLM uses camelCase keys. The normalization in `_after_invocation` was extended to check both snake_case and camelCase fallback keys (`inputTokens`, `outputTokens`, `totalTokens`). This fix was real but addressed the wrong bug — see [#4](#4-strands--run_summary-token-fields-always-none-priority-high--fixed) for the actual root cause, found while verifying the Logfire migration live against Logfire.

### 4. Strands — `run_summary` token fields always None (priority: high) — ✅ Fixed

Verifying the Logfire migration (see [Logfire (Strands)](#logfire-strands)) against a real game run
showed `run_summary` logging `input_tokens=None output_tokens=None total_tokens=None` every time,
even though the run played and scored normally. Root cause: `_after_invocation` read usage from
`event.result.metrics.accumulated_usage`, but `event.result` is only populated when `agent()`
returns via a normal `EventLoopStopEvent` — Strands' event loop (`agent.py`) sets `agent_result`
inside the `try` block and only publishes `AfterInvocationEvent` from the surrounding `finally`, so
any invocation that ends by raising (here, always: `_GameEndedError`, raised from
`_before_tool_call` on turn-limit or game-over) leaves `event.result` `None`. Since *every*
benchmarked run in this repo ends that way, `run_summary` tokens were `None` on effectively every
real run, for every provider — the earlier camelCase fix in #1 never actually got exercised against
that path. Fixed by reading `agent.event_loop_metrics.accumulated_usage` instead, which Strands
accumulates on the `Agent` object itself after every model call regardless of how the invocation
ends.

### 5. Strands — noisy `Failed to detach context` traceback on concurrent tool cancellation (priority: low, cosmetic)

Observed live while verifying #4: when a model fires several MCP tool calls concurrently (e.g. five
`take` calls in one turn) right as the turn limit is hit, this client's `_before_tool_call` hook
cancels the later ones via `event.cancel_tool`. Strands' `ConcurrentToolExecutor` runs each tool
call in its own `asyncio.create_task()` and wraps it in `trace_api.use_span(...)`
(`strands/tools/executors/_executor.py`); when tasks get torn down concurrently like this, an OTel
`contextvars.Token` ends up detached from a different task context than it was attached in, raising
`ValueError: ... was created in a different Context` from `opentelemetry/context/__init__.py`,
logged to stderr as "Failed to detach context" during generator cleanup.

This is in Strands' own concurrent-tool-execution + OTel plumbing, not in `strands_mcp_client.py`
(no hook here touches spans or context). It's cosmetic: the run still completes normally (exit 0,
game and Logfire data unaffected — verified via `query_run` that every `game_turn` event and the
correctly-cancelled tool spans' exception status all landed) and the trailing traceback prints after
`--- Session Complete ---`. Not fixed here since it would mean patching Strands' vendored code.

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

- Strands' native OTel spans (visible in Logfire when `LOGFIRE_ENABLED=1`) do carry per-model-call
  usage; only the custom `run_summary` event (built from `accumulated_usage` for local-log parity
  with Pydantic AI) is limited to per-invocation totals.
- Strands `cache_read_tokens` and `reasoning_tokens` are not extracted into the `run_summary` event (not present in LiteLLM's `accumulated_usage` for any tested provider).

## Related Documentation

- **Framework Setup & Overview:** [packages/README.md](file:///home/mfranz/github/vibepascal/packages/README.md)
- **Detailed Control Flow:** [packages/FLOW.md](file:///home/mfranz/github/vibepascal/packages/FLOW.md) — Comparison of execution loops and logical boundaries.
- **Client Implementations:** [packages/IMPL.md](file:///home/mfranz/github/vibepascal/packages/IMPL.md) — How each client functions.
- **Main Overview Index:** [README.md](file:///home/mfranz/github/vibepascal/README.md)
