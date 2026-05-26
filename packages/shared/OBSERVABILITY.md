# Observability in MCP Clients

This document summarizes the current observability implementation across the active framework clients in this repository:

- `packages/adk/adk_mcp_client.py`
- `packages/agno/agno_mcp_client.py`
- `packages/pydantic/pydantic_mcp_client.py`
- `packages/strands/strands_mcp_client.py`

> **MS Agent deprecated**: `packages/ms_agent/ms_agent_mcp_client.py` is no longer invoked by `play-mcp-game.sh`. It has no native hook API and token fields were not normalizable without significant wrapper work.

## Shared Foundation: `vibepascal_shared.llm_observability`

All clients rely on a shared utility module for structured telemetry:

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

> **Note:** `play-mcp-game.sh` exports `AI_REASONING=1` and `LOG_CONSOLE=1` unconditionally, so all benchmark runs have thinking enabled for Pydantic AI and console log output mirrored to stderr.

## Canonical Token Event Schema

All four active clients now emit a `provider_call` event with the following normalized top-level fields:

```
event="provider_call"  client=<fw>  model=<id>  latency_ms=<n>
  input_tokens=<n>  output_tokens=<n>  total_tokens=<n>
  reasoning_tokens=<n>          # when available
  cache_read_tokens=<n>         # when available
  tool_calls=<n>                # when available
```

All active clients emit a `run_summary` event at session end:

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

Pattern: iterator-based usage deltas in the `agent.iter()` loop.

- Cumulative usage is read from `agent_run.usage` on each node iteration.
- Per-step deltas are computed against `last_input_tokens`, `last_output_tokens`, `last_cache_read_tokens`.
- A `run_summary` event with `token_scope="run_total"` is emitted after the loop using `agent_run.result.usage`.

Token fields per step (delta):

- `input_tokens`, `output_tokens`, `total_tokens`, `cache_read_tokens`

Token fields in run summary:

- `input_tokens`, `output_tokens`, `total_tokens`, `cache_read_tokens`, `requests`

> Note: Pydantic AI 2.0.0b3 does expose a `Hooks` class, but it is located under the capabilities module (`pydantic_ai.capabilities.hooks.Hooks`) and must be registered as a capability (`capabilities=[hooks]`) rather than imported from the top-level `pydantic_ai.hooks` namespace. The current client uses the iterator-based loop as it integrates cleanly with the step-by-step console logging flow, but the native hooks capability is fully functional.

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
| Pydantic AI | Iterator delta loop | `input_tokens`, `output_tokens`, `total_tokens`, `cache_read_tokens` | ✅ | ✅ |
| Strands | `AfterInvocationEvent` hook | `input_tokens`, `output_tokens`, `total_tokens` | ✅ | ✅ Resolved (camelCase fallbacks added) |
| MS Agent | ~~`LoggingChatClient` wrapper~~ | ~~blob only~~ | — | **Deprecated** |

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
