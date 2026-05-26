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

## Canonical Token Event Schema

All four active clients now emit a `provider_call` event with the following normalized top-level fields:

```
event="provider_call"  client=<fw>  model=<id>  latency_ms=<n>
  input_tokens=<n>  output_tokens=<n>  total_tokens=<n>
  reasoning_tokens=<n>          # when available
  cache_read_tokens=<n>         # when available
  tool_calls=<n>                # when available
```

Pydantic AI additionally emits a run-level summary:

```
event="run_summary"  client="pydantic_ai"  token_scope="run_total"
  input_tokens=<n>  output_tokens=<n>  total_tokens=<n>
  cache_read_tokens=<n>  requests=<n>
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

> Note: Pydantic AI 2.0.0b3 does not expose a `hooks` module. The iterator approach is the correct pattern for this version.

### Strands

Pattern: lifecycle hooks registered via `agent.add_hook(...)`.

Registered hooks:

- `BeforeInvocationEvent` / `AfterInvocationEvent`
- `BeforeModelCallEvent` / `AfterModelCallEvent`
- `BeforeToolCallEvent` / `AfterToolCallEvent`

Token data is extracted in `_after_invocation` from `event.result.metrics.accumulated_usage`. Field names are resolved with LiteLLM fallbacks:

| LiteLLM field | Fallback | Emitted as |
| :--- | :--- | :--- |
| `prompt_tokens` | `input_tokens` | `input_tokens` |
| `completion_tokens` | `output_tokens` | `output_tokens` |
| `total_tokens` | — | `total_tokens` |

The raw `usage` and `metrics` blobs are still logged when `LOG_PAYLOADS` is enabled.

The `model_call` event logs per-call latency and `stop_reason` but does not emit token fields (those are accumulated at invocation level).

## Token Telemetry Status Matrix

| Framework | Hook/callback mechanism | Normalized token fields | Notes |
| :--- | :--- | :--- | :--- |
| ADK | `after_model_callback` | `input_tokens`, `output_tokens`, `total_tokens`, `reasoning_tokens`, `cache_read_tokens` | Per LLM call |
| Agno | `post_hooks` | `input_tokens`, `output_tokens`, `total_tokens`, `reasoning_tokens`, `tool_calls` | Per `arun()` call |
| Pydantic AI | Iterator delta loop | `input_tokens`, `output_tokens`, `total_tokens`, `cache_read_tokens` | Per step (delta) + run total |
| Strands | `AfterInvocationEvent` hook | `input_tokens`, `output_tokens`, `total_tokens` | Per invocation (accumulated) |
| MS Agent | ~~`LoggingChatClient` wrapper~~ | ~~blob only~~ | **Deprecated** |

## Remaining Gaps

- Strands does not emit per-model-call token deltas — only per-invocation accumulated totals. `AfterModelCallEvent` does not currently expose per-call usage.
- Strands `cache_read_tokens` and `reasoning_tokens` are not extracted (not present in LiteLLM's `accumulated_usage`).
- Pydantic AI hooks (`pydantic_ai.hooks.Hooks`) are not yet available in the installed version (2.0.0b3); revisit when upgrading.
- No `token_scope` field on Agno or Strands events (ADK and Pydantic AI emit it implicitly via event type).
