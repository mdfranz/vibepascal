# Observability in MCP Clients

This document summarizes the current observability implementation across the framework clients in this repository:

- `packages/adk/adk_mcp_client.py`
- `packages/agno/agno_mcp_client.py`
- `packages/ms_agent/ms_agent_mcp_client.py`
- `packages/pydantic/pydantic_mcp_client.py`
- `packages/strands/strands_mcp_client.py`

It also captures verified ADK capabilities from the installed package in `packages/adk/.venv`.

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

## Instrumentation Pattern by Framework

### Agno

Pattern: inline/procedural logging.

- Provider metrics logged after each `agent.arun(prompt)`.
- Extracts numeric token fields directly from `run_output.metrics`.
- Tool calls logged inline around `mcp_tools.session.call_tool`.

Current token fields in logs:

- `input_tokens`
- `output_tokens`
- `total_tokens`
- `reasoning_tokens`
- `tool_calls` (count of tool calls in run output, often `0` in current runs)

### Pydantic AI

Pattern: loop-based usage deltas plus final run usage line.

- Uses cumulative `agent_run.usage`, logs deltas per provider step.
- Logs `input_tokens`, `output_tokens`, `total_tokens` per step.
- Emits a final `Total Usage: RunUsage(...)` line with richer aggregate usage.

Current token coverage:

- Per step: `input_tokens`, `output_tokens`, `total_tokens`
- Run summary (string payload): includes `requests`, `tool_calls`, and in some runs `cache_read_tokens` and `reasoning_tokens` inside details.

### Microsoft Agent Framework

Pattern: wrapper/decorator + subclassed tool wrapper.

- `LoggingChatClient` intercepts provider responses and logs latency.
- Usage is available as `usage_details` and currently logged as a serialized blob (`usage=`), not normalized numeric fields.
- Tool logging via `DelayedMCPStreamableHTTPTool`.

Current token coverage:

- Potentially available via `usage_details`
- Not normalized into top-level `input_tokens`/`output_tokens`/`total_tokens` fields today

### Strands

Pattern: lifecycle hooks.

Registered hooks include:

- `BeforeInvocationEvent` / `AfterInvocationEvent`
- `BeforeModelCallEvent` / `AfterModelCallEvent`
- `BeforeToolCallEvent` / `AfterToolCallEvent`

Current behavior:

- Provider telemetry is logged from hook callbacks.
- Token usage comes from `event.result.metrics.accumulated_usage` and is logged as serialized `usage=` JSON blob (plus `metrics=` blob).
- `model_call` logs latency/stop reason, but token counters are not emitted as normalized top-level numeric fields.

### ADK

Pattern: event-stream processing (`Runner.run_async(...)`), not lifecycle hooks.

Current client behavior:

- Logs `tool_intent`, `tool_call`, and `run_summary`.
- Does not currently extract or log provider token metrics.

Verified ADK package capability (`packages/adk/.venv`):

- ADK `LlmResponse` includes `usage_metadata`.
- `Event` inherits from `LlmResponse`, so emitted events can carry `usage_metadata`.
- ADK LLM flow merges LLM response fields into events before yielding, so usage metadata is propagated to event stream.

Available ADK usage fields:

- `prompt_token_count`
- `candidates_token_count`
- `total_token_count`
- `thoughts_token_count`
- `cached_content_token_count`
- optional extras such as `tool_use_prompt_token_count` and detail structs

Implication: ADK can support normalized token logging now by reading `event.usage_metadata` inside the `run_async` loop.

## Lifecycle Hooks vs Non-Hook Patterns

Only Strands currently uses lifecycle hooks to capture metrics.

Other frameworks use non-hook mechanisms:

- Agno: inline loop/tool instrumentation
- Pydantic AI: iterator/usage loop instrumentation
- MS Agent: wrapper/decorator interception
- ADK: event stream consumption

## Token Telemetry Status Matrix

| Framework | Native token info available | Currently logged as numeric fields | Notes |
| :--- | :--- | :--- | :--- |
| ADK | Yes (`event.usage_metadata`) | No | Client not extracting usage yet |
| Agno | Yes (`run_output.metrics`) | Yes | Best current normalized per-call shape |
| MS Agent | Yes (`usage_details`) | No | Usage logged as blob |
| Pydantic AI | Yes (`agent_run.usage`) | Yes (per-step deltas) | Final total usage is currently plain text |
| Strands | Yes (`accumulated_usage`) | Partial | Usage logged as blob, not normalized fields |

## Current Gaps

- No single cross-framework token schema in emitted logs.
- ADK and MS Agent are missing normalized top-level token counters in current output.
- Strands usage is present but blob-shaped.
- Pydantic final run usage is not emitted as structured `log_kv` fields.

## Recommended Normalization Direction

Adopt canonical top-level fields across all provider-call events:

- `input_tokens`
- `output_tokens`
- `total_tokens`
- `reasoning_tokens`
- `cache_read_tokens`
- `cache_write_tokens` (when available)
- `tool_calls`
- `requests` (for run-level summaries)

Add two metadata fields:

- `token_scope`: `delta`, `cumulative`, or `run_total`
- `token_source`: `native`, `parsed_usage_blob`, or `unavailable`

Practical rollout:

1. Add token extraction in ADK from `event.usage_metadata`.
2. Parse and normalize Strands `usage` into numeric fields while keeping raw blob.
3. Parse and normalize MS Agent `usage_details` similarly.
4. Emit Pydantic final usage as structured `event="usage_summary"` instead of plain string.
5. Keep raw usage payloads for debugging, but treat normalized numeric fields as source of truth.
