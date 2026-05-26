# MCP Client Implementation Summary

Each framework package (`packages/<fw>/`) contains an `*_mcp_client.py` that connects an AI agent to the Dustwood game server over MCP (Model Context Protocol). All five share the same CLI interface and high-level goal but differ in how the agentic loop is structured.

## Common Patterns

Every client:

- Accepts four positional args: `level` (full/medium/minimal), `model_name`, `delay` (seconds between tool calls), `max_turns`
- Reads `MCP_URL` from env (default `http://127.0.0.1:8765/mcp`)
- Writes structured logs to `logs/<fw>_mcp_client-{epoch}.log`
- Emits `provider_call`, `tool_call`, and `run_summary` events via `log_kv()`
- Loads gameplay hints from `data/guidance_{level}.txt`
- Syncs `GOOGLE_API_KEY` ↔ `GEMINI_API_KEY`

The game server exposes a single `command` MCP tool. Each call returns `structuredContent` with `{ output: str, state: { roomName, turns, score, thirst, isPlaying, ... } }`.

---

## `pydantic/pydantic_mcp_client.py`

**Framework:** Pydantic AI (pre-release 2.0.0b3)

**Loop style:** Fully autonomous — one `agent.iter()` call, framework drives all tool calls.

**How it works:**

1. `MCPToolset(MCP_URL)` wraps the MCP server; passed to `Agent(toolsets=[...])`.
2. The agent is started with `agent.iter(prompt, usage_limits=UsageLimits(request_limit=...))` as an async context manager.
3. Each yielded node may contain `ThinkingPart`, `TextPart`, `ToolCallPart`, or `ToolReturnPart` messages. The client iterates `agent_run.all_messages()` and de-duplicates by `id(part)`.
4. Token usage is tracked incrementally: the delta between the current `agent_run.usage` and the previous snapshot is logged as a `provider_call` after each model response.
5. Turn limit is enforced by raising `UsageLimitExceeded` inside the `ToolReturnPart` handler when `state.turns >= max_turns`.
6. Thinking (extended reasoning) is optionally enabled via `AI_REASONING=1` env var, which adds a `Thinking()` capability and sets `anthropic_thinking` model settings.

**Model support:** Native Pydantic AI `KnownModelName` strings — e.g., `google-gla:gemini-3-flash-preview`, `anthropic:claude-3-5-sonnet-20241022`.

### Flow Diagram

```mermaid
flowchart TD
    A([CLI Entry]) --> B[Load guidance file]
    B --> C[MCPToolset\nStreamable HTTP to MCP_URL]
    C --> D[Create Agent\nmodel + toolset + system_prompt]
    D --> E["agent.iter(prompt, UsageLimits)"]
    E --> F{Node yielded?}
    F -->|Yes| G[Diff usage counters\nlog provider_call delta]
    G --> H[Scan all_messages for new parts]
    H --> I{Part type}
    I -->|ThinkingPart| J[log THINKING]
    I -->|TextPart| K[log AI text]
    I -->|ToolCallPart| L[apply delay\nlog tool intent]
    I -->|ToolReturnPart| M[parse structuredContent\nlog tool_call]
    M --> N{"turns >= max_turns?"}
    N -->|Yes| O[raise UsageLimitExceeded]
    N -->|No| F
    J --> F
    K --> F
    L --> F
    O --> P[log run_summary]
    F -->|StopIteration| P
    P([End])
```

---

## `agno/agno_mcp_client.py`

**Framework:** Agno 2.6.9+

**Loop style:** Hybrid — outer Python while loop, agent asked for one command at a time.

**How it works:**

1. `MCPTools(url=MCP_URL, transport=MCP_TRANSPORT)` is used as an async context manager. The client calls `mcp_tools.session.call_tool("command", ...)` directly, bypassing Agno's agent for the actual MCP dispatch.
2. An `Agent` with a system prompt is created. On each iteration the client builds a `prompt` string containing recent history and current game state, calls `await agent.arun(prompt)`, and extracts the raw command from `run_output.content`.
3. The raw model output is cleaned with `sanitize_command()` and rewritten by `CommandPolicy.rewrite()` before being dispatched to the game.
4. A `_provider_post_hook` registered on the agent extracts Agno metrics (input/output/reasoning tokens) from `run_output.metrics` and accumulates them.
5. History is windowed to `policy.history_limit` entries; the loop exits when `is_playing=False`, turns reach `max_turns`, or LLM call budget is exhausted.

**Model support:** Agno native model objects — `Claude`, `Gemini`, `OpenAIChat`, `Ollama` — selected by prefix-stripping the input model name.

### Flow Diagram

```mermaid
flowchart TD
    A([CLI Entry]) --> B[Load guidance\nCreate CommandPolicy]
    B --> C[MCPTools async context\nStreamable HTTP]
    C --> D["LOOK reset=True\n→ initial game state"]
    D --> E[Create Agent\nmodel + system_prompt]
    E --> F{"is_playing AND turns < max_turns\nAND llm_calls < budget?"}
    F -->|No| G[log run_summary]
    F -->|Yes| H[Build prompt\nrecent history + current state]
    H --> I["agent.arun(prompt)"]
    I --> J[_provider_post_hook\naccumulate + log tokens]
    J --> K[Extract command text\nsanitize_command]
    K --> L[CommandPolicy.rewrite\nloop-break if needed]
    L --> M["mcp_tools.session.call_tool\n(command, args)"]
    M --> N[Parse CommandOutput\nupdate last_state + history]
    N --> O[log tool_call]
    O --> F
    G([End])
```

---

## `strands/strands_mcp_client.py`

**Framework:** Strands Agents SDK + LiteLLM

**Loop style:** Fully autonomous — single synchronous `agent(prompt)` call, framework drives all tool calls.

**How it works:**

1. `MCPClient` is created with one of three transports (streamable-http, SSE, stdio) selected by `--transport` arg.
2. `LiteLLMModel(model_id=...)` handles model routing; bare `gemini-*` IDs are auto-prefixed with `gemini/`.
3. `SlidingWindowConversationManager(window_size=10)` keeps context bounded.
4. All observability is attached via the Strands hooks system: `BeforeInvocationEvent`, `AfterInvocationEvent`, `BeforeModelCallEvent`, `AfterModelCallEvent`, `BeforeToolCallEvent`, `AfterToolCallEvent`.
5. The `_before_tool_call` hook enforces the turn limit and `CommandPolicy`: it may set `event.cancel_tool` to abort a call, or rewrite `tool_input["command"]` in-place.
6. The `_after_tool_call` hook parses `structuredContent` from the result and updates `last_state_obj` and `last_output_text`.
7. Token usage is read from `event.result.metrics.accumulated_usage` in `_after_invocation`.

**Model support:** Any LiteLLM-supported model string (`gemini/...`, `anthropic/...`, `openai/...`, `ollama/...`, etc.).

### Flow Diagram

```mermaid
flowchart TD
    A([CLI Entry]) --> B[Normalize model_id\nadd gemini/ prefix if bare]
    B --> C[LiteLLMModel]
    C --> D[MCPClient\nhttp / sse / stdio]
    D --> E[SlidingWindowConversationManager\nwindow = 10]
    E --> F[Create Agent\nmodel + mcp_client + system_prompt]
    F --> G[Register 6 hooks\nBefore/After × Invocation/Model/Tool]
    G --> H["agent(prompt) — single blocking call"]

    H --> I{BeforeToolCall hook}
    I -->|game over OR turn limit| J[set event.cancel_tool]
    I -->|normal| K[CommandPolicy.rewrite\nrewrite tool_input in-place]
    K --> L[MCP command dispatched to server]
    L --> M{AfterToolCall hook}
    M --> N[Parse structuredContent\nupdate last_state_obj]
    N --> O[log tool_call]
    O --> H

    J --> H
    H -->|agent complete| P{AfterInvocation hook}
    P --> Q[Read accumulated_usage\nlog provider_call + run_summary]
    Q --> R[mcp_client.stop]
    R([End])
```

---

## `ms_agent/ms_agent_mcp_client.py`

**Framework:** Microsoft Agent Framework (`agent_framework`)

**Loop style:** Hybrid replanning — outer Python while loop, each iteration calls `agent.run()` for a small chunk of turns.

**How it works:**

1. `PolicyMCPTool(url=MCP_URL, ...)` is a custom subclass of `MCPStreamableHTTPTool`:
   - Overrides `call_tool()` to sanitize/rewrite commands via `CommandPolicy` and enforce the turn limit.
   - Passes `parse_tool_results=self._parse_tool_results` to the base class to decode `structuredContent` and update `last_state`.
2. `LoggingChatClient` wraps the underlying provider client (Anthropic/OpenAI/Ollama) to log latency and usage for every LLM call.
3. The outer loop computes `chunk_calls = min(6, remaining_turns + 2)` and injects it into `inner.function_invocation_configuration` to cap tool calls per replan.
4. If a replan iteration produces no turn advancement (`last_state.turns == last_turns_seen`), a forced exploratory move is dispatched directly via `mcp_tool.call_tool()`.
5. Gemini is accessed via the OpenAI-compatible endpoint (`generativelanguage.googleapis.com/v1beta/openai/`) using `OpenAIChatCompletionClient`.
6. A monkey-patch at import time fixes a `model_id` vs `model` kwarg mismatch in `ChatResponse.__init__`.

**Model support:** Anthropic (`AnthropicClient`), OpenAI (`OpenAIChatClient`), Gemini via OpenAI compat, Ollama (`OllamaChatClient`).

### Flow Diagram

```mermaid
flowchart TD
    A([CLI Entry]) --> B[Build LLM client\nAnthropic / OpenAI / Gemini compat / Ollama]
    B --> C[Wrap in LoggingChatClient\nlatency + usage on every call]
    C --> D[PolicyMCPTool async context\nStreamable HTTP]
    D --> E[Create Agent\nclient + PolicyMCPTool + system_prompt]
    E --> F["LOOK reset=True via mcp_tool.call_tool"]
    F --> G{"is_playing AND turns < max_turns\nAND llm_calls < budget?"}
    G -->|No| H[log final state]
    G -->|Yes| I["chunk_calls = min(6, remaining + 2)\ninject into function_invocation_configuration"]
    I --> J[Build prompt\nhistory + state block]
    J --> K["agent.run(prompt) — chunk of tool calls"]

    K --> L{PolicyMCPTool.call_tool}
    L --> M[sanitize + CommandPolicy.rewrite\ncheck turn limit]
    M --> N[super.call_tool → MCP server]
    N --> O[_parse_tool_results\nupdate last_state]
    O --> P[_on_step callback\nappend to history]
    P --> K

    K --> Q{"turns advanced?"}
    Q -->|No — stall detected| R[force exploratory move\nmcp_tool.call_tool directly]
    R --> G
    Q -->|Yes| G
    H([End])
```

---

## `adk/adk_mcp_client.py`

**Framework:** Google ADK 2.0.0 + LiteLLM extension

**Loop style:** Fully autonomous — single `runner.run_async()` event stream.

**How it works:**

1. `McpToolset(connection_params=StreamableHTTPConnectionParams(...), tool_filter=["command"])` connects to the MCP server and exposes the `command` tool to the agent.
2. An `Agent` is created with `tools=[toolset]` and an `after_model_callback` that reads `llm_response.usage_metadata` and accumulates token counts per call.
3. `Runner.run_async()` yields events; the client iterates them, skipping partials and de-duplicating by event ID.
4. `event.get_function_calls()` and `event.get_function_responses()` are used to log tool intent and results.
5. Game state is extracted from `response.structuredContent` inside tool response events; `state.is_playing` and `state.turns >= max_turns` determine when to set `stop_reason`.
6. The turn budget is capped via `RunConfig(max_llm_calls=max(8, max_turns * 4))`; `LlmCallsLimitExceededError` is caught and treated as a clean stop.
7. `_resolve_model()` maps model strings to either a native Gemini ID (string) or a `LiteLlm(model=...)` object, enabling non-Gemini models through LiteLLM.

**Model support:** Native Gemini IDs (e.g. `gemini-3.5-flash`) and any LiteLLM provider string (e.g. `openai/gpt-5-mini`, `anthropic/claude-3-5-sonnet-20241022`).

### Flow Diagram

```mermaid
flowchart TD
    A([CLI Entry]) --> B["_resolve_model\nnative Gemini str OR LiteLlm object"]
    B --> C[McpToolset\nStreamableHTTPConnectionParams\ntool_filter = command]
    C --> D[Create Agent\nmodel + toolset + after_model_callback]
    D --> E[InMemorySessionService + Runner]
    E --> F["runner.run_async\nnew_message, RunConfig max_llm_calls"]

    F --> G{Event from stream}
    G -->|partial OR duplicate| G
    G -->|text content| H[log AI text]
    G -->|function_call| I[log tool_intent]
    G -->|function_response| J[_extract_state_and_output\nparse structuredContent]

    H --> G
    I --> G

    J --> K{"is_playing == false?"}
    K -->|Yes| L[stop_reason = Game ended]
    K -->|No| M{"turns >= max_turns?"}
    M -->|Yes| N[stop_reason = Turn limit]
    M -->|No| G

    subgraph per_model_call ["after_model_callback — fires each LLM call"]
        CB[Read usage_metadata\naccumulate tokens\nlog provider_call]
    end

    D -.registers.-> per_model_call

    L --> O[runner.close\ntoolset.close]
    N --> O
    G -->|stream exhausted| O
    F -->|LlmCallsLimitExceededError| O
    O --> P[log run_summary]
    P([End])
```

---

## Shared Library (`packages/shared/vibepascal_shared/`)

### `llm_observability.py`

- `Timer` — perf_counter-based elapsed time helper.
- `log_kv(logger, **fields)` — emits a single `key=json_value ...` log line; all values JSON-encoded, newlines escaped.
- `redact_secrets(text)` — strips API keys and Bearer tokens from log output.
- `format_payload(value)` — serializes an object to JSON and truncates to `LOG_MAX_CHARS` (default 20 000).
- Env flags: `LOG_CONSOLE`, `GAME_CONSOLE`, `LOG_HTTP`, `LOG_PAYLOADS`, `LOG_MAX_CHARS`.

### `mcp_command_policy.py`

- `sanitize_command(cmd)` — normalises model output to a single uppercase game command: strips markdown fences, JSON wrappers, punctuation, and maps synonyms (`GET→TAKE`, `INSPECT→EXAMINE`, etc.).
- `LoopBreaker` — detects when the same command produces the same state+output twice in a row and substitutes a cardinal direction to escape the loop.
- `CommandPolicy` — combines `LoopBreaker` with a turn-budget rewriter: forces a move off the starting room on turn 0, avoids wasteful commands on the last turn in desert rooms, and overrides stuck snake encounters.
- Configured from env: `MCP_HISTORY_LIMIT`, `MCP_LOOP_REPEAT_THRESHOLD`, `MCP_MAX_LLM_CALLS_MULTIPLIER`.

### `guidance_loader.py`

- `load_guidance(value)` — resolves `"full"`, `"medium"`, or `"minimal"` to `data/guidance_{level}.txt` under the repo root, or accepts a path directly. Returns a `GuidanceConfig(path, text)` dataclass.

### CommandPolicy Flow

```mermaid
flowchart TD
    A[proposed_command] --> B[sanitize_command\nupcase, strip fences/JSON/punctuation\nmap synonyms]
    B --> C{cmd empty?}
    C -->|Yes| D[cmd = LOOK]
    C -->|No| E{room_id == 1\nAND turns == 0\nAND cmd is LOOK/INV?}
    E -->|Yes — bootstrap| F[cmd = NORTH]
    E -->|No| G{is_riding AND\nsnake in last output\nAND cmd is passive?}
    G -->|Yes| H[LoopBreaker.break_command\nnext cardinal direction]
    G -->|No| I{remaining_turns == 1\nAND low-value cmd\nAND in desert room?}
    I -->|Yes| J[escape move\nor LoopBreaker direction]
    I -->|No| K{same cmd + state + output\nas last N turns?}
    K -->|Yes — loop detected| L[LoopBreaker.break_command\nnext cardinal direction]
    K -->|No| M[return cmd as-is]
    D --> M
    F --> M
    H --> M
    J --> M
    L --> M
    M([Rewritten command])
```
