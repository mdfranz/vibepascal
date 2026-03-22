# Observability in MCP Clients

This document analyzes and compares how observability (logging, latency tracking, and telemetry) is implemented across various Python agent frameworks in the `scripts/` directory.

## Core Foundation: `llm_observability.py`

All clients share a common utility module, `llm_observability.py`, which provides:
- **`log_kv`**: A structured logging utility for key-value pairs (event, latency, tokens, etc.).
- **`Timer`**: A manual latency measurement utility.
- **`print_game`**: Console-based visualization for the game state.
- **Feature Flags**: Environment-driven flags. `provider_payload_logging_enabled()` (`LOG_PAYLOADS`) and `game_console_enabled()` (`GAME_CONSOLE`) both default to **`True`**. `console_logging_enabled()` (`LOG_CONSOLE`) and `http_debug_logging_enabled()` (`LOG_HTTP`) default to `False`.

---

## Client Implementation Patterns

### 1. Agno (`agno_mcp_client.py`)
**Pattern: Procedural / Inline**
- **Mechanism**: Telemetry is explicitly "baked" into the main execution loop and helper functions.
- **Detailed Implementation**:
    - **Tool Calls**: Within the local `command` async function, a `Timer` is started manually, and `log_kv` is called immediately after `mcp_tools.session.call_tool` returns.
    - **Provider Calls**: Metrics are logged directly in the `while` loop after `agent.arun(prompt)`. It extracts `input_tokens`, `output_tokens`, `total_tokens`, and `reasoning_tokens` from the `run_output.metrics` attribute.
- **Pros**: Zero abstraction overhead; easy to see exactly what is logged by reading the main loop.
- **Cons**: High code duplication if multiple agents or tools are used; business logic is cluttered with telemetry calls.

### 2. Strands (`strands_mcp_client.py`)
**Pattern: Event-Driven / Hooks**
- **Mechanism**: A formal lifecycle hook system where the agent emits events at specific stages.
- **Detailed Implementation**:
    - **Lifecycle Stages**: Hooks are registered for `BeforeInvocationEvent`, `AfterModelCallEvent`, and `AfterToolCallEvent`.
    - **State Management**: It uses the `event.invocation_state` dictionary to pass data across the lifecycle. For example, `_before_invocation` stores a timestamp in `_obs["invocation_start"]`, which `_after_invocation` later retrieves to calculate total latency.
    - **Granular Metrics**: The `AfterModelCallEvent` hook captures specific LLM metadata like `stop_reason`.
- **Pros**: Cleanest separation of concerns; the main execution path remains purely about agent logic.
- **Cons**: Requires a framework that supports a robust event/hook system.

### 3. Microsoft Agent Framework (`ms_agent_mcp_client.py`)
**Pattern: Structural / Decoration & Subclassing**
- **Mechanism**: Intercepting framework behavior by wrapping classes and overriding methods.
- **Detailed Implementation**:
    - **Decorator Pattern**: The `LoggingChatClient` class wraps any `ChatClient`. It intercepts the `get_response` method, using `with_result_hook` (for streaming) or an internal `_await_and_log` coroutine to capture usage and latency without changing the calling code.
    - **Inheritance Pattern**: It subclasses the official `MCPStreamableHTTPTool` into `DelayedMCPStreamableHTTPTool`, overriding `call_tool` to inject `Timer` logic and `log_kv` calls.
- **Pros**: Allows adding deep observability to "black-box" framework components without modifying their source code.
- **Cons**: Requires deep knowledge of the framework's internal class hierarchies and method signatures.

### 4. Pydantic AI (`pydantic_mcp_client.py`)
**Pattern: Encapsulated Procedural**
- **Mechanism**: Encapsulating tool telemetry in dependency classes while keeping provider telemetry in the loop.
- **Detailed Implementation**:
    - **Tool Encapsulation**: Since Pydantic AI uses a "Dependencies" pattern, all MCP tool telemetry (including the raw JSON-RPC `initialize` and `tools/call` handshakes) is encapsulated within the `MCPDeps` class.
    - **Built-in Metrics**: It utilizes Pydantic AI's native `result.usage()` for `requests`, `input_tokens`, `output_tokens`, `total_tokens`, and `tool_calls`. Additionally, `result.response.tool_calls()` is called separately to log per-call tool details (name + args) when payload logging is enabled.
- **Pros**: Tool-level logging is neatly tucked away in a helper class; provider logging is concise due to built-in framework support.
- **Cons**: The boundary between manual tool logging and framework provider logging can lead to slightly inconsistent event schemas if not carefully managed.

## Code Metrics & Complexity

| Metric | Agno | Strands | MS Agent | Pydantic AI |
| :--- | :---: | :---: | :---: | :---: |
| **Total LOC** | 377 | 427 | **605** | 397 |
| **Classes** | 2 | 0 | **5** | 3 |
| **Functions** | 4 | 8 | **15** | 5 |
| **Total Symbols** | 6 | 8 | **20** | 8 |

### Complexity Analysis
- **Agno (377 LOC / 6 Symbols)**: The lowest "symbol density." This reflects its procedural nature where logic and observability are flattened into the main execution loop. It is the easiest to follow linearly but the hardest to scale.
- **Strands (427 LOC / 8 Symbols)**: Highly functional. The 0-class architecture reflects its heavy reliance on independent hook functions. The higher function count relative to Agno shows its decomposition of the lifecycle into discrete, testable units.
- **Microsoft Agent (605 LOC / 20 Symbols)**: The most complex and abstract implementation. The class count (5: `GameSummary`, `CommandOutput`, `LoggingChatClient`, `DelayedMCPStreamableHTTPTool`, `PolicyMCPTool`) reflects the heavy use of the **Decorator** and **Subclassing** patterns. This results in the longest file but offers the most decoupled and reusable observability logic.
- **Pydantic AI (397 LOC / 8 Symbols)**: Balanced complexity. It uses classes (`MCPDeps`) to encapsulate tool-specific logic while keeping the core agent flow relatively flat.

---

## Architectural Insight
While **Agno** and **Pydantic AI** are effective for straightforward implementations, **Strands** and **MS Agent** offer more scalable patterns for production-grade observability. 

The **Strands Hook system** is the most sophisticated, allowing for shared state across a complex multi-step reasoning chain. The **MS Agent Wrapper** approach is the most flexible for retrofitting observability onto existing libraries where you cannot easily inject hooks into the core loop.
