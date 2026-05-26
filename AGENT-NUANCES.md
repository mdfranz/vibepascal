# Agent Nuances: Pydantic AI vs. Strands vs. MS Agent vs. Agno

This document provides a technical comparison of the four AI agent frameworks implemented in *Echoes of Dustwood*. While all frameworks achieve autonomous gameplay, they represent different architectural philosophies and performance profiles.

## 1. Orchestration Philosophy

| Framework | Philosophy | Primary Strength | Use Case |
| :--- | :--- | :--- | :--- |
| **Pydantic AI** | Functional/Direct | Speed & Type Safety | Production-grade survival runs. |
| **Strands SDK** | Agent-Centric | Robustness & Multi-Model | Local reasoning models (DeepSeek/Ollama). |
| **MS Agent** | High-Level Tasking | Surgical Efficiency | Rapid, goal-oriented missions. |
| **Agno** | Multimodal/Proactive | Native Feature Support | Latest Gemini models & narrative depth. |

## 2. Framework Comparisons

### Pydantic AI (`ai_client.py`)
*   **Performance**: The fastest execution profile. Optimized for direct provider calls.
*   **Perception**: Uses augmented reality logic to track inventory capacity and provide survival hints.
*   **MCP implementation**: Stateless HTTP with manually managed session persistence.

### Strands SDK (`strands_ai_client.py`)
*   **Robustness**: Uses a "Reverse-Search JSON Extractor" to handle models that output messy reasoning before their command.
*   **Connectivity**: Best-in-class handling of LiteLLM/Ollama edge cases. Integrates with LiteLLM's Google Gemini API handler to correctly forward `thought_signature` metadata.
*   **Limitation**: Currently struggles with the Go server's specific SSE implementation.

### Microsoft Agent Framework (`ms_agent_client.py`)
*   **Efficiency**: Extremely surgical behavior. Reaches goals with the minimum number of turns.
*   **Limitation**: The standard OpenAI client is incompatible with Gemini 3 models.
    *   `OpenAIChatClient` fails with `404` because Gemini does not support OpenAI's Responses API (`/responses`).
    *   `OpenAIChatCompletionClient` successfully calls the standard `/chat/completions` API but fails on the second turn with a `400 Bad Request` because the client does not preserve and forward Gemini 3's required `thought_signature` metadata.
*   **Tested Models**: Excellent performance with `gpt-5-mini` and `ollama/gpt-oss:20b`.

### Agno (formerly Phidata) (`agno_client.py`)
*   **Native SDKs**: Uses native Google/Anthropic SDKs rather than generic wrappers.
*   **Gemini 3 Champion**: The **only** framework successfully running Gemini 3 Pro/Flash natively. It handles Gemini's required `thought_signature` serialization automatically through the Google GenAI SDK.
*   **Proactivity**: Showed high initiative in logs, often exploring beyond the immediate goal to gather survival items.

## 3. Performance & Latency (MCP vs. Original)

Testing reveals a significant performance gap between interaction methods:

*   **Original (Direct Stdio)**: **~5.1 seconds/turn**. Bottlenecked by the 5s "Read Timeout" used to ensure the full game description is captured from the Pascal process.
*   **MCP (JSON-RPC)**: **~1.25 seconds/turn**. Atomic state returns eliminate the need for timeouts, resulting in a **400% speed increase**.

## 4. Model Performance Observations (Updated)

*   **gemini-3.1-pro-preview**: High-tier logic. Successfully handled complex puzzles via Agno.
*   **gemini-3.5-flash**: Excellent, high-performing model. Successfully verified across Pydantic AI (v2.0.0b3), Agno, and Strands SDK.
*   **gpt-5-mini**: The "Utility Player." Works reliably across all four frameworks with high efficiency.
*   **claude-opus-4-6**: The planning expert. Best at long-term inventory management.
*   **claude-haiku-4-5**: High-speed, highly capable logic, but sensitive to framework orchestration constraints. Successfully achieved a peak score of **60 points** (in 15 turns) via Strands SDK and **57 points** via ADK by managing inventory capacity, repairing the water pump (+20), and saddling the horse. However, it can get stuck by inventory limitations (Agno) or random hazards (Pydantic AI) if the orchestration does not enforce item dropping or hazard bypasses.
*   **gpt-oss:20b (Ollama)**: Strong performance via MS Agent. Navigated to goals with zero typos or logic loops.
*   **granite4:3b (Ollama)**: Struggles with "common sense." Requires frameworks with strong retry logic to recover from typos like `TAKE SPPOOL`.

## 5. Token Efficiency & Context Management

The frameworks differ significantly in how they manage conversation context, which directly impacts input token usage and cost:

*   **Agno**: Uses a lightweight loop that only forwards a sliding window of the last few turns (`policy.history_limit` formatted as a text string) as context. Because raw JSON-RPC tool payloads are discarded from the prompt, context sizes remain small and stable (~31k–43k total input tokens for 15 turns).
*   **Pydantic AI & Strands SDK**: Retain the complete message history (verbose system prompts, raw JSON-RPC tool calls, and tool returns) in memory, re-submitting the full thread on each turn. This causes quadratic context growth (~80k–100k total input tokens for 15 turns).
*   **Reasoning Models (`gpt-5-mini`)**: When running reasoning models, output token counts scale dramatically (e.g., from ~100 tokens to over 5,000 tokens) because the model's internal thinking/reasoning process is billed and returned as part of the `output_tokens` metrics, even if the final extracted gameplay command is only a single word.

## 6. Framework Decision Matrix

| If you want... | Use this Framework |
| :--- | :--- |
| **Maximum Speed** | Pydantic AI (via MCP) |
| **Gemini 3.1 Support** | Agno |
| **Local LLM Robustness** | Strands SDK |
| **Surgical Goal Completion** | MS Agent Framework |
| **Pascal Engine Testing** | Any "Original" (non-mcp) client |

## 7. Environment Variables Reference

| Variable | Frameworks | Purpose |
| :--- | :--- | :--- |
| `GOOGLE_API_KEY` | All | Gemini model authentication |
| `OPENAI_API_KEY` | All | OpenAI model authentication |
| `ANTHROPIC_API_KEY` | All | Anthropic model authentication |
| `OLLAMA_HOST` | All | Ollama server base URL (e.g. `http://127.0.0.1:11434`) |
| `MCP_URL` | MCP Clients | Go Server endpoint (default: `http://127.0.0.1:8765/mcp`) |
| `AI_REASONING` | Original | Toggle reasoning logs (`0`/`1`) |

## Related Documentation

- **Overview Index:** [README.md](file:///home/mfranz/github/vibepascal/README.md)
- **Agent Gameplay Logic:** [AI-GAMEPLAY.md](file:///home/mfranz/github/vibepascal/AI-GAMEPLAY.md) — Mechanics of autonomous play.
- **Performance Evaluation:** [mcp-client-analysis.md](file:///home/mfranz/github/vibepascal/mcp-client-analysis.md) — Latency, costs, and token efficiency analysis.
- **Historical Benchmarks:** [MEMORIAL-AGENTS-2026.md](file:///home/mfranz/github/vibepascal/MEMORIAL-AGENTS-2026.md) — Compatibility and scores database.
