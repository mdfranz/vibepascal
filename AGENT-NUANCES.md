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
*   **Connectivity**: Best-in-class handling of LiteLLM/Ollama edge cases.
*   **Limitation**: Currently struggles with the Go server's specific SSE implementation.

### Microsoft Agent Framework (`ms_agent_client.py`)
*   **Efficiency**: Extremely surgical behavior. Reaches goals with the minimum number of turns.
*   **Limitation**: The standard OpenAI client is incompatible with Gemini 3.1 models due to missing `thought_signature` support.
*   **Tested Models**: Excellent performance with `gpt-5-mini` and `ollama/gpt-oss:20b`.

### Agno (formerly Phidata) (`agno_client.py`)
*   **Native SDKs**: Uses native Google/Anthropic SDKs rather than generic wrappers.
*   **Gemini 3.1 Champion**: The **only** framework successfully running Gemini 3.1 Pro/Flash in this project.
*   **Proactivity**: Showed high initiative in logs, often exploring beyond the immediate goal to gather survival items.

## 3. Performance & Latency (MCP vs. Original)

Testing reveals a significant performance gap between interaction methods:

*   **Original (Direct Stdio)**: **~5.1 seconds/turn**. Bottlenecked by the 5s "Read Timeout" used to ensure the full game description is captured from the Pascal process.
*   **MCP (JSON-RPC)**: **~1.25 seconds/turn**. Atomic state returns eliminate the need for timeouts, resulting in a **400% speed increase**.

## 4. Model Performance Observations (Updated)

*   **gemini-3.1-pro-preview**: High-tier logic. Successfully handled complex puzzles via Agno.
*   **gpt-5-mini**: The "Utility Player." Works reliably across all four frameworks with high efficiency.
*   **claude-opus-4-6**: The planning expert. Best at long-term inventory management.
*   **gpt-oss:20b (Ollama)**: Strong performance via MS Agent. Navigated to goals with zero typos or logic loops.
*   **granite4:3b (Ollama)**: Struggles with "common sense." Requires frameworks with strong retry logic to recover from typos like `TAKE SPPOOL`.

## 5. Framework Decision Matrix

| If you want... | Use this Framework |
| :--- | :--- |
| **Maximum Speed** | Pydantic AI (via MCP) |
| **Gemini 3.1 Support** | Agno |
| **Local LLM Robustness** | Strands SDK |
| **Surgical Goal Completion** | MS Agent Framework |
| **Pascal Engine Testing** | Any "Original" (non-mcp) client |

## 6. Environment Variables Reference

| Variable | Frameworks | Purpose |
| :--- | :--- | :--- |
| `GOOGLE_API_KEY` | All | Gemini model authentication |
| `OPENAI_API_KEY` | All | OpenAI model authentication |
| `ANTHROPIC_API_KEY` | All | Anthropic model authentication |
| `OLLAMA_HOST` | All | Ollama server base URL (e.g. `http://127.0.0.1:11434`) |
| `MCP_URL` | MCP Clients | Go Server endpoint (default: `http://127.0.0.1:8765/mcp`) |
| `AI_REASONING` | Original | Toggle reasoning logs (`0`/`1`) |
