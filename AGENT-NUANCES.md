# Agent Nuances: Pydantic AI vs. Strands SDK

This document provides a technical comparison of the two AI agent implementations in *Echoes of Dustwood*. While both achieve the same goal—playing the game autonomously—they differ significantly in their orchestration, robustness strategies, and model support.

## 1. Orchestration Philosophy

### Pydantic AI (`ai_client.py`)
*   **Model-Centric**: Built around the idea of "Model Settings" and "Dependencies." 
*   **Static Context**: Historically requires manual management of message history slicing.
*   **Native Providers**: Uses specialized providers for OpenAI, Gemini, and Ollama, allowing it to leverage provider-specific features (like Ollama's `format="json"`).

### Strands SDK (`strands_ai_client.py`)
*   **Agent-Centric**: Focuses on a stateful `Agent` object that encapsulates history and state.
*   **LiteLLM Abstraction**: Uses LiteLLM as a unified middleman, providing instant access to hundreds of models but occasionally losing fine-grained control over provider-specific "nudges."
*   **Stateful Memory**: Uses built-in `ConversationManager` (e.g., `SlidingWindowConversationManager`) to handle context window management automatically.

## 2. Structured Output & Robustness

The most significant difference lies in how they force the model to return valid JSON commands.

| Feature | Pydantic AI Implementation | Strands Implementation (Customized) |
| :--- | :--- | :--- |
| **Primary Method** | Tool/Function Calling | Manual JSON Extraction (Custom Loop) |
| **JSON Enforcement** | Natively via `output_type` | Explicit prompting + Schema injection |
| **Reasoning Support** | High (Native handling) | High (Manual reverse-extraction) |
| **Repair Logic** | Automatic SDK-level retries | Manual 3-attempt "Repair & Nudge" loop |

### The "Harmony" & Reasoning Format
Some models (like `gpt-oss:20b` and `deepseek-r1`) use a "Harmony" response format that separates thinking (`analysis`) from the final response.

*   **Pydantic AI** handles these through specialized providers that often strip thinking tags before validation.
*   **Strands (LiteLLM)** initially struggled with empty responses from these models because the content was in an `analysis` channel. 
*   **Our Solution**: In `strands_ai_client.py`, we implemented a **Reverse-Search JSON Extractor**. It searches the model's full response for multiple `{ }` blocks and markdown code fences, attempting to parse them in reverse order (assuming the final block is the actual command). This approach is effective for any model that embeds its final answer after a block of reasoning text.

## 3. Strategic State Tracking

Both clients now implement an "Augmented Reality" layer that helps weaker models stay on track.

*   **Knowledge Base**: The scripts parse the game output to maintain a persistent map of which rooms have been cleared and which still contain items. This is injected into the model's context every turn.
*   **Inventory Management**: Both clients track current inventory (0-5 items) and provide **Strategy Hints** when the bag is full, suggesting which items (like the BOOK or NOTE) are safe to drop.
*   **Safety Overrides**: When `"DIRTY OUTLAW"` is detected, any command not in the safe set is replaced with `WAIT`. Safe commands permit combat (`SHOOT`, `KILL`), evasion (`WAIT`, movement directions), and inspection — no TAKE or FIX.
*   **Frustration Mechanic**: A running `frustration` counter increments on invalid commands, JSON failures, loop breaks, and fallback activations. When it reaches `AI_FRUSTRATION_THRESHOLD` (default: 3) with no active threat and MATCHES available, the client autonomously issues `BURN <ITEM>` in priority order: BOOK → LEDGER → MAP → LEATHER → SADDLE. This forces a game-state change without requiring LLM cooperation. The counter decays by `AI_FRUSTRATION_DECAY` (default: 1) per score update.

## 4. Environment & Connectivity

*   **Ollama Connectivity**:
    *   **Pydantic AI** (`ai_client.py`): Reads `OLLAMA_HOST`, constructs the `/v1` base URL, and sets `OLLAMA_BASE_URL` in the environment. Turn delay is reduced to 0.2× for local models.
    *   **Strands** (`strands_ai_client.py`): Also reads `OLLAMA_HOST`, but rewrites the model prefix to `openai/` and passes `api_base` pointing to the `/v1` endpoint with a dummy API key (`"ollama"`). This bypasses Ollama-specific bugs in LiteLLM's native provider (e.g., `functions_unsupported_model`) and produces more stable streaming. Sets `OLLAMA_API_BASE` as a side-effect.
    *   Both accept `OLLAMA_HOST` as the single user-facing variable.
*   **Model Naming**:
    *   Pydantic AI uses provider prefixes like `google-gla:` or `openai:`.
    *   Strands uses LiteLLM slashes like `gemini/` or `openai/` (for Ollama).

## 5. Model Performance Observations

*   **claude-opus-4-6**: The **Undisputed Champion**. Achieved a record score of **100**. It was the only model to successfully complete the full sequence: fixing the telegraph, gathering all survival gear, repairing the pump, mounting the horse, and navigating deep into the Mojave to find the Hidden Stream. It demonstrated superior long-term planning and perfect inventory management.
*   **Gemini 3 Pro**: High-tier "King." Managed inventory perfectly (dropped the book to make room for survival gear) and solved the pump puzzle efficiently.
*   **claude-sonnet-4-6**: High resilience. While it initially suffered from the same **Inventory Looping** as other high-tier models, it was the only model to consistently **break the loop autonomously** (by eventually dropping the Book), allowing it to fix the pump, mount the horse, and use matches to navigate in the dark.
*   **gpt-5 / gpt-5-mini**: High precision but prone to persistent **Inventory Looping**. These models followed the verb list perfectly but often wasted their entire turn budget swapping survival gear once the inventory was full.
*   **Gemini 3 Flash**: Excellent exploration but prone to small logic errors in inventory management without hints.
*   **gpt-oss:20b**: Extremely verbose reasoning. Requires the OpenAI-compatible endpoint and high `max_tokens` (4000+) to reach the final command block.
*   **Granite 4:3b**: Struggles with basic spatial reasoning. Often requires the client-side "Loop Detection" to force it out of `LOOK` cycles.

## 6. Environment Variables Reference

| Variable | Client(s) | Purpose | Default |
| :--- | :--- | :--- | :--- |
| `GOOGLE_API_KEY` | Both | Gemini model authentication | (required) |
| `OPENAI_API_KEY` | Both | OpenAI model authentication | (required) |
| `ANTHROPIC_API_KEY` | Both | Anthropic model authentication | (required) |
| `OLLAMA_HOST` | Both | Ollama server base URL | `http://localhost:11434` |
| `AI_REASONING` | Both | Log reasoning field (`0`=off, `1`=on) | `0` |
| `AI_FRUSTRATION_THRESHOLD` | Both | Invalid-action count before burn triggers | `3` |
| `AI_FRUSTRATION_DECAY` | Both | Frustration reduction per score update | `1` |
| `GAME_SEED` | Both | Integer seed for reproducible game runs | (random) |

## Summary: Which to use?

*   **Use Strands SDK if**: You want to experiment with a wide variety of models via LiteLLM or want a more stateful, "agentic" architecture. It is currently the most robust implementation for local reasoning models.
*   **Use Pydantic AI if**: You are using top-tier cloud models (Gemini, Claude) and want the highest performance and most mature "self-correction" features provided by a stable framework.
