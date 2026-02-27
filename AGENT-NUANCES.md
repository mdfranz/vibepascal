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
| **Reasoning Support** | High (Native handling) | High (Manual extraction of `{ }` blocks) |
| **Repair Logic** | Automatic SDK-level retries | Manual 3-attempt "Repair & Nudge" loop |

### The "Reasoning Model" Problem
Reasoning models (like `cogito:14b` or `deepseek-r1`) often fail when forced into a **Tool Call** (Function Call) because they prefer to emit `<thought>` blocks before their final answer.

*   **Pydantic AI** handles this well through mature provider integrations that know when to wait for the final tool call.
*   **Strands** (by default) uses LiteLLM's tool-calling abstraction, which can be brittle with local reasoning models. 
*   **Our Solution**: In `strands_ai_client.py`, we bypassed the "Forced Tool Call" in favor of a **Manual JSON Mode**. This allows the model to "think" as much as it wants, and our script surgically extracts the JSON command from the raw output.

## 3. Sanitization & Sanitization

The game engine is sensitive to command formatting (e.g., `TAKE CANTEEN.` vs `TAKE CANTEEN`).

*   **Pydantic AI Client**: Uses basic sanitization (strip punctuation, uppercase).
*   **Strands Client**: Implements **"Deep Sanitization."** It can handle fragments like `command: NORTH` (missing quotes) or JSON buried inside markdown blocks, which is common when using smaller local models.

## 4. Environment & Connectivity

*   **Ollama Connectivity**: 
    *   Pydantic AI uses `OLLAMA_HOST` directly.
    *   Strands (via LiteLLM) uses `api_base`. Our implementation bridges this by translating `OLLAMA_HOST` to the appropriate `api_base` dynamically.
*   **Model Naming**:
    *   Pydantic AI uses provider prefixes like `google-gla:` or `openai:`.
    *   Strands uses LiteLLM slashes like `gemini/` or `ollama/`.

## Summary: Which to use?

*   **Use Strands SDK if**: You want to experiment with a wide variety of models via LiteLLM or want a more stateful, "agentic" architecture. It is currently the most robust implementation for local reasoning models.
*   **Use Pydantic AI if**: You are using top-tier cloud models (Gemini, Claude) and want the highest performance and most mature "self-correction" features provided by a stable framework.
