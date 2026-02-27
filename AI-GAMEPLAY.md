# AI Gameplay: How the Agent Plays Dustwood

This document explains the technical implementation and reasoning logic behind the autonomous AI agents that play *Echoes of Dustwood*.

## Orchestration: `ai-game.sh` and `strands-ai-game.sh`

The `scripts/ai-game.sh` and `scripts/strands-ai-game.sh` scripts act as entry points and environment managers. Their primary responsibilities are:
1.  **Binary Integrity**: Ensures the Pascal engine (`bin/dustwood`) is compiled and up-to-date.
2.  **Environment Setup**: Cleans up previous save states and ensures logging directories exist.
3.  **Parameter Passing**: Translates high-level arguments (difficulty, model, delay) into the specific configuration needed by the AI client.

## The "Brains": `scripts/ai_client.py` and `scripts/strands_ai_client.py`

The system supports two implementation backends that share the same high-level logic but use different orchestration frameworks:

### 1. Pydantic AI Backend (`ai_client.py`)
The original implementation. It uses `pydantic-ai` to manage model interactions and structured output validation. It is highly optimized for direct provider integrations (Google, OpenAI, Anthropic).

### 2. Strands SDK Backend (`strands_ai_client.py`)
A modern port using the **Strands Agents SDK** and **LiteLLM**. 
-   **Conversation Management**: Uses a built-in `SlidingWindowConversationManager` to automatically handle history pruning.
-   **LiteLLM Integration**: Supports a vast array of models through a unified interface.
-   **Manual JSON Mode**: To support reasoning models (like `cogito:14b` or `deepseek-r1`) which often conflict with "Forced Tool Calling," the Strands client uses a custom validation loop. It allows the model to output raw text (including `<thought>` blocks) and then surgically extracts and validates the JSON command.

### 1. Direct Process Interaction
Unlike the web UI, the AI client does not use a web server. It spawns the Pascal binary as a direct subprocess using the `--headless` flag. It communicates via `stdin` and `stdout` using a non-blocking selector pattern to detect when the game is waiting for input (the `> ` prompt).

### 2. Structured Output (Reasoning Optional)
The agent is configured to return a structured JSON response for every turn:
-   **Command**: The actual string sent to the Pascal engine (e.g., "TAKE CANTEEN").
-   **Reasoning** (optional): A brief explanation of why the move was chosen.

Reasoning can be disabled to reduce token usage by setting `AI_REASONING=0` (default). When enabled, reasoning is logged for visibility.

### 3. Perception and Memory
The agent "sees" the same text a human player would. On every turn, the prompt sent to the LLM includes:
-   **Current Room State**: The latest description, visible items, and exits.
-   **Message History**: A capped window of recent turns to avoid unbounded context growth.
-   **Output Trimming**: Long game output is truncated to keep prompts compact while preserving recent text.
-   **Guidance Context**: Strategic instructions loaded from `data/guidance_*.txt` that define the game's objectives and survival rules.

## Strategy and Guidance

The AI's behavior changes based on the "difficulty" (guidance level) selected:

-   **Full**: The agent is given a detailed task sequence (Find map -> Get supplies -> Fix pump -> Reach stream).
-   **Medium/Minimal**: The agent is given only general survival hints, forcing it to deduce the correct path through exploration and trial-and-error.

### Loop Detection and Robustness
The client includes several "peer-over-the-shoulder" features to handle common LLM failure modes:
-   **Enhanced Sanitization**: Automatically cleans up punctuation, formatting errors, and artifacts from reasoning models (like leaked `<thought>` tags or malformed JSON strings).
-   **Repeat Detection**: If the agent repeats the same command and the game output is unchanged for multiple turns, the client forces a simple exploratory command (e.g., `LOOK`, `NORTH`, `EAST`, `SOUTH`, `WEST`) to break loops.
-   **Validation**: Every command is checked against the game's valid verb list before being sent to the engine.
-   **Repair Logic**: Both clients feature retry loops that detect validation failures and provide immediate corrective feedback to the model.

## Supported Providers
By using `pydantic-ai`, the system is model-agnostic. It supports:
-   **OpenAI/Gemini/Anthropic**: High-reasoning cloud models.
-   **Ollama**: Local execution for private, offline play.
-   **OpenAI Compatibility Layer**: Any provider following the OpenAI API standard.
