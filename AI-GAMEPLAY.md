# AI Gameplay: How the Agent Plays Dustwood

This document explains the technical implementation and reasoning logic behind the autonomous AI agents that play *Echoes of Dustwood*.

## Orchestration: `ai-game.sh` and `strands-ai-game.sh`

The `scripts/ai-game.sh` and `scripts/strands-ai-game.sh` scripts act as entry points and environment managers. Their primary responsibilities are:
1.  **Binary Integrity**: Ensures the Pascal engine (`bin/dustwood`) is compiled and up-to-date.
2.  **Environment Setup**: Cleans up previous save states and ensures logging directories exist.
3.  **Parameter Passing**: Translates high-level arguments (difficulty, model, delay) into the specific configuration needed by the AI client.

### Script Arguments

Both scripts accept the same four positional arguments:
```
./scripts/ai-game.sh         [difficulty] [model] [delay] [max_turns]
./scripts/strands-ai-game.sh [difficulty] [model] [delay] [max_turns]
```

| Argument | Values | Default |
| :--- | :--- | :--- |
| `difficulty` | `full`, `medium`, `minimal` | `full` |
| `model` | Provider-prefixed model string | `google-gla:gemini-3-flash-preview` / `gemini/gemini-3-flash-preview` |
| `delay` | Seconds between turns | `1` |
| `max_turns` | Max turns before stopping | `25` |

Model naming differs: `ai-game.sh` uses Pydantic AI colons (`google-gla:`, `openai:`, `ollama:`); `strands-ai-game.sh` uses LiteLLM slashes (`gemini/`, `openai/`, `ollama/`).

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
-   **Message History**: A rolling window of the **4 most recent turns** (`MESSAGE_HISTORY_LIMIT = 4`) to avoid unbounded context growth.
-   **Output Trimming**: Long game output is truncated to keep prompts compact while preserving recent text.
-   **Guidance Context**: Strategic instructions loaded from `data/guidance_*.txt` that define the game's objectives and survival rules.
-   **Knowledge Base**: Persistent room→item map parsed from game output each turn. Tracks cleared rooms and remaining items; injected into context so the agent avoids re-visiting empty rooms.
-   **Inventory Strategy Hint**: When inventory reaches capacity (5/5), a `STRATEGY HINT` is injected suggesting dropping a non-essential item (BOOK or NOTE).

## Strategy and Guidance

The AI's behavior changes based on the "difficulty" (guidance level) selected:

-   **`guidance_minimal.txt`** (~20 lines): Basic survival hints only — short verb list, brief desert/horse notes (saddle, stream, key), strict output rules. No map or task sequence. Forces the agent to discover the path through exploration.
-   **`guidance_medium.txt`** (~28 lines): Adds named town layout (General Store, Livery Stables, Sheriff's Office), movement efficiency rules, a 48-verb list, and explicit thirst/desert warnings. No step-by-step task order.
-   **`guidance_full.txt`** (~88 lines): Comprehensive guide: full directional map, 16-step numbered task sequence (find MAP through OPEN BOX and TAKE REVOLVER), inventory management rules (essential vs. disposable items), threat handling for rattlesnakes and outlaws, horse riding constraints, and puzzle notes (locked gun box key location).

### Loop Detection and Robustness
The client includes several "peer-over-the-shoulder" features to handle common LLM failure modes:
-   **Enhanced Sanitization**: Automatically cleans up punctuation, formatting errors, and artifacts from reasoning models (like leaked `<thought>` tags or malformed JSON strings).
-   **Repeat Detection**: If the agent repeats the same command and the game output is unchanged for multiple turns, the client forces a simple exploratory command (e.g., `LOOK`, `NORTH`, `EAST`, `SOUTH`, `WEST`) to break loops.
-   **Validation**: Every command is checked against the game's valid verb list before being sent to the engine.
-   **Repair Logic**: Both clients feature retry loops that detect validation failures and provide immediate corrective feedback to the model.
-   **Frustration Mechanic**: A `frustration` counter increments on invalid commands, JSON parse failures, and loop activations. When it reaches `AI_FRUSTRATION_THRESHOLD` (default: 3) and MATCHES are in inventory with no active threat, the client forces `BURN <ITEM>` in priority order: BOOK → LEDGER → MAP → LEATHER → SADDLE. The counter decays by `AI_FRUSTRATION_DECAY` (default: 1) on score updates.
-   **Outlaw Safety Override**: When `"DIRTY OUTLAW"` appears in game output, any command outside the safe set is replaced with `WAIT`. Safe commands: `SHOOT`, `KILL`, `WAIT`, movement (N/S/E/W), and inspection (`LOOK`, `INVENTORY`, `MOUNT`, `DISMOUNT`).
-   **Late-Game Stall Prevention**: In the final 5 turns, if the score hasn't changed for 5+ consecutive turns, `LOOK` and `SEARCH` commands are replaced with a forced exploratory move.

## MCP-Based Gameplay (New)

The latest evolution of the AI players moves away from direct subprocess interaction and regex parsing in favor of the **Model Context Protocol (MCP)**.

### Advantages of MCP
1.  **Structured State**: Instead of parsing raw terminal text, the agent receives a validated JSON `GameSummary` object containing precise values for thirst, turns, inventory, and location.
2.  **Native Tool Calling**: Agents treat game commands as standard model tools. This allows models to "think" about their next move and then call the `command` tool directly.
3.  **Engine Independence**: The client no longer needs to know about the binary path or headless flags; it simply communicates with an MCP-compliant server.

### MCP Implementation Variants

#### 1. Pydantic AI MCP Client (`pydantic_mcp_client.py`)
This is the most robust implementation for the project's current stateless HTTP architecture.
- **Type-Safe Mapping**: Directly maps the Go server's JSON output to a Pydantic `GameSummary` model.
- **Explicit Tools**: Defines a `@agent.tool` wrapper that formats both narrative and structured state for the model's consumption.

#### 2. Strands SDK MCP Client (`strands_mcp_client.py`)
Utilizes the Strands SDK's dynamic tool discovery.
- **Dynamic Ingestion**: Uses `strands.tools.mcp.MCPClient` to automatically "suck in" tool definitions from the server at runtime.
- **Agentic Autonomy**: Relies on the SDK's internal loop to manage the tool-calling lifecycle.

## Supported Providers
By using `pydantic-ai`, the system is model-agnostic. It supports:
-   **OpenAI/Gemini/Anthropic**: High-reasoning cloud models.
-   **Ollama**: Local execution for private, offline play.
-   **OpenAI Compatibility Layer**: Any provider following the OpenAI API standard.

### Environment Variables

| Variable | Purpose | Default |
| :--- | :--- | :--- |
| `GOOGLE_API_KEY` | Gemini model authentication | (required) |
| `OPENAI_API_KEY` | OpenAI model authentication | (required) |
| `ANTHROPIC_API_KEY` | Anthropic model authentication | (required) |
| `OLLAMA_HOST` | Ollama server base URL | `http://localhost:11434` |
| `AI_REASONING` | Log reasoning field (`0`=off, `1`=on) | `0` |
| `AI_FRUSTRATION_THRESHOLD` | Invalid-action count before burn triggers | `3` |
| `AI_FRUSTRATION_DECAY` | Frustration reduction per score update | `1` |
| `GAME_SEED` | Integer seed for reproducible game runs | (random) |

Note: `OLLAMA_BASE_URL` / `OLLAMA_API_BASE` are set programmatically; users only set `OLLAMA_HOST`.
