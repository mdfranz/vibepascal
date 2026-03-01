# Echoes of Dustwood: A Tale of Two Engines and Many Agents

This document chronicles the rapid evolution of *Echoes of Dustwood*, a journey from a modular Pascal text adventure to a multi-engine, AI-powered immersive experience using the Model Context Protocol (MCP).

**Project Age:** Approximately 4 days (Started: February 26, 2026)

## Phase 1: Foundation (Pascal & World DB)
**Timeline:** February 26, 2026 (Morning/Afternoon)

The project began as a pure modular Free Pascal text adventure. The core vision was established early: a survival-focused exploration of a deserted frontier town. Key to this phase was the decision to move the game world (rooms, items, exits) into an external `world.ini` file, allowing for easy expansion and modding without recompiling the engine.

*   **Key Milestones:** Initial world loading from INI, basic command parsing (N/S/E/W, LOOK, TAKE), and the implementation of survival mechanics like thirst.
*   **Key Code Changes & Improvements:**
    *   `5494ac9`: Refactored hardcoded world data to external `data/world.ini`.
    *   `f1935cc`: Fixed early Pascal pointer issues in item handling.
    *   `u_world.pas`: Implemented deterministic randomization for item placement.
    *   `u_io.pas`: Added automatic word-wrapping for terminal output.

## Phase 2: Sidecars & Python Integration (The AI Bridge)
**Timeline:** February 26, 2026 (Evening)

As the engine matured, the focus shifted towards AI-driven gameplay. The "Persistent Sidecar" architecture was born. By creating a FastAPI wrapper (`sidecar.py`), the legacy-style Pascal binary could be controlled via REST, providing a bridge for modern AI agents.

*   **Key Milestones:** Introduction of `sidecar.py`, `ai_client.py` using Pydantic AI, and early experiments with local LLMs via Ollama.
*   **Key Code Changes & Improvements:**
    *   `scripts/sidecar.py`: Created a FastAPI wrapper that manages a long-running subprocess and handles I/O redirection.
    *   `scripts/ai_client.py`: Initial implementation of a Pydantic AI agent with specific game-state awareness.
    *   `scripts/client.py`: Developed a simple HTTP client to test the sidecar API independently.

## Phase 3: Gameplay Mechanics & Refinement
**Timeline:** February 26 - 27, 2026

With the bridge in place, the game world became more interactive. Features like drinking from a canteen, saddling a horse, and time-of-day mechanics were added. The AI client was continuously refined to handle these new mechanics, ensuring it could navigate and survive the desert.

*   **Key Milestones:** `TAKE CANTEEN`, `DRINK`, horse mechanics ("Horseplay"), and better "LOOK" functionality to help agents understand their surroundings.
*   **Key Code Changes & Improvements:**
    *   `1780099`: Added canteen-specific state and the `DRINK` command.
    *   `8059886`: Implemented complex horse mechanics: `SADDLE`, `RIDE`, and horse-specific thirst.
    *   `0cea0f5`: Enhanced the Pascal binary with command-line arguments for `--headless` mode and `--turns` limits.
    *   `e524d3b`: Improved `LOOK` command to allow peering into adjacent rooms, crucial for AI spatial reasoning.

## Phase 4: Agent Robustness & Multi-Model Support
**Timeline:** February 27, 2026

The project saw a major push for robustness. It became clear that different LLMs (from GPT-4 to smaller local models) had varying abilities to follow game logic. The introduction of "Strands" support and the Strands Agents SDK provided a more robust framework for agents to plan and execute tasks.

*   **Key Milestones:** Integration of Strands SDK, improved guidance for agents, and fixes for various LLM-specific behaviors (e.g., handling unexpected output formats).
*   **Key Code Changes & Improvements:**
    *   `54c3c41`: Integrated Strands Agents SDK and LiteLLM for broad model support.
    *   `283090f`: Massive update to agent guidance and error handling based on failures with local Ollama models.
    *   `model_tester.sh`: Created a utility to benchmark different LLM models against a standard game scenario.
    *   `AGENT-NUANCES.md`: Started documenting specific model quirks discovered during testing.

## Phase 5: The Go Port & Modern Architecture
**Timeline:** February 28, 2026 (Early Morning)

To improve performance, terminal handling, and modernize the codebase, the entire Pascal engine was ported to Go. This "Go Port" was designed to be logic-identical to the original Pascal version but offered a cleaner foundation for future features.

*   **Key Milestones:** Complete rewrite of the engine in Go (`bin/dustwood-go`), maintaining parity with all Pascal features.
*   **Key Code Changes & Improvements:**
    *   `src/golang/`: Re-implemented the entire game logic in Go, using structs and interfaces for better modularity.
    *   `src/golang/persistence.go`: Ported the INI-based save/load system to Go with full compatibility.
    *   `Makefile`: Updated to support multi-language builds (Pascal and Go).

## Phase 6: MCP Maturity & Ecosystem Integration
**Timeline:** February 28, 2026 (Mid-day)

The final (and current) phase saw the Go engine evolve into a full Model Context Protocol (MCP) server. This allowed *Echoes of Dustwood* to be seamlessly integrated into modern AI tools like Claude Code, where the game engine acts as a "tool" that agents can use directly.

*   **Key Milestones:** MCP Streamable HTTP server implementation, stateless JSON mode for Claude Code, and specialized MCP clients (`pydantic_mcp_client.py`, `strands_mcp_client.py`).
*   **Key Code Changes & Improvements:**
    *   `1d31778`: Added a full MCP server implementation to the Go engine using `mcp-golang-sdk`.
    *   `4d334c8`: Fixed turn tracking bugs in the Go engine discovered during MCP integration.
    *   `scripts/pydantic_mcp_client.py`: Created a next-gen AI client that uses structured tool-calls via the MCP interface.
    *   `src/golang/mcp_server.go`: Added support for stateless sessions and JSON-RPC over HTTP.

## Learnings & The "Broken" Path
Building a game for AI agents revealed fundamental differences between human and machine play. Each "broken" behavior led to a more robust architecture.

*   **The Inventory Loop**: High-reasoning models (GPT-4o, Claude 3.5) often got stuck in "inventory loops"—repeatedly swapping items when full.
    *   *Solution*: Implemented an **Augmented Reality** layer that injects strategy hints (e.g., "Drop the Book") when capacity is reached.
*   **Reasoning Interruption**: Reasoning models (like DeepSeek-R1) would output `<thought>` blocks that broke standard JSON parsers.
    *   *Solution*: Developed a **Reverse-Search JSON Extractor** that surgically finds the final command block even if preceded by pages of "thinking."
*   **AI Frustration**: When models got stuck in logical dead-ends, they would often loop on the same invalid command.
    *   *Solution*: The **Frustration Mechanic**. After 3 failed attempts, the client forces a state change (like `BURN <ITEM>`) to "shake" the AI into a new branch of the game tree.
*   **The Safety Override**: Models would often try to "TAKE" or "FIX" things while being shot at by outlaws or hissed at by snakes.
    *   *Solution*: **Hard-coded safety overrides** that force a `WAIT` or `SHOOT` command when a threat is detected, overriding the LLM's intended move.
*   **Spatial Blindness**: Early agents struggled to remember where they had been.
    *   *Solution*: A **Knowledge Base** that parses game output to maintain a persistent map of cleared vs. uncleared rooms, which is then fed back into the model's prompt.

---
*The journey of Dustwood continues, bridging the gap between retro-computing and the frontier of AI.*
