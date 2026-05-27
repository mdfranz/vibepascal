# Comparative Analysis: Google ADK, Agno, Pydantic AI, & Strands
**Date:** Wednesday, May 27, 2026

This document presents a comprehensive comparative analysis of the four agent frameworks evaluated on the "Echoes of Dustwood" MCP text adventure game benchmark. The analysis is based on the session databases and log files generated from fresh runs of each framework.

---

## 1. Executive Summary & Metrics

Below is the consolidated performance and token metric matrix across all four runs:

| Dimension / Metric | Google ADK | Agno | Pydantic AI | Strands |
| :--- | :---: | :---: | :---: | :---: |
| **Final Game Score** | **80** | **80** | **70** | **73** |
| **Game Turns Taken** | 20 / 20 | 20 / 20 | 20 / 20 | 20 / 20 |
| **LLM Calls (Requests)** | 23 | **20** | 22 | 25 |
| **Total Input Tokens** | 150,707 | **73,036** | 141,733 | 142,079 |
| **Total Output Tokens** | 554 | **50** | 4,181 | 2,897 |
| **Total Run Tokens** | 152,757 | **73,086** | 145,914 | 144,976 |
| **Total Run Duration** | 56.8s | 52.2s | 67.6s | **39.1s** |
| **Prompt Cache Reads** | **36,029** | *Unreported* | **47,764** | *Unreported* |
| **Reasoning Tokens** | Yes | **1,616** | Yes | *Unreported* |
| **Session Format** | SQLite DB | SQLite DB | Single JSON | Multi-JSON Dir |
| **Session Messages Count** | 46 events | 20 turns | 42 messages | 50 files |

![Framework Profile: Normalized Multi-Axis Comparison](charts/may27_radar.png)

---

## 2. Gameplay Behavior & Strategy

The agents encountered different starting environments (due to map randomization and threat placement), yielding distinct gameplay outcomes:

*   **Google ADK (Score: 80)**: Played a systematic and cautious game. It cleared out the General Store first (canteen, leather, matches, saddle), repaired the telegraph in the Telegraph Office, encountered the outlaw in the Assayer's Office, safely retreated back to Main Street, repaired the pump in the Livery Stables, drank, and saddled the horse on turn 20. It mounted on turn 21 — a trailing action executed after the turn limit was reached before ADK's invocation concluded cleanly.
*   **Agno (Score: 80)**: Navigated away from a combined snake/outlaw room on turn 1. It dropped the starting book to free up inventory space, gathered the General Store items, repaired the pump, filled/drank water, and successfully saddled/mounted the horse to ride deep into the Howling Desert, reaching Room 10 by turn 20.
*   **Strands (Score: 73)**: Successfully repaired the telegraph and located the map. It collected all General Store items, repaired the pump, but ran out of turns before it could fill, drink, saddle, or mount the horse.
*   **Pydantic AI (Score: 70)**: Collected General Store items but got caught in an exploration/pathfinding loop moving between Main Street, Sheriff's Office, and Assayer's Office to avoid rattlesnakes and outlaws. The Telegraph Office was visited only once during this detour, not as a repeated loop destination. It repaired the pump and drank, but did not saddle or mount.

![Score Trajectory: All 4 Frameworks](charts/may27_score_trajectory.png)

---

## 3. Session & State Serialization

The four frameworks take fundamentally different approaches to storing session history and state data:

### Google ADK
*   **Target**: `sessions/adk_sessions.db` (SQLite via SQLAlchemy/aiosqlite).
*   **Design**: Highly structured relational tables (`sessions`, `events`, `app_states`, `user_states`). Individual conversational steps are logged as separate rows in the `events` table (1 user prompt + 1 model reply per call, resulting in exactly **46 event rows** for the 23 requests).
*   **Cleanup**: Configured with a foreign key constraint (`ondelete="CASCADE"`), meaning deleting a session clean-deletes all associated event records.

### Agno
*   **Target**: `sessions/agno_sessions.db` (SQLite via `sqlite3`).
*   **Design**: Flat table serialization. High-level state and run metrics are stored directly inside JSON columns in the `dustwood_agno_sessions` table. 
*   **Auditability**: Session token and reasoning counts are stored under the `session_metrics` JSON field, matching the log output exactly (e.g. `input_tokens: 73036`).

### Pydantic AI
*   **Target**: `sessions/pydantic_sessions/pydantic-session-*.json` (JSON).
*   **Design**: Serializes the list of `ModelMessage` objects into a single JSON file.
*   **Count**: Contains exactly **42 messages** (1 start prompt + 20 execution turns consisting of a request/return pair + 1 summary response), matching `agent_run.all_messages()` exactly.
*   **Overhead**: Simple to read, but requires rewriting the entire conversation array to disk on every step.

### Strands
*   **Target**: `sessions/strands_sessions/session_strands-*/` (Directory).
*   **Design**: Extremely modular directory structure.
*   **Count**: Stores each conversational turn in its own file under `/agents/agent_default/messages/` (exactly **50 JSON files** representing the 25 user/model request pairs).
*   **Efficiency**: Extremely fast incremental writes since Strands only appends one small file per turn instead of rewriting the entire history.

---

## 4. Efficiency & Output Verbosity

*   **Agno is the most token-efficient framework (73,086 total tokens)**. It enforces a strict output policy where the model responds with only the raw command verb/noun (e.g., `"TAKE CANTEEN"` or `"SOUTH"`), yielding just **50 output tokens** across the entire 20-turn run.
*   **Pydantic AI (4,181 output tokens) and Strands (2,897 output tokens) suffer from output bloat**. The model frequently includes markdown lists, plans, and summaries in its responses, inflating the context window and API costs on subsequent turns.
*   **ADK lies in the middle (554 output tokens)**. It uses direct native tool calls, which avoids conversational fluff but still retains structured tool arguments in the conversation history.

![Token Composition: Input vs Output breakdown](charts/may27_token_composition.png)

---

## 5. Performance & Latency

*   **Strands is the fastest (39.1s total run)**, utilizing LiteLLM with minimal framework loop overhead.
*   **Agno is close behind (52.2s)**, processing sequential commands quickly with low latency.
*   **ADK (56.8s) and Pydantic AI (67.6s) are slower** due to additional framework validation, event tracking, and state rehydration logic during execution.

![Efficiency: Score vs Latency (bubble = token cost)](charts/may27_efficiency_scatter.png)

![Score Efficiency: Points per 1,000 Tokens](charts/may27_score_efficiency.png)

![Cost-Normalized Ranking: Score per Token-Second](charts/may27_cost_normalized.png)

---

## 6. Prompt Caching & Reasoning

*   **Pydantic AI (47,764 cached tokens)** and **Google ADK (36,029 cached tokens)** natively capture and log Gemini prompt caching statistics (`cache_read_tokens`). 
*   **Strands and Agno do not report cache reads** because their transport wrappers (LiteLLM for Strands, and custom client wrappers for Agno) discard Gemini-specific `cached_content_token_count` metadata before it reaches logging hooks.
*   **Agno (1,616 reasoning tokens)** and **ADK** explicitly track model reasoning tokens (Gemini's internal thinking budget), providing visibility into how much compute the model spent planning its game moves.
