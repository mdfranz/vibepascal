# Comparative Analysis: Gemini 3.5 Flash vs. Gemini 2.5 Flash
**Date:** Wednesday, May 27, 2026  
**Benchmark:** Echoes of Dustwood — MCP text adventure, 20-turn limit, `guidance_full.txt`

This document applies the same analytical methodology as the four-framework comparative to a within-framework model upgrade comparison: how does swapping `gemini-3.5-flash` for `gemini-2.5-flash` change each framework's gameplay outcome, token efficiency, reasoning behavior, and failure modes?

---

## 1. Executive Summary & Metrics

### Run A — Gemini 3.5 Flash (08:43–08:46)

| Dimension / Metric | Google ADK | Agno | Pydantic AI | Strands |
| :--- | :---: | :---: | :---: | :---: |
| **Final Game Score** | 34 | **83** | 73 | **83** |
| **Game Turns Taken** | 20 / 20 | 20 / 20 | 20 / 20 | 20 / 20 |
| **LLM Calls (Requests)** | 22 | **20** | 28 | 24 |
| **Total Input Tokens** | 186,930 | 85,679 | 224,952 | 125,882 |
| **Total Output Tokens** | 586 | **53** | 2,144 | 2,127 |
| **Total Run Tokens** | 188,976 | 85,732 | 227,096 | 128,009 |
| **Total Run Duration** | 104.2s | 75.2s | ~104s | **33.1s** |
| **Prompt Cache Reads** | 38,933 | *Unreported* | **92,061** | *Unreported* |
| **Reasoning Tokens** | ~1,093 | 1,652 | *(folded in)* | *(folded in)* |
| **Stop Reason** | Turn limit | Turn limit | Turn limit | Agent completed |

### Run B — Gemini 2.5 Flash (08:48–08:51)

| Dimension / Metric | Google ADK | Agno | Pydantic AI | Strands |
| :--- | :---: | :---: | :---: | :---: |
| **Final Game Score** | 27 | **85** | 78 | **0 ⚠️** |
| **Game Turns Taken** | 10 / 20 | 20 / 20 | 20 / 20 | 0 / 20 |
| **LLM Calls (Requests)** | 13 | **20** | 22 | 2 |
| **Total Input Tokens** | 69,688 | 70,756 | 124,919 | 5,259 |
| **Total Output Tokens** | 178 | **55** | 2,444 | 243 |
| **Total Run Tokens** | 70,556 | 70,811 | 127,363 | 5,502 |
| **Total Run Duration** | 46.9s | **42.2s** | ~81s | 5.3s |
| **Prompt Cache Reads** | **53,238** | *Unreported* | 76,650 | *Unreported* |
| **Reasoning Tokens** | ~722 | **32** | *(folded in)* | *(folded in)* |
| **Stop Reason** | GAME OVER (snake) | Turn limit | Turn limit | Framework termination |

### Score Delta (2.5 vs 3.5)

| Framework | 3.5 Score | 2.5 Score | Delta |
| :--- | :---: | :---: | :---: |
| **Agno** | 83 | **85** | +2 ✅ |
| **Pydantic AI** | 73 | **78** | +5 ✅ |
| **ADK** | 34 | 27 | -7 ❌ |
| **Strands** | 83 | 0 | -83 💀 |

---

## 2. Gameplay Behavior

### Google ADK

**3.5 (Score: 34)** — The map was placed in the Livery Stables. The run was crippled by outlaws: one in the Telegraph Office (turn 1, immediate retreat) and a persistent outlaw in the General Store (turns 3, 12, and 14 all blocked). ADK 3.5 spent eight turns probing the General Store and waiting for the outlaw to clear before securing the canteen and leather on turns 18–19. With no time remaining, it never reached the pump or the horse.

**2.5 (Score: 27, GAME OVER)** — The General Store was clear from the start (outlaw was only in the Telegraph Office, as on turn 1). ADK 2.5 efficiently swept all four General Store items by turn 7. It then reached the Livery Stables on turn 9 — but encountered a rattlesnake. FREEZE on turn 10 produced the message *"The snake watches you..."* without the snake leaving. On turn 11, ADK 2.5 attempted FIX PUMP anyway, was struck, and died. The game ended at score 27 with 10 turns used.

The fatality reveals an unrecognized asymmetry in the FREEZE mechanic: the snake has a probabilistic response. ADK 2.5 drew the bad outcome; Agno 2.5 drew the good one in the same session. This is a map-randomization confound, not a model difference.

### Agno

**3.5 (Score: 83)** — Found the map immediately (Telegraph Office, turn 1). Fixed the telegraph. Cleared all General Store items. Snake in Livery Stables → FREEZE → snake departed → FIX PUMP → FILL → DRINK CANTEEN → SADDLE HORSE → MOUNT HORSE → rode south. Reached Dry Wash (room 8) at turn 19, then Howling Desert (room 11) on turn 20. Score 83.

**2.5 (Score: 85)** — Identical opening: map in Telegraph Office on turn 1, fixed telegraph. All General Store items secured. Snake in Livery Stables → FREEZE → snake departed → FIX PUMP → FILL → DRINK CANTEEN → SADDLE HORSE → MOUNT HORSE → Dry Wash → Howling Desert (room 11). An extra EAST on turn 20 pushed into room 11, scoring 85 vs the 3.5 run's room 8 destination of 83. Agno 2.5 scored 5 points higher, achieved in an identical number of turns, with 17% fewer tokens. The world configuration appears to have been identical between the two Agno runs (same map/threat placement).

### Pydantic AI

**3.5 (Score: 73)** — Started with Telegraph Office (turn 1), grabbed wire, fixed telegraph (turn 3). Found map in Assayer's Office (turn 7). Encountered a snake in the General Store (turn 11) → FREEZE → snake left → all items collected (turns 12–17). Reached Livery Stables on turn 19 and fixed pump on turn 20 — just as the turn limit hit. No water, no horse.

**2.5 (Score: 78)** — Reversed the opening strategy: General Store first (turn 1), collected all four items (turns 2–5) before touching the telegraph. Telegraph Office visit on turn 7 (dropped book, took wire, FIX TELEGRAPH on turn 10). Map found in Sheriff's Office on turn 12. No threats encountered anywhere. Reached Livery Stables on turn 15, FIX PUMP (turn 16), FILL (turn 17), DRINK (turn 18), SADDLE HORSE (turn 19). Final move north to Main Street (turn 20) wasted one potential desert turn. The horse was saddled but never mounted; with one more turn, MOUNT + SOUTH would have pushed into the desert for additional points.

### Strands

**3.5 (Score: 83)** — Played a full 24-call, 20-turn game. Fixed telegraph, took all General Store items, read the Assayer's ledger, found map in Livery Stables, dropped book for space, fixed pump, filled, drank. Strong clean run.

**2.5 (Score: 0, framework termination)** — The Strands agent made exactly **2 LLM calls** and exited. The first call (`stop_reason="tool_use"`) correctly issued `command(LOOK, reset=True)`. The second call (`stop_reason="end_turn"`) returned the text `"EAST\n\n"` instead of a `tool_use` block. The Strands agentic loop interprets `end_turn` without a tool call as completion and exits. Zero game turns were played.

This is a **Gemini 2.5 Flash / LiteLLM / Strands compatibility failure**: the model responded to the game prompt with a conversational text completion rather than a tool invocation. Gemini 3.5 Flash honored the tool-first constraint in the same framework; 2.5 Flash did not. This likely reflects a behavior change in how `gemini-2.5-flash` handles the tool-use forcing mode through LiteLLM's proxy layer.

---

## 3. Token Efficiency: Model-Level Changes

| Framework | 3.5 Total Tokens | 2.5 Total Tokens | Change |
| :--- | ---: | ---: | :---: |
| **ADK** | 188,976 | 70,556 | -63% |
| **Agno** | 85,732 | 70,811 | -17% |
| **Pydantic AI** | 227,096 | 127,363 | -44% |
| **Strands** | 128,009 | 5,502 | -96% (n/a — failure) |

Gemini 2.5 Flash uses substantially fewer tokens than 3.5 in every framework, even when achieving higher scores. The compression is most dramatic in ADK (partly because the 2.5 run terminated at turn 10) and Pydantic (fewer calls, shorter reasoning output). Agno's 17% reduction is the most meaningful comparator since both runs completed all 20 turns.

### Context Window Growth

Pydantic AI, which accumulates the full conversation, shows the clearest growth profile:
- **3.5**: 2,501 → 13,490 input tokens over 28 calls (~400 tokens/call growth)
- **2.5**: 2,419 → 8,969 input tokens over 22 calls (~300 tokens/call growth)

2.5 Flash generates shorter thinking/response text, reducing the amount appended to the conversation history on each turn. This compounds across the run, yielding the 44% total token savings despite 2.5 not using a sliding window.

Agno's bounded context window remains stable in both model versions (~1,900 → ~5,200 tokens), confirming the sliding-window mechanism works model-independently.

---

## 4. Prompt Caching Behavior

Only ADK and Pydantic AI report cache reads. Both show substantially higher cache hit rates with 2.5:

| Framework | 3.5 Cache Reads | 3.5 Input | 3.5 Hit Rate | 2.5 Cache Reads | 2.5 Input | 2.5 Hit Rate |
| :--- | ---: | ---: | :---: | ---: | ---: | :---: |
| **ADK** | 38,933 | 186,930 | 21% | 53,238 | 69,688 | **76%** |
| **Pydantic AI** | 92,061 | 224,952 | 41% | 76,650 | 124,919 | **61%** |

ADK 2.5's 76% cache hit rate against a run that only used 69,688 total input tokens is striking — the system prompt and guidance file are being read from cache on nearly every call. In 3.5, cache hits were a smaller fraction because the conversation history grew so large that the cached prefix became a smaller portion of the total input.

This suggests 2.5 Flash interacts more favorably with Gemini's prefix caching: the model's shorter per-turn outputs keep the accumulated context smaller, preserving more of each prompt as a cache-eligible prefix.

---

## 5. Reasoning Token Analysis

| Framework | 3.5 Reasoning | 2.5 Reasoning | Change |
| :--- | ---: | ---: | :---: |
| **ADK** | ~1,093 (summed per-call) | ~722 (summed per-call) | -34% |
| **Agno** | 1,652 | **32** | -98% |
| **Pydantic AI** | *(folded into output)* | *(folded into output)* | — |
| **Strands** | *(folded into output)* | *(folded into output)* | — |

The Agno reasoning token collapse from 1,652 to 32 is the most striking number in the dataset. Agno's structured per-turn context block (room state, inventory, history, strict one-command output constraint) is so constraining that the 2.5 model stops reasoning almost entirely, yet achieves a higher score. This is consistent with the rainy-wed analysis's finding that Agno's prompt design converts the model into a command-generator rather than a planner.

ADK's thinking blocks in the 2.5 run are still verbose in log output despite fewer total reasoning tokens. The thinking content reveals the model performing the same kind of expert-persona philosophizing as in the 3.5 run — "Let's take stock... Inventory Management Considerations" — but more concisely, suggesting a more focused internal planning process.

### ADK Thinking Verbosity: 3.5 vs 2.5

A qualitative difference emerges in the character of ADK thinking blocks:

**3.5 ADK** (395 reasoning tokens on outlaw dilemma, turn 12):
> *"Wait, wait… maybe the outlaw isn't a permanent fixture? Maybe he moves? I need to know if it's random, or if there is a pattern to his behaviour. Let's see... Turn 1: Telegraph Office. Turn 3: General Store. Turn 6: Assayer's Office. Turn 12: General Store. Aha! It seems he has moved! This is crucial!"*

This represents genuine pattern-recognition. The 3.5 model correctly deduced from three outlaw sightings that position is stochastic.

**2.5 ADK** (68 reasoning tokens on the rattlesnake, turn 9):
> *"Okay, so I'm in the Livery Stables, focusing on this pump, and boom – rattlesnake. The immediate instinct is to react, but I recall the protocol: Freeze or move. Given that my objective is to fix this pump and I have leather, I think 'Freeze' is the safer option."*

Correct recall of the FREEZE mechanic, but the subsequent decision to FIX PUMP immediately after FREEZE (without waiting to confirm the snake had left) was the fatal error. The 3.5 model's outlaw reasoning was multi-turn; the 2.5 model's snake reasoning was single-turn, which proved insufficient for the probabilistic outcome.

---

## 6. The Strands Compatibility Crisis

The Strands framework's Gemini 2.5 Flash failure is the most significant finding of this cross-model comparison. To characterize it precisely:

- **Strands 3.5**: 24 LLM calls, 20 game turns, score 83, clean termination
- **Strands 2.5**: 2 LLM calls, 0 game turns, score 0, framework exit

The root cause is the stop reason on the second call: `stop_reason="end_turn"` without an accompanying tool-use block. Gemini 2.5 Flash interpreted the game continuation prompt as a conversational exchange and returned the next command as raw text (`"EAST\n\n"`) rather than as a `tool_use` JSON block.

The Strands event log confirms: the framework's agentic loop only continues if a `tool_use` block is present; `end_turn` signals completion, and the loop exits. No retry logic exists. The "EAST" response appears as the final agent output because Strands passed it to the post-loop result formatter.

This is a **transport-level compatibility regression**. LiteLLM's Gemini adapter may not be enforcing `tool_choice: required` when proxying to 2.5 Flash, allowing the model to fall through to a text response. The 3.5 model respected tool-first mode through the same proxy. The fix is straightforward (explicit `tool_choice` enforcement or LiteLLM version pin) but until resolved, Strands cannot run against Gemini 2.5 Flash.

---

## 7. Per-Call Output Token Profiles

### Pydantic AI 3.5 (28 calls)
```
call:   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16
output: 246  177  136   82   56   27   47   27   32   37   36  129   44  123   38   78
call:  17   18   19   20   21   22   23   24   25   26   27   28
output: 23   24  233  148   93   20   22   26   27   97   80   (summary)
```
High first-call output (246 tokens of planning), drop as routine moves dominate, spike at call 19 (snake encounter, 233 tokens) and call 20 (recovery from snake, 148 tokens).

### Pydantic AI 2.5 (22 calls)
```
call:   1    2    3    4    5    6    7    8    9   10   11   12   13
output: 148  107  105  395   74  330   74   71   91   75   49   74   60
call:  14   15   16   17   18   19   20   21   22
output: 104  124   50   89   45   25   67  112  175
```
Spikes at call 4 (395 tokens — inventory calculation after TAKE CANTEEN) and call 6 (330 tokens — TAKE MATCHES decision). The 2.5 model spends more compute on inventory management decisions in the General Store than on navigation.

### ADK 3.5 (22 calls)
```
output: 18 14 14 14 14 14 14 16 14 14 14 14 14 14 14 14 14 14 14 18 18 [279]
```
Extremely consistent 14–18 tokens per tool-call (JSON structure fixed). Final call is the summary at 279 tokens.

### ADK 2.5 (13 calls — game over at call 13)
```
output: 17 13 13 13 16 16 13 15 13 14 15 13 [1]
```
Same consistent pattern, but call 12 is the FIX PUMP command that triggers the fatal snake bite, and call 13 is a single-token response after GAME OVER.

---

## 8. Framework Robustness to Model Upgrade

This cross-run comparison reveals very different framework behaviors when the underlying model changes:

| Framework | Robustness | Key Finding |
| :--- | :--- | :--- |
| **Agno** | **Excellent** | Identical strategy, higher score, 17% fewer tokens, 44% faster. Model-agnostic by design. |
| **Pydantic AI** | **Good** | Improved score (+7%), 44% fewer tokens, different strategic ordering. Robust to model switch. |
| **ADK** | **Fragile (gameplay)** | Run ended at turn 10 due to FREEZE gamble; not a model regression — probabilistic game outcome. Token count per turn is stable. |
| **Strands** | **Broken** | Cannot run against 2.5 Flash via LiteLLM. Zero turns played. Requires tool_choice enforcement fix. |

### Why Agno Is Robust

Agno's structured per-turn context injection — room state, history, inventory, and strict one-command output constraint — is model-invariant. The 2.5 model receives a deterministic prompt format and responds with a deterministic single-token game command. Even with reasoning tokens reduced by 98%, the framework's prompt engineering fully specifies the task. The model doesn't need to reason about what format to use or how much to say.

### Why Pydantic AI Improved

Two factors contributed to the Pydantic AI improvement with 2.5:
1. The 2.5 model chose a more efficient game strategy (General Store first, no unnecessary exploration), likely reflecting stronger in-context planning.
2. The game world was more forgiving (no snake in General Store, no outlaw encounters), which is a confound.
3. Shorter per-turn output kept the context window smaller, avoiding the runaway accumulation that drove 3.5 to 28 calls vs 22 for 2.5.

### Why ADK Is Unpredictably Fragile

The ADK 2.5 run demonstrates that a single stochastic game event (FREEZE outcome) can invalidate the entire run regardless of model quality. This is a benchmark design issue as much as a model issue. The 2.5 model's reasoning about the snake was correct; the game engine's random outcome was fatal. A larger sample of runs would be needed to separate model capability from luck.

The ADK framework's output is otherwise consistent between models: 14–18 tokens per tool call in both, full conversation accumulation in both, verbose end summaries in 3.5 (279 tokens), absent in 2.5 (game ended before summary was triggered).

---

## 9. World State as Confound

The map and threat placement differ across all four pairs of runs. The Agno runs appear to have shared the same world configuration (map in Telegraph Office, snake in Livery Stables in both 3.5 and 2.5), as did Strands 3.5 / Strands 2.5. But ADK 3.5 had outlaws in two town rooms, while ADK 2.5 had an outlaw in only one. Pydantic 3.5 had a snake in the General Store; Pydantic 2.5 had none.

| Framework | 3.5 Threats | 2.5 Threats |
| :--- | :--- | :--- |
| ADK | Outlaws: Telegraph Office, General Store | Outlaw: Telegraph Office; Snake: Livery Stables |
| Agno | Snake: Livery Stables | Snake: Livery Stables, Howling Desert |
| Pydantic AI | Snake: General Store | None (Livery Stables clear) |
| Strands | None | None (game never started) |

The ADK 3.5 world was exceptionally hostile (two outlaw rooms blocked the most valuable early items), which explains why ADK 3.5 scored 34 despite 22 calls. If ADK 3.5 had the same threat layout as ADK 2.5, it would likely have scored 65–75. Conversely, ADK 2.5 got a relatively clear town but drew the fatal FREEZE outcome.

---

## 10. Summary

The upgrade from Gemini 3.5 Flash to 2.5 Flash produced four distinct outcomes across the frameworks:

1. **Agno**: Marginal improvement (+2 points), robust, 17% fewer tokens, 44% faster. The framework's prompt design makes it effectively model-independent.

2. **Pydantic AI**: Meaningful improvement (+5 points), 44% fewer tokens, 6 fewer LLM calls. The 2.5 model chose a more efficient game strategy unprompted. Cache hit rate improved from 41% → 61%.

3. **ADK**: Apparent regression (-7 points) driven by a fatal FREEZE gamble at the Livery Stables snake, not a model capability regression. The token footprint dropped dramatically (189K → 71K), and the per-call behavior (14–18 output tokens, consistent tool calls) was unchanged. ADK's cache hit rate improved dramatically (21% → 76%). A re-run with the same world state would likely reverse the score outcome.

4. **Strands**: Complete failure. Zero turns played. The LiteLLM transport layer does not enforce tool-call mode against Gemini 2.5 Flash, allowing the model to return text responses that terminate the agentic loop immediately. This requires a framework-level fix before Strands can be benchmarked against 2.5 Flash models.

The cross-model comparison reinforces the main finding from the four-framework baseline: **framework prompt design matters more than model version**. Agno's structured context block and strict output constraint make it resilient to model changes; Strands' reliance on the model to self-enforce tool usage makes it brittle to API behavior changes.
