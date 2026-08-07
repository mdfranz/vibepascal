# OpenRouter Model Comparison: DeepSeek v4 Flash vs Gemini 3.6 Flash
**Date:** Thursday, August 6, 2026 (23:46 UTC Aug 6 – 00:12 UTC Aug 7)
**Benchmark:** Echoes of Dustwood — Pydantic AI MCP client, 25-turn limit, `guidance_full.txt`, 1s inter-turn delay
**Framework:** `packages/pydantic/pydantic_mcp_client.py` via `./pydantic-mcp-game.sh`
**Observability:** [Logfire](https://logfire-us.pydantic.dev/mdfranz/tomfoolery) (`mdfranz/tomfoolery`), `LOGFIRE_ENABLED=1` — see [`packages/shared/OBSERVABILITY.md`](../packages/shared/OBSERVABILITY.md#logfire-pydantic-ai-only) for how this client is instrumented.

This is the first comparative run using the client's new Logfire integration instead of local
`log_kv` file logs. All figures below came straight from `query_run` against Logfire — no manual
log-scraping — using the client's `pydantic_game_run` span (one per session) and the custom
`game_turn`/`run_summary` log events nested inside it.

---

## 1. All runs this session

Five OpenRouter models were run for 25 turns each; DeepSeek and Gemini were re-run a second time
to check run-to-run consistency.

| # | Model | Outcome | Final score | Turns reached | Requests | Total tokens |
| :-- | :--- | :--- | ---: | ---: | ---: | ---: |
| 1 | `openrouter:deepseek/deepseek-v4-flash-0731` | ✅ Completed | 77 | 24 | 29 | 193,476 |
| 2 | `openrouter:qwen/qwen3.7-flash` | ❌ Crashed turn 1 | — | — | — | — |
| 3 | `openrouter:google/gemini-3.6-flash` | ✅ Completed | 88 | 24 | 28 | 136,908 |
| 4 | `openrouter:qwen/qwen3.7-flash` (after hardening fix) | ✅ Completed | 60 | 24 | 53 | 583,775 |
| 5 | `openrouter:deepseek/deepseek-v4-flash-0731` (rerun) | ✅ Completed | 65 | 24 | 26 | 204,456 |
| 6 | `openrouter:google/gemini-3.6-flash` (rerun) | ✅ Completed | 93 | 24 | 28 | 137,700 |
| 7 | `openrouter:moonshotai/kimi-k3` | ✅ Completed | 93 | 24 | 49 | 462,720 |
| 8 | `openrouter:z-ai/glm-5.2` | ⚠️ Hit 100-request cap | 75 (mid-4th playthrough) | 20 | 100 | 1,292,557 |

**Correction from the first version of this doc**: runs 1-6 were *not* all ended by the in-game
day/night cutoff — that claim was wrongly generalized from the Qwen crash trace, which was a
different situation. Querying each run's actual tool-call results in Logfire (`gen_ai.tool.call.result`)
shows every run ended differently:

| Run | How it actually ended |
| :--- | :--- |
| DeepSeek run 1 & 2 | Model itself sent a plain-text **`QUIT`** response instead of calling another tool — the agent loop ends naturally, no exception, no exit code from either the game engine or the client. |
| Gemini run 1 & 2 | Model self-narrated a wrap-up ("I have completed 25 game turns... Score: 88" / "the session has ended (Game Over at turn 25)") and stopped calling tools — again a natural stop, not a real engine `GAME OVER`. |
| Qwen run (post-fix) | Model self-narrated "🏜️ GAME OVER 🏜️ Final Score: 65" — note the engine's actual last recorded score was **60**; the model's own closing summary doesn't match its telemetry. |
| Kimi-k3 | Actually reached turn 24/score 93 in-engine, then a *trailing* request after that got cut off by the client's `max_tokens=4096` `ModelSettings` limit, raising `UnexpectedModelBehavior` — caught gracefully, `run_summary` still logged. |
| GLM-5.2 | The only run with a **real** in-engine `GAME OVER` (checked directly via `gen_ai.tool.call.result` in Logfire) — three times, in fact — see [Section 5](#5-glm-52-resets-the-game-after-every-game-over). |

None of this was visible before this session's Logfire migration — the old local `log_kv` schema
never captured full tool-call arguments/results, only a truncated/optional payload string. Getting
`gen_ai.tool.call.arguments`/`gen_ai.tool.call.result` on every span for free from
`logfire.instrument_pydantic_ai()` is what made this diagnosis possible.

## 2. DeepSeek vs Gemini — repeat-run comparison

| Model | Run | Score | Requests | Input tok | Output tok | Total tok |
| :--- | :-: | ---: | ---: | ---: | ---: | ---: |
| `deepseek/deepseek-v4-flash-0731` | 1 | 77 | 29 | 190,090 | 3,386 | 193,476 |
| `deepseek/deepseek-v4-flash-0731` | 2 | 65 | 26 | 199,871 | 4,585 | 204,456 |
| `google/gemini-3.6-flash` | 1 | 88 | 28 | 134,390 | 2,518 | 136,908 |
| `google/gemini-3.6-flash` | 2 | 93 | 28 | 135,223 | 2,477 | 137,700 |

**Gemini is consistent**: score 88 → 93, virtually identical request count and token usage between
runs (Δinput ~+800, Δoutput ~-40 — noise-level). Same play style both times.

**DeepSeek is more variable**: score dropped 77 → 65 despite spending *more* tokens (204k vs 193k,
mostly more output tokens: 4,585 vs 3,386) on *fewer* requests (26 vs 29). More output per turn did
not translate into a better outcome the second time.

**Overall**: gemini-3.6-flash is both cheaper (~137k vs ~193-204k total tokens) and scores higher/
more consistently than deepseek-v4-flash-0731 on this game, across both runs.

## 3. Score trajectory (selected turns)

Pulled directly from the `game_turn` log events (`turn`, `score` attributes), deduped by turn:

| Turn | DeepSeek run 1 | DeepSeek run 2 | Gemini run 1 | Gemini run 2 |
| ---: | ---: | ---: | ---: | ---: |
| 0 | 0 | 0 | 0 | 0 |
| 5 | 23 | 15 | 14 | 21 |
| 10 | 36 | 21 | 45 | 29 |
| 15 | 36 | 30 | 55 | 43 |
| 20 | 42 | 65 | 80 | 73 |
| 24 (final) | **77** | **65** | **88** | **93** |

Notable: DeepSeek run 2 was actually *behind* run 1 through turn 15 (30 vs 36) but caught up
sharply by turn 20 (65 vs 42) — the pump/water/canteen sequence appears to have landed earlier
in run 2. Both Gemini runs show the same late-game acceleration pattern (turns 16-17 and 21-24),
consistent with the same portion of the map (canteen → pump → water fill) driving most of the
score in both.

## 4. Gameplay narrative

Reconstructed from the console transcript each run printed (`GAME_CONSOLE=1`), cross-checked
against the `game_turn` events in Logfire.

### DeepSeek run 1 (score 77)

Headed east into the **General Store** on turn 1 but didn't take anything yet, then detoured
north to the **Telegraph Office**, took the copper wire (turn 4), and spliced it to restore the
line (turn 5, +10 → 23) — the single biggest early-game score jump either model found. From there
it wandered west to the **Sheriff's Office** and **Assayer's Office**, picked up the hand-drawn
map (turn 9), and tried a second `take` that failed ("Not here", turn 10 — a wasted turn).

Getting back into the General Store to grab its four items took three attempts: turn 13 a
**DIRTY OUTLAW** was blocking it (retreated); turn 15 *both* the outlaw *and* a **RATTLESNAKE**
were present (retreated again); turn 17 the outlaw was gone but the snake remained, so it issued
`FREEZE` and successfully waited the snake out (turn 18, "The snake loses interest and slithers
into the shadows"). Only after that did it take the canteen (19) and leather (20).

It reached the **Livery Stables** at turn 22, fixed the pump (23, +20 → 67), and filled the
canteen (24, +10 → 77) — but the run ended there. It never saddled or mounted the horse and never
left town for the desert. The three failed store entries (turns 13-17) cost roughly 4-5 turns that
a cleaner run could have spent exploring further.

### Gemini run 1 (score 88)

Took the hand-drawn map already sitting on Main Street (turn 1), then cleared the **General
Store** in one pass with no outlaw or snake present this run: dropped a spare book to make room
(turn 3), then took the canteen, leather, saddle, and matches back-to-back (turns 4-7) — all four
items in four consecutive turns, versus DeepSeek's three separate visits.

Reached the **Livery Stables** by turn 9 (13 turns faster than DeepSeek run 1) and fixed the pump
immediately (turn 10, +20 → 45), filled the canteen (11, +10 → 55), drank (12), refilled (13),
saddled (14), and mounted the horse (15) — a tight, uninterrupted utility sequence. It then rode
south through **The Desert Edge → Dry Wash → Howling Desert → Butte** (turns 16-20, +5 per room),
passing a rattlesnake in the Howling Desert (turn 18) without incident — it just kept moving,
never needing to `FREEZE`. It reached **Hidden Stream** — a room neither DeepSeek run visited — on
turn 21 (+5 → 85), found a hidden brass key under a mossy rock, took it (turn 23, +3 → 88), and
watered the horse (turn 24) before the run ended.

Skipped the Telegraph Office entirely this run — the +10 splice bonus was left on the table in
favor of reaching Hidden Stream instead.

### DeepSeek run 2 (score 65)

Went straight for the Telegraph Office on turn 1, but this time a **DIRTY OUTLAW** was already
there and it retreated to Main Street without touching the copper wire — the telegraph was never
fixed this run, giving up the same +10 it earned in run 1. The hand-drawn map turned up in the
**Livery Stables** instead of the Assayer's Office this run (item placement varies by map seed,
consistent with prior comparative-analysis findings).

Back in the General Store (turn 8, no threats this time), it dropped the spare book, then fired
**four `take` tool calls in a single model turn** — canteen, matches, leather, and saddle all
grabbed in one burst (turns 10-13 logged almost simultaneously, +18 total) rather than one
item per turn the way both Gemini runs did. This parallel-tool-calling behavior wasn't seen from
Gemini in either run.

Fixed the pump (turn 16, +20 → 50) and filled the canteen (17, +10 → 60) about 6 turns faster
than run 1 (which didn't reach the stables until turn 22) — but then saddled the horse (19) without
mounting it, walked south into **The Desert Edge** on foot (turn 20, +5 → 65), and was blocked
trying to go further: *"The desert is too dangerous on foot. You must be riding a saddled horse."*
(turn 21, wasted turn). It backtracked to the Livery Stables (22), finally mounted (23), and rode
back out to the Desert Edge (24) — ending the run back where it had already been, at score 65,
never reaching Dry Wash or beyond.

### Gemini run 2 (score 93)

Followed a similar early path to run 1 (map on turn 1) but this time detoured through the
**Telegraph Office** — took the copper wire (turn 3) and spliced it (turn 4, +10 → 21), the bonus
run 1 had skipped. General Store had an outlaw present on the first visit (turn 6, retreated) but
was clear on the second (turn 8) — cleared all four items by turn 13, same as run 1's pacing.

Reached the Livery Stables at turn 15, fixed the pump (16, +20 → 63), filled the canteen (17,
+10 → 73), drank, saddled, and mounted (18-20) — then rode south through **The Desert Edge → Dry
Wash → Howling Desert** (turns 21-24, +5 each), ending at score 93 one room short of where run 1
reached Hidden Stream. Trading the Hidden Stream/brass-key detour for the guaranteed Telegraph
Office bonus produced a *higher* final score (93 vs 88) despite covering less new ground overall —
the telegraph's flat +10 for two turns of detour beat the multi-turn trip to Hidden Stream's +8
(rock +3, distance itself).

### Kimi-k3 (score 93, one death + reset)

Started the same as most runs: took the map (turn 1), cleared the General Store across turns 2-7
(canteen, leather, saddle, dropped-book, matches — one item per turn, no threats), reached the
Livery Stables at turn 9, fixed the pump (10, +20 → 45), filled the canteen (11, +10 → 55), drank
(12), saddled (13), and mounted (14) — pacing almost identical to Gemini's runs.

Rode south through **The Desert Edge → Dry Wash → Howling Desert** (turns 15-18, passing a
DIRTY OUTLAW at turn 15 and another at turn 18 without incident), reached **Butte** at turn 19
(score 80) — then tried to `CLIMB` the "narrow, treacherous path" up the rock formation. A
rattlesnake struck: *"As you reach out, the rattlesnake strikes! You feel a sharp pain in your
hand. The venom works quickly. GAME OVER."* This was a genuine in-engine death (confirmed via the
tool-call result in Logfire), unlike Gemini/DeepSeek's self-narrated endings.

Kimi's response to this was the same pattern GLM would later show far more often: it called
`command(command="LOOK", reset=true)` and started over from Main Street. The second playthrough
detoured to the Telegraph Office first (turn 1) but an outlaw was there, so it retreated to Main
Street without touching the wire (turn 2, score unchanged at 5 — the telegraph was never fixed
this run either). It then cleared the General Store (turns 3-9), reached the Livery Stables and
fixed the pump (turns 11-12), filled the canteen and drank (13-14), saddled and mounted (15-16),
then rode south through the same four desert rooms (turns 17-20, this time passing the Desert Edge
outlaw and Howling Desert outlaw without incident), reached **Butte** again (turn 21) — and this
time just drank water instead of climbing (turn 22, avoiding the same mistake), continued to
**Hidden Stream** (turn 23, +5 → 90), took the brass key under the mossy rock (turn 24, +3 → 93),
and stopped there — a trailing 50th request then exceeded the client's `max_tokens=4096` limit,
ending the run via `UnexpectedModelBehavior` after the score was already locked in.

Net effect: one death cost roughly 19 turns' worth of replayed ground and the telegraph bonus, but
the second attempt avoided the fatal `CLIMB` and pushed one room further than either Gemini run —
reaching Hidden Stream despite skipping the telegraph, landing the highest score of any run this
session (93, tied with Gemini run 2).

### GLM-5.2 (partial — cut off mid-4th playthrough at score 75)

GLM-5.2's full run is covered in detail in [Section 5](#5-glm-52-resets-the-game-after-every-game-over)
since its defining behavior — resetting after every `GAME OVER` — is itself the finding, not
incidental to it. In short: it played four full playthroughs inside one 100-request budget
(reaching score 50, 77, 77, and 75-so-far respectively) rather than one continuous 25-turn game,
because it treated the engine's post-death "Fix the errors and try again." text as an instruction
to restart.

### Cross-cutting patterns

- **Store-clearing efficiency**: Gemini cleared the General Store's four items in one visit both
  times (4 consecutive `take` turns). DeepSeek needed 2-3 separate visits across both runs,
  interrupted by an outlaw and/or rattlesnake each time — this is the single biggest driver of
  DeepSeek reaching the Livery Stables 6-13 turns later than Gemini in both runs.
- **Parallel tool calls**: DeepSeek run 2 issued four `take` calls in one model turn; Gemini never
  batched tool calls in either run, always one action per turn.
- **Horse/desert mechanic**: only DeepSeek (run 2) got tripped up by the "must be riding a saddled
  horse" desert-entry rule, costing a wasted turn and a backtrack. Both Gemini runs saddled *and*
  mounted before attempting to leave town.
- **Telegraph vs. Hidden Stream trade-off**: across all four runs, whichever run skipped the
  Telegraph Office reached further into the desert (Gemini run 1 → Hidden Stream; DeepSeek run 1
  got the telegraph but never left town at all, so the trade-off wasn't symmetric for DeepSeek).
- **`CLIMB` at Butte is a death trap**: both Kimi-k3 and GLM-5.2 tried `CLIMB` at Butte and were
  killed by a rattlesnake bite ("The venom works quickly. GAME OVER."). Neither DeepSeek nor Gemini
  attempted it in any run (DeepSeek never reached Butte; Gemini reached it in both runs but moved
  straight through to Hidden Stream instead of climbing).
- **How a model reacts to `GAME OVER` varies enormously**: DeepSeek quits with plain text
  (`QUIT`), Gemini self-narrates a wrap-up and stops, Kimi-k3 reset *once* after an actual death and
  then played a clean second game, and GLM-5.2 reset after *every* `GAME OVER` — including the two
  natural day/night endings, not just the death — turning one 25-turn game into four. See
  [Section 5](#5-glm-52-resets-the-game-after-every-game-over).

## 5. GLM-5.2 resets the game after every `GAME OVER`

GLM-5.2 was the run with by far the highest resource use of this whole session — 100 requests
(the client's hard `request_limit = max_turns * 4` cap) and 1,292,557 tokens, roughly 7-9x what
DeepSeek/Gemini used, yet it only ever reported reaching turn 20. Digging into why (via
`gen_ai.tool.call.arguments`/`gen_ai.tool.call.result` on each `execute_tool` span in Logfire —
not visible in the old local-log format) showed it wasn't retries or malformed arguments like
Qwen's bug. It was **replaying the whole game from scratch, three times**, inside a single trace:

| Turn 0 (reset) at | Score reached before this reset | Trigger |
| :--- | ---: | :--- |
| 00:30:45 (session start) | — | — |
| 00:32:10 | 50 (turn 24) | Natural day/night `GAME OVER` |
| 00:33:07 | 77 (turn 18) | Killed by rattlesnake — tried `CLIMB` at Butte |
| 00:34:27 | 77 (turn 24) | Natural day/night `GAME OVER` |
| *(cut off by request cap)* | 75 (turn 20, in progress) | — |

Every reset followed the exact same shape. The engine's `GAME OVER` responses all end with a
trailing line the other five models simply ignored: **`Fix the errors and try again.`** GLM-5.2
took that literally. Two verbatim examples from the trace:

```json
// Natural day/night ending, turn 25:
{"output": "You swing yourself into the saddle. You are now riding.\n\n⏳ You have taken too long. The sun dips below the horizon.\nGAME OVER.\n", "state": {..., "is_playing": false, "score": 77, "turns": 25}}\n\nFix the errors and try again.

// Its next tool call:
command(command="LOOK", reset=true)
```

```json
// Death at Butte, turn 18:
{"output": "🐍 As you reach out, the rattlesnake strikes! ... 💀 The venom works quickly. GAME OVER.\n", "state": {..., "is_playing": false, "score": 77, "turns": 18}}\n\nFix the errors and try again.

// Its next tool call (after a few more no-op "command" attempts against the dead session):
tools/call reset_game
```

This is a genuine finding about the game's MCP tool responses, not a bug in the pydantic client:
`GAME OVER` and a real invalid-argument error were returning the same trailing hint text, and at
least one model (GLM-5.2) couldn't tell the two apart. Kimi-k3 hit the same text once (after its
own death) and also reset — but only once, then played a clean second game to a natural stop.

**Fixed** in `src/golang/mcp_server.go`: every tool handler (`look`, `go`, `take`, `drop`,
`inventory`, `drink`, `water_horse`, `light`, `score`, `reset_game`, and both branches of
`command`) was setting `mcp.CallToolResult{IsError: true}` whenever `!summary.IsPlaying` — i.e.
flagging *any* game end (win, death, day/night timeout) as an MCP tool-call error, indistinguishable
from a genuine bad-argument error. That's what the client library (fastmcp/pydantic-ai) turns into
the "Fix the errors and try again." hint. Removed the `IsError: true` branch from all of them —
genuine validation errors (`Unknown direction: ...`, invalid item names) are separate code paths
and are untouched. Verified with a raw JSON-RPC call against `--turns 1`: the day/night `GAME OVER`
response no longer carries `isError` at all. This affects every framework client, not just pydantic,
since they all talk to the same Go MCP server.

Net result: GLM-5.2's 75-at-turn-20 headline number understates what it's actually capable of in
this game — its *best single playthrough* (77, reached twice) is competitive with DeepSeek's best
run (77) and only slightly behind Gemini/Kimi's 88-93 range, but the run as executed never got to
finish a playthrough with all 25 turns available to it, because it kept spending its budget on
restarts instead.

## 6. Kimi-k3 `max_tokens` fix (found via this session)

Kimi-k3's first run (Section 4) ended not via `GAME OVER` but via `UnexpectedModelBehavior: Model
token limit (4096) exceeded before any response was generated` — on a request made immediately
after taking the brass key at turn 24 (score 93), i.e. a genuine attempt at a turn-25 action, not
a wrap-up. `pydantic_mcp_client.py` hardcoded `max_tokens=4096` uniformly for every model; Kimi
apparently generates enough reasoning/narration before committing to a tool call that it can
exceed that cap before producing anything usable. The client's existing exception handling caught
this gracefully (no crash, `run_summary` still logged), but the run stopped one action earlier
than it might have.

**Fix**: raised `max_tokens` to `8192` in `pydantic_mcp_client.py`'s `ModelSettings`.

**Verified** by re-running Kimi-k3 for 25 turns: no truncation this time, natural day/night
`GAME OVER` at turn 25 (and — confirming the Section 5 MCP-server fix works end-to-end — it did
*not* reset afterward), finishing with the highest score of this entire comparison:

| | Before fix | After fix |
| :--- | ---: | ---: |
| Score | 93 (cut short by truncation) | **98** (full natural run) |
| Turns | 24 | **25** |
| Requests | 49 | 28 |
| Total tokens | 462,720 | 164,014 |

The token/request drop is mostly incidental run-to-run variance (no reset this time, versus one
death+reset in the earlier run) rather than solely attributable to the `max_tokens` change, but the
higher score and clean natural ending are a direct result of both fixes together.

## 7. Qwen tool-call hardening (found via this session)

`qwen/qwen3.7-flash`'s first run crashed on turn 1: it called the `command` MCP tool with
`seed: "None"` (a string) where the tool's JSON schema requires `null` or an integer. Pydantic
AI's `MCPToolset` only auto-retries `fastmcp.exceptions.ToolError`, not the
`mcp.shared.exceptions.McpError` this raised, so the error propagated uncaught and killed the
whole script.

Logfire's Issues/trace view made the failure trivial to pin down — the same `McpError` and message
was visible nested at every span level (`pydantic_game_run` → `invoke_agent agent` →
`execute_tool command` → `tools/call command`).

**Fix** (`packages/pydantic/pydantic_mcp_client.py`, see
[`OBSERVABILITY.md`](../packages/shared/OBSERVABILITY.md#error-hardening-found-via-logfire) for
detail):
1. A `process_tool_call` hook on `MCPToolset` catches `McpError` and re-raises it as `ModelRetry`,
   the same treatment Pydantic AI already gives `ToolError` — lets the model see the schema error
   and retry with corrected args.
2. A broad `except Exception` around the game loop as a second line of defense, so any other
   unexpected error ends that run gracefully (with `run_summary` and a `logfire.exception(...)`
   still recorded) instead of crashing the whole script.

After the fix, `qwen/qwen3.7-flash` completed a full 25-turn run (score 60) — though its far higher
request/token count relative to deepseek and gemini (53 requests, 583,775 tokens vs ~26-29
requests / ~137-204k tokens for the others) suggests it was still generating malformed or
retried tool calls throughout the run, just recovering from them instead of dying on the first one.

## 8. Trace IDs (for drill-down in Logfire)

| Run | Trace ID |
| :--- | :--- |
| DeepSeek run 1 | `019fd97ff6a299875a781a7cf2cb9ed4` |
| Qwen crash | `019fd981a9aa27bdb6436e513f8b8d36` |
| Gemini run 1 | `019fd982a48a945ca12a1e560f511fc3` |
| Qwen run 2 (post-fix) | `019fd987809c3cd3d2e28540a5cdfe91` |
| DeepSeek run 2 | `019fd98d0af3027043a984f623f8571b` |
| Gemini run 2 | `019fd98f5aa34f0065f5f14c24f32da4` |
| Kimi-k3 | `019fd999c2aab1dda468670f0a90e1b8` |
| GLM-5.2 | `019fd9a103d6f0d3563b5f9144256168` |
| Kimi-k3 (max_tokens=8192 verification, score 98) | `019fd9c1e5c55fb4c01de271f68b1da3` |

Query pattern used throughout (adjust `trace_id`/time window as needed):

```sql
select trace_id, span_name, start_timestamp,
       attributes->>'turn' as turn, attributes->>'score' as score
from records
where service_name = 'pydantic-mcp-client'
order by start_timestamp desc
limit 50
```

Query used to pull actual tool-call arguments/results for diagnosing how a run really ended
(the `gen_ai.tool.call.*` attributes come from `logfire.instrument_pydantic_ai()` automatically):

```sql
select start_timestamp,
       attributes->'gen_ai.tool.call.arguments' as args,
       attributes->>'gen_ai.tool.call.result' as result
from records
where trace_id = '<trace_id>' and span_name = 'execute_tool command'
order by start_timestamp
```

## Related Documentation

- [`packages/shared/OBSERVABILITY.md`](../packages/shared/OBSERVABILITY.md) — full instrumentation reference
- [`gemini35-vs-25-flash-analysis.md`](../gemini35-vs-25-flash-analysis.md) — prior cross-framework/model comparison methodology (pre-Logfire, log-file based)
- [`rainy-wed-4-frameworks.md`](../rainy-wed-4-frameworks.md) — original 4-framework comparative analysis
