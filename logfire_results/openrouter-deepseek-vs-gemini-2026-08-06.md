# OpenRouter Dustwood MCP Evaluation: Fixes, Findings, and Validity Audit

**Test window:** August 6–7, 2026

**Benchmark:** Echoes of Dustwood via `packages/pydantic/pydantic_mcp_client.py`

**Models:** DeepSeek v4 Flash, Gemini 3.6 Flash, Qwen 3.7 Flash/Plus, Kimi K3, and GLM-5.2

**Observability:** [Logfire](https://logfire-us.pydantic.dev/mdfranz/tomfoolery), project
`mdfranz/tomfoolery`

This document began as a DeepSeek-versus-Gemini score comparison. The larger result of the test
sequence was more important: Logfire exposed several MCP server, client, prompt, and evaluation
contract defects that made some early scores incomplete or non-comparable. This rewrite treats the
runs as an **evaluation-system audit first** and a model comparison second.

All scores, turns, tool calls, and termination causes below use engine state from Logfire
`game_turn` events or `gen_ai.tool.call.result`, not the model's closing prose. Model-authored final
scores were occasionally wrong.

---

## 1. Executive summary

The test sequence produced five concrete improvements:

1. **Full Logfire tracing replaced lossy local telemetry.** We can now distinguish an engine
   `GAME OVER`, a model choosing to stop, a schema retry, a token cutoff, and a request-limit stop.
2. **Normal game endings are no longer MCP errors.** The server had marked any
   `is_playing=false` result as `isError=true`, which caused Pydantic AI to append “Fix the errors
   and try again.” GLM interpreted that as an instruction to restart repeatedly.
3. **Qwen's malformed tool arguments recover instead of crashing the client.** `McpError` is
   converted to `ModelRetry`, and unexpected failures still produce a `run_summary`.
4. **The output-token ceiling increased from 4096 to 8192.** This removed a demonstrated Kimi
   truncation at turn 24.
5. **Runs now default to one life.** `--allow-restart` is off by default, and a fresh server is
   required for every independent run.

A later minimal-guidance test found and fixed a sixth issue: “Output only one command” could make
Qwen emit plain-text `LOOK`, which Pydantic AI treated as a final answer. The guidance now requires
every game action to be an MCP tool call.

These fixes do **not** make every recorded score current-comparable. In particular:

- Kimi's original score 93, GLM's original score 75, and Qwen's 30-turn score 65 combine multiple
  lives or an artificial cutoff and must not be used as single-life leaderboard results.
- The original 25-turn DeepSeek/Gemini runs remain useful as a historical same-cohort comparison,
  but most stopped via model-authored text at engine turn 24 rather than a real turn-25 ending.
- The first minimal-guidance cohort is superseded by a prompt change, and **both minimal-guidance
  versions omit `MOUNT` from the allowed verb list**. Those runs are diagnostic, not a valid
  end-to-end gameplay leaderboard.
- Seeds were not fixed. Item and threat placement varied materially, so a one-run score difference
  cannot be attributed solely to model ability.

## 2. Benchmark generations and current contract

The runs span several different contracts. They should not be pooled without qualification.

| Generation | Main properties | Use |
| :-- | :-- | :-- |
| A: initial 25-turn | Full guidance; restarts implicitly allowed; 4096 output-token cap; pre-fix game endings marked as MCP errors | Historical/diagnostic |
| B: hardened 25/30-turn | Correct game-ending MCP semantics; Qwen schema retries; 8192 output-token cap; restarts still implicitly allowed | Usable if no restart occurred |
| C: one-life 30-turn | Fresh server per model; `--turns 30`; no `--allow-restart`; full guidance | Current full-guidance contract |
| D1: minimal v1 | One life; 30 turns; original ambiguous tool-use wording | Diagnostic and superseded |
| D2: minimal v2 | One life; 30 turns; explicit MCP-only actions | Current tool-use wording, but gameplay contract still incomplete |

The intended full-guidance contract is now:

- `./pydantic-mcp-game.sh openrouter:<model> 30 1 full`
- fresh `bin/dustwood-go` process per run
- server flags `--mcp-http --mcp-addr 127.0.0.1:8765 --mcp-json-response --turns 30`
- no `--allow-restart`
- `max_tokens=8192`
- Logfire enabled
- engine state, not model narration, is authoritative

Even this contract still needs seeded multi-run evaluation and serialized state-changing tools for
a strong model ranking; see Section 8.

## 3. Full-guidance results and validity

### Initial 25-turn cohort

| Model/run | Engine score / turn | Requests | Total tokens | Status |
| :-- | --: | --: | --: | :-- |
| DeepSeek run 1 | 77 / 24 | 29 | 193,476 | Historical, conditionally usable |
| Gemini run 1 | 88 / 24 | 28 | 136,908 | Historical, conditionally usable |
| Qwen initial | — | — | — | **Invalid: schema crash** |
| Qwen after retry hardening | 60 / 24 | 53 | 583,775 | Historical; engine score is 60, not model-reported 65 |
| DeepSeek run 2 | 65 / 24 | 26 | 204,456 | Historical, conditionally usable |
| Gemini run 2 | 93 / 24 | 28 | 137,700 | Historical, conditionally usable |
| Kimi original | 93 / 24 | 49 | 462,720 | **Invalid as one-life result: death/reset plus token cutoff** |
| GLM original | 75 / 20 of fourth attempt | 100 | 1,292,557 | **Invalid as one-life result: four attempts plus request cap** |
| Kimi verification | 98 / 25 | 28 | 164,014 | Post-fix verification; no reset occurred |

The repeated DeepSeek/Gemini subset still supports a narrow historical observation: Gemini scored
88/93 with about 137k tokens, versus DeepSeek's 77/65 with about 193–204k. However, the conclusion
must stay narrow. These were 25-turn full-guidance runs with random placements, and the models
usually stopped themselves at turn 24 instead of reaching an engine `GAME OVER`.

### 30-turn full-guidance cohort

| Model | Engine outcome | Requests | Total tokens | Validity |
| :-- | :-- | --: | --: | :-- |
| DeepSeek v4 Flash | 106, natural `GAME OVER` at turn 30 | 28 | 199,193 | Current-compatible; no reset occurred |
| Gemini 3.6 Flash | 88, natural `GAME OVER` at turn 30 | 35 | 190,441 | Current-compatible; no reset occurred |
| Qwen 3.7 Flash | Died at 27/turn 10, reset, then reached 65/turn 30 | 46 | 399,968 | **Aggregate 65 invalid under one-life contract; first-life 27 is diagnostic** |
| Kimi K3 | 108, killed by snake after `CLIMB` at turn 29 | 35 | 265,843 | Current one-life run |
| GLM-5.2 | 98, killed by outlaw after `CLIMB` at turn 24 | 28 | 144,498 | Current one-life run |

DeepSeek and Gemini were recorded just before restarts became opt-in, but neither reset. Their
observed playthroughs are therefore compatible with the current one-life rule. Kimi and GLM were
run with a fresh server and the current default explicitly enforced.

The 106/88/108/98 scores are useful examples, not a reliable ranking. Kimi found its map in the
Livery Stables and faced a snake there; GLM found map and wire together in Telegraph Office. The
random environment affected route length and threat exposure by several turns.

## 4. Minimal-guidance results: diagnostic only

### Minimal v1 — before the MCP-only wording fix

| Model | Engine outcome | Requests | Total tokens | Main diagnostic finding |
| :-- | :-- | --: | --: | :-- |
| Kimi K3 | 53, outlaw death at turn 16 | 27 | 130,064 | Used `FREEZE` against an outlaw |
| GLM-5.2 | 16, snake death at turn 6 | 11 | 26,056 | Froze once, then acted while snake remained |
| DeepSeek v4 Flash | 70, natural end at turn 30 | 29 | 183,664 | Parallel inventory overflow; saddled but never mounted |
| Gemini 3.6 Flash | 35, outlaw death at turn 17 | 23 | 79,731 | Invalid moves, dropped needed leather, froze against outlaw |
| Qwen 3.7 Flash | 0, stopped at turn 0 | 2 | 3,949 | Returned plain-text `LOOK` instead of a tool call |

The Qwen trace proved the wording defect precisely. Its bootstrap
`command(command="LOOK", reset=true)` succeeded, and the game remained active. The next model
response had `finish_reason="stop"` and a text body of `LOOK`. Pydantic AI correctly treated that
as a final answer; no MCP or provider error occurred.

Minimal v1 is superseded because the prompt changed after this finding. The other four runs are
still informative about behavior under sparse instructions, but they are no longer strict matches
for the checked-in guidance.

### Minimal v2 — explicit MCP tool calls

| Model | Engine outcome | Requests | Total tokens | What it verifies |
| :-- | :-- | --: | --: | :-- |
| Qwen 3.7 Plus | 43, outlaw death at turn 23 | 31 | 173,699 | Continued using tools; one-life restart rejection worked |
| Qwen 3.7 Flash | 8, outlaw death at turn 2 | 9 | 26,985 | Plain-text stop fixed; malformed-seed retries still recover |

The revised Qwen Flash run is particularly useful as an integration test. It sent
`seed="None"` twice; both invalid calls became `ToolRetryError`, the third call omitted `seed` and
succeeded, and the model continued using MCP tools. It later chose `FREEZE` in front of an outlaw,
died, attempted a reset, and received the expected one-life rejection.

These v2 runs validate tool adherence and client/server hardening, not general gameplay quality.
The current `guidance_minimal.txt` still has two structural problems:

1. It says threats may be handled by `FREEZE` or moving without distinguishing snakes from armed
   outlaws. Kimi, Gemini, Qwen Plus, and revised Qwen Flash all exposed this ambiguity.
2. It says a saddled horse is needed for the desert but omits `MOUNT`, `DISMOUNT`, and `RIDE` from
   the allowed verb list. A compliant model cannot discover the complete desert-entry sequence.

Until those are resolved and all models are rerun on minimal v3, no minimal-guidance score should
be used in a model leaderboard.

## 5. Improvements found through Logfire

### 5.1 Full-fidelity capture changed the conclusions

Commit `c2ba315` added `logfire.instrument_pydantic_ai()` and a root `pydantic_game_run` span. The
client also emits `game_turn` and `run_summary` records inside the same trace.

This made the following distinctions observable:

- DeepSeek and Gemini's early 25-turn runs were model-authored stops, not engine endings.
- Qwen's prose said score 65 when engine telemetry said 60.
- Kimi's first run ended at the model output cap after a prior death/reset.
- GLM's 100 requests were repeated playthroughs, not ordinary retries.
- Tool-call thoughts showed both Kimi and GLM knowingly choosing `CLIMB` under threat.
- Qwen's plain-text `LOOK` was a normal `finish_reason="stop"`, not a transport failure.

The evaluation rule is now: **tool result/game state wins over model narration**.

### 5.2 MCP game endings no longer masquerade as errors

Before `c2ba315`, every MCP handler returned `IsError: true` whenever the game state had
`is_playing=false`. FastMCP/Pydantic AI appended “Fix the errors and try again.” to deaths and
normal day/night endings. GLM obeyed that false recovery hint and replayed four times in one trace;
Kimi also restarted after a death.

The server now reserves MCP errors for real validation failures. A normal win, death, or timeout
returns ordinary structured game state with `is_playing=false`.

**Invalidated results:** original GLM 75 and original Kimi 93 as single-run outcomes. Their traces
remain valuable regression evidence for the server bug.

### 5.3 Restarts are explicit and off by default

Commit `b61f88b` added `--allow-restart`. Without it, `reset_game` and
`command(reset=true)` are rejected after the game ends. The initial bootstrap reset still works.

Because restart permission is server-process state, every independent benchmark invocation needs a
fresh server. Reusing a dead server would also reject the next model's bootstrap and invalidate the
run.

**Invalidated result:** Qwen's full-guidance 30-turn score 65, because it came from a second life.
Its first-life score 27 at turn 10 remains a valid diagnostic point.

The one-life behavior was exercised repeatedly: Kimi, GLM, Gemini, Qwen Plus, and Qwen Flash all
attempted restarts after later deaths and were rejected.

### 5.4 Qwen schema failures now become model retries

Qwen repeatedly sends `seed="None"` even though the schema allows only `null` or an integer.
Originally, the resulting `mcp.shared.exceptions.McpError` escaped and crashed the script.

The client's `process_tool_call` hook converts `McpError` to `ModelRetry`, matching Pydantic AI's
handling for retryable tool errors. A broad outer exception handler guarantees that unexpected
failures still emit `run_summary` and exception telemetry.

This fix was exercised in both the full-guidance 30-turn run and the minimal-v2 verification. In
the latter, Qwen failed with the same string seed twice and then corrected itself. Retry requests
and tokens remain part of the model's real system cost; the hardening prevents a crash but does not
hide inefficiency.

**Invalidated result:** only the original turn-1 crash as a gameplay measurement. Later recovered
runs remain usable subject to their other contract limitations.

### 5.5 The output-token cap no longer truncates Kimi at 4096

Kimi's first trace reached turn 24/score 93, then exceeded `max_tokens=4096` before producing the
next usable response. Commit `5c3e691` raised the cap to 8192. A verification run completed all 25
turns at score 98 with 28 requests and 164,014 tokens.

The before/after request and token drop is not attributable solely to the cap—the first run also
contained a death and reset—but the original 93 is unquestionably incomplete.

### 5.6 Minimal guidance now distinguishes commands from tool calls

Commit `2265154` changed `guidance_minimal.txt` so a command such as `LOOK` must be executed through
an MCP tool and must never be returned as plain assistant text. It also says not to issue another
bootstrap `LOOK` and not to finalize while the game remains active.

Qwen Flash v1 stopped at turn 0; Qwen Flash v2 continued through schema retries and gameplay. This
verifies the targeted fix. It does not repair the separate threat-policy and missing-`MOUNT`
problems described in Section 4.

## 6. Reasoning findings

Logfire records provider-exposed `thinking` parts in `pydantic_ai.all_messages` and
`gen_ai.output.messages`, associated with the next tool call. These records explain failures that
scores alone cannot.

### Full guidance

- **Kimi** tracked inventory and thirst carefully, successfully inferred that two freezes would
  clear a stables snake, and explicitly recognized that freezing at Butte was safer. It chose
  `CLIMB` anyway to save two turns, classifying it as “movement-like,” and died.
- **GLM** was much terser and faster, applied the threat policy correctly throughout the desert,
  then made the same lexical exception at Butte: `CLIMB` was “not taking or fixing.” It died to an
  outlaw. Its later suggestion to obtain the revolver before entering the desert was causally
  impossible because the required key was beyond the Butte.

The shared failure was a prompt-policy gap, not missing state: both models saw the threat and named
the safe alternative.

### Minimal guidance

- Kimi formed richer hypotheses and tracked more state than GLM, but wandered, inferred a gun box
  in the wrong room, delayed water, and applied `FREEZE` to an outlaw.
- GLM executed rapidly but failed to verify the result of its first freeze; the tool said the snake
  was still watching, and it immediately tried to take the canteen.
- DeepSeek's parallel item calls overflowed inventory and produced out-of-order turn events, then
  it saddled without mounting.
- Gemini spent turns on invalid exits, dropped leather needed for the pump, and froze against an
  outlaw.
- Qwen Plus used tools consistently but dropped leather for wire, saddled without mounting, and
  repeatedly tried to reset after death.

These are useful behavioral observations, but the prompt defects and random seeds prevent a clean
reasoning leaderboard.

## 7. Result validity matrix

| Result or cohort | Classification | Reason |
| :-- | :-- | :-- |
| DeepSeek/Gemini repeated 25-turn runs | **Historical, conditionally usable** | Same cohort and no reset, but self-stopped at turn 24 and used random seeds |
| Qwen initial crash | **Invalid gameplay result** | Client did not yet recover `McpError` |
| Qwen post-hardening 25-turn score 60 | **Historical, conditionally usable** | Recovered but expensive; engine score corrects model prose |
| Kimi original score 93 | **Invalid/incomplete** | Death/reset plus 4096-token cutoff |
| GLM original score 75 | **Invalid as one run** | Four playthroughs and request-cap stop |
| Kimi verification score 98 | **Post-fix verification** | Full 25 turns, no reset |
| DeepSeek 106 / Gemini 88 at 30 turns | **Current-compatible examples** | No reset occurred; random seeds and one sample remain |
| Qwen 30-turn score 65 | **Invalid under one-life contract** | Second playthrough after death; first-life score was 27 |
| Kimi 108 / GLM 98 at 30 turns | **Current full-guidance examples** | Fresh server, one life, current fixes |
| Minimal v1 cohort | **Superseded diagnostic** | Guidance wording changed afterward |
| Minimal v2 Qwen runs | **Tooling verification only** | Tool-use fix works, but minimal prompt still lacks `MOUNT` and conflates threats |

## 8. Remaining benchmark risks and next rerun contract

The infrastructure is substantially more trustworthy, but a defensible model comparison still
needs these changes:

1. **Version the guidance in telemetry.** Record a content hash or prompt version on
   `pydantic_game_run`; `level="minimal"` alone cannot distinguish v1 from v2.
2. **Fix minimal guidance before rerunning.** Add `MOUNT`/`DISMOUNT`/`RIDE` and decide whether
   snake-versus-outlaw handling is knowledge the benchmark supplies or knowledge the model must
   discover.
3. **Use explicit seeds and multiple seeds per model.** A reasonable baseline is the same 5–10
   seeds for every model, reported as median/range rather than one headline score.
4. **Serialize state-changing tool calls.** DeepSeek issued multiple `take` calls concurrently
   against one mutable game. Mark mutating tools sequential or enforce serialization server-side.
5. **Enforce active-game continuation in the client.** Prompt wording reduced plain-text stops,
   but a model can still return text while `is_playing=true`. The client should either classify
   that as an explicit early stop or retry under a documented policy.
6. **Record benchmark generation directly.** Server flags, restart policy, guidance hash, binary
   commit, and client commit should be trace attributes rather than prose reconstructed later.

Until those changes land, report runs in cohorts and preserve invalidated traces as regression
tests. Do not collapse all scores in this file into a single ranking.

## 9. Trace index

### Original and full-guidance traces

| Run | Trace ID |
| :-- | :-- |
| DeepSeek 25-turn run 1 | `019fd97ff6a299875a781a7cf2cb9ed4` |
| Qwen initial crash | `019fd981a9aa27bdb6436e513f8b8d36` |
| Gemini 25-turn run 1 | `019fd982a48a945ca12a1e560f511fc3` |
| Qwen 25-turn post-hardening | `019fd987809c3cd3d2e28540a5cdfe91` |
| DeepSeek 25-turn run 2 | `019fd98d0af3027043a984f623f8571b` |
| Gemini 25-turn run 2 | `019fd98f5aa34f0065f5f14c24f32da4` |
| Kimi original | `019fd999c2aab1dda468670f0a90e1b8` |
| GLM original | `019fd9a103d6f0d3563b5f9144256168` |
| Kimi 8192-token verification | `019fd9c1e5c55fb4c01de271f68b1da3` |
| DeepSeek full 30-turn | `019fd9deb397e5143d88e0dcc556e3f1` |
| Gemini full 30-turn | `019fd9e198fa922b84b8f6db511e5a26` |
| Qwen full 30-turn, multi-life | `019fd9e3a230f4ee742dd10bae6afe44` |
| Kimi full 30-turn, one-life | `019fdbe4f17fa96aaa962db50fa22150` |
| GLM full 30-turn, one-life | `019fdbe93f25fb1c33d1e8ab3b65fc5c` |

### Minimal-guidance traces

| Run | Trace ID | Guidance |
| :-- | :-- | :-- |
| Kimi minimal | `019fdbf765134a9dbcd5c9d60e300ef5` | v1 |
| GLM minimal | `019fdbfa8dba569f1e006a346f533970` | v1 |
| DeepSeek minimal | `019fdc280c828e23b6d575be577c30a8` | v1 |
| Gemini minimal | `019fdc2b20c2fd4a80f40fb1db1eb3d7` | v1 |
| Qwen Flash minimal early stop | `019fdc2c760f8234ee3f5a3e49e4307c` | v1 |
| Qwen Plus minimal | `019fdc3091cc06c27d5a70a5aa030dff` | v2 |
| Qwen Flash minimal verification | `019fdc349c0af2db965afda174fb7800` | v2 |

Useful Logfire queries:

```sql
select trace_id, start_timestamp,
       attributes->>'turn' as turn,
       attributes->>'score' as score,
       attributes->>'room' as room
from records
where trace_id = '<trace_id>'
  and span_name = 'game_turn {turn} room={room} score={score} thirst={thirst}'
order by start_timestamp
limit 100
```

```sql
select start_timestamp,
       attributes->'gen_ai.tool.call.arguments' as args,
       attributes->>'gen_ai.tool.call.result' as result,
       exception_type,
       exception_message
from records
where trace_id = '<trace_id>'
  and span_name like 'execute_tool %'
order by start_timestamp
limit 100
```

## Related documentation

- [`packages/shared/OBSERVABILITY.md`](../packages/shared/OBSERVABILITY.md) — instrumentation and
  telemetry schema
- [`gemini35-vs-25-flash-analysis.md`](../gemini35-vs-25-flash-analysis.md) — earlier pre-Logfire
  methodology
- [`rainy-wed-4-frameworks.md`](../rainy-wed-4-frameworks.md) — original cross-framework analysis
