# OpenRouter Nemotron Dustwood Evaluation

**Test date:** August 17, 2026

**Benchmark:** Echoes of Dustwood via `packages/pydantic/pydantic_mcp_client.py`

**Model:** `nvidia/nemotron-3.5-lightning`

**Guidance:** medium

**Game configuration:** seed `43`, 30-turn maximum, one fresh Go MCP server per run, and
restart disabled.

**Observability:** Logfire project `mcp-eval`, EU region. Engine state from `game_turn` telemetry
and the MCP tool returns is authoritative for score, turn count, and outcome.

These runs were recorded before the latest client/server hardening changes. They did not show the
malformed-output failure seen with Mistral or the post-game mutation seen in an earlier Glimmer
run, but the second run did issue a harmless `QUIT` command after game over.

## Executive summary

Nemotron was the strongest of the two-run cohorts operationally. One run reached the 30-turn cap
with score **29**, while the other survived to turn 18 and scored **18** before an outlaw killed
it. Both runs used MCP tools throughout and completed without client protocol errors.

The main weakness was strategic efficiency rather than tool compliance. The 30-turn run spent a
large portion of its later turns in the General Store attempting invalid inventory operations;
its score stopped increasing at 29 around turn 24. The shorter run explored several locations and
collected the copper wire, but eventually issued `TALK TO OUTLAW` in the Assayer's Office and was
shot.

## Run results

| Run | Engine result | Terminal event | Requests | Total tokens | Logfire trace |
| --: | :-- | :-- | --: | --: | :-- |
| 1 | 29 / turn 30 | Client turn limit reached | 41 | 229,698 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a01201-4601-76fd-aee4-e455122c965b%27) |
| 2 | 18 / turn 18 | Outlaw in Assayer's Office | 28 | 125,209 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a01202-49f6-7169-85b9-5b8224deaf41%27) |

## Aggregate comparison

| Metric | Result |
| :-- | --: |
| Runs | 2 |
| Mean score | 23.5 |
| Median score | 23.5 |
| Score range | 18–29 |
| Mean terminal turn | 24.0 |
| Runs reaching turn 30 | 1 / 2 |
| Mean total tokens | 177,454 |
| Mean requests | 34.5 |

## Gameplay observations

### Run 1: long survival, low conversion

Nemotron reached the 30-turn client limit without dying and accumulated 29 points. It visited
the Sheriff's Office, Livery Stables, Main Street, and General Store, but did not convert the
available route into additional score after reaching the store.

From approximately turn 24 onward, the model repeatedly tried to drop items it did not actually
have or could not identify correctly. The engine responses were variations of “You aren't
carrying that,” while score remained fixed at 29. This produced a long trace and high token use
without corresponding gameplay progress.

### Run 2: exploration followed by threat failure

The second run reached score 18 and turn 18. It explored the Sheriff's Office, Telegraph Office,
Main Street, and Assayer's Office, and collected the copper wire. It then issued `TALK TO OUTLAW`
while an outlaw was present in the Assayer's Office. The command was enough to trigger the outlaw's
attack, ending the game.

After the terminal tool result, the model issued `QUIT`; the server returned an empty result while
keeping `is_playing=false`. There was no observed change to score, room, inventory, or turns, but
this is precisely the class of post-game call now blocked by the server's central immutability
guard.

## Reliability assessment

Both runs demonstrate reliable MCP interaction: the model selected tools and the server returned
structured state after each action. Neither run produced plain-text commands that were mistaken
for executed gameplay actions, and neither run crashed the client.

Nemotron's variability was moderate in this small sample: an 11-point score spread and a 12-turn
survival spread. The fixed seed made the locations and hazards comparable, but the model's route
selection and inventory decisions determined whether it made progress or stalled.

## Conclusion

Under medium guidance and seed `43`, Nemotron produced one durable but inefficient run and one
shorter, more exploratory run. Its tool-use reliability was good; the main opportunity is better
state-aware planning, especially recognizing inventory failures and avoiding direct interaction
with visible outlaws. A larger cohort should measure how often the model converts survival into
score after reaching the General Store and whether stronger threat guidance improves that result.
