# OpenRouter Muse Dustwood Evaluation

**Test date:** August 17, 2026

**Benchmark:** Echoes of Dustwood via `packages/pydantic/pydantic_mcp_client.py`

**Models:** `meta/muse-spark-1.2` and `meta/muse-glimmer-30b`

**Guidance:** medium

**Game configuration:** seed `43`, 30-turn maximum, no inter-turn delay, one fresh Go MCP
server per run, and restart disabled.

**Observability:** Logfire project `mcp-eval`, EU region. Engine state from `game_turn` telemetry
is authoritative for score, turn count, and outcome.

## Executive summary

Muse Spark completed four valid post-fix runs, with scores ranging from **5 to 40** and an
average of **19.8**. The same world seed produced substantially different routes and terminal
turns because the model's actions and provider responses remained variable.

All four Spark runs ended through normal in-game hazards. Three ended in outlaw encounters at
turns 1, 2, and 9; the fourth reached turn 17 before dying to a rattlesnake. None produced a
plain-text command termination, client protocol error, or post-game state mutation.

The single post-fix Glimmer run reached score **62 at turn 17** before an outlaw killed it. It
survived longer and scored higher than every Spark run in this small sample, but one Glimmer run
is not enough for a model-level comparison.

## Run results

| Model | Run | Engine result | Terminal event | Requests | Total tokens | Logfire trace |
| :-- | --: | :-- | :-- | --: | --: | :-- |
| Muse Spark 1.2 | 1 | 40 / turn 17 | Rattlesnake in Assayer's Office | 23 | 110,080 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a0121a-2b2a-76f1-a0b6-9653ddf2d80f%27) |
| Muse Spark 1.2 | 2 | 26 / turn 9 | Outlaw in General Store | 12 | 42,713 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a0121c-a1b7-7144-b8af-7fa1e9a204ad%27) |
| Muse Spark 1.2 | 3 | 5 / turn 1 | Outlaw in Telegraph Office | 5 | 14,190 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a0121d-6cc7-7038-abaa-53471fd4c4be%27) |
| Muse Spark 1.2 | 4 | 8 / turn 2 | Outlaw in Telegraph Office | 5 | 14,847 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a0121e-2a42-71f4-9b55-64d3cbb42172%27) |
| Muse Glimmer 30B | 1 | 62 / turn 17 | Outlaw in General Store | 28 | 156,818 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a01216-e657-72cf-ae6f-c80945838dc7%27) |

## Spark aggregate

| Metric | Result |
| :-- | --: |
| Runs | 4 |
| Mean score | 19.8 |
| Median score | 17 |
| Score range | 5–40 |
| Mean terminal turn | 7.3 |
| Runs reaching turn 30 | 0 / 4 |
| Mean total tokens | 45,458 |
| Mean requests | 11.3 |

## Gameplay observations

### Muse Spark 1.2

Spark followed the MCP tool protocol consistently after the client hardening changes. It used
`command`, `go`, `take`, and `inventory` calls, and each run stopped when the server returned a
terminal game state. The client logged `Game ended` and did not issue actions after `GAME OVER`.

The main gameplay weakness was threat handling. In two runs Spark entered the Telegraph Office
while an outlaw was present and died immediately. In another it reached the General Store before
being shot. The strongest Spark run made it to the Assayer's Office and accumulated 40 points,
but took a risky action near a rattlesnake and died at turn 17.

The score spread is large relative to the fixed world: **35 points** from worst to best. This
shows that seed control makes the environment comparable, but does not remove model-side
variability.

### Muse Glimmer 30B

The Glimmer run was more productive in this limited comparison. It collected the book clue,
wire, map, canteen, leather, repaired the water pump, filled and drank from the canteen, and
reached score 62 by turn 17. It ultimately failed in the General Store when it investigated in
the presence of an outlaw.

Because there is only one post-fix Glimmer run, this should be treated as an observation rather
than a reliable ranking against Spark's four-run sample.

## Fix validation

The runs validate the two changes motivated by the earlier bad traces:

- The client no longer accepts a model's plain-text command as a game action while play is active.
- The server and client both stop progressing once the game is over, preventing post-game calls
  from changing room, score, inventory, or turn state.

Spark produced no equivalent of the earlier Mistral turn-zero `TAKE MAP` failure, and neither
Muse model produced the earlier Glimmer post-game mutation pattern in these confirmation runs.

## Conclusion

Muse Spark was operationally reliable after the fixes but strategically inconsistent: all four
runs ended early, with an average score of 19.8. Glimmer showed stronger progress in its one
confirmation run, reaching 62 points, but needs additional same-seed runs before drawing a fair
comparison. The next useful experiment is a larger cohort for both Muse models focused on outlaw
avoidance and threat-response decisions.
