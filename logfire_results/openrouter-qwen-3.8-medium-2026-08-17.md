# OpenRouter Qwen 3.8 Dustwood Evaluation

**Test date:** August 17, 2026

**Benchmark:** Echoes of Dustwood via `packages/pydantic/pydantic_mcp_client.py`

**Models:** `openrouter:qwen/qwen3.8-27b` and `openrouter:qwen/qwen3.8-max`

**Guidance:** medium

**Game configuration:** seed `43`, 30-turn maximum, 1-second delay, one fresh Go MCP server
per run, and restart disabled.

**Observability:** Logfire project `mcp-eval`, EU region. Engine state from `game_turn` telemetry
is authoritative for score, turn count, and outcome.

## Executive summary

Qwen 3.8 Max substantially outperformed Qwen 3.8 27B across three same-seed runs:

- Qwen 3.8 Max averaged **73.7 points** and reached the 30-turn limit twice.
- Qwen 3.8 27B averaged **37.7 points** and reached the 30-turn limit once.
- Max's scores were **78, 65, 78**; 27B's were **13, 65, 35**.
- Max never scored below 65 in this cohort; 27B had a 52-point spread.

The result favors Max clearly, but this is still a three-run sample. The fixed game seed controls
world generation; it does not make model responses deterministic.

## Run results

| Round | Model | Engine result | Terminal event | Requests | Total tokens | Logfire trace |
| :-- | :-- | :-- | :-- | --: | --: | :-- |
| 1 | Qwen 3.8 27B | 13 / turn 4 | Killed by outlaw in General Store | 8 | 24,182 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a011d0c062bb792b3088e7b2ecc63d%27) |
| 1 | Qwen 3.8 Max | 78 / turn 30 | Natural day/night game ending | 34 | 257,675 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a011d21e7e1f2c872f3791341876ea%27) |
| 2 | Qwen 3.8 27B | 65 / turn 20 | Killed by outlaw at The Desert Edge | 28 | 176,480 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a011d8da683a3253ee9aa1a6b8ba11%27) |
| 2 | Qwen 3.8 Max | 65 / turn 21 | Killed by outlaw at The Desert Edge | 29 | 176,648 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a011dd81f98b11332bbaa70b730d45%27) |
| 3 | Qwen 3.8 27B | 35 / turn 13 | Killed by outlaw in Assayer's Office | 18 | 77,667 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a011f847198df6e51d966476fc46c4%27) |
| 3 | Qwen 3.8 Max | 78 / turn 30 | Natural day/night game ending | 38 | 253,362 | [trace](https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%2701a011faaaae7db52ec81d677deb4c2c%27) |

## Aggregate comparison

| Model | Mean score | Median | Range | Mean turns | Full 30-turn runs | Mean total tokens | Mean requests |
| :-- | --: | --: | --: | --: | :--: | --: | --: |
| Qwen 3.8 27B | 37.7 | 35 | 13–65 | 12.3 | 1 / 3 | 92,776 | 18.0 |
| Qwen 3.8 Max | 73.7 | 78 | 65–78 | 27.0 | 2 / 3 | 229,228 | 33.7 |

Max's mean score was 36 points higher, and its median was 43 points higher. The extra cost is
substantial: Max used about 2.5 times as many total tokens across the cohort, largely because it
survived much longer and continued reasoning through more game turns.

## Gameplay observations

### Qwen 3.8 27B

The 27B model was highly variable. In the first round it made an early fatal decision in the
General Store. In the second round it reached The Desert Edge and scored 65 before dying at turn
20. In the third round it reached the Assayer's Office with 35 points before dying at turn 13.

Its main weakness in this cohort was threat handling: all three runs ended in an outlaw encounter,
but at very different points in the route. The model could make meaningful progress, yet it did
not reliably preserve a successful line of play.

### Qwen 3.8 Max

Max was more consistent. It reached turn 30 twice, scored 78 in both of those runs, and reached
65 before dying in the remaining run. In the successful runs it explored far enough to reach the
Desert Edge and continued managing supplies through the day/night cutoff.

The second Max run still died at The Desert Edge, showing that the model was not immune to the
outlaw hazard. Its advantage was that it generally accumulated enough progress before the same
failure mode occurred.

## Validity notes

- All six traces used the same server seed, medium guidance, 30-turn limit, and one fresh server
  process per run.
- The server's restart protection remained enabled. After some deaths, the models issued additional
  `reset_game` or `score` calls; those did not create a second life. Their request and token counts
  include this post-game recovery behavior.
- Scores and terminal turns come from engine telemetry, not the model's closing prose.
- The sample is too small for a definitive model ranking, but the gap between the two models is
  large and consistent enough to justify a larger Qwen Max/27B cohort.

## Conclusion

Under medium guidance with seed `43`, Qwen 3.8 Max was the stronger gameplay model. It produced
two high-scoring full-length runs and one respectable 65-point run, while Qwen 3.8 27B produced
one moderate run and two early deaths. The tradeoff is cost: Max's greater survival and reasoning
depth came with roughly 2.5x the token usage in this small cohort.
