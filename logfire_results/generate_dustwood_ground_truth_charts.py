"""Generate dependency-free SVG charts from the Dustwood ground-truth manifest."""

from __future__ import annotations

import html
import json
import math
from collections import Counter, defaultdict
from pathlib import Path


RESULTS_DIR = Path(__file__).parent
MANIFEST_PATH = RESULTS_DIR / "dustwood-ground-truth-runs-2026-08-17.json"
TRACE_BASE_URL = "https://logfire-eu.pydantic.dev/last-name-franz/mcp-eval/?q=trace_id%3D%27{trace_id}%27"

COLORS = {
    "Muse Spark 1.2": "#2563eb",
    "Muse Glimmer 30B": "#059669",
    "Nemotron 3.5 Lightning": "#d97706",
}
OUTCOME_COLORS = {
    "outlaw": "#dc2626",
    "rattlesnake": "#7c3aed",
    "turn_cap": "#0284c7",
}
OUTCOME_LABELS = {
    "outlaw": "Outlaw",
    "rattlesnake": "Rattlesnake",
    "turn_cap": "Turn cap",
}


def esc(value: object) -> str:
    return html.escape(str(value), quote=True)


def save_svg(filename: str, body: list[str], width: int, height: int) -> None:
    document = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}" role="img">',
        "<style>text{font-family:Inter,Segoe UI,Arial,sans-serif;fill:#172033}.title{font-size:22px;font-weight:700}.subtitle{font-size:13px;fill:#526072}.axis{font-size:12px;fill:#526072}.label{font-size:12px;font-weight:600}.note{font-size:11px;fill:#526072}</style>",
        '<rect width="100%" height="100%" fill="#ffffff"/>',
        *body,
        "</svg>",
    ]
    (RESULTS_DIR / filename).write_text("\n".join(document) + "\n", encoding="utf-8")


def score_efficiency_chart(runs: list[dict[str, object]]) -> None:
    width, height = 1100, 650
    left, right, top, bottom = 95, 55, 85, 105
    plot_width, plot_height = width - left - right, height - top - bottom
    x_max = math.ceil(max(int(run["total_tokens"]) for run in runs) / 50_000) * 50_000
    y_max = math.ceil(max(int(run["score"]) for run in runs) / 10) * 10

    def x(value: float) -> float:
        return left + value / x_max * plot_width

    def y(value: float) -> float:
        return top + (1 - value / y_max) * plot_height

    body = [
        '<text class="title" x="95" y="36">Dustwood run efficiency: score versus total tokens</text>',
        '<text class="subtitle" x="95" y="58">Each bubble is one run; bubble area represents model requests. Upper-left is preferable.</text>',
        f'<line x1="{left}" y1="{top + plot_height}" x2="{left + plot_width}" y2="{top + plot_height}" stroke="#94a3b8"/>',
        f'<line x1="{left}" y1="{top}" x2="{left}" y2="{top + plot_height}" stroke="#94a3b8"/>',
    ]

    for tick in range(0, x_max + 1, 50_000):
        px = x(tick)
        body.extend(
            [
                f'<line x1="{px:.1f}" y1="{top}" x2="{px:.1f}" y2="{top + plot_height}" stroke="#e2e8f0"/>',
                f'<text class="axis" x="{px:.1f}" y="{top + plot_height + 24}" text-anchor="middle">{tick // 1000}k</text>',
            ]
        )
    for tick in range(0, y_max + 1, 10):
        py = y(tick)
        body.extend(
            [
                f'<line x1="{left}" y1="{py:.1f}" x2="{left + plot_width}" y2="{py:.1f}" stroke="#e2e8f0"/>',
                f'<text class="axis" x="{left - 12}" y="{py + 4:.1f}" text-anchor="end">{tick}</text>',
            ]
        )

    label_offsets = [(12, -10), (12, 18), (12, -10), (12, 18), (12, -10), (12, -10), (12, 18)]
    for index, run in enumerate(runs):
        px, py = x(int(run["total_tokens"])), y(int(run["score"]))
        radius = 5 + math.sqrt(int(run["requests"])) * 2.7
        color = COLORS[str(run["display_model"])]
        label_dx, label_dy = label_offsets[index]
        label = f'{run["display_model"].replace(" 3.5 Lightning", "").replace("Muse ", "")} #{run["run"]}'
        trace_url = TRACE_BASE_URL.format(trace_id=run["trace_id"])
        body.extend(
            [
                f'<a href="{esc(trace_url)}" target="_blank">',
                f'<circle cx="{px:.1f}" cy="{py:.1f}" r="{radius:.1f}" fill="{color}" fill-opacity="0.78" stroke="#ffffff" stroke-width="2"/>',
                "</a>",
                f'<text class="label" x="{px + label_dx:.1f}" y="{py + label_dy:.1f}">{esc(label)}</text>',
            ]
        )

    legend_x = left
    for model, color in COLORS.items():
        body.extend(
            [
                f'<circle cx="{legend_x}" cy="{height - 42}" r="7" fill="{color}"/>',
                f'<text class="axis" x="{legend_x + 12}" y="{height - 38}">{esc(model)}</text>',
            ]
        )
        legend_x += 220
    body.extend(
        [
            f'<text class="axis" x="{left + plot_width / 2:.1f}" y="{height - 70}" text-anchor="middle">Total tokens (lower is leaner)</text>',
            f'<text class="axis" x="28" y="{top + plot_height / 2:.1f}" text-anchor="middle" transform="rotate(-90 28 {top + plot_height / 2:.1f})">Final score (higher is better)</text>',
            f'<text class="note" x="{left}" y="{height - 14}">Nemotron runs are a pre-fix cohort; Muse runs are post-fix confirmations. Click bubbles to open their Logfire traces.</text>',
        ]
    )
    save_svg("dustwood-score-efficiency-2026-08-17.svg", body, width, height)


def terminal_outcomes_chart(runs: list[dict[str, object]]) -> None:
    width, height = 980, 490
    left, right, top, bottom = 210, 55, 90, 85
    plot_width = width - left - right
    by_model: dict[str, Counter[str]] = defaultdict(Counter)
    for run in runs:
        by_model[str(run["display_model"])][str(run["terminal_kind"])] += 1
    models = list(COLORS)
    max_runs = max(sum(by_model[model].values()) for model in models)
    row_height = 78
    body = [
        '<text class="title" x="210" y="36">Terminal outcomes by model</text>',
        '<text class="subtitle" x="210" y="58">Counts show why runs ended, not whether the model made useful progress before the terminal event.</text>',
    ]
    for index, model in enumerate(models):
        y = top + index * row_height
        body.append(f'<text class="label" x="{left - 15}" y="{y + 26}" text-anchor="end">{esc(model)}</text>')
        cursor = left
        for kind in OUTCOME_LABELS:
            count = by_model[model][kind]
            if not count:
                continue
            segment = plot_width * count / max_runs
            body.append(f'<rect x="{cursor:.1f}" y="{y}" width="{segment:.1f}" height="38" rx="5" fill="{OUTCOME_COLORS[kind]}"/>')
            if segment > 42:
                body.append(f'<text x="{cursor + segment / 2:.1f}" y="{y + 25}" text-anchor="middle" font-family="Inter,Segoe UI,Arial,sans-serif" font-size="14" font-weight="700" fill="#ffffff">{count}</text>')
            cursor += segment
        body.append(f'<text class="axis" x="{left + plot_width + 10}" y="{y + 25}">n={sum(by_model[model].values())}</text>')
    legend_x = left
    for kind, label in OUTCOME_LABELS.items():
        body.extend(
            [
                f'<rect x="{legend_x}" y="{height - 52}" width="15" height="15" rx="3" fill="{OUTCOME_COLORS[kind]}"/>',
                f'<text class="axis" x="{legend_x + 22}" y="{height - 40}">{label}</text>',
            ]
        )
        legend_x += 150
    body.append(f'<text class="note" x="{left}" y="{height - 14}">All runs use seed 43 and medium guidance; cohort labels remain important because Nemotron predates the post-fix confirmation runs.</text>')
    save_svg("dustwood-terminal-outcomes-2026-08-17.svg", body, width, height)


def main() -> None:
    manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
    runs = manifest["runs"]
    score_efficiency_chart(runs)
    terminal_outcomes_chart(runs)


if __name__ == "__main__":
    main()
