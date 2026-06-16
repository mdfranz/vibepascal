#!/usr/bin/env python3
"""Benchmark visualizations for the May 27, 2026 four-framework run (gemini-3.5-flash)."""

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
from pathlib import Path

OUTPUT_DIR = Path(__file__).parent

COLORS = {
    "ADK":     "#e74c3c",
    "Agno":    "#2ecc71",
    "Pydantic":"#3498db",
    "Strands": "#e67e22",
}
FRAMEWORKS = ["ADK", "Agno", "Pydantic", "Strands"]

# ── Validated run metrics ──────────────────────────────────────────────────
SCORES      = {"ADK": 80,     "Agno": 80,    "Pydantic": 70,     "Strands": 73}
LATENCY     = {"ADK": 56.87,  "Agno": 52.26, "Pydantic": 67.6,   "Strands": 39.13}
TOTAL_TOK   = {"ADK": 152757, "Agno": 73086, "Pydantic": 145914,  "Strands": 144976}
INPUT_TOK   = {"ADK": 150707, "Agno": 73036, "Pydantic": 141733,  "Strands": 142079}
OUTPUT_TOK  = {"ADK": 554,    "Agno": 50,    "Pydantic": 4181,    "Strands": 2897}
CACHE_READ  = {"ADK": 36029,  "Agno": None,  "Pydantic": 47764,   "Strands": None}
REASONING   = {"ADK": None,   "Agno": 1616,  "Pydantic": None,    "Strands": None}
LLM_CALLS   = {"ADK": 23,     "Agno": 20,    "Pydantic": 22,      "Strands": 25}

# Per-turn scores extracted from game state logs (turn 0–20)
SCORE_TRAJ = {
    "ADK":     [(0,0),(1,5),(2,8),(3,11),(4,14),(5,17),(6,17),(7,22),(8,22),(9,25),
                (10,35),(11,35),(12,40),(13,45),(14,45),(15,45),(16,50),(17,70),(18,80),(19,80),(20,80)],
    "Agno":    [(0,0),(1,5),(2,5),(3,10),(4,13),(5,16),(6,19),(7,22),(8,22),(9,22),
                (10,27),(11,47),(12,57),(13,57),(14,60),(15,60),(16,60),(17,65),(18,70),(19,75),(20,80)],
    "Pydantic":[(0,0),(1,5),(2,8),(3,11),(4,14),(5,17),(6,17),(7,20),(8,20),(9,25),
                (10,25),(11,30),(12,35),(13,35),(14,35),(15,40),(16,40),(17,40),(18,60),(19,70),(20,70)],
    "Strands": [(0,0),(1,5),(2,8),(3,18),(4,18),(5,23),(6,26),(7,31),(8,31),(9,31),
                (10,36),(11,36),(12,39),(13,42),(14,45),(15,48),(16,48),(17,53),(18,53),(19,53),(20,73)],
}

plt.style.use("seaborn-v0_8-darkgrid")
plt.rcParams.update({"font.size": 11, "figure.dpi": 150})


def chart_score_trajectory():
    """Line chart: cumulative score per game turn, all 4 frameworks."""
    fig, ax = plt.subplots(figsize=(10, 6))

    for fw in FRAMEWORKS:
        turns, scores = zip(*SCORE_TRAJ[fw])
        ax.plot(turns, scores, "o-", color=COLORS[fw], label=fw, linewidth=2, markersize=4)
        # Annotate final score
        ax.annotate(f"{scores[-1]}", xy=(turns[-1], scores[-1]),
                    xytext=(6, 0), textcoords="offset points",
                    color=COLORS[fw], fontweight="bold", va="center", fontsize=10)

    # Annotate key scoring events on ADK as reference
    events = [(10, "Telegraph\nrepaired"), (17, "Pump\nfixed"), (18, "Water\nfilled")]
    for turn, label in events:
        ax.axvline(x=turn, color="gray", linestyle=":", alpha=0.4, linewidth=1)
        ax.text(turn + 0.15, 2, label, fontsize=7, color="gray", va="bottom")

    ax.set_xlabel("Game Turn")
    ax.set_ylabel("Score")
    ax.set_title("Score Trajectory: All 4 Frameworks (gemini-3.5-flash, 20 turns)")
    ax.set_xlim(-0.5, 22)
    ax.set_ylim(-2, 88)
    ax.set_xticks(range(0, 21, 2))
    ax.axhline(y=80, color="gray", linestyle="--", alpha=0.3, linewidth=1)
    ax.legend(loc="upper left")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "may27_score_trajectory.png")
    plt.close(fig)


def chart_token_composition():
    """Stacked bar: token composition per framework (input, output, with cache and reasoning overlays)."""
    fig, axes = plt.subplots(1, 2, figsize=(13, 6))

    # Left panel: input vs output (stacked), total scale
    ax = axes[0]
    x = np.arange(len(FRAMEWORKS))
    inp_bars = ax.bar(x, [INPUT_TOK[fw] / 1000 for fw in FRAMEWORKS],
                      color=[COLORS[fw] for fw in FRAMEWORKS], alpha=0.85, label="Input tokens")
    out_bars = ax.bar(x, [OUTPUT_TOK[fw] / 1000 for fw in FRAMEWORKS],
                      bottom=[INPUT_TOK[fw] / 1000 for fw in FRAMEWORKS],
                      color=[COLORS[fw] for fw in FRAMEWORKS], alpha=0.4,
                      hatch="///", label="Output tokens")

    # Overlay cache-read extent as a horizontal bracket on the input bar
    for i, fw in enumerate(FRAMEWORKS):
        cr = CACHE_READ[fw]
        inp = INPUT_TOK[fw] / 1000
        out = OUTPUT_TOK[fw] / 1000
        total = inp + out
        ax.text(i, total + 1.5, f"{int(total)}k", ha="center", fontsize=8, fontweight="bold")
        if cr is not None:
            ax.annotate("", xy=(i + 0.35, cr / 1000), xytext=(i + 0.35, 0),
                        arrowprops=dict(arrowstyle="-", color="navy", lw=1.5))
            ax.text(i + 0.38, cr / 2000, f"cache\n{cr//1000}k",
                    fontsize=7, color="navy", va="center")

    ax.set_xticks(x)
    ax.set_xticklabels(FRAMEWORKS)
    ax.set_ylabel("Tokens (thousands)")
    ax.set_title("Token Volume: Input vs Output\n(hatched = output; navy bracket = cache reads)")
    ax.set_ylim(0, 175)
    handles = [mpatches.Patch(color="gray", alpha=0.85, label="Input"),
               mpatches.Patch(color="gray", alpha=0.4, hatch="///", label="Output")]
    ax.legend(handles=handles, loc="upper right", fontsize=9)

    # Right panel: output token zoom (log scale shows Agno's 50 vs Pydantic's 4181)
    ax2 = axes[1]
    out_vals = [OUTPUT_TOK[fw] for fw in FRAMEWORKS]
    bars = ax2.bar(x, out_vals, color=[COLORS[fw] for fw in FRAMEWORKS], alpha=0.85)
    for bar, fw in zip(bars, FRAMEWORKS):
        val = OUTPUT_TOK[fw]
        reas = REASONING[fw]
        label = f"{val:,}"
        if reas:
            label += f"\n({reas} reasoning)"
        ax2.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 30,
                 label, ha="center", va="bottom", fontsize=9, fontweight="bold")

    ax2.set_xticks(x)
    ax2.set_xticklabels(FRAMEWORKS)
    ax2.set_ylabel("Output Tokens")
    ax2.set_title("Output Token Detail\n(Agno enforces terse single-command responses)")
    ax2.set_ylim(0, 5200)
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "may27_token_composition.png")
    plt.close(fig)


def chart_efficiency_scatter():
    """Scatter: Score vs Latency, bubble size = total tokens."""
    fig, ax = plt.subplots(figsize=(9, 7))

    for fw in FRAMEWORKS:
        s = SCORES[fw]
        lat = LATENCY[fw]
        tok = TOTAL_TOK[fw]
        size = tok / 500
        ax.scatter(lat, s, s=size, color=COLORS[fw], alpha=0.8,
                   edgecolors="black", linewidth=1, zorder=3)
        ax.annotate(fw, (lat, s), textcoords="offset points",
                    xytext=(8, 4), fontsize=11, fontweight="bold", color=COLORS[fw])
        ax.annotate(f"{tok // 1000}k tok", (lat, s), textcoords="offset points",
                    xytext=(8, -10), fontsize=8, color="gray")

    # Bubble size legend
    for tok_val, label in [(73000, "73k"), (145000, "145k"), (153000, "153k")]:
        ax.scatter([], [], s=tok_val / 500, color="gray", alpha=0.5, label=f"{label} tokens")

    ax.set_xlabel("Total Run Duration (seconds)")
    ax.set_ylabel("Final Score")
    ax.set_title("Efficiency: Score vs Latency\n(bubble area ∝ total token cost)")
    ax.set_xlim(25, 80)
    ax.set_ylim(60, 85)
    ax.axhline(y=80, color="gray", linestyle="--", alpha=0.3)
    ax.legend(loc="lower right", title="Token Cost", fontsize=9)
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "may27_efficiency_scatter.png")
    plt.close(fig)


def chart_score_efficiency():
    """Horizontal bar: points scored per 1,000 total tokens."""
    fig, ax = plt.subplots(figsize=(9, 5))

    vals = {fw: SCORES[fw] / (TOTAL_TOK[fw] / 1000) for fw in FRAMEWORKS}
    sorted_fw = sorted(FRAMEWORKS, key=lambda fw: vals[fw])

    bars = ax.barh(sorted_fw, [vals[fw] for fw in sorted_fw],
                   color=[COLORS[fw] for fw in sorted_fw],
                   edgecolor="black", linewidth=0.5)
    for bar, fw in zip(bars, sorted_fw):
        v = vals[fw]
        ax.text(bar.get_width() + 0.005, bar.get_y() + bar.get_height() / 2,
                f"{v:.3f}  ({SCORES[fw]} pts / {TOTAL_TOK[fw]//1000}k tok)",
                va="center", fontsize=9)

    ax.set_xlabel("Points per 1,000 Tokens  (higher = more efficient)")
    ax.set_title("Score Efficiency: Points Earned per 1,000 Tokens")
    ax.set_xlim(0, 1.35)
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "may27_score_efficiency.png")
    plt.close(fig)


def chart_cost_normalized():
    """Horizontal bar: composite bang-for-buck = score / (tokens_k × latency_s) × 1000."""
    fig, ax = plt.subplots(figsize=(9, 5))

    vals = {fw: SCORES[fw] / ((TOTAL_TOK[fw] / 1000) * LATENCY[fw]) * 1000
            for fw in FRAMEWORKS}
    sorted_fw = sorted(FRAMEWORKS, key=lambda fw: vals[fw])

    bars = ax.barh(sorted_fw, [vals[fw] for fw in sorted_fw],
                   color=[COLORS[fw] for fw in sorted_fw],
                   edgecolor="black", linewidth=0.5)
    for bar, fw in zip(bars, sorted_fw):
        v = vals[fw]
        ax.text(bar.get_width() + 0.002, bar.get_y() + bar.get_height() / 2,
                f"{v:.3f}", va="center", fontsize=10, fontweight="bold")

    ax.set_xlabel("score / (total_tokens_k × latency_s) × 1000  (higher = better value)")
    ax.set_title("Cost-Normalized Ranking: Score per Token-Second Spent")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "may27_cost_normalized.png")
    plt.close(fig)


def chart_radar():
    """Radar: 5-axis normalized framework profile."""
    categories = ["Score", "Speed\n(1/latency)", "Token\nEfficiency", "Output\nDiscipline", "Call\nEfficiency"]
    N = len(categories)

    raw = {
        fw: [
            SCORES[fw],
            1.0 / LATENCY[fw],
            SCORES[fw] / (TOTAL_TOK[fw] / 1000),
            1.0 / OUTPUT_TOK[fw],
            20 / LLM_CALLS[fw],   # game turns per LLM call (higher = fewer wasted calls)
        ]
        for fw in FRAMEWORKS
    }

    # Normalize each axis to 0–1 across the 4 frameworks
    normalized = {fw: [] for fw in FRAMEWORKS}
    for axis_idx in range(N):
        axis_vals = [raw[fw][axis_idx] for fw in FRAMEWORKS]
        max_val = max(axis_vals)
        for fw in FRAMEWORKS:
            normalized[fw].append(raw[fw][axis_idx] / max_val)

    angles = np.linspace(0, 2 * np.pi, N, endpoint=False).tolist()
    angles += angles[:1]

    fig, ax = plt.subplots(figsize=(8, 8), subplot_kw=dict(polar=True))

    for fw in FRAMEWORKS:
        values = normalized[fw] + normalized[fw][:1]
        ax.plot(angles, values, "o-", linewidth=2, label=fw, color=COLORS[fw])
        ax.fill(angles, values, alpha=0.08, color=COLORS[fw])

    ax.set_xticks(angles[:-1])
    ax.set_xticklabels(categories, fontsize=11)
    ax.set_ylim(0, 1.1)
    ax.set_yticks([0.25, 0.5, 0.75, 1.0])
    ax.set_yticklabels(["25%", "50%", "75%", "100%"], fontsize=8, alpha=0.5)
    ax.set_title("Framework Profile: Normalized Multi-Axis Comparison\n(gemini-3.5-flash, May 27 run)",
                 y=1.1, fontsize=12)
    ax.legend(loc="upper right", bbox_to_anchor=(1.35, 1.15))
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "may27_radar.png")
    plt.close(fig)


if __name__ == "__main__":
    chart_score_trajectory()
    chart_token_composition()
    chart_efficiency_scatter()
    chart_score_efficiency()
    chart_cost_normalized()
    chart_radar()
    print(f"Charts saved to {OUTPUT_DIR}/")
    for f in sorted(OUTPUT_DIR.glob("may27_*.png")):
        print(f"  {f.name}")
