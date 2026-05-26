#!/usr/bin/env python3
"""Generate benchmark visualizations for Echoes of Dustwood agent framework comparison."""

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
from pathlib import Path

OUTPUT_DIR = Path(__file__).parent

# Color scheme
COLORS = {
    "Agno": "#2ecc71",
    "Pydantic": "#3498db",
    "Strands": "#e67e22",
    "ADK": "#e74c3c",
}

MODEL_COLORS = {
    "gemini-3.5-flash": "#4285F4",
    "gpt-5-mini": "#10a37f",
    "claude-haiku-4-5": "#d4a574",
}

# Data: scores (best run per model/framework combo, excluding errors)
scores = {
    "Agno":     {"gemini-3.5-flash": 70, "gpt-5-mini": 45, "claude-haiku-4-5": 32},
    "Pydantic": {"gemini-3.5-flash": 48, "gpt-5-mini": 38, "claude-haiku-4-5": 28},
    "Strands":  {"gemini-3.5-flash": 55, "gpt-5-mini": 35, "claude-haiku-4-5": 60},
    "ADK":      {"gemini-3.5-flash": 73, "gpt-5-mini": 32, "claude-haiku-4-5": 57},
}

# Latency in seconds (best successful run)
latency = {
    "Agno":     {"gemini-3.5-flash": 51.6, "gpt-5-mini": 91.1, "claude-haiku-4-5": 42.4},
    "Pydantic": {"gemini-3.5-flash": 66.0, "gpt-5-mini": 101.0, "claude-haiku-4-5": 49.0},
    "Strands":  {"gemini-3.5-flash": 29.7, "gpt-5-mini": 43.7, "claude-haiku-4-5": 28.4},
    "ADK":      {"gemini-3.5-flash": 49.8, "gpt-5-mini": 75.2, "claude-haiku-4-5": 46.7},
}

# Per-turn latency (seconds)
per_turn = {
    "Agno":     {"gemini-3.5-flash": 3.4, "gpt-5-mini": 6.1, "claude-haiku-4-5": 2.8},
    "Pydantic": {"gemini-3.5-flash": 4.3, "gpt-5-mini": 6.7, "claude-haiku-4-5": 3.2},
    "Strands":  {"gemini-3.5-flash": 2.0, "gpt-5-mini": 2.9, "claude-haiku-4-5": 1.9},
    "ADK":      {"gemini-3.5-flash": 3.3, "gpt-5-mini": 5.0, "claude-haiku-4-5": 3.1},
}

# Total tokens (approximate, using consistent measure across frameworks)
tokens = {
    "Agno":     {"gemini-3.5-flash": 32392, "gpt-5-mini": 36242, "claude-haiku-4-5": 43458},
    "Pydantic": {"gemini-3.5-flash": 109602, "gpt-5-mini": 104494, "claude-haiku-4-5": 98461},
    "Strands":  {"gemini-3.5-flash": 97117, "gpt-5-mini": 80397, "claude-haiku-4-5": 92764},
    "ADK":      {"gemini-3.5-flash": 99100, "gpt-5-mini": 98834, "claude-haiku-4-5": 99326},
}

frameworks = ["Agno", "Pydantic", "Strands", "ADK"]
models = ["gemini-3.5-flash", "gpt-5-mini", "claude-haiku-4-5"]

plt.style.use("seaborn-v0_8-darkgrid")
plt.rcParams.update({"font.size": 11, "figure.dpi": 150})


def chart_1_grouped_bar_scores():
    """Grouped bar chart: Score by Framework, grouped by Model."""
    fig, ax = plt.subplots(figsize=(10, 6))
    x = np.arange(len(frameworks))
    width = 0.25

    for i, model in enumerate(models):
        vals = [scores[fw][model] for fw in frameworks]
        bars = ax.bar(x + i * width, vals, width, label=model, color=MODEL_COLORS[model])
        for bar, val in zip(bars, vals):
            ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 0.5,
                    str(val), ha="center", va="bottom", fontsize=9, fontweight="bold")

    ax.set_xlabel("Framework")
    ax.set_ylabel("Score (10-turn session)")
    ax.set_title("Game Score by Framework & Model (Best Run)")
    ax.set_xticks(x + width)
    ax.set_xticklabels(frameworks)
    ax.set_ylim(0, 80)
    ax.axhline(y=73, color="gray", linestyle="--", alpha=0.5, label="Max observed (73)")
    ax.legend(loc="upper right")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "score_by_framework_model.png")
    plt.close(fig)


def chart_2_latency_heatmap():
    """Heatmap: Wall time (seconds) by framework x model."""
    fig, ax = plt.subplots(figsize=(8, 5))

    data = np.array([
        [latency[fw].get(m) or 0 for m in models]
        for fw in frameworks
    ], dtype=float)

    # Mask MS Agent + gemini (no data)
    mask = data == 0
    masked_data = np.ma.masked_where(mask, data)

    im = ax.imshow(masked_data, cmap="YlOrRd", aspect="auto", vmin=0, vmax=100)

    ax.set_xticks(range(len(models)))
    ax.set_xticklabels(models, fontsize=10)
    ax.set_yticks(range(len(frameworks)))
    ax.set_yticklabels(frameworks, fontsize=10)

    for i in range(len(frameworks)):
        for j in range(len(models)):
            val = data[i, j]
            if val == 0:
                ax.text(j, i, "N/A", ha="center", va="center", color="gray", fontsize=11)
            else:
                ax.text(j, i, f"{val:.0f}s", ha="center", va="center",
                        color="white" if val > 50 else "black", fontsize=12, fontweight="bold")

    ax.set_title("Wall Time (seconds) — 10-Turn Game Session")
    fig.colorbar(im, ax=ax, label="Seconds")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "latency_heatmap.png")
    plt.close(fig)


def chart_3_per_turn_latency():
    """Bar chart: per-turn latency by model, stacked by framework."""
    fig, ax = plt.subplots(figsize=(10, 6))
    x = np.arange(len(models))
    width = 0.2

    for i, fw in enumerate(frameworks):
        vals = [per_turn[fw][m] if per_turn[fw][m] is not None else 0 for m in models]
        bars = ax.bar(x + i * width, vals, width, label=fw, color=COLORS[fw])
        for bar, val in zip(bars, vals):
            if val > 0:
                ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 0.1,
                        f"{val:.1f}", ha="center", va="bottom", fontsize=8)

    ax.set_xlabel("Model")
    ax.set_ylabel("Per-Turn Latency (seconds)")
    ax.set_title("Per-Turn Latency by Model & Framework")
    ax.set_xticks(x + width * 1.5)
    ax.set_xticklabels(models)
    ax.legend(loc="upper right")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "per_turn_latency.png")
    plt.close(fig)


def chart_4_token_usage():
    """Grouped bar chart: total token usage."""
    fig, ax = plt.subplots(figsize=(10, 6))
    x = np.arange(len(models))
    width = 0.2

    for i, fw in enumerate(frameworks):
        vals = [tokens[fw][m] if tokens[fw][m] is not None else 0 for m in models]
        bars = ax.bar(x + i * width, [v / 1000 for v in vals], width, label=fw, color=COLORS[fw])
        for bar, val in zip(bars, vals):
            if val > 0:
                ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 0.5,
                        f"{val // 1000}k", ha="center", va="bottom", fontsize=8)

    ax.set_xlabel("Model")
    ax.set_ylabel("Total Tokens (thousands)")
    ax.set_title("Token Usage by Model & Framework (10-Turn Session)")
    ax.set_xticks(x + width * 1.5)
    ax.set_xticklabels(models)
    ax.legend(loc="upper left")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "token_usage.png")
    plt.close(fig)


def chart_5_efficiency_scatter():
    """Scatter plot: Score vs Latency, bubble size = tokens."""
    fig, ax = plt.subplots(figsize=(10, 7))

    for fw in frameworks:
        for model in models:
            s = scores[fw][model]
            lat = latency[fw][model]
            tok = tokens[fw][model]
            if lat is None or tok is None:
                continue
            size = tok / 300  # scale for bubble
            ax.scatter(lat, s, s=size, color=COLORS[fw], alpha=0.7,
                       edgecolors="black", linewidth=0.5)
            ax.annotate(f"{model.split('-')[0][:3]}",
                        (lat, s), textcoords="offset points",
                        xytext=(5, 5), fontsize=7, alpha=0.8)

    # Legend for frameworks
    fw_patches = [mpatches.Patch(color=COLORS[fw], label=fw) for fw in frameworks]
    legend1 = ax.legend(handles=fw_patches, loc="lower left", title="Framework")
    ax.add_artist(legend1)

    # Legend for bubble size
    for tok_val, label in [(15000, "15k"), (45000, "45k"), (67000, "67k")]:
        ax.scatter([], [], s=tok_val / 300, color="gray", alpha=0.5, label=f"{label} tokens")
    ax.legend(loc="lower right", title="Token Cost (bubble size)")

    ax.set_xlabel("Wall Time (seconds)")
    ax.set_ylabel("Score")
    ax.set_title("Efficiency: Score vs Latency (bubble size = token cost)")
    ax.set_xlim(0, 100)
    ax.set_ylim(0, 80)
    ax.axhline(y=73, color="gray", linestyle="--", alpha=0.3)
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "efficiency_scatter.png")
    plt.close(fig)


def chart_6_framework_sensitivity():
    """Radar-like bar: score variance per model across frameworks (framework sensitivity)."""
    fig, ax = plt.subplots(figsize=(8, 5))

    model_ranges = {}
    for model in models:
        valid_scores = [scores[fw][model] for fw in frameworks if scores[fw][model] > 5]
        model_ranges[model] = max(valid_scores) - min(valid_scores)

    bars = ax.bar(models, [model_ranges[m] for m in models],
                  color=[MODEL_COLORS[m] for m in models], edgecolor="black", linewidth=0.5)

    for bar, model in zip(bars, models):
        valid_scores = [scores[fw][model] for fw in frameworks if scores[fw][model] > 5]
        ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 0.3,
                f"{min(valid_scores)}–{max(valid_scores)}", ha="center", fontsize=10)

    ax.set_ylabel("Score Spread (max - min)")
    ax.set_title("Framework Sensitivity: Score Variance by Model\n(lower = more consistent across frameworks)")
    ax.set_ylim(0, 35)
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "framework_sensitivity.png")
    plt.close(fig)


def chart_7_reliability_matrix():
    """Heatmap: success rate (completed / attempted) per framework x model."""
    # All runs from logs: (completed, total_attempted)
    reliability = {
        "Agno":     {"gemini-3.5-flash": (1, 1), "gpt-5-mini": (1, 1), "claude-haiku-4-5": (1, 1)},
        "Pydantic": {"gemini-3.5-flash": (1, 1), "gpt-5-mini": (1, 1), "claude-haiku-4-5": (1, 1)},
        "Strands":  {"gemini-3.5-flash": (1, 1), "gpt-5-mini": (1, 1), "claude-haiku-4-5": (1, 1)},
        "ADK":      {"gemini-3.5-flash": (1, 1), "gpt-5-mini": (1, 1), "claude-haiku-4-5": (1, 1)},
    }

    fig, ax = plt.subplots(figsize=(8, 5))
    data = np.array([
        [reliability[fw][m][0] / reliability[fw][m][1] * 100 for m in models]
        for fw in frameworks
    ])

    im = ax.imshow(data, cmap="RdYlGn", aspect="auto", vmin=0, vmax=100)

    ax.set_xticks(range(len(models)))
    ax.set_xticklabels(models, fontsize=10)
    ax.set_yticks(range(len(frameworks)))
    ax.set_yticklabels(frameworks, fontsize=10)

    for i in range(len(frameworks)):
        for j in range(len(models)):
            fw, m = frameworks[i], models[j]
            c, t = reliability[fw][m]
            pct = data[i, j]
            color = "white" if pct < 40 else "black"
            ax.text(j, i, f"{c}/{t}\n({pct:.0f}%)", ha="center", va="center",
                    color=color, fontsize=11, fontweight="bold")

    ax.set_title("Reliability: Successful Completions / Total Attempts")
    fig.colorbar(im, ax=ax, label="Success Rate (%)")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "reliability_matrix.png")
    plt.close(fig)


def chart_8_score_efficiency():
    """Bar chart: points scored per 1k tokens."""
    fig, ax = plt.subplots(figsize=(10, 6))
    x = np.arange(len(models))
    width = 0.2

    for i, fw in enumerate(frameworks):
        vals = []
        for m in models:
            s = scores[fw][m]
            t = tokens[fw][m]
            if t is None or t == 0:
                vals.append(0)
            else:
                vals.append(s / (t / 1000))
        bars = ax.bar(x + i * width, vals, width, label=fw, color=COLORS[fw])
        for bar, val in zip(bars, vals):
            if val > 0:
                ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 0.03,
                        f"{val:.1f}", ha="center", va="bottom", fontsize=8, fontweight="bold")

    ax.set_xlabel("Model")
    ax.set_ylabel("Points per 1k Tokens")
    ax.set_title("Score Efficiency: Points Earned per 1,000 Tokens\n(higher = more gameplay value per token)")
    ax.set_xticks(x + width * 1.5)
    ax.set_xticklabels(models)
    ax.legend(loc="upper right")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "score_efficiency.png")
    plt.close(fig)


def chart_9_radar():
    """Radar chart: multi-axis framework comparison."""
    from matplotlib.patches import FancyBboxPatch

    # Normalize each axis to 0-1 (higher = better)
    # Axes: Score, Speed, Token Efficiency, Reliability, Telegraph Rate

    # Avg best score across models (excluding N/A)
    avg_scores = {}
    for fw in frameworks:
        valid = [scores[fw][m] for m in models if scores[fw][m] > 5]
        avg_scores[fw] = np.mean(valid) if valid else 0

    # Avg speed = 1/latency (higher = faster), normalized
    avg_speed = {}
    for fw in frameworks:
        valid = [1.0 / latency[fw][m] for m in models if latency[fw][m] is not None]
        avg_speed[fw] = np.mean(valid) if valid else 0

    # Token efficiency = avg(score/tokens*1000)
    avg_tok_eff = {}
    for fw in frameworks:
        valid = [scores[fw][m] / (tokens[fw][m] / 1000)
                 for m in models if tokens[fw][m] is not None and tokens[fw][m] > 0]
        avg_tok_eff[fw] = np.mean(valid) if valid else 0

    # Reliability = avg success rate
    reliability_raw = {
        "Agno": (3, 3), "Pydantic": (3, 3), "Strands": (3, 3), "ADK": (3, 3),
    }
    avg_reliability = {fw: c / t for fw, (c, t) in reliability_raw.items()}

    # Telegraph rate: fraction of model combos that achieved FIX TELEGRAPH
    telegraph = {
        "Agno": 2 / 3,       # gemini + gpt-5-mini
        "Pydantic": 2 / 3,   # gemini + gpt-5-mini
        "Strands": 0 / 3,    # none
        "ADK": 1 / 3,        # gemini only
    }

    categories = ["Score", "Speed", "Token\nEfficiency", "Reliability", "Telegraph\nRate"]
    N = len(categories)

    # Build raw values dict
    raw = {}
    for fw in frameworks:
        raw[fw] = [avg_scores[fw], avg_speed[fw], avg_tok_eff[fw],
                   avg_reliability[fw], telegraph[fw]]

    # Normalize each axis to 0-1 across frameworks
    normalized = {}
    for fw in frameworks:
        normalized[fw] = []
    for axis_idx in range(N):
        axis_vals = [raw[fw][axis_idx] for fw in frameworks]
        max_val = max(axis_vals) if max(axis_vals) > 0 else 1
        for fw in frameworks:
            normalized[fw].append(raw[fw][axis_idx] / max_val)

    angles = np.linspace(0, 2 * np.pi, N, endpoint=False).tolist()
    angles += angles[:1]  # close the polygon

    fig, ax = plt.subplots(figsize=(8, 8), subplot_kw=dict(polar=True))

    for fw in frameworks:
        values = normalized[fw] + normalized[fw][:1]
        ax.plot(angles, values, "o-", linewidth=2, label=fw, color=COLORS[fw])
        ax.fill(angles, values, alpha=0.1, color=COLORS[fw])

    ax.set_xticks(angles[:-1])
    ax.set_xticklabels(categories, fontsize=11)
    ax.set_ylim(0, 1.1)
    ax.set_yticks([0.25, 0.5, 0.75, 1.0])
    ax.set_yticklabels(["25%", "50%", "75%", "100%"], fontsize=8, alpha=0.6)
    ax.set_title("Framework Profile: Normalized Multi-Axis Comparison", y=1.08, fontsize=13)
    ax.legend(loc="upper right", bbox_to_anchor=(1.3, 1.1))
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "radar_framework_profile.png")
    plt.close(fig)


def chart_10_all_runs_boxplot():
    """Box plot: all runs per framework (including failures as 0)."""
    # All individual run scores per framework
    all_runs = {
        "Agno":     [70, 45, 32],
        "Pydantic": [48, 38, 28],
        "Strands":  [55, 35, 60],
        "ADK":      [73, 32, 57],
    }

    fig, ax = plt.subplots(figsize=(9, 6))

    positions = range(len(frameworks))
    bp = ax.boxplot(
        [all_runs[fw] for fw in frameworks],
        positions=positions,
        widths=0.5,
        patch_artist=True,
        showmeans=True,
        meanprops=dict(marker="D", markerfacecolor="white", markeredgecolor="black", markersize=7),
    )

    for patch, fw in zip(bp["boxes"], frameworks):
        patch.set_facecolor(COLORS[fw])
        patch.set_alpha(0.6)

    # Overlay individual points
    for i, fw in enumerate(frameworks):
        jitter = np.random.default_rng(42).uniform(-0.12, 0.12, len(all_runs[fw]))
        ax.scatter([i + j for j in jitter], all_runs[fw],
                   color=COLORS[fw], edgecolors="black", linewidth=0.5, s=50, zorder=3)

    ax.set_xticks(positions)
    ax.set_xticklabels(frameworks)
    ax.set_ylabel("Score")
    ax.set_title("Score Distribution: All Runs per Framework\n(including errors as 0, diamond = mean)")
    ax.set_ylim(-3, 80)
    ax.axhline(y=73, color="gray", linestyle="--", alpha=0.3, label="Max score (73)")
    ax.axhline(y=0, color="red", linestyle=":", alpha=0.3, label="Error/failure")
    ax.legend(loc="upper left")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "all_runs_boxplot.png")
    plt.close(fig)


def chart_11_cost_normalized_ranking():
    """Horizontal bar: composite bang-for-buck ranking = score / (tokens_k * latency_s)."""
    fig, ax = plt.subplots(figsize=(10, 7))

    combos = []
    for fw in frameworks:
        for m in models:
            s = scores[fw][m]
            lat = latency[fw][m]
            tok = tokens[fw][m]
            if lat is None or tok is None or tok == 0 or lat == 0:
                continue
            composite = s / ((tok / 1000) * lat) * 1000  # scale for readability
            label = f"{fw} + {m}"
            combos.append((label, composite, fw, m))

    combos.sort(key=lambda x: x[1])
    labels = [c[0] for c in combos]
    values = [c[1] for c in combos]
    colors = [COLORS[c[2]] for c in combos]

    bars = ax.barh(labels, values, color=colors, edgecolor="black", linewidth=0.5)
    for bar, val in zip(bars, values):
        ax.text(bar.get_width() + 0.3, bar.get_y() + bar.get_height() / 2,
                f"{val:.1f}", va="center", fontsize=9, fontweight="bold")

    ax.set_xlabel("Composite Score: score / (tokens_k × latency_s) × 1000\n(higher = more efficient)")
    ax.set_title("Cost-Normalized Ranking: Bang for Your Buck")
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "cost_normalized_ranking.png")
    plt.close(fig)


if __name__ == "__main__":
    chart_1_grouped_bar_scores()
    chart_2_latency_heatmap()
    chart_3_per_turn_latency()
    chart_4_token_usage()
    chart_5_efficiency_scatter()
    chart_6_framework_sensitivity()
    chart_7_reliability_matrix()
    chart_8_score_efficiency()
    chart_9_radar()
    chart_10_all_runs_boxplot()
    chart_11_cost_normalized_ranking()
    print(f"Charts saved to {OUTPUT_DIR}/")
    for f in sorted(OUTPUT_DIR.glob("*.png")):
        print(f"  {f.name}")
