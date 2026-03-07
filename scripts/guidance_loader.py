from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Optional


@dataclass(frozen=True)
class GuidanceConfig:
    path: Optional[Path]
    text: Optional[str]


def _repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def resolve_guidance_path(value: Optional[str]) -> Optional[Path]:
    if not value:
        return None

    guidance_map = {
        "full": Path("data/guidance_full.txt"),
        "medium": Path("data/guidance_medium.txt"),
        "minimal": Path("data/guidance_minimal.txt"),
    }

    v = value.strip()
    mapped = guidance_map.get(v.lower())
    if mapped is not None:
        return (_repo_root() / mapped).resolve()

    p = Path(v)
    if not p.is_absolute():
        p = (_repo_root() / p).resolve()
    return p


def load_guidance(value: Optional[str]) -> GuidanceConfig:
    path = resolve_guidance_path(value)
    if path is None:
        return GuidanceConfig(path=None, text=None)

    if not path.exists():
        raise FileNotFoundError(f"Guidance file not found: {path}")

    text = path.read_text(encoding="utf-8").strip()
    return GuidanceConfig(path=path, text=text or None)

