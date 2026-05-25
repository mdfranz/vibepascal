import os
import re
from dataclasses import dataclass, field
from typing import Optional, Protocol


class _GameState(Protocol):
    room_id: int
    room_name: str
    turns: int
    score: int
    thirst: int
    is_playing: bool
    is_riding: bool
    has_water: bool
    horse_saddled: bool
    inventory: Optional[list[str]]


_RISKY_LAST_TURN_COMMANDS = {
    "DRINK",
    "FILL",
    "WATER",
    "LOOK",
    "INVENTORY",
    "I",
    "SEARCH",
    "SCORE",
}

_DESERT_ROOMS = {
    "The Desert Edge",
    "Dry Wash",
    "Howling Desert",
    "Butte",
    "Hidden Stream",
}

_MOVE_COMMANDS = ("NORTH", "EAST", "SOUTH", "WEST")


def sanitize_command(command: str) -> str:
    """Best-effort cleanup for model output; returns a single-line uppercase command."""
    cmd = (command or "").strip()
    if not cmd:
        return ""

    if "<thought>" in cmd:
        cmd = cmd.split("</thought>")[-1].strip()

    cmd = cmd.strip()

    # Strip markdown code fences: ```lang\nCOMMAND\n``` (must run before strip("`"))
    if cmd.startswith("```"):
        lines = cmd.splitlines()
        inner = [l for l in lines[1:] if l.strip() != "```"]
        cmd = "\n".join(inner).strip()

    cmd = cmd.strip("`")

    if "{" in cmd:
        match = re.search(r'"command"\s*:\s*"([^"]+)"', cmd, re.IGNORECASE)
        if match:
            cmd = match.group(1)
        else:
            match = re.search(r'command\s*:\s*([^,\}\n]+)', cmd, re.IGNORECASE)
            if match:
                cmd = match.group(1).strip().strip('"').strip("'")
            else:
                match = re.search(r':\s*"([^"]+)"', cmd)
                if match:
                    cmd = match.group(1)

    cmd = cmd.splitlines()[0].strip()
    cmd = cmd.upper()

    for ch in [".", ",", "!", "?", ";", ":", "(", ")", "[", "]", "{", "}", "\"", "'"]:
        cmd = cmd.replace(ch, "")

    cmd = cmd.strip()

    if cmd.startswith("INSPECT "):
        cmd = cmd.replace("INSPECT ", "EXAMINE ", 1)
    if cmd == "INSPECT":
        cmd = "LOOK"
    if cmd.startswith("GET "):
        cmd = cmd.replace("GET ", "TAKE ", 1)
    if cmd.startswith("GO "):
        cmd = cmd.replace("GO ", "", 1)
    if cmd in {"OUT", "EXIT", "LEAVE"}:
        cmd = "SOUTH"
    if "USE KEY" in cmd or "UNLOCK" in cmd:
        cmd = "OPEN BOX"
    if "USE SADDLE" in cmd:
        cmd = "SADDLE HORSE"
    if "DRINK WATER" in cmd:
        cmd = "DRINK"
    if "FILL CANTEEN" in cmd:
        cmd = "FILL"
    if "WATER HORSE" in cmd:
        cmd = "WATER"

    return cmd


def output_signature(text: str) -> str:
    return " ".join((text or "").strip().split()).lower()


@dataclass(frozen=True)
class StateSignature:
    room_id: int
    turns: int
    score: int
    thirst: int
    is_riding: bool
    has_water: bool
    horse_saddled: bool
    inventory_count: int

    @staticmethod
    def from_state(state: _GameState) -> "StateSignature":
        inv = state.inventory or []
        return StateSignature(
            room_id=int(state.room_id),
            turns=int(state.turns),
            score=int(state.score),
            thirst=int(state.thirst),
            is_riding=bool(state.is_riding),
            has_water=bool(state.has_water),
            horse_saddled=bool(state.horse_saddled),
            inventory_count=len(inv),
        )


@dataclass
class LoopBreaker:
    history_limit: int = 6
    repeat_threshold: int = 2
    _events: list[tuple[str, StateSignature, str]] = field(default_factory=list)
    _explore_index: int = 0

    def observe(self, *, command: str, state: _GameState, output_text: str) -> None:
        sig = StateSignature.from_state(state)
        out_sig = output_signature(output_text)
        self._events.append((command, sig, out_sig))
        if len(self._events) > self.history_limit:
            self._events = self._events[-self.history_limit :]

    def _choose_explore_move(self, *, last_command: str) -> str:
        for i in range(len(_MOVE_COMMANDS)):
            idx = (self._explore_index + i) % len(_MOVE_COMMANDS)
            cmd = _MOVE_COMMANDS[idx]
            if cmd != last_command:
                self._explore_index = idx + 1
                return cmd
        self._explore_index += 1
        return "NORTH"

    def should_break_loop(self, *, proposed_command: str, state: _GameState, output_text: str) -> bool:
        if not self._events:
            return False
        last_cmd, last_sig, last_out = self._events[-1]
        cur_sig = StateSignature.from_state(state)
        cur_out = output_signature(output_text)
        if proposed_command != last_cmd:
            return False
        if cur_sig != last_sig:
            return False
        if cur_out != last_out:
            return False

        repeats = 1
        for i in range(len(self._events) - 2, -1, -1):
            cmd_i, sig_i, out_i = self._events[i]
            if cmd_i == proposed_command and sig_i == cur_sig and out_i == cur_out:
                repeats += 1
            else:
                break
        return repeats >= self.repeat_threshold

    def break_command(self, *, last_command: str) -> str:
        return self._choose_explore_move(last_command=last_command)


@dataclass
class CommandPolicy:
    history_limit: int = 6
    repeat_threshold: int = 2
    max_llm_calls_multiplier: int = 3
    loop_breaker: LoopBreaker = field(init=False)
    last_state: _GameState | None = None
    last_output_text: str = ""
    last_command: str = ""

    def __post_init__(self) -> None:
        self.loop_breaker = LoopBreaker(
            history_limit=self.history_limit,
            repeat_threshold=self.repeat_threshold,
        )

    @staticmethod
    def from_env() -> "CommandPolicy":
        def _int(name: str, default: int) -> int:
            try:
                return int(os.environ.get(name, str(default)))
            except Exception:
                return default

        return CommandPolicy(
            history_limit=_int("MCP_HISTORY_LIMIT", 6),
            repeat_threshold=_int("MCP_LOOP_REPEAT_THRESHOLD", 2),
            max_llm_calls_multiplier=_int("MCP_MAX_LLM_CALLS_MULTIPLIER", 3),
        )

    def observe(self, *, command: str, state: _GameState, output_text: str) -> None:
        self.last_state = state
        self.last_output_text = output_text or ""
        self.last_command = command
        self.loop_breaker.observe(command=command, state=state, output_text=output_text)

    def rewrite(self, *, proposed_command: str, state: _GameState, max_turns: int) -> str:
        cmd = sanitize_command(proposed_command)
        if not cmd:
            cmd = "LOOK"

        remaining_turns = max(0, int(max_turns) - int(state.turns))

        # Bootstrap: Main Street often does not show exits; break the LOOK local minimum deterministically.
        if state.room_id == 1 and state.turns == 0 and cmd in {"LOOK", "INVENTORY", "I", "SEARCH"}:
            cmd = "NORTH"

        # If mounted and a snake is present, don't waste a turn freezing/looking; just move.
        if (
            state.is_riding
            and ("RATTLESNAKE" in (self.last_output_text or "").upper() or "SNAKE" in (self.last_output_text or "").upper())
            and cmd in {"FREEZE", "LOOK", "INVENTORY", "I", "SEARCH"}
        ):
            cmd = self.loop_breaker.break_command(last_command=self.last_command or cmd)

        # Late-game stall prevention: avoid low-value actions on the very last turn when not critical.
        if remaining_turns == 1 and cmd.split(" ", 1)[0] in _RISKY_LAST_TURN_COMMANDS and int(state.thirst) <= 15:
            if state.room_name in _DESERT_ROOMS or "DESERT" in (state.room_name or "").upper():
                cmd = "SOUTH"
            else:
                cmd = self.loop_breaker.break_command(last_command=self.last_command or cmd)

        # Loop break: same command at identical state/output.
        if self.loop_breaker.should_break_loop(proposed_command=cmd, state=state, output_text=self.last_output_text):
            cmd = self.loop_breaker.break_command(last_command=cmd)

        return cmd

