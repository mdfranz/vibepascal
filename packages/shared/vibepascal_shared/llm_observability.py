import json
import logging
import os
import re
import sys
import time
from dataclasses import dataclass
from typing import Any, Iterable, Mapping, MutableMapping, Optional


_DEFAULT_MAX_CHARS = 20_000


def _env_bool(name: str, default: bool) -> bool:
    val = os.environ.get(name)
    if val is None:
        return default
    return val.strip().lower() not in {"0", "false", "no", "off", ""}


def _env_int(name: str, default: int) -> int:
    val = os.environ.get(name)
    if val is None:
        return default
    try:
        return int(val)
    except ValueError:
        return default


def console_logging_enabled() -> bool:
    """Whether scripts should attach a console handler.

    Default is off to keep logs in file destinations only.
    """
    return _env_bool("LOG_CONSOLE", False)


def game_console_enabled() -> bool:
    """Whether to print basic game output to stdout.

    Default is on for interactive visibility, while keeping structured logs in file handlers.
    """
    return _env_bool("GAME_CONSOLE", True)


def print_game(text: str) -> None:
    if not text:
        return
    sys.stdout.write(text.rstrip() + "\n")
    sys.stdout.flush()


def http_debug_logging_enabled() -> bool:
    """Enable very verbose HTTP client logging (httpx/httpcore/h11).

    Intended for debugging provider calls. Default is off.
    """
    return _env_bool("LOG_HTTP", False)


def enable_http_debug_logging(*, handlers: Iterable[logging.Handler]) -> None:
    """Attach handlers and set DEBUG level for low-level HTTP logging."""
    handler_list = list(handlers)
    for logger_name in ("httpx", "httpcore", "h11", "h2"):
        http_logger = logging.getLogger(logger_name)
        http_logger.setLevel(logging.DEBUG)
        for h in handler_list:
            if h not in http_logger.handlers:
                http_logger.addHandler(h)
        # Prevent double logging via root
        http_logger.propagate = False


def _escape_newlines(s: str) -> str:
    return s.replace("\r\n", "\n").replace("\r", "\n").replace("\n", "\\n")


def truncate_text(text: str, max_chars: int) -> str:
    if max_chars <= 0:
        return ""
    if len(text) <= max_chars:
        return text
    return text[: max(0, max_chars - 20)] + f"...(truncated,len={len(text)})"


_SENSITIVE_KEYS = [
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GEMINI_API_KEY",
    "GOOGLE_API_KEY",
    "OLLAMA_API_KEY",
    "AZURE_OPENAI_API_KEY",
    "AZUREAI_API_KEY",
    "COHERE_API_KEY",
    "MISTRAL_API_KEY",
    "OPENROUTER_API_KEY",
    "TOGETHER_API_KEY",
    "DEEPSEEK_API_KEY",
    "XAI_API_KEY",
    "HF_TOKEN",
]


def redact_secrets(text: str) -> str:
    if not text:
        return text

    redacted = text

    # Authorization headers
    redacted = re.sub(
        r"(?im)^(authorization\s*:\s*)(bearer|basic)\s+([^\s]+)$",
        r"\1\2 ***REDACTED***",
        redacted,
    )

    # Bearer tokens inside JSON/logs
    redacted = re.sub(
        r"(?i)(bearer)\s+([a-z0-9_\-\.=]{10,})",
        r"\1 ***REDACTED***",
        redacted,
    )

    # Common key=value patterns
    for key in _SENSITIVE_KEYS:
        redacted = re.sub(
            rf"(?i)\b({re.escape(key)})\b\s*=\s*([^\s'\"\\]+|\".*?\"|'.*?')",
            r"\1=***REDACTED***",
            redacted,
        )
        redacted = re.sub(
            rf'(?i)("{re.escape(key)}"\s*:\s*)(".*?"|\'.*?\'|[^\s,}}]+)',
            r'\1"***REDACTED***"',
            redacted,
        )

    return redacted


def _try_to_dict(value: Any) -> Any:
    if value is None:
        return None
    if isinstance(value, (str, int, float, bool)):
        return value
    if isinstance(value, bytes):
        try:
            return value.decode("utf-8", errors="replace")
        except Exception:
            return repr(value)
    if isinstance(value, Mapping):
        return {str(k): _try_to_dict(v) for k, v in value.items()}
    if isinstance(value, (list, tuple, set)):
        return [_try_to_dict(v) for v in value]
    for attr in ("to_dict", "model_dump", "dict"):
        if hasattr(value, attr):
            try:
                return _try_to_dict(getattr(value, attr)())
            except Exception:
                pass
    return repr(value)


def safe_json_dumps(value: Any) -> str:
    try:
        return json.dumps(_try_to_dict(value), ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    except Exception:
        return json.dumps(repr(value), ensure_ascii=False)


def log_kv(logger: Any, *, level: str = "info", **fields: Any) -> None:
    """Emit a single key=value log line, with JSON-encoded values.

    All values are JSON-encoded for safety (handles quotes/newlines).
    """
    parts: list[str] = []
    for k, v in fields.items():
        if v is None:
            continue
        parts.append(f"{k}={safe_json_dumps(v)}")
    line = " ".join(parts)
    line = redact_secrets(_escape_newlines(line))
    getattr(logger, level, logger.info)(line)


@dataclass
class Timer:
    start: float

    @classmethod
    def start_new(cls) -> "Timer":
        return cls(start=time.perf_counter())

    def elapsed_ms(self) -> int:
        return int((time.perf_counter() - self.start) * 1000)


def provider_payload_logging_enabled() -> bool:
    return _env_bool("LOG_PAYLOADS", True)


def max_payload_chars() -> int:
    return _env_int("LOG_MAX_CHARS", _DEFAULT_MAX_CHARS)


def format_payload(value: Any) -> str:
    s = safe_json_dumps(value)
    s = redact_secrets(s)
    return truncate_text(s, max_payload_chars())
