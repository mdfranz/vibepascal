import os
import time
import logging
import sys
import subprocess
import selectors
import asyncio
from typing import Optional
from dotenv import load_dotenv

from agent_framework import Agent
from agent_framework.openai import OpenAIChatClient
from agent_framework.anthropic import AnthropicClient
from agent_framework.ollama import OllamaChatClient

from llm_observability import enable_http_debug_logging, http_debug_logging_enabled
from llm_observability import Timer, format_payload, log_kv, provider_payload_logging_enabled

# Load environment variables
load_dotenv()

# --- Configuration ---
BINARY_PATH = os.environ.get("DUSTWOOD_BIN", "bin/dustwood")
DEFAULT_MODEL = "gpt-5-mini"
TURN_DELAY = 1
MAX_TURNS = 25

# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/ms_agent_client-{EPOCH}.log"

# --- Setup Logging ---
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
os.makedirs("logs", exist_ok=True)

file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s [%(levelname)s] %(message)s'))
logger.addHandler(file_handler)

console_handler = logging.StreamHandler()
console_handler.setLevel(logging.INFO)
console_handler.setFormatter(logging.Formatter('%(message)s'))
logger.addHandler(console_handler)

# Optional: very verbose low-level HTTP logging for provider calls (httpx/httpcore/h11/h2)
if http_debug_logging_enabled():
    enable_http_debug_logging(handlers=[file_handler])
else:
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("httpcore").setLevel(logging.WARNING)

# Global variables for delay
global_delay = TURN_DELAY

# Observability logger: file-only (no console spam)
obs_logger = logging.getLogger(__name__ + ".obs")
obs_logger.setLevel(logging.DEBUG)
if file_handler not in obs_logger.handlers:
    obs_logger.addHandler(file_handler)
obs_logger.propagate = False

# --- Provider Observability (non-MCP) ---

def _serialize_messages(messages: object) -> object:
    try:
        return [getattr(m, "to_dict")() if hasattr(m, "to_dict") else str(m) for m in (messages or [])]  # type: ignore[arg-type]
    except Exception:
        return str(messages)


def _extract_tool_calls_from_response(response: object) -> list[dict[str, object]]:
    tool_calls: list[dict[str, object]] = []
    try:
        for msg in getattr(response, "messages", []) or []:
            for content in getattr(msg, "contents", []) or []:
                ctype = getattr(content, "type", None)
                if ctype in {
                    "function_call",
                    "mcp_server_tool_call",
                    "shell_tool_call",
                    "code_interpreter_tool_call",
                    "image_generation_tool_call",
                }:
                    tool_calls.append(
                        {
                            "type": ctype,
                            "call_id": getattr(content, "call_id", None),
                            "tool_name": getattr(content, "tool_name", None),
                            "server_name": getattr(content, "server_name", None),
                            "arguments": getattr(content, "arguments", None),
                        }
                    )
    except Exception:
        return []
    return tool_calls


class LoggingChatClient:
    """Wrap a chat client to log provider calls, usage, tool calls, and latency."""

    def __init__(self, inner: object, *, client_name: str, default_model_id: str):
        self._inner = inner
        self._client_name = client_name
        self._default_model_id = default_model_id

    @property
    def additional_properties(self) -> dict[str, object]:
        return getattr(self._inner, "additional_properties", {})

    def get_response(self, messages: object, *, stream: bool = False, options: object = None, **kwargs: object):
        request_timer = Timer.start_new()
        request_payload = _serialize_messages(messages) if provider_payload_logging_enabled() else None

        try:
            inner_result = self._inner.get_response(messages, stream=stream, options=options, **kwargs)  # type: ignore[attr-defined]
        except Exception as e:
            log_kv(
                obs_logger,
                level="error",
                event="provider_call",
                client="ms_agent",
                provider=self._client_name,
                model=self._default_model_id,
                latency_ms=request_timer.elapsed_ms(),
                request=(format_payload(request_payload) if request_payload is not None else None),
                error=str(e),
            )
            raise

        if stream and hasattr(inner_result, "with_result_hook"):
            def _hook(final_response: object):
                usage_details = getattr(final_response, "usage_details", None)
                tool_calls = _extract_tool_calls_from_response(final_response)
                log_kv(
                    obs_logger,
                    event="provider_call",
                    client="ms_agent",
                    provider=self._client_name,
                    model=getattr(final_response, "model_id", None) or self._default_model_id,
                    latency_ms=request_timer.elapsed_ms(),
                    usage=(format_payload(usage_details) if (usage_details is not None and provider_payload_logging_enabled()) else None),
                    tool_call_count=len(tool_calls) if tool_calls else 0,
                    tool_calls=(format_payload(tool_calls) if (tool_calls and provider_payload_logging_enabled()) else None),
                    request=(format_payload(request_payload) if request_payload is not None else None),
                    response=(format_payload(getattr(final_response, "text", None)) if provider_payload_logging_enabled() else None),
                )
                return final_response

            return inner_result.with_result_hook(_hook)

        async def _await_and_log():
            try:
                final_response = await inner_result
            except Exception as e:
                log_kv(
                    obs_logger,
                    level="error",
                    event="provider_call",
                    client="ms_agent",
                    provider=self._client_name,
                    model=self._default_model_id,
                    latency_ms=request_timer.elapsed_ms(),
                    request=(format_payload(request_payload) if request_payload is not None else None),
                    error=str(e),
                )
                raise

            usage_details = getattr(final_response, "usage_details", None)
            tool_calls = _extract_tool_calls_from_response(final_response)
            log_kv(
                obs_logger,
                event="provider_call",
                client="ms_agent",
                provider=self._client_name,
                model=getattr(final_response, "model_id", None) or self._default_model_id,
                latency_ms=request_timer.elapsed_ms(),
                usage=(format_payload(usage_details) if (usage_details is not None and provider_payload_logging_enabled()) else None),
                tool_call_count=len(tool_calls) if tool_calls else 0,
                tool_calls=(format_payload(tool_calls) if (tool_calls and provider_payload_logging_enabled()) else None),
                request=(format_payload(request_payload) if request_payload is not None else None),
                response=(format_payload(getattr(final_response, "text", None)) if provider_payload_logging_enabled() else None),
            )
            return final_response

        return _await_and_log()

# --- Game Interface ---
class DustwoodGame:
    """Manages the lifecycle of the headless game process."""
    def __init__(self, binary_path: str = BINARY_PATH, prompt: str = "> "):
        self.binary_path = binary_path
        self.prompt = prompt
        self.prompt_bytes = prompt.encode()
        self.proc: Optional[subprocess.Popen] = None

    def start(self, seed: Optional[int] = None, turns: Optional[int] = None) -> str:
        """Starts the game process and returns the initial output."""
        if not os.path.exists(self.binary_path):
            # Try relative to script location if not found
            script_dir = os.path.dirname(os.path.abspath(__file__))
            rel_path = os.path.join(script_dir, "..", self.binary_path)
            if os.path.exists(rel_path):
                self.binary_path = rel_path
            else:
                raise FileNotFoundError(f"Game binary not found at {self.binary_path}. Run 'make build' first.")

        cmd = [self.binary_path, "--headless"]
        if seed is not None:
            cmd.extend(["--seed", str(seed)])
        if turns is not None:
            cmd.extend(["--turns", str(turns)])
        
        logger.debug(f"Starting game: {' '.join(cmd)}")
        self.proc = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=False,
            bufsize=0
        )
        return self._read_until_prompt()

    def _read_until_prompt(self, timeout: float = 5.0) -> str:
        """Reads output from the process until the prompt is encountered or timeout occurs."""
        if not self.proc or not self.proc.stdout:
            return "Error: Game not running."
        
        buf = bytearray()
        end_time = time.time() + timeout
        sel = selectors.DefaultSelector()
        sel.register(self.proc.stdout, selectors.EVENT_READ)
        try:
            while time.time() < end_time:
                events = sel.select(timeout=0.1)
                if not events:
                    if self.proc.poll() is not None:
                        break
                    continue
                for key, _ in events:
                    chunk = os.read(key.fileobj.fileno(), 4096)
                    if not chunk:
                        return buf.decode(errors='replace')
                    buf.extend(chunk)
                    if buf.endswith(self.prompt_bytes):
                        return buf.decode(errors='replace')
                if not events and self.proc.poll() is not None:
                    break
        finally:
            sel.close()
        
        output = buf.decode(errors='replace')
        return output

    def send_command(self, command: str) -> str:
        """Sends a command to the game and returns the resulting output."""
        if not self.proc or not self.proc.stdin or self.proc.poll() is not None:
            return "Error: Game process has terminated (GAME OVER)."
        
        logger.debug(f"Sending command: {command}")
        try:
            self.proc.stdin.write((command + "\n").encode())
            self.proc.stdin.flush()
        except BrokenPipeError:
            return "Error: Game process has terminated (GAME OVER)."
        
        time.sleep(0.05)
        output = self._read_until_prompt()
        if output.endswith(self.prompt):
            output = output[:-len(self.prompt)]
        return output

    def stop(self):
        """Terminates the game process."""
        if self.proc:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=2)
            except subprocess.TimeoutExpired:
                self.proc.kill()
            self.proc = None

# Create a global game instance
game = DustwoodGame()

def play_command(command: str) -> str:
    """
    Sends a text command to the game and returns the narrative output.
    Use this tool to interact with the game world.
    
    Args:
        command: The action you want to take (e.g., 'NORTH', 'TAKE CANTEEN', 'LOOK')
    """
    logger.info(f"Agent executing command: {command}")
    if global_delay > 0:
        time.sleep(global_delay)
    result = game.send_command(command)
    logger.info(f"Game output: {result}")
    return result

async def run_ms_agent(level: str, model_name: str, delay: int, max_turns: int):
    logger.info(f"--- Microsoft Agent Framework Client Starting (Model: {model_name}) ---")
    
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }
    guidance_file = guidance_map.get(level, "data/guidance_full.txt")
    
    if not os.path.exists(guidance_file):
        logger.error(f"Guidance file not found: {guidance_file}")
        return

    with open(guidance_file, 'r') as f:
        system_instruction = f.read().strip()

    global global_delay
    global_delay = delay

    try:
        # Initial look, pass max_turns to game binary
        initial_output = game.start(turns=max_turns)
        logger.info(f"\n[STARTING GAME]\n{initial_output}")
        
        # Instantiate the client
        if "claude" in model_name.lower():
            client = AnthropicClient(model_id=model_name)
            client = LoggingChatClient(client, client_name="anthropic", default_model_id=model_name)
        elif "gemini" in model_name.lower():
            # Use Google's OpenAI-compatible endpoint
            api_key = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
            client = OpenAIChatClient(
                model_id=model_name,
                api_key=api_key,
                base_url="https://generativelanguage.googleapis.com/v1beta/openai/"
            )
            client = LoggingChatClient(client, client_name="google", default_model_id=model_name)
        elif "ollama" in model_name.lower():
            # Robustly remove 'ollama:' or 'ollama/' prefix if present
            clean_model = model_name
            for prefix in ["ollama:", "ollama/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix):]
            
            client = OllamaChatClient(
                model_id=clean_model,
                host=os.environ.get("OLLAMA_HOST", "http://localhost:11434")
            )
            client = LoggingChatClient(client, client_name="ollama", default_model_id=clean_model)
        else:
            client = OpenAIChatClient(model_id=model_name)
            client = LoggingChatClient(client, client_name="openai", default_model_id=model_name)
        
        # Instantiate the agent
        agent = Agent(
            client=client,
            name="DustwoodAdventurer",
            instructions=(
                f"{system_instruction}\n\n"
                "You are an expert adventurer playing 'Echoes of Dustwood'.\n"
                "You interact with the game via the 'play_command' tool.\n"
                "Your goal is to survive, explore, and increase your score.\n"
                "LATE GAME: When you have only a few turns left, use the 'SCORE' command to check your final progress.\n"
                "Keep your responses concise. When you receive game output, decide on the next command and call 'play_command'."
            ),
            tools=[play_command]
        )
        
        # Start the interaction loop
        prompt = f"STARTING STATE: {initial_output}\nWhat is your first command?"
        
        # Use agent.run which handles tool calling loop internally
        response = await agent.run(prompt)
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{response.text}")

    finally:
        game.stop()

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS
    
    asyncio.run(run_ms_agent(level, model, delay, max_turns))
