import os
import time
import logging
import sys
import subprocess
import selectors
import asyncio
from typing import Optional
from dotenv import load_dotenv

from agno.agent import Agent
from agno.models.openai import OpenAIChat
from agno.models.anthropic import Claude
from agno.models.google import Gemini
from agno.models.ollama import Ollama

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
LOG_FILE = f"logs/agno_client-{EPOCH}.log"

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

async def run_agno_agent(level: str, model_name: str, delay: int, max_turns: int):
    logger.info(f"--- Agno Client Starting (Model: {model_name}) ---")
    
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
        # Initial look
        initial_output = game.start(turns=max_turns)
        logger.info(f"\n[STARTING GAME]\n{initial_output}")
        
        # Instantiate the model
        if "claude" in model_name.lower():
            model = Claude(id=model_name)
        elif "gemini" in model_name.lower():
            model = Gemini(id=model_name)
        elif "ollama" in model_name.lower():
            clean_model = model_name
            for prefix in ["ollama:", "ollama/"]:
                if clean_model.lower().startswith(prefix):
                    clean_model = clean_model[len(prefix):]
            model = Ollama(id=clean_model, host=os.environ.get("OLLAMA_HOST", "http://localhost:11434"))
        else:
            model = OpenAIChat(id=model_name)
        
        # Instantiate the agent
        agent = Agent(
            model=model,
            name="DustwoodAgnoAdventurer",
            description=(
                f"{system_instruction}\n\n"
                "You are an expert adventurer playing 'Echoes of Dustwood'.\n"
                "You interact with the game via the 'play_command' tool.\n"
                "Your goal is to survive, explore, and increase your score.\n"
                "LATE GAME: When you have only a few turns left, use the 'SCORE' command to check your final progress.\n"
                "Keep your responses concise. When you receive game output, decide on the next command and call 'play_command'."
            ),
            tools=[play_command],
            markdown=True
        )
        
        # Start the interaction loop
        prompt = f"STARTING STATE: {initial_output}\nWhat is your first command?"
        
        # Agno's agent.run or agent.print_response
        # For programmatic access we use agent.run()
        provider_timer = Timer.start_new()
        response = agent.run(prompt)
        latency_ms = provider_timer.elapsed_ms()
        metrics = getattr(response, "metrics", None)
        log_kv(
            obs_logger,
            event="provider_call",
            client="agno",
            model_provider=getattr(response, "model_provider", None),
            model=getattr(response, "model", None) or model_name,
            latency_ms=latency_ms,
            input_tokens=getattr(metrics, "input_tokens", None),
            output_tokens=getattr(metrics, "output_tokens", None),
            total_tokens=getattr(metrics, "total_tokens", None),
            reasoning_tokens=getattr(metrics, "reasoning_tokens", None),
            tool_calls=(len(getattr(response, "tools", []) or []) if getattr(response, "tools", None) is not None else None),
            prompt=(format_payload(prompt) if provider_payload_logging_enabled() else None),
            response=(format_payload(getattr(response, "content", None)) if provider_payload_logging_enabled() else None),
        )
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{response.content}")

    finally:
        game.stop()

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else MAX_TURNS
    
    asyncio.run(run_agno_agent(level, model, delay, max_turns))
