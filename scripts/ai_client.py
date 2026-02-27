import os
import time
import logging
import sys
import subprocess
import selectors
from typing import Optional, Literal
from pydantic import BaseModel, Field
from dotenv import load_dotenv
from pydantic_ai import Agent, RunContext
from pydantic_ai.models import KnownModelName

# Load environment variables from .env if present
load_dotenv()

# --- Configuration ---
BINARY_PATH = "bin/dustwood"
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-3-flash-preview"
LOG_FILE = "logs/ai_client.log"
TURN_DELAY = 1

# --- Setup Logging ---
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

# Ensure logs directory exists
os.makedirs("logs", exist_ok=True)

# Detailed File Logger
file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s [%(levelname)s] %(message)s'))
logger.addHandler(file_handler)

# Clean Console Logger
console_handler = logging.StreamHandler()
console_handler.setLevel(logging.INFO)
console_handler.setFormatter(logging.Formatter('%(message)s'))
logger.addHandler(console_handler)

# --- Game Interface ---

class DustwoodGame:
    """Manages the lifecycle of the headless Pascal game process."""
    def __init__(self, binary_path: str = BINARY_PATH, prompt: str = "> "):
        self.binary_path = binary_path
        self.prompt = prompt
        self.prompt_bytes = prompt.encode()
        self.proc: Optional[subprocess.Popen] = None

    def start(self, seed: Optional[int] = None, turns: Optional[int] = None) -> str:
        """Starts the game process and returns the initial output."""
        if not os.path.exists(self.binary_path):
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
                    if self.proc.poll() is not None: # Process exited
                        break
                    continue
                for key, _ in events:
                    # Use os.read for non-blocking feel with bufsize=0
                    chunk = os.read(key.fileobj.fileno(), 4096)
                    if not chunk:
                        break
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
        if not self.proc or not self.proc.stdin:
            return "Error: Game not running."
        
        logger.debug(f"Sending command: {command}")
        self.proc.stdin.write((command + "\n").encode())
        self.proc.stdin.flush()
        
        output = self._read_until_prompt()
        # Strip the prompt from the end for cleaner display
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

# --- AI Agent Models ---

class CommandResponse(BaseModel):
    """The structured response format for the AI agent."""
    command: str = Field(..., description="A single valid game command (e.g., 'NORTH', 'TAKE CANTEEN', 'LOOK')")
    reasoning: str = Field(..., description="Brief explanation of why this command was chosen")

class GameDeps:
    """Dependencies for the Pydantic AI agent."""
    def __init__(self, game: DustwoodGame):
        self.game = game

# --- AI Logic ---

ALLOWED_VERBS = {
    "N", "NORTH", "S", "SOUTH", "E", "EAST", "W", "WEST",
    "LOOK", "L", "EXAMINE", "X", "SEARCH",
    "HELP", "H", "?", "INVENTORY", "I",
    "DRINK", "FILL", "WATER", "LIGHT", "FIX", "SADDLE",
    "PUT", "CLIMB", "SAVE", "LOAD", "SCORE",
    "TAKE", "GET", "DROP",
    "QUIT", "Q",
    "MOUNT", "RIDE", "DISMOUNT",
    "OPEN", "SHOOT", "KILL", "FREEZE", "WAIT", "CHECK",
}

def sanitize_command(command: str) -> str:
    """Cleans up the command string from the AI."""
    cmd = command.strip().upper()
    # Remove common prefixes/suffixes or punctuation that AI might add
    for ch in [".", ",", "!", "?", ";", ":", "`"]:
        cmd = cmd.replace(ch, "")
    # Take only the first line if it leaked multiple lines
    cmd = cmd.split("\n")[0].strip()
    # Handle common synonyms
    if cmd.startswith("INSPECT "): cmd = cmd.replace("INSPECT ", "EXAMINE ")
    if cmd == "INSPECT": cmd = "LOOK"
    return cmd

def is_valid_command(command: str) -> bool:
    """Verifies if the command starts with a valid verb."""
    if not command:
        return False
    verb = command.split(" ", 1)[0]
    return verb in ALLOWED_VERBS

def setup_ollama(model_name: str):
    """Configures Ollama environment variables if needed."""
    if model_name.startswith('ollama:'):
        # Default to localhost if OLLAMA_HOST is not set
        host = os.environ.get('OLLAMA_HOST', 'http://localhost:11434')
        
        # Ensure the protocol is present
        if not host.startswith(('http://', 'https://')):
            host = 'http://' + host
        
        base_url = host.rstrip('/')
        if not base_url.endswith('/v1'):
            base_url += '/v1'
        
        # Pydantic AI 1.x Ollama provider expects the base URL.
        # If it uses the OpenAI compatibility layer internally, it often handles the /v1 path itself.
        os.environ['OLLAMA_BASE_URL'] = base_url
        logger.info(f"Ollama config: OLLAMA_BASE_URL set to {base_url}")

def ai_play(guidance_file: str, model_name: str, delay: int, max_turns: int):
    """Main loop for the AI agent to play the game."""
    if not os.path.exists(guidance_file):
        logger.error(f"Guidance file not found: {guidance_file}")
        return

    setup_ollama(model_name)

    with open(guidance_file, 'r') as f:
        system_instruction = f.read().strip()

    game = DustwoodGame()
    deps = GameDeps(game)
    
    agent = Agent(
        model_name,
        deps_type=GameDeps,
        output_type=CommandResponse,
        instructions=(
            f"{system_instruction}\n\n"
            "You are an expert adventurer. Provide your next action as a single command. "
            "Valid verbs include: " + ", ".join(sorted(ALLOWED_VERBS)) + ".\n"
            "Always respond in the specified JSON format."
        )
    )

    logger.info(f"--- AI Player starting (Model: {model_name}) ---")
    
    # Optional seed for reproducibility
    seed = os.environ.get("GAME_SEED")
    last_output = game.start(seed=int(seed) if seed else None, turns=max_turns)
    
    logger.info(f"\n[STARTING GAME]\n{last_output.strip()}")

    turns = 0
    message_history = []
    last_command = ""
    try:
        while turns < max_turns:
            turns += 1
            logger.info(f"\n--- Turn {turns} ---")
            
            # Prepare context for the agent
            context = f"Current Game Output:\n{last_output}\n\nWhat is your next move?"
            if last_command:
                context = f"Your last command was '{last_command}'.\n\n" + context

            # Use retry logic for robustness against model hallucinations
            try:
                result = agent.run_sync(
                    context,
                    deps=deps,
                    message_history=message_history
                )
                choice = result.output
                command = sanitize_command(choice.command)
                logger.info(f"AI THINKING: {choice.reasoning}")
                message_history = result.new_messages()
            except Exception as e:
                logger.warning(f"Agent failed to provide structured output: {e}. Attempting fallback...")
                # Fallback to simple string if structured fails
                fallback_agent = Agent(model_name, instructions=system_instruction)
                result = fallback_agent.run_sync(f"The previous output was:\n{last_output}\n\nGive me ONE command.")
                command = sanitize_command(result.output)

            if not is_valid_command(command):
                logger.warning(f"Invalid command generated: '{command}'. Defaulting to 'LOOK'.")
                command = "LOOK"
            
            # Simple loop detection
            if command == last_command and "🤷" in last_output:
                logger.warning(f"Command '{command}' repeated but failed. Forcing exploration.")
                # We don't force it here, but we could add a system message to the history
                pass

            logger.info(f"AI COMMAND: {command}")
            last_command = command
            
            if command in {"QUIT", "Q"}:
                logger.info("AI decided to quit.")
                break

            last_output = game.send_command(command)
            logger.info(f"GAME RESPONSE:\n{last_output.strip()}")

            if "GAME OVER" in last_output or "Final score" in last_output:
                logger.info("\n--- Game Ended ---")
                break

            time.sleep(delay)
    finally:
        game.stop()
        logger.info("AI session complete.")

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else 50
    
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }

    file_to_use = guidance_map.get(level, "data/guidance_full.txt")
    ai_play(file_to_use, model, delay, max_turns)
