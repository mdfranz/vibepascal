import os
import time
import logging
import sys
import subprocess
import selectors
from typing import Optional, Literal, Union
from pydantic import BaseModel, Field
from dotenv import load_dotenv

# Strands Imports
from strands import Agent
from strands.models.litellm import LiteLLMModel
from strands.agent.conversation_manager import SlidingWindowConversationManager

# Load environment variables from .env if present
load_dotenv()

# --- Configuration ---
BINARY_PATH = "bin/dustwood"
# Map Pydantic AI style model names to LiteLLM style
DEFAULT_MODEL_ID = "gemini/gemini-3-flash-preview" 
# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/strands_ai_client-{EPOCH}.log"
TURN_DELAY = 1
MAX_OUTPUT_CHARS = 2000
MESSAGE_HISTORY_LIMIT = 4
REASONING_ENV_VAR = "AI_REASONING"
LOOP_REPEAT_LIMIT = 2
EXPLORE_COMMANDS = ["LOOK", "NORTH", "EAST", "SOUTH", "WEST"]

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

class CommandOnlyResponse(BaseModel):
    """Structured response format without reasoning to reduce tokens."""
    command: str = Field(..., description="A single valid game command (e.g., 'NORTH', 'TAKE CANTEEN', 'LOOK')")

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
    """Cleans up the command string from the AI, handling raw JSON or thinking blocks."""
    cmd = command.strip()
    
    # Handle thinking blocks
    if "<thought>" in cmd:
        cmd = cmd.split("</thought>")[-1].strip()
    
    # Check if the output is the whole JSON or a fragment
    if "{" in cmd:
        import re
        # Try to find "command": "VALUE"
        match = re.search(r'"command"\s*:\s*"([^"]+)"', cmd, re.IGNORECASE)
        if match:
            cmd = match.group(1)
        else:
            # Maybe it's unquoted? command: VALUE
            match = re.search(r'command\s*:\s*([^,\}\n]+)', cmd, re.IGNORECASE)
            if match:
                cmd = match.group(1).strip().strip('"').strip("'")
            else:
                # Last resort for JSON: just find the first string value
                match = re.search(r':\s*"([^"]+)"', cmd)
                if match:
                    cmd = match.group(1)

    cmd = cmd.upper()
    # Remove all common punctuation that isn't part of a command
    for ch in [".", ",", "!", "?", ";", ":", "`", "(", ")", "[", "]", "{", "}", "\"", "'"]:
        cmd = cmd.replace(ch, "")
    
    # Take only the first line
    cmd = cmd.split("\n")[0].strip()
    
    if cmd.startswith("INSPECT "): cmd = cmd.replace("INSPECT ", "EXAMINE ")
    if cmd == "INSPECT": cmd = "LOOK"
    return cmd

def is_valid_command(command: str) -> bool:
    """Verifies if the command starts with a valid verb."""
    if not command:
        return False
    verb = command.split(" ", 1)[0]
    return verb in ALLOWED_VERBS

def trim_output(text: str, max_chars: int = MAX_OUTPUT_CHARS) -> str:
    """Trims long game output to keep prompts compact."""
    if len(text) <= max_chars:
        return text
    return text[-max_chars:]

def output_signature(text: str) -> str:
    """Normalizes output to detect repeated non-progress states."""
    return " ".join(text.strip().split()).lower()

def next_explore_command(last_command: str, explore_index: int) -> tuple[str, int]:
    """Selects a simple exploratory command to break loops."""
    for i in range(len(EXPLORE_COMMANDS)):
        idx = (explore_index + i) % len(EXPLORE_COMMANDS)
        cmd = EXPLORE_COMMANDS[idx]
        if cmd != last_command:
            return cmd, idx + 1
    return "LOOK", explore_index + 1

def map_model_name(model_name: str) -> str:
    """Maps Pydantic AI model names to LiteLLM/Strands format."""
    if model_name.startswith("google-gla:"):
        # e.g. google-gla:gemini-1.5-flash -> gemini/gemini-1.5-flash
        return "gemini/" + model_name.replace("google-gla:", "")
    if model_name.startswith("ollama:"):
        # e.g. ollama:llama3 -> ollama/llama3
        return model_name.replace("ollama:", "ollama/")
    return model_name

def setup_ollama(model_id: str):
    """Configures Ollama environment variables if needed."""
    if model_id.startswith('ollama/'):
        host = os.environ.get('OLLAMA_HOST', 'http://localhost:11434')
        if not host.startswith(('http://', 'https://')):
            host = 'http://' + host
        base_url = host.rstrip('/')
        # LiteLLM often uses OLLAMA_API_BASE
        os.environ['OLLAMA_API_BASE'] = base_url
        logger.info(f"Ollama config: OLLAMA_API_BASE set to {base_url}")

def ai_play(guidance_file: str, raw_model_name: str, delay: int, max_turns: int):
    """Main loop for the AI agent to play the game using Strands."""
    if not os.path.exists(guidance_file):
        logger.error(f"Guidance file not found: {guidance_file}")
        return

    model_id = map_model_name(raw_model_name)
    setup_ollama(model_id)

    with open(guidance_file, 'r') as f:
        system_instruction = f.read().strip()

    game = DustwoodGame()
    
    reasoning_enabled = os.environ.get(REASONING_ENV_VAR, "0") not in {"0", "false", "False"}
    output_model = CommandResponse if reasoning_enabled else CommandOnlyResponse
    example_json = '{"command": "LOOK", "reasoning": "Gather current room details."}' if reasoning_enabled else '{"command": "LOOK"}'

    # Initialize Strands Agent
    # LiteLLMModel handles the interaction with LiteLLM
    client_args = {}
    if model_id.startswith("ollama/"):
        host = os.environ.get('OLLAMA_HOST', 'http://localhost:11434')
        if not host.startswith(('http://', 'https://')):
            host = 'http://' + host
        client_args["api_base"] = host.rstrip('/')
        logger.info(f"Configuring LiteLLM with api_base: {client_args['api_base']}")

    llm_model = LiteLLMModel(model_id=model_id, client_args=client_args)
    
    # SlidingWindowConversationManager handles history pruning automatically
    conv_manager = SlidingWindowConversationManager(window_size=MESSAGE_HISTORY_LIMIT)

    agent = Agent(
        model=llm_model,
        system_prompt=(
            f"{system_instruction}\n\n"
            "You are an expert adventurer. Provide your next action as a single command.\n"
            "CRITICAL: Use ONLY the valid verbs listed below. Do NOT attempt complex phrases or actions not in this list.\n"
            "Valid verbs: " + ", ".join(sorted(ALLOWED_VERBS)) + ".\n"
            "IMPORTANT: Your final response MUST be a valid JSON object. "
            "Do NOT include <thought> blocks or any text outside the JSON object.\n"
            f"Example JSON: {example_json}"
        ),
        conversation_manager=conv_manager
    )

    logger.info(f"--- Strands AI Player starting (Model: {model_id}) ---")
    
    seed = os.environ.get("GAME_SEED")
    last_output = game.start(seed=int(seed) if seed else None, turns=max_turns)
    
    logger.info(f"\n[STARTING GAME]\n{last_output.strip()}")

    turns = 0
    last_command = ""
    last_output_sig = ""
    repeat_count = 0
    explore_index = 0
    
    try:
        while turns < max_turns:
            turns += 1
            logger.info(f"\n--- Turn {turns} ---")
            
            trimmed_output = trim_output(last_output)
            
            # Create a very explicit schema description for the model
            if reasoning_enabled:
                schema_desc = "JSON object with keys: 'command' (string) and 'reasoning' (string)"
            else:
                schema_desc = "JSON object with key: 'command' (string)"

            context = (
                f"Current Game Output:\n{trimmed_output}\n\n"
                f"Last Command: {last_command or 'None'}\n\n"
                f"TASK: Provide your next action as a {schema_desc}.\n"
                "IMPORTANT: Output ONLY the JSON. No conversational text, no thought blocks, no markdown formatting."
            )

            choice = None
            try:
                # Use a manual retry/repair loop to mimic Pydantic AI's robustness
                for attempt in range(3):
                    try:
                        # Call agent without structured_output_model to avoid forced tool calls
                        # This allows reasoning models to "think" if they must, 
                        # and then we extract the JSON ourselves.
                        # AgentResult can be cast to string to get content
                        result = agent(context)
                        raw_text = str(result)
                        
                        # Extract JSON
                        import re
                        import json
                        # Find the last/most complete { } block
                        json_matches = list(re.finditer(r'\{.*\}', raw_text, re.DOTALL))
                        if not json_matches:
                            raise ValueError("No JSON block found in response")
                            
                        json_str = json_matches[-1].group()
                        data = json.loads(json_str)
                        
                        # Validate against our Pydantic model
                        choice = output_model(**data)
                        break
                    except Exception as e:
                        if attempt == 2: raise e
                        logger.warning(f"JSON Parse/Validation attempt {attempt+1} failed: {e}. Retrying...")
                        # On retry, be even more explicit
                        context += "\n\nCRITICAL: You must output valid JSON only. Your previous attempt failed validation."
                        time.sleep(1)
                
                if not choice:
                    raise ValueError("Failed to get valid choice after retries")

                command = sanitize_command(choice.command)
                if reasoning_enabled and hasattr(choice, "reasoning"):
                    logger.info(f"AI THINKING: {choice.reasoning}")
                
            except Exception as e:
                logger.error(f"Strands Agent failed to provide valid JSON: {e}. Final fallback to raw extraction.")
                # Last ditch effort: sanitize whatever the model said
                command = sanitize_command(str(result) if 'result' in locals() else "LOOK")

            if not is_valid_command(command):
                logger.warning(f"Invalid command generated: '{command}'. Defaulting to 'LOOK'.")
                command = "LOOK"
            
            # Simple loop detection
            current_sig = output_signature(last_output)
            if command == last_command and current_sig == last_output_sig:
                repeat_count += 1
            else:
                repeat_count = 0

            if repeat_count >= LOOP_REPEAT_LIMIT:
                forced_command, explore_index = next_explore_command(last_command, explore_index)
                logger.warning(
                    f"Detected loop on '{command}' with unchanged output. Forcing exploration: {forced_command}"
                )
                command = forced_command
                repeat_count = 0

            logger.info(f"AI COMMAND: {command}")
            last_command = command
            
            if command in {"QUIT", "Q"}:
                logger.info("AI decided to quit.")
                break

            last_output = game.send_command(command)
            logger.info(f"GAME RESPONSE:\n{last_output.strip()}")
            last_output_sig = output_signature(last_output)

            if "GAME OVER" in last_output or "Final score" in last_output:
                logger.info("\n--- Game Ended ---")
                break

            time.sleep(delay)
    finally:
        game.stop()
        logger.info("Strands AI session complete.")

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL_ID
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else 25
    
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }

    file_to_use = guidance_map.get(level, "data/guidance_full.txt")
    ai_play(file_to_use, model, delay, max_turns)
