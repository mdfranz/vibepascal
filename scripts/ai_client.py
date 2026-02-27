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
# Create a unique log file for each session
EPOCH = int(time.time())
LOG_FILE = f"logs/ai_client-{EPOCH}.log"
TURN_DELAY = 1
MAX_OUTPUT_CHARS = 2000
MESSAGE_HISTORY_LIMIT = 4
REASONING_ENV_VAR = "AI_REASONING"
LOOP_REPEAT_LIMIT = 2
EXPLORE_COMMANDS = ["LOOK", "NORTH", "EAST", "SOUTH", "WEST"]
FRUSTRATION_THRESHOLD = int(os.environ.get("AI_FRUSTRATION_THRESHOLD", "3"))
FRUSTRATION_DECAY = int(os.environ.get("AI_FRUSTRATION_DECAY", "1"))
FRUSTRATION_BURN_ORDER = ["BOOK", "LEDGER", "MAP", "LEATHER", "SADDLE"]
OUTLAW_SAFE_COMMANDS = {
    "SHOOT", "KILL", "WAIT",
    "NORTH", "SOUTH", "EAST", "WEST",
    "LOOK", "L", "INVENTORY", "I",
    "DISMOUNT", "MOUNT",
}

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
                        # EOF reached! Return what we have.
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
        if not self.proc or not self.proc.stdin:
            return "Error: Game not running."
        
        logger.debug(f"Sending command: {command}")
        self.proc.stdin.write((command + "\n").encode())
        self.proc.stdin.flush()
        
        # Give the process a tiny bit of time to react before reading
        time.sleep(0.05)
        
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
    "BURN", "FIRE",
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
    if cmd in ["OUT", "EXIT", "LEAVE"]: cmd = "SOUTH"
    if cmd.startswith("GET "): cmd = cmd.replace("GET ", "TAKE ")
    if cmd.startswith("GO "): cmd = cmd.replace("GO ", "")
    if "USE KEY" in cmd: cmd = "OPEN BOX"
    if "USE SADDLE" in cmd: cmd = "SADDLE HORSE"
    if "DRINK WATER" in cmd: cmd = "DRINK"
    if "FILL CANTEEN" in cmd: cmd = "FILL"
    if "WATER HORSE" in cmd: cmd = "WATER"
    if "UNLOCK" in cmd: cmd = "OPEN BOX"
    if "FORCE" in cmd: cmd = "EXAMINE BOX"
    
    # Smart item mapping for common model hallucinations
    if "WIRE" in cmd:
        if "TAKE" in cmd: cmd = "TAKE WIRE"
        if "EXAMINE" in cmd: cmd = "EXAMINE WIRE"
    if "MAP" in cmd:
        if "TAKE" in cmd: cmd = "TAKE MAP"
        if "EXAMINE" in cmd: cmd = "EXAMINE MAP"
    if "CANTEEN" in cmd:
        if "TAKE" in cmd: cmd = "TAKE CANTEEN"
    if "SADDLE" in cmd and "SADDLE HORSE" not in cmd:
        if "TAKE" in cmd: cmd = "TAKE SADDLE"
    
    return cmd

def extract_command_from_raw(raw_text: str) -> tuple[str, Optional[str]]:
    """Extracts command (and optional reasoning) from raw model output."""
    import re
    import json

    reasoning = None
    json_matches = list(re.finditer(r'\{.*\}', raw_text, re.DOTALL))
    if json_matches:
        json_str = json_matches[-1].group()
        try:
            data = json.loads(json_str)
            if isinstance(data, dict):
                if "reasoning" in data:
                    reasoning = str(data.get("reasoning", "")).strip()
                if "command" in data:
                    return sanitize_command(str(data["command"])), reasoning
        except Exception:
            pass
    return sanitize_command(raw_text), reasoning

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
    # Keep the most recent portion
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
    
    # Dynamic delay: lower for local models
    if model_name.startswith('ollama'):
        delay = max(0.1, delay * 0.2)
        logger.info(f"Dynamic delay applied: {delay:.2f}s")

    with open(guidance_file, 'r') as f:
        system_instruction = f.read().strip()

    game = DustwoodGame()
    deps = GameDeps(game)
    
    reasoning_enabled = os.environ.get(REASONING_ENV_VAR, "0") not in {"0", "false", "False"}
    output_model = CommandResponse if reasoning_enabled else CommandOnlyResponse
    example_json = '{"command": "LOOK", "reasoning": "Gather current room details."}' if reasoning_enabled else '{"command": "LOOK"}'

    use_raw_json = model_name.startswith('ollama')

    system_prompt = (
        f"{system_instruction}\n\n"
        "You are an expert adventurer. Provide your next action as a single command.\n"
        "CRITICAL: Use ONLY the valid verbs listed below. Do NOT attempt complex phrases or actions not in this list.\n"
        "Valid verbs: " + ", ".join(sorted(ALLOWED_VERBS)) + ".\n"
        "Return JSON only. No extra text.\n"
        f"Example JSON: {example_json}"
    )

    if use_raw_json:
        agent = Agent(
            model_name,
            deps_type=GameDeps,
            system_prompt=system_prompt
        )
    else:
        agent = Agent(
            model_name,
            deps_type=GameDeps,
            output_type=output_model,
            output_retries=3,  # Set retries here for self-correction
            system_prompt=system_prompt
        )

    logger.info(f"--- AI Player starting (Model: {model_name}) ---")
    
    # Optional seed for reproducibility
    seed = os.environ.get("GAME_SEED")
    last_output = game.start(seed=int(seed) if seed else None, turns=max_turns)
    
    logger.info(f"\n[STARTING GAME]\n{last_output.strip()}")

    turns = 0
    message_history = []
    last_command = ""
    last_output_sig = ""
    repeat_count = 0
    explore_index = 0
    last_invalid_command = ""
    invalid_repeat_count = 0
    last_score_update_turn = 0
    frustration = 0

    # Advanced State Tracking
    inventory = []
    knowledge_base = {} # Room -> list of items seen
    current_room = "Unknown"
    last_error = ""

    try:
        while turns < max_turns:
            turns += 1
            
            # --- Parse State from last_output ---
            import re
            
            # 1. Detect Room
            room_match = re.search(r"📍 === (.*?) ===", last_output)
            if room_match:
                current_room = room_match.group(1).strip()
                if current_room not in knowledge_base:
                    knowledge_base[current_room] = []
            
            # 2. Detect Items in Room
            if "You see the following here:" in last_output:
                items_part = last_output.split("You see the following here:")[1].split("\n\n")[0]
                found_items = []
                for line in items_part.split("\n"):
                    if "-" in line:
                        item_desc = line.split("-")[1].strip()
                        # Extract the last word as the canonical item name
                        item_name = item_desc.split()[-1].strip(".").upper()
                        found_items.append(item_name)
                knowledge_base[current_room] = found_items
            
            # 3. Detect Inventory Changes
            taken_match = re.search(r"Taken:\s*(.*?)\.", last_output)
            if taken_match:
                item_desc = taken_match.group(1)
                item_name = item_desc.split()[-1].strip(".").upper()
                if item_name not in inventory:
                    inventory.append(item_name)
                # Remove from room knowledge
                if current_room in knowledge_base and item_name in knowledge_base[current_room]:
                    knowledge_base[current_room].remove(item_name)

            dropped_match = re.search(r"Dropped:\s*(.*?)\.", last_output)
            if dropped_match:
                item_desc = dropped_match.group(1)
                item_name = item_desc.split()[-1].strip(".").upper()
                if item_name in inventory:
                    inventory.remove(item_name)
                # Add back to room knowledge
                if current_room in knowledge_base and item_name not in knowledge_base[current_room]:
                    knowledge_base[current_room].append(item_name)

            # 4. Detect Inventory Full Refresh
            if "You are carrying:" in last_output:
                inv_part = last_output.split("You are carrying:")[1].split("\n\n")[0]
                new_inv = []
                for line in inv_part.split("\n"):
                    if "-" in line:
                        item_desc = line.split("-")[1].strip()
                        item_name = item_desc.split()[-1].strip(".").upper()
                        new_inv.append(item_name)
                inventory = new_inv

            # 5. Detect Errors
            if "Not here" in last_output:
                last_error = f"ERROR: The item you tried to interact with is NOT in '{current_room}'."
            elif "You cannot go that way" in last_output:
                last_error = f"ERROR: There is no exit in that direction from '{current_room}'."
            elif "You can't carry any more" in last_output:
                last_error = "ERROR: Your inventory is FULL. You must DROP something before taking more."
            else:
                last_error = ""
            
            # Track invalid actions to prevent loops
            invalid_action = any(
                phrase in last_output
                for phrase in [
                    "Not here.",
                    "You can't take that.",
                    "You don't see that here.",
                    "You cannot go that way.",
                ]
            )
            if invalid_action and last_command:
                if last_command == last_invalid_command:
                    invalid_repeat_count += 1
                else:
                    invalid_repeat_count = 1
                    last_invalid_command = last_command
                frustration += 1
            else:
                invalid_repeat_count = 0

            logger.info(f"\n--- Turn {turns} ---")
            
            # Prepare context for the agent
            trimmed_output = trim_output(last_output)
            
            # Format Knowledge Base for context
            kb_summary = []
            for room, items in knowledge_base.items():
                if items:
                    kb_summary.append(f"- {room}: Items remaining: {', '.join(items)}")
                else:
                    kb_summary.append(f"- {room}: Cleared.")
            kb_text = "\n".join(kb_summary)

            # Create a very explicit schema description for the model
            if reasoning_enabled:
                schema_desc = "JSON object with keys: 'command' (string) and 'reasoning' (string)"
            else:
                schema_desc = "JSON object with key: 'command' (string)"

            context = (
                f"### RESULT of your LAST COMMAND ('{last_command or 'START'}'):\n{trimmed_output}\n\n"
                f"### STATE TRACKING:\n"
                f"CURRENT ROOM: {current_room}\n"
                f"CURRENT INVENTORY ({len(inventory)}/5): {', '.join(inventory) if inventory else 'Empty'}\n"
                f"KNOWLEDGE BASE (Rooms Visited):\n{kb_text}\n"
            )
            
            if last_error:
                context += f"\n### IMPORTANT CORRECTION:\n{last_error}\n"
            elif len(inventory) >= 5:
                context += "\n### STRATEGY HINT:\nYour inventory is FULL (5/5). You cannot take new items. Consider dropping a non-essential item like the BOOK or NOTE if you need space for survival gear.\n"

            context += (
                f"\nTASK: Provide your next action as a {schema_desc}.\n"
                "PERCEPTION GUIDE:\n"
                "1. Narrative text is flavor. Focus on lines with icons:\n"
                "   - 📦 'You see the following here:' lists interactable items.\n"
                "   - 🚪 'Exits:' lists available movement directions.\n"
                "   - 🐍 or 🤠 indicates immediate threats.\n"
                "2. For 'TAKE', 'DROP', or 'EXAMINE', you MUST include the item name (e.g., 'TAKE CANTEEN').\n"
                "3. SHORT NAMES: Use only the last word of an item description (e.g., for 'a spool of copper wire', use 'WIRE').\n"
                "4. ONCE YOU HAVE AN ITEM, MOVE ON. Do not keep searching for it.\n"
                "Output ONLY the JSON. No conversational text, no thought blocks."
            )

            # Use retry logic for robustness against model hallucinations
            try:
                result = agent.run_sync(
                    context,
                    deps=deps,
                    message_history=message_history
                )
                if use_raw_json:
                    raw_text = str(result.output)
                    command, extracted_reasoning = extract_command_from_raw(raw_text)
                    if reasoning_enabled and extracted_reasoning:
                        logger.info(f"AI THINKING: {extracted_reasoning}")
                else:
                    choice = result.output
                    command = sanitize_command(choice.command)
                    if reasoning_enabled and hasattr(choice, "reasoning"):
                        logger.info(f"AI THINKING: {choice.reasoning}")
                message_history = result.new_messages()[-MESSAGE_HISTORY_LIMIT:]
            except Exception as e:
                logger.warning(f"Agent failed to provide structured output: {e}. Attempting robust fallback...")
                frustration += 1
                try:
                    # Fallback to simple string if structured fails
                    fallback_agent = Agent(model_name, instructions=system_instruction)
                    raw_result = fallback_agent.run_sync(f"{context}\n\nProvide the JSON object ONLY. No thinking, no intro text.")
                    raw_text = str(raw_result.output)
                    command, extracted_reasoning = extract_command_from_raw(raw_text)
                    if reasoning_enabled and extracted_reasoning:
                        logger.info(f"AI THINKING (extracted): {extracted_reasoning}")
                except Exception as e2:
                    logger.error(f"Fallback also failed: {e2}")
                    command = "LOOK"

            if not is_valid_command(command):
                logger.warning(f"Invalid command generated: '{command}'. Defaulting to 'LOOK'.")
                frustration += 1
                command = "LOOK"
            
            # Simple loop detection: repeated command with identical output
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
                frustration += 1
                repeat_count = 0
            
            # Invalid-action loop breaker
            if invalid_repeat_count >= 2:
                forced_command, explore_index = next_explore_command(last_command, explore_index)
                logger.warning(
                    f"Repeated invalid action '{last_invalid_command}'. Forcing exploration: {forced_command}"
                )
                command = forced_command
                frustration += 1
                invalid_repeat_count = 0

            # Outlaw safety rule
            outlaw_present = "DIRTY OUTLAW" in last_output
            if outlaw_present and command not in OUTLAW_SAFE_COMMANDS:
                logger.warning(f"Outlaw present. Overriding unsafe action '{command}' with WAIT.")
                command = "WAIT"

            # Frustration-driven burn override (internal behavior only)
            if frustration >= FRUSTRATION_THRESHOLD:
                snake_present = ("🐍" in last_output) or ("rattlesnake" in last_output.lower())
                if (not outlaw_present) and (not snake_present) and ("MATCHES" in inventory):
                    burn_target = next((item for item in FRUSTRATION_BURN_ORDER if item in inventory), None)
                    if burn_target:
                        logger.warning(
                            f"Frustration threshold reached ({frustration}). Burning item: {burn_target}"
                        )
                        command = f"BURN {burn_target}"
                        frustration = max(0, frustration - FRUSTRATION_DECAY)

            # Late-game stall: avoid wasting turns on LOOK/SEARCH
            if turns >= max_turns - 5 and (turns - last_score_update_turn) >= 5:
                if command in {"LOOK", "SEARCH"}:
                    forced_command, explore_index = next_explore_command(last_command, explore_index)
                    logger.warning(
                        f"Late-game stall detected. Forcing exploration: {forced_command}"
                    )
                    command = forced_command

            logger.info(f"AI COMMAND: {command}")
            last_command = command
            
            if command in {"QUIT", "Q"}:
                logger.info("AI decided to quit.")
                break

            last_output = game.send_command(command)
            logger.info(f"GAME RESPONSE:\n{last_output.strip()}")
            if "Not here" in last_output:
                logger.warning(f"⚠️ Hallucination detected: AI tried to interact with an item that isn't present.")
            last_output_sig = output_signature(last_output)

            # --- Score Tracking ---
            import re
            score_match = re.search(r"🏆 Score:\s*(\d+)", last_output)
            if score_match:
                logger.info(f"🏆 [SCORE UPDATE]: {score_match.group(1)}")
                last_score_update_turn = turns
                frustration = max(0, frustration - FRUSTRATION_DECAY)
            # Also catch the final score if it's there
            final_match = re.search(r"Final score:\s*(\d+)", last_output)
            if final_match:
                logger.info(f"🏆 [FINAL SCORE]: {final_match.group(1)}")
                last_score_update_turn = turns
                frustration = max(0, frustration - FRUSTRATION_DECAY)

            if "GAME OVER" in last_output or "Final score" in last_output:
                logger.info("\n--- Game Ended ---")
                break

            time.sleep(delay)
    finally:
        # One last read to capture final score if game ended
        final_output = game._read_until_prompt(timeout=1.0)
        import re
        # Check for both regular score and final score at the end
        score_match = re.search(r"🏆 Score:\s*(\d+)", final_output)
        if score_match:
            logger.info(f"🏆 [SCORE UPDATE]: {score_match.group(1)}")
        final_match = re.search(r"Final score:\s*(\d+)", final_output)
        if final_match:
            logger.info(f"🏆 [FINAL SCORE]: {final_match.group(1)}")
        
        game.stop()
        logger.info("AI session complete.")

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    delay = int(sys.argv[3]) if len(sys.argv) > 3 else TURN_DELAY
    max_turns = int(sys.argv[4]) if len(sys.argv) > 4 else 25
    
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }

    file_to_use = guidance_map.get(level, "data/guidance_full.txt")
    ai_play(file_to_use, model, delay, max_turns)
