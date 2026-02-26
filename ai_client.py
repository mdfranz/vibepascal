import os
import httpx
import time
import logging
import sys
import pydantic_ai
from dotenv import load_dotenv
from pydantic_ai import Agent
from pydantic_ai.models import KnownModelName

# Load environment variables from .env if present
load_dotenv()

# --- Configuration ---
BASE_URL = "http://localhost:8000/game"
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-1.5-flash"
LOG_FILE = "ai_client.log"
TURN_DELAY = 5 

# --- Setup Logging ---
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG) # Catch everything

# Detailed File Logger (DEBUG and above)
file_handler = logging.FileHandler(LOG_FILE)
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(logging.Formatter('%(asctime)s [%(levelname)s] %(message)s'))
logger.addHandler(file_handler)

# Clean Console Logger (INFO and above, for gameplay)
console_handler = logging.StreamHandler()
console_handler.setLevel(logging.INFO)
console_handler.setFormatter(logging.Formatter('%(message)s'))
logger.addHandler(console_handler)

logger.debug(f"Pydantic AI Version: {pydantic_ai.__version__}")

ITEM_KEYWORDS = [
    "CANTEEN",
    "LEATHER",
    "MATCHES",
    "SADDLE",
    "BOOK",
    "WIRE",
    "REVOLVER",
    "LAMP",
    "BOX",
]

def _extract_visible_items(game_output: str) -> list[str]:
    items = []
    for line in game_output.splitlines():
        line = line.strip()
        if line.startswith("- "):
            items.append(line[2:].strip())
        elif line.startswith(" - "):
            items.append(line[3:].strip())
    return items

def _choose_item_keyword(command: str, visible_items: list[str]) -> str | None:
    for keyword in ITEM_KEYWORDS:
        if keyword in command:
            return keyword
    for desc in visible_items:
        upper_desc = desc.upper()
        for keyword in ITEM_KEYWORDS:
            if keyword in upper_desc:
                return keyword
    return None

def normalize_command(command: str, last_response: str) -> str:
    cmd = command.strip().upper()
    for ch in [".", ",", "!", "?", ";", ":"]:
        cmd = cmd.replace(ch, "")
    cmd = cmd.replace("INSPECT", "EXAMINE")
    cmd = cmd.replace("LOOK AROUND", "LOOK")
    cmd = cmd.replace("LOOK ABOUT", "LOOK")
    cmd = cmd.replace("CHECK INVENTORY", "INVENTORY")
    cmd = cmd.replace("CHECK INV", "INVENTORY")
    cmd = cmd.replace("CHECK ITEMS", "INVENTORY")
    cmd = cmd.replace("SEARCH", "LOOK")
    cmd = cmd.replace("ASK FOR WATER", "FILL")
    cmd = cmd.replace("BUY WATER", "FILL")
    cmd = cmd.replace("THE ", "")

    if cmd.startswith("TAKE"):
        visible_items = _extract_visible_items(last_response)
        if "ALL" in cmd:
            keyword = _choose_item_keyword(cmd, visible_items)
            if keyword:
                return f"TAKE {keyword}"
        keyword = _choose_item_keyword(cmd, visible_items)
        if keyword:
            return f"TAKE {keyword}"

    return cmd.split("\n")[0].strip()

def _current_room_name(game_output: str) -> str | None:
    for line in game_output.splitlines():
        line = line.strip()
        if line.startswith("*** ") and line.endswith(" ***"):
            return line[4:-4]
    return None

def _threat_override(game_output: str) -> str | None:
    if "RATTLESNAKE" in game_output:
        return "FREEZE"
    if "DIRTY OUTLAW" in game_output:
        room = _current_room_name(game_output) or ""
        escape_map = {
            "General Store": "WEST",
            "Sheriff's Office": "EAST",
            "Assayer's Office": "EAST",
            "Livery Stables": "NORTH",
            "Telegraph Office": "SOUTH",
            "The Desert Edge": "NORTH",
            "Dry Wash": "NORTH",
            "Howling Desert": "NORTH",
            "Butte": "NORTH",
            "Hidden Stream": "SOUTH",
            "Main Street": "EAST",
        }
        return escape_map.get(room, "NORTH")
    return None

def ai_play_scripted():
    script = [
        "EAST",
        "TAKE CANTEEN",
        "TAKE LEATHER",
        "TAKE SADDLE",
        "TAKE MATCHES",
        "WEST",
        "SOUTH",
        "FIX PUMP",
        "FILL",
        "DRINK",
        "SADDLE HORSE",
        "SOUTH",
        "SOUTH",
        "SOUTH",
        "SOUTH",
        "CLIMB",
        "EXAMINE ROCK",
        "TAKE KEY",
        "FILL",
        "WATER",
        "SOUTH",
        "SOUTH",
        "NORTH",
        "NORTH",
        "NORTH",
        "NORTH",
        "WEST",
        "OPEN BOX",
        "TAKE REVOLVER",
        "QUIT",
    ]

    logger.info("--- AI Player starting scripted win path ---")
    last_response = get_game_output("LOOK")
    logger.info(f"\n[INITIAL STATE]\n{last_response}")

    step = 0
    max_steps = len(script) + 20
    while step < len(script) and step < max_steps:
        logger.info(f"\n--- Step {step + 1} ---")
        override = _threat_override(last_response)
        if override:
            command = override
        else:
            command = script[step]
            step += 1

        logger.info(f"AI CHOICE: {command}")
        if "QUIT" in command:
            break

        last_response = get_game_output(command)
        logger.info(f"GAME RESPONSE:\n{last_response}")

        score_response = get_game_output("SCORE")
        logger.info(f"SCORE UPDATE:\n{score_response}")

        if "GAME OVER" in last_response:
            logger.info("\n!!! AI DIED !!!")
            break

        time.sleep(TURN_DELAY)

def get_game_output(command: str) -> str:
    """Sends a command to the sidecar and returns the cleaned output."""
    try:
        response = httpx.post(BASE_URL, json={"command": command}, timeout=15.0)
        response.raise_for_status()
        data = response.json()
        output = data.get("output", "")
        
        if "> Game loaded." in output:
            parts = output.split("> Game loaded.")
            content = parts[1].split("> Game saved.")[0].strip()
            return content
        return output
    except Exception as e:
        logger.error(f"Error communicating with game: {e}")
        return f"Error communicating with game: {e}"

def ai_play(guidance_file: str, model_name: str):
    if not os.path.exists(guidance_file):
        logger.error(f"Guidance file not found: {guidance_file}")
        exit(1)

    # Only setup Ollama if we are actually using it
    if model_name.startswith('ollama:') and os.environ.get('OLLAMA_HOST'):
        base_url = os.environ['OLLAMA_HOST'].rstrip('/')
        if not base_url.endswith('/v1'):
            test_url = base_url + '/v1/models'
        else:
            test_url = base_url + '/models'
        
        logger.info(f"Checking Ollama connectivity at {test_url}...")
        try:
            with httpx.Client(timeout=2.0) as client:
                r = client.get(test_url)
                logger.debug(f"Ollama connection successful: {r.status_code}")
                if not base_url.endswith('/v1'):
                    base_url += '/v1'
        except Exception as e:
            logger.warning(f"Could not connect to Ollama: {e}")
            if not base_url.endswith('/v1'):
                base_url += '/v1'

        os.environ['OLLAMA_BASE_URL'] = base_url
        logger.debug(f"Ollama mapping: OLLAMA_BASE_URL set to {base_url}")
        
    with open(guidance_file, 'r') as f:
        system_instruction = f.read().strip()

    logger.info(f"--- AI Player starting with {model_name} ---")
    
    initial_look = get_game_output("LOOK")
    logger.info(f"\n[INITIAL STATE]\n{initial_look}")

    # Use output_type for Pydantic AI 1.x
    agent = Agent(
        model_name,
        system_prompt=system_instruction,
        output_type=str
    )

    turns = 0
    max_turns = 30
    last_response = initial_look
    history = []

    while turns < max_turns:
        turns += 1
        logger.info(f"\n--- Turn {turns} ---")
        
        try:
            prompt = f"Previous Game Output:\n{last_response}\n\nWhat is your next command?"
            
            result = agent.run_sync(prompt, message_history=history)
            
            # Prefer 'output' or 'data' for the command text
            if hasattr(result, 'output'):
                command = str(result.output)
            elif hasattr(result, 'data'):
                command = str(result.data)
            else:
                # Fallback to last message if standard attributes are missing
                logger.debug(f"Result object missing 'output' or 'data'. Attributes: {dir(result)}")
                history = result.new_messages()
                command = "QUIT" 
                for msg in reversed(history):
                    if hasattr(msg, 'parts'):
                        for part in msg.parts:
                            if hasattr(part, 'content'):
                                command = str(part.content)
                                break
            
            command = command.strip().upper()
            history = result.new_messages()
            
            # Sanitization
            command = command.split('\n')[0].replace("COMMAND:", "").strip()
            command = command.replace("`", "").strip()
            command = normalize_command(command, last_response)
            
            logger.info(f"AI CHOICE: {command}")
            
            if "QUIT" in command:
                break
                
            last_response = get_game_output(command)
            logger.info(f"GAME RESPONSE:\n{last_response}")

            score_response = get_game_output("SCORE")
            logger.info(f"SCORE UPDATE:\n{score_response}")
            
            if "GAME OVER" in last_response:
                logger.info("\n!!! AI DIED !!!")
                break
        except Exception as e:
            logger.error(f"Error during AI turn: {e}")
            break

        logger.debug(f"Pacing: Waiting {TURN_DELAY} seconds...")
        time.sleep(TURN_DELAY)

if __name__ == "__main__":
    level = sys.argv[1] if len(sys.argv) > 1 else "full"
    model = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_MODEL
    try:
        if len(sys.argv) > 3:
            TURN_DELAY = int(sys.argv[3])
    except ValueError:
        logger.warning(f"Invalid delay value: {sys.argv[3]}. Using default: {TURN_DELAY}")
    
    guidance_map = {
        "full": "guidance_full.txt",
        "medium": "guidance_medium.txt",
        "minimal": "guidance_minimal.txt"
    }

    if level == "win":
        ai_play_scripted()
    else:
        file_to_use = guidance_map.get(level, "guidance_full.txt")
        ai_play(file_to_use, model)
