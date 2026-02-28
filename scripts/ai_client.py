import os
import httpx
import time
import logging
import sys
import pydantic_ai
from pydantic import BaseModel, Field
from dotenv import load_dotenv
from pydantic_ai import Agent
from pydantic_ai.models import KnownModelName

# Load environment variables from .env if present
load_dotenv()

# --- Configuration ---
BASE_URL = "http://localhost:8000/game"
DEFAULT_MODEL: KnownModelName = "google-gla:gemini-2.0-flash"
LOG_FILE = "logs/ai_client.log"
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
    "KEY",
    "ROCK",
    "MAP",
    "PUMP",
    "HORSE",
]

ALLOWED_VERBS = {
    "N", "NORTH", "S", "SOUTH", "E", "EAST", "W", "WEST",
    "LOOK", "L", "EXAMINE", "X", "SEARCH",
    "HELP", "H", "?", "INVENTORY", "I", "INV",
    "DRINK", "FILL", "WATER", "LIGHT", "FIX", "SADDLE",
    "PUT", "CLIMB", "SAVE", "LOAD", "SCORE",
    "TAKE", "GET", "DROP", "D",
    "QUIT", "Q",
    "MOUNT", "RIDE", "DISMOUNT",
    "OPEN", "SHOOT", "KILL", "FREEZE", "WAIT", "CHECK",
    "BURN", "FIRE",
}

class CommandResponse(BaseModel):
    command: str = Field(..., description="Single game command like LOOK, NORTH, TAKE CANTEEN, EXAMINE BOOK")

ALLOWED_COMMANDS_HINT = (
    "Valid commands include: N, SOUTH, EAST, WEST, LOOK, EXAMINE <ITEM>, "
    "TAKE <ITEM>, DROP <ITEM>, INVENTORY, HELP, SCORE, DRINK, FILL, LIGHT, "
    "FIX <ITEM>, SADDLE, CLIMB, OPEN <ITEM>, SHOOT <TARGET>, SAVE, LOAD, QUIT, "
    "BURN <ITEM>, FIRE."
)

NOUN_ALIASES = {
    "GUN BOX": "BOX",
    "GUNBOX": "BOX",
    "MARE": "HORSE",
    "STURDY CHESTNUT MARE": "HORSE",
    "FADED HAND-DRAWN MAP": "MAP",
    "FADED MAP": "MAP",
    "RUSTED IRON WATER PUMP": "PUMP",
    "WATER PUMP": "PUMP",
}

def _extract_visible_items(game_output: str) -> list[str]:
    items = []
    for line in game_output.splitlines():
        line = line.strip()
        if line.startswith("- "):
            items.append(line[2:].strip())
        elif line.startswith(" - "):
            items.append(line[3:].strip())
    return items

def _extract_inventory_items(game_output: str) -> list[str]:
    items = []
    in_inventory = False
    for line in game_output.splitlines():
        line = line.strip()
        if line.startswith("You are carrying:"):
            in_inventory = True
            continue
        if in_inventory:
            if not line:
                break
            if line.startswith("- "):
                items.append(line[2:].strip())
            elif line.startswith(" - "):
                items.append(line[3:].strip())
    return items

def _extract_nouns(game_output: str) -> set[str]:
    nouns = set()
    for desc in _extract_visible_items(game_output) + _extract_inventory_items(game_output):
        upper_desc = desc.upper()
        matched = False
        for keyword in ITEM_KEYWORDS:
            if keyword in upper_desc:
                nouns.add(keyword)
                matched = True
        if not matched:
            for token in upper_desc.replace(".", "").replace(",", "").split():
                if len(token) >= 3:
                    nouns.add(token)
    return nouns

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
    cmd = cmd.replace("READ", "EXAMINE")
    cmd = cmd.replace("CHECK INVENTORY", "INVENTORY")
    cmd = cmd.replace("CHECK INV", "INVENTORY")
    cmd = cmd.replace("CHECK ITEMS", "INVENTORY")
    cmd = cmd.replace("SEARCH", "LOOK")
    cmd = cmd.replace("ASK FOR WATER", "FILL")
    cmd = cmd.replace("BUY WATER", "FILL")
    cmd = cmd.replace("BACK AWAY", "LOOK")
    cmd = cmd.replace("RETREAT", "LOOK")
    cmd = cmd.replace("STEP BACK", "LOOK")
    cmd = cmd.replace("GO BACK", "LOOK")
    cmd = cmd.replace("THE ", "")

    if cmd.startswith("TAKE") or cmd.startswith("GET"):
        visible_items = _extract_visible_items(last_response)
        if "ALL" in cmd:
            keyword = _choose_item_keyword(cmd, visible_items)
            if keyword:
                return f"TAKE {keyword}"
        keyword = _choose_item_keyword(cmd, visible_items)
        if keyword:
            return f"TAKE {keyword}"
        if cmd.startswith("GET"):
            return cmd.replace("GET", "TAKE")

    if cmd.startswith("DROP") or cmd.startswith("D "):
         if cmd.startswith("D "):
             return cmd.replace("D ", "DROP ")

    if cmd.startswith("SADDLE"):
        return "SADDLE"

    return cmd.split("\n")[0].strip()

def _current_room_name(game_output: str) -> str | None:
    for line in game_output.splitlines():
        line = line.strip()
        if line.startswith("*** ") and line.endswith(" ***"):
            return line[4:-4]
    return None

def _threat_override(game_output: str) -> str | None:
    upper_output = game_output.upper()
    if "RATTLESNAKE" in upper_output or "SNAKE" in upper_output:
        return "FREEZE"
    if "DIRTY OUTLAW" in upper_output:
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
        return output
    except Exception as e:
        logger.error(f"Error communicating with game: {e}")
        return f"Error communicating with game: {e}"

def _is_valid_command(command: str) -> bool:
    if not command:
        return False
    verb = command.split(" ", 1)[0].strip().upper()
    return verb in ALLOWED_VERBS

def _normalize_noun_to_known(command: str, known_nouns: set[str]) -> str:
    parts = command.split(" ", 1)
    if len(parts) == 1:
        return command
    verb, noun = parts[0], parts[1].strip().upper()
    if noun in NOUN_ALIASES:
        noun = NOUN_ALIASES[noun]
    if noun.endswith("MARE"):
        noun = "HORSE"
    if "HORSE" in noun:
        noun = "HORSE"
    if noun in known_nouns:
        return f"{verb} {noun}" if len(parts) > 1 else command
    if noun.endswith("S") and noun[:-1] in known_nouns:
        return f"{verb} {noun[:-1]}"
    return command

def ai_play(guidance_file: str, model_name: str):
    if not os.path.exists(guidance_file):
        logger.error(f"Guidance file not found: {guidance_file}")
        exit(1)

    if model_name.startswith("anthropic:") and not os.environ.get("ANTHROPIC_API_KEY"):
        logger.warning("ANTHROPIC_API_KEY is not set; Anthropic calls may fail.")
    if model_name.startswith("gateway/anthropic:") and not os.environ.get("PYDANTIC_AI_GATEWAY_API_KEY"):
        logger.warning("PYDANTIC_AI_GATEWAY_API_KEY is not set; Gateway Anthropic calls may fail.")

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

    use_structured = not model_name.startswith("ollama:")
    # Use output_type for Pydantic AI 1.x
    agent = Agent(
        model_name,
        system_prompt=system_instruction,
        output_type=CommandResponse
    )
    raw_agent = Agent(
        model_name,
        system_prompt=system_instruction,
        output_type=str
    )

    turns = 0
    max_turns = int(os.environ.get("MAX_TURNS", "50"))
    last_response = initial_look
    prev_response = ""
    history = []
    fallback_moves = ["NORTH", "SOUTH", "EAST", "WEST", "INVENTORY"]
    fallback_idx = 0
    repeat_count = 0

    while turns < max_turns:
        turns += 1
        logger.info(f"\n--- Turn {turns} ---")
        
        try:
            prompt = f"Previous Game Output:\n{last_response}\n\nWhat is your next command?"
            
            known_nouns = _extract_nouns(last_response)
            visible_items = _extract_visible_items(last_response)
            inventory_items = _extract_inventory_items(last_response)
            inventory_upper = " ".join(inventory_items).upper()
            override = _threat_override(last_response)
            if override:
                command = override
                logger.info(f"AI CHOICE: {command}")
                last_response = get_game_output(command)
                logger.info(f"GAME RESPONSE:\n{last_response}")
                if last_response.startswith("Error communicating with game:"):
                    logger.error("Stopping due to game backend error.")
                    break

                score_response = get_game_output("SCORE")
                logger.info(f"SCORE UPDATE:\n{score_response}")
                if "GAME OVER" in last_response:
                    logger.info("\n!!! AI DIED !!!")
                    break
                prev_response = last_response
                logger.debug(f"Pacing: Waiting {TURN_DELAY} seconds...")
                time.sleep(TURN_DELAY)
                continue

            def _run_model(request_note: str = ""):
                prompt = (
                    f"Previous Game Output:\n{last_response}\n\n"
                    "Return ONLY a single valid game command. "
                    "Do not include explanations or extra text. "
                    f"{ALLOWED_COMMANDS_HINT} "
                    f"Visible items: {', '.join(visible_items) if visible_items else 'none'}. "
                    f"Inventory: {', '.join(inventory_items) if inventory_items else 'none'}. "
                    f"Use item names exactly; avoid plurals unless shown. "
                    f"{request_note}"
                )
                return agent.run_sync(prompt, message_history=history)

            if use_structured:
                try:
                    result = _run_model()
                except Exception as e:
                    msg = str(e).lower()
                    if "validation" in msg or "output" in msg:
                        logger.warning(f"Structured output failed: {e}. Falling back to raw output.")
                        raw_result = raw_agent.run_sync(
                            f"Previous Game Output:\n{last_response}\n\n"
                            "Return ONLY a single valid game command. "
                            "Do not include explanations or extra text. "
                            f"{ALLOWED_COMMANDS_HINT} "
                            f"Visible items: {', '.join(visible_items) if visible_items else 'none'}. "
                            f"Inventory: {', '.join(inventory_items) if inventory_items else 'none'}. "
                            "Use item names exactly; avoid plurals unless shown. ",
                            message_history=history,
                        )
                        result = raw_result
                    else:
                        raise
            else:
                result = raw_agent.run_sync(
                    f"Previous Game Output:\n{last_response}\n\n"
                    "Return ONLY a single valid game command. "
                    "Do not include explanations or extra text. "
                    f"{ALLOWED_COMMANDS_HINT} "
                    f"Visible items: {', '.join(visible_items) if visible_items else 'none'}. "
                    f"Inventory: {', '.join(inventory_items) if inventory_items else 'none'}. "
                    "Use item names exactly; avoid plurals unless shown. ",
                    message_history=history,
                )
            
            if hasattr(result, 'output') and isinstance(result.output, CommandResponse):
                command = str(result.output.command)
            elif hasattr(result, 'data') and isinstance(result.data, CommandResponse):
                command = str(result.data.command)
            elif hasattr(result, 'output'):
                command = str(result.output)
            elif hasattr(result, 'data'):
                command = str(result.data)
            else:
                logger.debug(f"Result object missing 'output' or 'data'. Attributes: {dir(result)}")
                history = result.new_messages()
                command = "LOOK"
            
            command = command.strip().upper()
            history = result.new_messages()
            
            # Sanitization
            command = command.split('\n')[0].replace("COMMAND:", "").strip()
            command = command.replace("`", "").strip()
            command = normalize_command(command, last_response)
            command = _normalize_noun_to_known(command, known_nouns)

            if not _is_valid_command(command):
                logger.warning(f"Invalid command from model: {command}. Retrying with stricter prompt.")
                if use_structured:
                    result = _run_model(request_note=f"Your previous output was invalid: {command}.")
                    if hasattr(result, 'output'):
                        command = str(result.output.command)
                    elif hasattr(result, 'data'):
                        command = str(result.data.command)
                else:
                    result = raw_agent.run_sync(
                        f"Previous Game Output:\n{last_response}\n\n"
                        "Return ONLY a single valid game command. "
                        "Do not include explanations or extra text. "
                        f"{ALLOWED_COMMANDS_HINT} "
                        f"Visible items: {', '.join(visible_items) if visible_items else 'none'}. "
                        f"Inventory: {', '.join(inventory_items) if inventory_items else 'none'}. "
                        "Use item names exactly; avoid plurals unless shown. "
                        f"Your previous output was invalid: {command}.",
                        message_history=history,
                    )
                    if hasattr(result, 'output'):
                        command = str(result.output)
                    elif hasattr(result, 'data'):
                        command = str(result.data)
                command = command.strip().upper()
                command = command.split('\n')[0].replace("COMMAND:", "").strip()
                command = command.replace("`", "").strip()
                command = normalize_command(command, last_response)
                command = _normalize_noun_to_known(command, known_nouns)

            if not _is_valid_command(command):
                logger.warning(f"Invalid command from model after retry: {command}. Using fallback.")
                if last_response == prev_response and command == "LOOK":
                    command = fallback_moves[fallback_idx % len(fallback_moves)]
                    fallback_idx += 1
                else:
                    command = "LOOK"

            if command.startswith("EXAMINE"):
                parts = command.split(" ", 1)
                noun = parts[1].strip().upper() if len(parts) > 1 else ""
                if noun and noun not in known_nouns:
                    if visible_items:
                        keyword = _choose_item_keyword(command, visible_items)
                        if keyword:
                            command = f"TAKE {keyword}"
                    if command.startswith("EXAMINE"):
                        command = "LOOK"

            if last_response == prev_response:
                repeat_count += 1
            else:
                repeat_count = 0
            if repeat_count >= 2 and command in {"LOOK", "EXAMINE", "SEARCH", "INVENTORY"}:
                command = fallback_moves[fallback_idx % len(fallback_moves)]
                fallback_idx += 1

            if command == "SADDLE" and "SADDLE" not in inventory_upper:
                logger.warning("No saddle in inventory; avoiding SADDLE and moving on.")
                command = fallback_moves[fallback_idx % len(fallback_moves)]
                fallback_idx += 1
            
            logger.info(f"AI CHOICE: {command}")
            
            if "QUIT" in command:
                break
                
            last_response = get_game_output(command)
            logger.info(f"GAME RESPONSE:\n{last_response}")
            if last_response.startswith("Error communicating with game:"):
                logger.error("Stopping due to game backend error.")
                break

            score_response = get_game_output("SCORE")
            logger.info(f"SCORE UPDATE:\n{score_response}")
            
            if "GAME OVER" in last_response:
                logger.info("\n!!! AI DIED !!!")
                break
        except Exception as e:
            logger.error(f"Error during AI turn: {e}")
            break

        prev_response = last_response
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
    if len(sys.argv) > 4:
        try:
            os.environ["MAX_TURNS"] = str(int(sys.argv[4]))
        except ValueError:
            logger.warning(f"Invalid max turns value: {sys.argv[4]}. Using default.")
    
    guidance_map = {
        "full": "data/guidance_full.txt",
        "medium": "data/guidance_medium.txt",
        "minimal": "data/guidance_minimal.txt"
    }

    if level == "win":
        ai_play_scripted()
    else:
        file_to_use = guidance_map.get(level, "data/guidance_full.txt")
        ai_play(file_to_use, model)
