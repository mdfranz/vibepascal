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

# Load environment variables
load_dotenv()

# --- Configuration ---
BINARY_PATH = os.environ.get("DUSTWOOD_BIN", "bin/dustwood")
DEFAULT_MODEL = "gpt-5-mini"

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
        if not self.proc or not self.proc.stdin:
            return "Error: Game not running."
        
        logger.debug(f"Sending command: {command}")
        self.proc.stdin.write((command + "\n").encode())
        self.proc.stdin.flush()
        
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
    result = game.send_command(command)
    logger.info(f"Game output: {result}")
    return result

async def run_ms_agent(model_name: str, goal: str):
    logger.info(f"--- Microsoft Agent Framework Client Starting (Model: {model_name}) ---")
    logger.info(f"Goal: {goal}")
    
    try:
        # Initial look
        initial_output = game.start()
        logger.info(f"\n[STARTING GAME]\n{initial_output}")
        
        # Instantiate the client
        client = OpenAIChatClient(model_id=model_name)
        
        # Instantiate the agent
        agent = Agent(
            client=client,
            name="DustwoodAdventurer",
            instructions=(
                "You are an expert adventurer playing 'Echoes of Dustwood'.\n"
                "You interact with the game via the 'play_command' tool.\n"
                "Your goal is to survive, explore, and increase your score.\n"
                "Keep your responses concise. When you receive game output, decide on the next command and call 'play_command'."
            ),
            tools=[play_command]
        )
        
        # Start the interaction loop
        prompt = f"GOAL: {goal}. \nSTARTING STATE: {initial_output}\nWhat is your first command?"
        
        # Use agent.run which handles tool calling loop internally
        response = await agent.run(prompt)
        logger.info(f"\n[FINAL AGENT RESPONSE]\n{response.text}")

    finally:
        game.stop()

if __name__ == "__main__":
    model = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_MODEL
    goal = sys.argv[2] if len(sys.argv) > 2 else "Find the general store and get some water."
    
    asyncio.run(run_ms_agent(model, goal))
