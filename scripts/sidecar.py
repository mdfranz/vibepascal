from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel
import subprocess
import os
import threading
import time
import selectors
import logging
import traceback

app = FastAPI()
logger = logging.getLogger("sidecar")
if not logger.handlers:
    logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s")

# Enable CORS for web UI
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

GAME_BIN = os.environ.get("DUSTWOOD_BIN", "bin/dustwood")
SAVE_PATH = "data/save.ini"
PROMPT = "> "

_proc_lock = threading.Lock()

class GameProcess:
    def __init__(self, game_bin: str, prompt: str):
        self.game_bin = game_bin
        self.prompt = prompt
        self.prompt_bytes = prompt.encode()
        self.proc: subprocess.Popen | None = None
        self.initial_output: str = ""

    def _read_until_prompt(self, timeout: float) -> str:
        if not self.proc or not self.proc.stdout:
            raise RuntimeError("Game process is not running.")

        buf = bytearray()
        end_time = time.time() + timeout
        sel = selectors.DefaultSelector()
        sel.register(self.proc.stdout, selectors.EVENT_READ)
        try:
            while time.time() < end_time:
                events = sel.select(timeout=0.1)
                if not events:
                    continue
                for key, _ in events:
                    chunk = os.read(key.fileobj.fileno(), 4096)
                    if not chunk:
                        # Process exited; return whatever we have so far.
                        if buf:
                            return buf.decode(errors="replace")
                        raise RuntimeError("Game process exited while reading output.")
                    buf.extend(chunk)
                    if buf.endswith(self.prompt_bytes):
                        return buf.decode(errors="replace")
        finally:
            sel.close()
        raise TimeoutError("Timed out waiting for game prompt.")

    def _strip_prompt(self, output: str) -> str:
        if output.endswith(self.prompt):
            return output[:-len(self.prompt)]
        return output

    def start(self) -> None:
        if self.proc and self.proc.poll() is None:
            return
        self.proc = subprocess.Popen(
            [self.game_bin, "--headless"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=False,
            bufsize=0,
        )
        # Consume initial banner/look output and prompt.
        self.initial_output = self._strip_prompt(self._read_until_prompt(timeout=5.0))

    def stop(self) -> None:
        if not self.proc:
            return
        try:
            self.proc.terminate()
            self.proc.wait(timeout=2)
        except Exception:
            try:
                self.proc.kill()
            except Exception:
                pass
        finally:
            self.proc = None

    def send_command(self, command: str, timeout: float = 5.0) -> str:
        self.start()
        if not self.proc or not self.proc.stdin:
            raise RuntimeError("Game process failed to start.")
        self.proc.stdin.write((command + "\n").encode())
        self.proc.stdin.flush()
        output = self._read_until_prompt(timeout=timeout)
        if self.proc and self.proc.poll() is not None:
            # Game ended; clean up process.
            self.stop()
        return self._strip_prompt(output)

_game = GameProcess(GAME_BIN, PROMPT)

class Command(BaseModel):
    command: str

@app.post("/game")
async def run_command(cmd: Command):
    """
    Runs a single command against a persistent headless game process.
    """
    try:
        with _proc_lock:
            output = _game.send_command(cmd.command, timeout=5.0)
        return {"output": output}
    except TimeoutError as e:
        with _proc_lock:
            _game.stop()
        raise HTTPException(status_code=504, detail="Game process timed out")
    except Exception as e:
        logger.error("Error handling command: %s", e)
        logger.debug("Traceback:\n%s", traceback.format_exc())
        with _proc_lock:
            _game.stop()
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/reset")
async def reset_game():
    """
    Restarts the game process and returns the initial output.
    """
    try:
        with _proc_lock:
            _game.stop()
            _game.start()
            output = _game.initial_output
        return {"output": output}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/save")
async def save_game():
    """
    Explicitly save the game state.
    """
    try:
        with _proc_lock:
            output = _game.send_command("SAVE", timeout=5.0)
        return {"output": output}
    except TimeoutError as e:
        with _proc_lock:
            _game.stop()
        raise HTTPException(status_code=504, detail="Game process timed out")
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/load")
async def load_game():
    """
    Explicitly load the game state.
    """
    try:
        with _proc_lock:
            output = _game.send_command("LOAD", timeout=5.0)
        return {"output": output}
    except TimeoutError as e:
        with _proc_lock:
            _game.stop()
        raise HTTPException(status_code=504, detail="Game process timed out")
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Serve the web UI
if os.path.exists("web"):
    app.mount("/", StaticFiles(directory="web", html=True), name="web")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
