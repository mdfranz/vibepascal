from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import subprocess
import os

app = FastAPI()

GAME_BIN = "./dustwood"

class Command(BaseModel):
    command: str

@app.post("/game")
async def run_command(cmd: Command):
    """
    Runs a single command by starting the game, executing the command, 
    and returning the output. 
    State is managed via save.ini.
    """
    # Create empty save if none exists
    if not os.path.exists("save.ini"):
        init_proc = subprocess.Popen([GAME_BIN, "--headless"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
        init_proc.communicate(input="SAVE\nQUIT\n")

    input_str = f"LOAD\n{cmd.command}\nSAVE\nQUIT\n"
    
    try:
        process = subprocess.Popen(
            [GAME_BIN, "--headless"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        stdout, _ = process.communicate(input=input_str, timeout=5)
        
        # Simple cleanup to show only the relevant turn
        # We look for the prompt before the user command and after.
        return {"output": stdout}
        
    except subprocess.TimeoutExpired:
        process.kill()
        raise HTTPException(status_code=504, detail="Game process timed out")
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
