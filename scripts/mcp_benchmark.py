import subprocess
import time
import json
import sys
import os
from typing import Dict, List, Any

# --- Configuration ---
MCP_URL = "http://127.0.0.1:8765/mcp"
DEFAULT_MODEL = "claude-opus-4-6"
STRANDS_MODEL = "anthropic/claude-3-opus-20240229" # Strands uses LiteLLM
GOAL = "Explore Dustwood thoroughly for 50 turns. Try to find the telegraph office, general store, livery stables, and the hidden stream. Keep playing until you have reached 50 turns or the game ends."
STEPS = 50

CLIENTS = [
    {
        "name": "Pydantic AI",
        "script": "scripts/pydantic_mcp_client.py",
        "args": ["anthropic:claude-3-opus-20240229", GOAL], # Pydantic AI naming
        "type": "json-rpc"
    },
    {
        "name": "MS Agent Framework",
        "script": "scripts/ms_agent_mcp_client.py",
        "args": [DEFAULT_MODEL, GOAL, "--steps", str(STEPS)],
        "type": "json-rpc"
    },
    {
        "name": "Agno (formerly Phidata)",
        "script": "scripts/agno_mcp_client.py",
        "args": [DEFAULT_MODEL, GOAL, "--steps", str(STEPS)],
        "type": "json-rpc"
    },
    {
        "name": "Strands SDK",
        "script": "scripts/strands_mcp_client.py",
        "args": [STRANDS_MODEL, GOAL, "stdio"],
        "type": "stdio"
    }
]

def run_client(client: Dict[str, Any]) -> Dict[str, Any]:
    print(f"\n>>> Running Benchmark for: {client['name']} ...")
    start_time = time.time()
    
    cmd = ["uv", "run", "python3", client["script"]] + client["args"]
    
    try:
        # Run with a 10-minute timeout for a 50-turn run
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        stdout, stderr = process.communicate(timeout=600)
        end_time = time.time()
        
        duration = end_time - start_time
        exit_code = process.returncode
        
        # Combined output for search
        full_output = stdout + stderr
        
        # Log to file for later review
        log_dir = "logs/benchmark"
        os.makedirs(log_dir, exist_ok=True)
        log_file = os.path.join(log_dir, f"{client['name'].replace(' ', '_').lower()}.log")
        with open(log_file, "w") as f:
            f.write(full_output)
        
        # Simple heuristics to count turns
        turns = (full_output.count("Agent executing command:") + 
                 full_output.count("HTTP Request: POST http://127.0.0.1:8765/mcp") +
                 full_output.count("Calling tool"))
        
        # Adjust turn count
        if client["name"] == "Pydantic AI":
            turns = max(0, turns - 1)
        elif client["name"] in ["Agno (formerly Phidata)", "MS Agent Framework"]:
            turns = max(0, turns - 1)

        # Check if any progress was made
        success = "--- Game State ---" in full_output or ">" in full_output
        
        return {
            "name": client["name"],
            "duration": round(duration, 2),
            "turns": turns,
            "success": success and exit_code == 0,
            "exit_code": exit_code,
            "error": stderr if exit_code != 0 else ""
        }
    except subprocess.TimeoutExpired:
        process.kill()
        return {
            "name": client["name"],
            "duration": 120,
            "turns": "Timeout",
            "success": False,
            "exit_code": -1,
            "error": "Timed out after 120 seconds"
        }
    except Exception as e:
        return {
            "name": client["name"],
            "duration": 0,
            "turns": 0,
            "success": False,
            "exit_code": -1,
            "error": str(e)
        }

def main():
    print("=== Echoes of Dustwood: MCP Client Benchmark ===")
    print(f"Goal: {GOAL}")
    print(f"Model: {DEFAULT_MODEL}")
    
    results = []
    for client in CLIENTS:
        res = run_client(client)
        results.append(res)
        if res["success"]:
            print(f"Result: SUCCESS ({res['duration']}s, {res['turns']} turns)")
        else:
            print(f"Result: FAILURE (Code: {res['exit_code']})")
            if res["error"]:
                print(f"Error snippet: {res['error'][:200]}...")

    # Generate Markdown Table
    print("\n\n### Benchmark Results")
    print("| Client Framework | Duration (s) | Turns | Success | Status |")
    print("| :--- | :--- | :--- | :--- | :--- |")
    for res in results:
        status = "✅ OK" if res["success"] else "❌ Failed"
        success_icon = "👍" if res["success"] else "👎"
        print(f"| {res['name']} | {res['duration']} | {res['turns']} | {success_icon} | {status} |")

if __name__ == "__main__":
    main()
