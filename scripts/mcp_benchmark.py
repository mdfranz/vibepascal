import subprocess
import time
import json
import sys
import os
from typing import Dict, List, Any

# --- Configuration ---
MCP_URL = "http://127.0.0.1:8765/mcp"
DEFAULT_MODEL = "gpt-5-mini"
STRANDS_MODEL = "openai/gpt-5-mini"
GOAL = "Go East to the General Store, take the canteen, and then go West back to Main Street. Stop when you are back at Main Street."

CLIENTS = [
    {
        "name": "Pydantic AI",
        "script": "scripts/pydantic_mcp_client.py",
        "args": ["openai:gpt-5-mini", GOAL],
        "type": "json-rpc"
    },
    {
        "name": "MS Agent Framework",
        "script": "scripts/ms_agent_mcp_client.py",
        "args": [DEFAULT_MODEL, GOAL],
        "type": "json-rpc"
    },
    {
        "name": "Agno (formerly Phidata)",
        "script": "scripts/agno_mcp_client.py",
        "args": [DEFAULT_MODEL, GOAL],
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
        # Run with a 3-minute timeout per agent
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        stdout, stderr = process.communicate(timeout=180)
        end_time = time.time()
        
        duration = end_time - start_time
        exit_code = process.returncode
        
        # Combined output for search
        full_output = stdout + stderr
        
        # Simple heuristics to count turns
        # Pydantic logs HTTP Request for tools
        # MS Agent logs "Agent executing command:"
        # Agno logs "Agent executing command:"
        # Strands logs "Calling tool"
        turns = (full_output.count("Agent executing command:") + 
                 full_output.count("HTTP Request: POST http://127.0.0.1:8765/mcp") +
                 full_output.count("Calling tool"))
        
        # Adjust turn count because each turn might log multiple things
        if client["name"] == "Pydantic AI":
            # Pydantic AI logs 1 HTTP request for initial LOOK, then 1 per turn
            turns = max(0, turns - 1)
        elif client["name"] == "Agno (formerly Phidata)":
            # Agno logs 1 for LOOK, then 1 per turn
            turns = max(0, turns - 1)
        elif client["name"] == "MS Agent Framework":
            # MS Agent logs 1 for LOOK, then 1 per turn
            turns = max(0, turns - 1)

        # Check if goal was reached (General Store and Main Street mentioned)
        # We look for "General Store" and later "Main Street" in the narrative
        success = "General Store" in full_output and "Main Street" in full_output and "canteen" in full_output.lower()
        
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
