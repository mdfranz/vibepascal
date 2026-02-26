import httpx
import time
import json
import sys

BASE_URL = "http://localhost:8000/game"

def send_command(command: str):
    print(f"\n>>> SENDING: {command}")
    try:
        response = httpx.post(BASE_URL, json={"command": command}, timeout=10.0)
        response.raise_for_status()
        data = response.json()
        
        # Extract the response output
        output = data.get("output", "")
        # Parsing for cleaner display
        if "> Game loaded." in output:
            parts = output.split("> Game loaded.")
            content = parts[1].split("> Game saved.")[0].strip()
            # Clean up the extra look output that LOAD always prints
            print(content)
        else:
            print(output)
            
    except Exception as e:
        print(f"Error: {e}")

def play_walkthrough():
    commands = [
        "NORTH",
        "EXAMINE WIRE",
        "SOUTH",
        "EAST",
        "TAKE CANTEEN",
        "WEST",
        "SOUTH",
        "EXAMINE PUMP",
        "INVENTORY"
    ]
    
    for cmd in commands:
        send_command(cmd)
        time.sleep(0.3) 

if __name__ == "__main__":
    print("--- Dustwood Client Walkthrough ---")
    play_walkthrough()
    print("\n--- Walkthrough Complete ---")
