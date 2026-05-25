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
        
        output = data.get("output", "")
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
