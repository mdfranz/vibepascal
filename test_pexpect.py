import pexpect
import sys

def test_game():
    print("--- Starting Game ---")
    try:
        child = pexpect.spawn('./dustwood', encoding='utf-8', dimensions=(24, 80))
        # Wait for the first prompt
        child.expect('> ')
        print(f"\n--- Initial Output ---\n{child.before}\n--- Done Initial ---")

        print("\n--- Sending HELP ---")
        child.sendline('HELP')
        child.expect('> ')
        print(f"\n--- HELP Output ---\n{child.before}\n--- Done HELP ---")

        print("\n--- Sending QUIT ---")
        child.sendline('QUIT')
        child.wait()
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    test_game()
