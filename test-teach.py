#!/usr/bin/env python3
"""
Test script for the teach MCP tool.

Usage:
    python3 test-teach.py "the scoring system" "compositeScore"
    python3 test-teach.py "graph exploration" "exploreEff,ScoutQuery"
"""
import socket
import json
import sys

def main():
    if len(sys.argv) < 3:
        print("Usage: test-teach.py <topic> <seeds>")
        print("  topic: what to teach (e.g., 'the scoring system')")
        print("  seeds: comma-separated symbol names (e.g., 'compositeScore,Rubric')")
        sys.exit(1)

    topic = sys.argv[1]
    seeds = sys.argv[2].split(',')
    budget = int(sys.argv[3]) if len(sys.argv) > 3 else 20

    msg = json.dumps({
        'type': 'MCPToolCall',
        'id': 'test-teach-1',
        'tool_name': 'teach',
        'arguments': {
            'topic': topic,
            'seeds': seeds,
            'budget': budget
        }
    }) + '\n'

    print(f"Connecting to .tidepool/control.sock...")
    print(f"  topic: {topic}")
    print(f"  seeds: {seeds}")
    print(f"  budget: {budget}")
    print()

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        sock.connect('.tidepool/control.sock')
        sock.sendall(msg.encode())

        # Read response
        response = b''
        while True:
            chunk = sock.recv(65536)
            if not chunk:
                break
            response += chunk
            # Check for complete JSON (ends with newline)
            if response.endswith(b'\n'):
                break

        if response:
            data = json.loads(response.decode())
            print(json.dumps(data, indent=2))

            # If successful, print teaching document summary
            if 'result' in data and data.get('result'):
                result = data['result']
                print("\n" + "=" * 60)
                print("TEACHING DOCUMENT SUMMARY")
                print("=" * 60)
                print(f"Title: {result.get('tdTitle', 'N/A')}")
                print(f"Topic: {result.get('tdTopic', 'N/A')}")
                print()

                prereqs = result.get('tdPrereqs', [])
                core = result.get('tdCore', [])
                support = result.get('tdSupport', [])

                print(f"Prerequisites ({len(prereqs)} units):")
                for unit in prereqs:
                    sym = unit.get('tuSymbol', {})
                    print(f"  - {sym.get('lsName', '?')}: {unit.get('tuRole', '?')}")

                print(f"\nCore ({len(core)} units):")
                for unit in core:
                    sym = unit.get('tuSymbol', {})
                    print(f"  - {sym.get('lsName', '?')}: {unit.get('tuRole', '?')}")

                print(f"\nSupport ({len(support)} units):")
                for unit in support:
                    sym = unit.get('tuSymbol', {})
                    print(f"  - {sym.get('lsName', '?')}: {unit.get('tuRole', '?')}")

                print()
                print(f"Total: {len(prereqs) + len(core) + len(support)} teaching units")
        else:
            print("No response received")

    except FileNotFoundError:
        print("Error: .tidepool/control.sock not found")
        print("Make sure tidepool-control-server is running in this directory")
        sys.exit(1)
    except ConnectionRefusedError:
        print("Error: Connection refused")
        print("Make sure tidepool-control-server is running")
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"Error parsing response: {e}")
        print(f"Raw response: {response.decode()}")
        sys.exit(1)
    finally:
        sock.close()

if __name__ == '__main__':
    main()
