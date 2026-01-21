#!/usr/bin/env python3
import socket
import json
import sys
import os

SOCKET_PATH = os.environ.get('TIDEPOOL_CONTROL_SOCKET')
if not SOCKET_PATH:
    print("ERROR: TIDEPOOL_CONTROL_SOCKET environment variable not set")
    sys.exit(1)

query = sys.argv[1] if len(sys.argv) > 1 else "What uses Goto?"
symbols = sys.argv[2].split(',') if len(sys.argv) > 2 else ["Goto"]

sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect(SOCKET_PATH)
sock.settimeout(120)

msg = json.dumps({
    'type': 'MCPToolCall',
    'id': 'test-1',
    'tool_name': 'scout',
    'arguments': {
        'query': query,
        'symbols': symbols,
        'depth': 'shallow'
    }
}) + '\n'

sock.sendall(msg.encode())

response = b''
while True:
    chunk = sock.recv(4096)
    if not chunk or b'\n' in chunk:
        response += chunk
        break
    response += chunk

print(json.dumps(json.loads(response), indent=2))
sock.close()
