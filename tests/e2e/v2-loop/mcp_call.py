#!/usr/bin/env python3
"""Drive one MCP tool call against a v2 `exo node` sidecar over JSON-RPC/stdio.

Usage: mcp_call.py <exo_bin> <papers_path> <cwd> <tool_name> <args_json>

`exo-node`'s outbound loop (rust/exo-node/src/outbound.rs) hand-rolls a minimal JSON-RPC
server over stdin/stdout: `initialize` -> `tools/call`. This spawns `exo node --papers
<papers_path>` with cwd=<cwd>, performs that handshake, then closes stdin so the sidecar's
outbound loop (its lifetime anchor) ends and the process exits.

On success, prints one line of JSON to stdout: `{"text": ..., "data": ...}` — the tool's
`ToolOutput`, unpacked from the MCP `content` blocks (`data` comes from the second block,
which the sidecar renders as `"Data: <json>"`; `None` if the tool returned no data).

On failure (JSON-RPC error, timeout, unexpected stdout close), prints diagnostics to stderr
and exits non-zero.
"""
import json
import queue
import subprocess
import sys
import threading

TIMEOUT_S = 20


def read_lines(pipe, q):
    for line in iter(pipe.readline, ""):
        q.put(line)
    q.put(None)


def main():
    if len(sys.argv) != 6:
        print(
            "usage: mcp_call.py <exo_bin> <papers_path> <cwd> <tool_name> <args_json>",
            file=sys.stderr,
        )
        sys.exit(2)
    exo_bin, papers_path, cwd, tool_name, args_json = sys.argv[1:6]
    args = json.loads(args_json)

    proc = subprocess.Popen(
        [exo_bin, "node", "--papers", papers_path],
        cwd=cwd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
    )

    out_q = queue.Queue()
    reader = threading.Thread(target=read_lines, args=(proc.stdout, out_q), daemon=True)
    reader.start()

    def send(msg):
        proc.stdin.write(json.dumps(msg) + "\n")
        proc.stdin.flush()

    def recv():
        try:
            line = out_q.get(timeout=TIMEOUT_S)
        except queue.Empty:
            proc.kill()
            stderr = proc.stderr.read()
            print(
                f"mcp_call: timed out after {TIMEOUT_S}s waiting for exo node response; "
                f"stderr:\n{stderr}",
                file=sys.stderr,
            )
            sys.exit(1)
        if line is None:
            stderr = proc.stderr.read()
            print(
                f"mcp_call: exo node closed stdout unexpectedly; stderr:\n{stderr}",
                file=sys.stderr,
            )
            sys.exit(1)
        return json.loads(line)

    try:
        send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}})
        recv()
        send(
            {
                "jsonrpc": "2.0",
                "id": 2,
                "method": "tools/call",
                "params": {"name": tool_name, "arguments": args},
            }
        )
        response = recv()
    finally:
        try:
            proc.stdin.close()
        except Exception:
            pass

    try:
        proc.wait(timeout=10)
    except subprocess.TimeoutExpired:
        proc.kill()

    if "error" in response:
        print(json.dumps(response["error"]), file=sys.stderr)
        sys.exit(1)

    result = response.get("result", {})
    content = result.get("content", [])
    text = content[0]["text"] if content else ""
    data = None
    for block in content[1:]:
        block_text = block.get("text", "")
        if block_text.startswith("Data: "):
            data = json.loads(block_text[len("Data: "):])
            break

    print(json.dumps({"text": text, "data": data}))


if __name__ == "__main__":
    main()
