#!/usr/bin/env bash
set -euo pipefail

# Setup: temp dir with git repo + config + WASM symlink
WORK_DIR=$(mktemp -d)
trap 'kill "$SERVER_PID" 2>/dev/null; rm -rf "$WORK_DIR"' EXIT

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WASM_SRC="$PROJECT_ROOT/.exomonad/wasm"
if [ ! -d "$WASM_SRC" ]; then
    echo "WASM not built. Run: just wasm-all" >&2
    exit 1
fi

mkdir -p "$WORK_DIR/.exomonad"
ln -s "$WASM_SRC" "$WORK_DIR/.exomonad/wasm"
cat > "$WORK_DIR/.exomonad/config.toml" <<'EOF'
default_role = "tl"
project_dir = "."
zellij_session = "test"
EOF

git -C "$WORK_DIR" init -q
git -C "$WORK_DIR" -c user.name=Test -c user.email=test@test.com commit -q --allow-empty -m initial

# Find free port, start server
PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("",0)); print(s.getsockname()[1]); s.close()')

EXOMONAD_BIN="$PROJECT_ROOT/target/debug/exomonad"
if [ ! -x "$EXOMONAD_BIN" ]; then
    echo "Building exomonad..." >&2
    cargo build -p exomonad --manifest-path "$PROJECT_ROOT/Cargo.toml"
fi

cd "$WORK_DIR"
"$EXOMONAD_BIN" serve --port "$PORT" &
SERVER_PID=$!

# Wait for health
for i in $(seq 1 100); do
    if curl -sf "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then break; fi
    if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        echo "Server exited early" >&2
        exit 1
    fi
    sleep 0.1
done

# Run tests
cd "$PROJECT_ROOT"
MCP_TEST_PORT="$PORT" cargo test -p exomonad --test mcp_integration "$@"
