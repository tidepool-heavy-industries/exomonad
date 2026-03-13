#!/usr/bin/env bash
set -euo pipefail

# Setup: temp dir with git repo + config + WASM symlink
WORK_DIR=$(mktemp -d)
trap 'kill "$SERVER_PID" 2>/dev/null; rm -rf "$WORK_DIR"' EXIT

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WASM_SRC="$PROJECT_ROOT/.exo/wasm"
if [ ! -d "$WASM_SRC" ]; then
    echo "WASM not built. Run: just wasm-all" >&2
    exit 1
fi

mkdir -p "$WORK_DIR/.exo"
ln -s "$WASM_SRC" "$WORK_DIR/.exo/wasm"
cat > "$WORK_DIR/.exo/config.toml" <<'EOF'
default_role = "tl"
project_dir = "."
tmux_session = "test"
EOF

git -C "$WORK_DIR" init -q
git -C "$WORK_DIR" -c user.name=Test -c user.email=test@test.com commit -q --allow-empty -m initial

# Start server
EXOMONAD_BIN="$PROJECT_ROOT/target/debug/exomonad"
if [ ! -x "$EXOMONAD_BIN" ]; then
    echo "Building exomonad..." >&2
    cargo build -p exomonad --manifest-path "$PROJECT_ROOT/Cargo.toml"
fi

cd "$WORK_DIR"
"$EXOMONAD_BIN" serve &
SERVER_PID=$!

# Wait for health (socket creation)
for i in $(seq 1 100); do
    if [ -S ".exo/server.sock" ]; then break; fi
    if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        echo "Server exited early" >&2
        exit 1
    fi
    sleep 0.1
done

# Run tests
cd "$PROJECT_ROOT"
cargo test -p exomonad --test mcp_integration "$@"
