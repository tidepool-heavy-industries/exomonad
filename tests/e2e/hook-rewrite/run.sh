#!/usr/bin/env bash
set -euo pipefail

# E2E Hook Rewrite Test — Gemini Root Agent with PII Rewriting
# Uses a dedicated e2e-test WASM where the root role includes httpDevHooks.
# Gemini IS the root agent (--role root → e2e-test WASM root → PII hooks active).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
E2E_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$E2E_DIR/../.." && pwd)"

# --- Phase 0: Preconditions ---

echo ">>> [Phase 0] Checking preconditions..."

EXOMONAD_BIN=""
if command -v exomonad &>/dev/null; then
    EXOMONAD_BIN="$(command -v exomonad)"
elif [[ -x "$PROJECT_ROOT/target/debug/exomonad" ]]; then
    EXOMONAD_BIN="$PROJECT_ROOT/target/debug/exomonad"
else
    echo "ERROR: exomonad binary not found. Run 'just install-all-dev' or 'cargo build -p exomonad'."
    exit 1
fi
echo "  exomonad: $EXOMONAD_BIN"

# Check for e2e-test WASM specifically
if [[ ! -f "$PROJECT_ROOT/.exo/wasm/wasm-guest-e2e-test.wasm" ]]; then
    echo "ERROR: wasm-guest-e2e-test.wasm not found. Run 'just wasm e2e-test'."
    exit 1
fi
echo "  WASM: wasm-guest-e2e-test.wasm found"

for cmd in tmux git; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "ERROR: $cmd not found in PATH."
        exit 1
    fi
done
echo "  tmux, git: OK"

# --- Phase 1: Create temp environment ---

echo ">>> [Phase 1] Creating temp environment..."

WORK_DIR="$(mktemp -d /tmp/exomonad-e2e-rewrite.XXXXXXXX)"
echo "  Work dir: $WORK_DIR"

cleanup() {
    echo ""
    echo ">>> [Cleanup] Tearing down..."
    tmux kill-session -t e2e-rewrite 2>/dev/null || true
    echo "  Killed tmux session"
    rm -rf "$WORK_DIR"
    echo "  Removed $WORK_DIR"
    echo ">>> Done."
}
trap cleanup EXIT

# Create bare remote (Gemini agent needs a pushable remote)
REMOTE_DIR="$WORK_DIR/remote.git"
git init --bare "$REMOTE_DIR" -q

# Create working repo
REPO_DIR="$WORK_DIR/repo"
mkdir -p "$REPO_DIR"
cd "$REPO_DIR"
git init -q -b main
git remote add origin "$REMOTE_DIR"
git config user.name "Exomonad E2E"
git config user.email "e2e@example.com"
git commit --allow-empty -m "initial commit" -q
git push -u origin main -q

# Bootstrap via exomonad new
if ! "$EXOMONAD_BIN" new 2>&1 | sed 's/^/  /'; then
    echo "ERROR: 'exomonad new' failed during E2E setup."
    exit 1
fi

# Symlink WASM from project
mkdir -p .exo/wasm
for wasm_file in "$PROJECT_ROOT/.exo/wasm/"wasm-guest-*.wasm; do
    ln -sf "$wasm_file" ".exo/wasm/$(basename "$wasm_file")"
done

# Write config: Gemini root with e2e-test WASM
cat > .exo/config.toml <<'EOF'
default_role = "root"
wasm_name = "e2e-test"
shell_command = "bash"
tmux_session = "e2e-rewrite"
root_agent_type = "gemini"
yolo = true
initial_prompt = "Write a file named greeting.txt containing a greeting to John Smith, CEO of Acme Corp. Include jane.doe@acme.com. After writing, exit."

[[companions]]
name = "test-runner"
agent_type = "claude"
role = "testrunner"
model = "haiku"
command = "claude --dangerously-skip-permissions"
task = "Execute the test plan from your role context. Start immediately."
EOF

# Copy testrunner context into the e2e-test role
mkdir -p .exo/roles/e2e-test/context
cp "$SCRIPT_DIR/testrunner.md" .exo/roles/e2e-test/context/testrunner.md

# Create root TL rule for this test
mkdir -p .claude/rules
cp "$SCRIPT_DIR/e2e-test.md" .claude/rules/e2e-test.md

echo "  Repo: $REPO_DIR"
echo "  Remote: $REMOTE_DIR"

# --- Phase 2: Set environment ---

echo ">>> [Phase 2] Configuring environment..."

# Set dummy token to avoid auth errors
export GITHUB_TOKEN="test-token-e2e"
echo "  GITHUB_TOKEN=test-token-e2e"

# --- Phase 3: Run exomonad init ---

echo ">>> [Phase 3] Launching exomonad init..."

echo ""
echo "============================================"
echo "  E2E Hook Rewrite Test Ready"
echo "  Session: e2e-rewrite"
echo "  Work dir: $WORK_DIR/repo"
echo ""
echo "  Gemini root writes greeting.txt with PII terms"
echo "  PII hooks rewrite terms in BeforeModel/AfterModel"
echo "  Testrunner validates hook rewriting worked"
echo "============================================"
echo ""

# Launch exomonad init — creates tmux session and attaches.
"$EXOMONAD_BIN" init --session e2e-rewrite
