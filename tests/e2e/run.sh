#!/usr/bin/env bash
set -euo pipefail

# E2E Test Orchestrator
# Sets up a fully mocked environment, then drops you into a real exomonad init
# tmux session where all GitHub interactions hit mocks instead of real APIs.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

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

if [[ ! -d "$PROJECT_ROOT/.exo/wasm" ]] || ! ls "$PROJECT_ROOT/.exo/wasm/"wasm-guest-*.wasm &>/dev/null; then
    echo "ERROR: No WASM plugins found in $PROJECT_ROOT/.exo/wasm/. Run 'just wasm-all'."
    exit 1
fi
echo "  WASM: $(ls "$PROJECT_ROOT/.exo/wasm/"wasm-guest-*.wasm)"

for cmd in tmux python3 git; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "ERROR: $cmd not found in PATH."
        exit 1
    fi
done
echo "  tmux, python3, git: OK"

# --- Phase 1: Create temp environment ---

echo ">>> [Phase 1] Creating temp environment..."

WORK_DIR="$(mktemp -d /tmp/exomonad-e2e.XXXXXXXX)"
echo "  Work dir: $WORK_DIR"

MOCK_PID=""
cleanup() {
    echo ""
    echo ">>> [Cleanup] Tearing down..."
    if [[ -n "$MOCK_PID" ]] && kill -0 "$MOCK_PID" 2>/dev/null; then
        kill "$MOCK_PID" 2>/dev/null || true
        wait "$MOCK_PID" 2>/dev/null || true
        echo "  Killed mock GitHub API (PID $MOCK_PID)"
    fi
    # Clean up tmux global env vars
    for var in GITHUB_API_URL MOCK_LOG GH_MOCK_LOG REMOTE_DIR MOCK_PORT E2E_SCRIPT_DIR MOCK_STDOUT; do
        tmux set-environment -gu "$var" 2>/dev/null || true
    done
    tmux kill-session -t e2e-test 2>/dev/null || true
    echo "  Killed tmux session"
    rm -rf "$WORK_DIR"
    echo "  Removed $WORK_DIR"
    echo ">>> Done."
}
trap cleanup EXIT

# Create bare remote for push/fetch
REMOTE_DIR="$WORK_DIR/remote.git"
git init --bare "$REMOTE_DIR" -q

# Create working repo
REPO_DIR="$WORK_DIR/repo"
mkdir -p "$REPO_DIR"
cd "$REPO_DIR"
git init -q -b main
git remote add origin "$REMOTE_DIR"

# Configure local Git identity for reproducible commits in fresh environments
git config user.name "Exomonad E2E"
git config user.email "e2e@example.com"

# Initial commit + push
git commit --allow-empty -m "initial commit" -q
git push -u origin main -q

# Bootstrap via exomonad new — generates .exo/config.toml, .gitignore, copies WASM + rules
if ! "$EXOMONAD_BIN" new 2>&1 | sed 's/^/  /'; then
    echo "ERROR: 'exomonad new' failed during E2E setup. See output above."
    exit 1
fi

# Symlink WASM from project (overwrite whatever new copied — symlinks save disk)
mkdir -p .exo/wasm
for wasm_file in "$PROJECT_ROOT/.exo/wasm/"wasm-guest-*.wasm; do
    ln -sf "$wasm_file" ".exo/wasm/$(basename "$wasm_file")"
done

# Patch config: use bash instead of nix develop (temp env has no flake.nix)
if [[ -f .exo/config.toml ]]; then
    # Append/override shell_command
    if grep -q 'shell_command' .exo/config.toml; then
        sed -i 's|^shell_command.*|shell_command = "bash"|' .exo/config.toml
    else
        echo 'shell_command = "bash"' >> .exo/config.toml
    fi
else
    cat > .exo/config.toml <<'EOF'
default_role = "devswarm"
wasm_name = "devswarm"
shell_command = "bash"
EOF
fi

# Set session name, root TL model, poller interval, and companion config
cat >> .exo/config.toml <<'EOF'
tmux_session = "e2e-test"
model = "sonnet"
yolo = true
poll_interval = 10

[[companions]]
name = "test-runner"
agent_type = "claude"
role = "testrunner"
model = "haiku"
command = "claude --dangerously-skip-permissions"
task = "Execute the test plan from your role context. Start immediately."
EOF

# Create e2e test mode rule for the root TL
mkdir -p .claude/rules
cat > .claude/rules/e2e-test.md <<'EOF'
# E2E Test Mode — Root TL Protocol

You are the ROOT TECH LEAD in E2E test mode. A test-runner companion sends you instructions via Teams inbox. Follow them exactly.

## Your Tools
- **`spawn_worker`** — Spawn ephemeral Gemini workers in tmux panes (no branch, no PR). They share your working directory. Use for scaffolding, boilerplate, non-conflicting edits.
- **`fork_wave`** — Fork parallel Claude sub-TLs in own worktrees. They inherit your full conversation context. Each gets branch main.{slug}. They can spawn their own workers/leaves.
- **`spawn_gemini`** — Spawn Gemini leaves in own worktrees. Each files its own PR.
- **`merge_pr`** — Merge a child's PR by number.
- **`file_pr`** — File a PR for your current branch.

## How spawn_worker Works
Workers are ephemeral Gemini agents in tmux panes. They run in YOUR directory — no branch, no PR. Use them for:
- Scaffolding (create directories, boilerplate files)
- Parallel file creation that you'll commit yourself afterward
- Research or one-shot edits

After workers complete their task, they exit and the pane closes. You then commit and push their work yourself.

## How fork_wave Works
`fork_wave` creates Claude agents in their own git worktrees. Each child:
1. Gets its own branch (main.{slug})
2. Inherits your full conversation context (they know the task)
3. Has TL-role tools (fork_wave, spawn_gemini, spawn_worker, file_pr, merge_pr, notify_parent)
4. Works independently — can spawn their own sub-tree of agents

Children file PRs targeting YOUR branch. You merge them when notified.

## How spawn_gemini Works
`spawn_gemini` creates Gemini agents in their own git worktrees. Each gets:
1. Its own branch ({parent_branch}.{slug})
2. A self-contained spec (task, steps, verify, boundary, read_first)
3. Dev-role tools (file_pr, notify_parent — no spawning)

Gemini leaves file PRs. Their PRs target the SPAWNER's branch (your branch or a sub-TL's branch).

## Execution Protocol
1. Read the instruction from test-runner carefully
2. Execute EXACTLY what is asked — use the specified tool, slug names, file paths
3. After spawning agents, IDLE — end your turn and wait for notifications
4. When notified ([FIXES PUSHED], [REVIEW TIMEOUT], [PR READY]), merge with merge_pr
5. After merging, check if more waves are needed

## NEVER Do These Things
- NEVER run `gh pr create` or `gh` commands — use MCP tools only
- NEVER curl the server socket directly
- NEVER do work that belongs to a child agent
- NEVER take on a child's identity or act on behalf of a child
- NEVER investigate or debug child failures — re-decompose or escalate

## Notification Vocabulary
- `[from: id] ...` — status report from child. Read it. Do not act AS the child.
- `[PR READY]` — Copilot approved. Merge with `merge_pr`.
- `[FIXES PUSHED]` — child addressed review. Merge if CI passes.
- `[REVIEW TIMEOUT]` — no review after timeout. Merge if CI passes.
- `[FAILED: id]` — child exhausted retries. Re-decompose or escalate.
EOF

echo "  Repo: $REPO_DIR"
echo "  Remote: $REMOTE_DIR"

# --- Phase 2: Start mock GitHub API ---

echo ">>> [Phase 2] Starting mock GitHub API..."

# Pick ephemeral port
MOCK_PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()')
export MOCK_LOG="$WORK_DIR/mock_github.log"

MOCK_STDOUT="$WORK_DIR/mock_github_stdout.log"
REMOTE_DIR="$REMOTE_DIR" MOCK_LOG="$MOCK_LOG" python3 "$SCRIPT_DIR/mock_github.py" --port "$MOCK_PORT" \
    > "$MOCK_STDOUT" 2>&1 &
MOCK_PID=$!

# Poll until responsive
for i in $(seq 1 20); do
    if curl -sf "http://127.0.0.1:$MOCK_PORT/repos/test/repo/pulls" &>/dev/null; then
        echo "  Mock GitHub API listening on port $MOCK_PORT (PID $MOCK_PID)"
        break
    fi
    if [[ $i -eq 20 ]]; then
        echo "ERROR: Mock GitHub API failed to start. Output:"
        cat "$MOCK_STDOUT" 2>/dev/null || true
        exit 1
    fi
    sleep 0.25
done

# --- Phase 3: Set environment ---

echo ">>> [Phase 3] Configuring environment..."

export PATH="$SCRIPT_DIR:$PATH"
export GITHUB_TOKEN="test-token-e2e"
export GITHUB_API_URL="http://127.0.0.1:$MOCK_PORT"
export GH_MOCK_LOG="$WORK_DIR/gh_mock.log"

# Set tmux global env vars so companion windows inherit them
tmux set-environment -g GITHUB_API_URL "http://127.0.0.1:$MOCK_PORT"
tmux set-environment -g MOCK_LOG "$MOCK_LOG"
tmux set-environment -g GH_MOCK_LOG "$GH_MOCK_LOG"
tmux set-environment -g REMOTE_DIR "$REMOTE_DIR"
tmux set-environment -g MOCK_PORT "$MOCK_PORT"
tmux set-environment -g E2E_SCRIPT_DIR "$SCRIPT_DIR"
tmux set-environment -g MOCK_STDOUT "$MOCK_STDOUT"

echo "  PATH prepended with: $SCRIPT_DIR (mock_gh)"
echo "  GITHUB_TOKEN=test-token-e2e"
echo "  GITHUB_API_URL=http://127.0.0.1:$MOCK_PORT"
echo "  GH_MOCK_LOG=$GH_MOCK_LOG"
echo "  MOCK_LOG=$MOCK_LOG"
echo "  REMOTE_DIR=$REMOTE_DIR"

# --- Phase 4: Run exomonad init ---

echo ">>> [Phase 4] Launching exomonad init..."

# Copy helper scripts into repo for convenience
cp "$SCRIPT_DIR/validate.sh" "$WORK_DIR/repo/validate.sh" 2>/dev/null || true
cp "$SCRIPT_DIR/post_review.sh" "$WORK_DIR/repo/post_review.sh" 2>/dev/null || true

echo ""
echo "============================================"
echo "  E2E Environment Ready"
echo "  Session: e2e-test"
echo "  Work dir: $WORK_DIR/repo"
echo "  Mock GitHub: http://127.0.0.1:$MOCK_PORT"
echo "  Mock request log: $MOCK_LOG"
echo "  Mock stdout/err: $MOCK_STDOUT"
echo "  GH mock log: $GH_MOCK_LOG"
echo "  Remote dir: $REMOTE_DIR"
echo ""
echo "  Run ./validate.sh from the TL window"
echo "  to verify the pipeline."
echo "============================================"
echo ""

# Launch exomonad init — creates tmux session and attaches
"$EXOMONAD_BIN" init --session e2e-test
