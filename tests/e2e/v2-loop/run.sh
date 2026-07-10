#!/usr/bin/env bash
set -euo pipefail

# E2E: v2 node-mode core loop (spawn -> child commit -> parent merge -> teardown).
#
# Boots a real v2 root node (`exo init`), spawns a real dev child (`spawn_dev`, over a live
# `exo node` MCP sidecar), lets the child commit, then merges the child's branch back
# (`merge`) and asserts teardown. No real Claude is launched — a fake `claude` shim
# (claude-shim.sh) stands in for the whole conversational agent, so this runs offline, with
# no network/API keys. See tests/e2e/CLAUDE.md for the harness pattern this follows.
#
# Root's own orchestration (spawn_dev, merge) is driven directly by this script via
# mcp_call.py against root's papers file — NOT via a "live" root shim process — because an
# MCP tool call is a fresh, stateless `exo node --papers <path>` subprocess regardless of
# whether some conversational agent happens to be running in root's tmux pane. This is the
# documented fallback scope from the spec: the spawn+teardown halves are driven and asserted
# directly; the child's own `submit_branch` MCP round-trip (a child calling submit_branch
# through ITS OWN sidecar, which would in turn need a spawned reviewer or the review-disabled
# skip path) is not exercised — the dev child here commits and the parent merges directly,
# matching how `submit_branch` behaves anyway when `review_enabled` is off (the project
# default): it forwards `[READY]` straight to the parent with no reviewer spawn. Follow-up:
# drive `submit_branch` itself through the child's sidecar for full fidelity.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
E2E_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$E2E_DIR/../.." && pwd)"

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

# --- Phase 0: Preconditions ---

echo ">>> [Phase 0] Checking preconditions..."

EXO_BIN=""
if command -v exo &>/dev/null; then
  EXO_BIN="$(command -v exo)"
elif [[ -x "$PROJECT_ROOT/target/debug/exo" ]]; then
  EXO_BIN="$PROJECT_ROOT/target/debug/exo"
else
  fail "exo binary not found. Run 'cargo build -p exo' (or 'just install-all-dev')."
fi
echo "  exo: $EXO_BIN"

for cmd in tmux git python3 jq; do
  command -v "$cmd" &>/dev/null || fail "$cmd not found in PATH."
done
echo "  tmux, git, python3, jq: OK"

# --- Phase 1: Scratch environment ---

echo ">>> [Phase 1] Creating scratch environment..."

WORK_DIR="$(mktemp -d /tmp/exomonad-e2e-v2loop.XXXXXXXX)"
SESSION="e2e-v2loop-$$"
REPO_DIR="$WORK_DIR/repo"
SCRATCH_HOME="$WORK_DIR/home"
BIN_DIR="$WORK_DIR/bin"
STATUS_DIR="$WORK_DIR/status"
MCP_CLIENT="$SCRIPT_DIR/mcp_call.py"

mkdir -p "$REPO_DIR" "$SCRATCH_HOME" "$BIN_DIR" "$STATUS_DIR"

cleanup() {
  local rc=$?
  echo ""
  echo ">>> [Cleanup] Tearing down (exit code: $rc)..."
  tmux kill-session -t "$SESSION" 2>/dev/null && echo "  Killed tmux session $SESSION" \
    || echo "  No tmux session $SESSION to kill"
  rm -rf "$WORK_DIR"
  echo "  Removed $WORK_DIR"
  echo ">>> Done."
}
trap cleanup EXIT

echo "  Work dir: $WORK_DIR"
echo "  Session:  $SESSION"

# A scratch $HOME keeps node papers/inboxes/sockets (all under ~/.claude/exo/...) off the
# developer's real ~/.claude — see rust/exo-caps/CLAUDE.md paths. A scratch bin dir prepended
# to PATH shadows the real `claude` binary with the offline shim for every process spawned
# inside this scratch tmux session, without touching the real PATH for anything else.
cp "$SCRIPT_DIR/claude-shim.sh" "$BIN_DIR/claude"
chmod +x "$BIN_DIR/claude"

export HOME="$SCRATCH_HOME"
export PATH="$BIN_DIR:$PATH"
export EXO_TEST_STATUS_DIR="$STATUS_DIR"

cd "$REPO_DIR"
git init -q -b main
git config user.name "Exomonad E2E"
git config user.email "e2e@example.com"
echo "base" >README.md
git add README.md
git commit -q -m "base"

echo "  Repo: $REPO_DIR"

# --- Phase 2: Boot the v2 root node ---

echo ">>> [Phase 2] Booting v2 root node ('exo init')..."

# `exo init` ends by exec'ing `tmux attach-session` (rust/exo/src/init.rs) — with stdin/stdout
# redirected away from a tty, that attach fails fast and the process exits non-zero. That's
# expected and fine here: every boot step (tmux session, root papers, root launch) already ran
# by the time it gets there. `timeout` is a safety net in case tmux ever blocks instead.
set +e
(cd "$REPO_DIR" && timeout 20 "$EXO_BIN" init --session "$SESSION" --recreate) \
  </dev/null >"$WORK_DIR/root-init.log" 2>&1
INIT_RC=$?
set -e
echo "  exo init exit code: $INIT_RC (non-zero is expected: the final tmux-attach has no tty)"

for i in $(seq 1 20); do
  tmux has-session -t "$SESSION" 2>/dev/null && break
  sleep 0.5
  if [[ "$i" -eq 20 ]]; then
    echo "--- root-init.log ---" >&2
    cat "$WORK_DIR/root-init.log" >&2
    fail "root tmux session '$SESSION' never appeared"
  fi
done
echo "  tmux session up: $SESSION"

ROOT_PAPERS=""
for i in $(seq 1 20); do
  found="$(find "$REPO_DIR/.exo/node" -maxdepth 2 -name root.json 2>/dev/null | head -1 || true)"
  if [[ -n "$found" ]]; then
    ROOT_PAPERS="$found"
    break
  fi
  sleep 0.5
done
[[ -n "$ROOT_PAPERS" ]] || fail "root papers (.exo/node/*/root.json) never appeared"
RUN_ID="$(basename "$(dirname "$ROOT_PAPERS")")"
echo "  Root papers: $ROOT_PAPERS"
echo "  Run ID: $RUN_ID"

# Every `exo node` subprocess this script drives (via mcp_call.py) needs the same ambient
# context a real node reads at bootstrap (rust/exo-node/src/bootstrap.rs): run id + tmux
# session (both hard-required — MissingContext, no silent default) plus $HOME (already scratch).
export EXOMONAD_SWARM_RUN_ID="$RUN_ID"
export EXOMONAD_TMUX_SESSION="$SESSION"

# LOAD-BEARING, not just defensive: a brand-new tmux SESSION does not inherit the env of the
# client process that created it when the tmux SERVER was already running (e.g. from other
# `exo`/tmux sessions on the developer's machine) — it inherits the server's original startup
# environment instead (verified empirically; `tmux new-session` on an existing server does NOT
# propagate the creating client's exported vars). So our scratch PATH/HOME/status-dir may not
# reach ANY pane in this session without this. `set-environment` only reaches panes created
# AFTER the call ("tmux set-environment ordering" — see project memory), which is exactly what
# we need: the dev child (spawned below) is such a pane. Root's own pane predates this call and
# is never relied on (see the file header — all orchestration is driven externally).
tmux set-environment -t "$SESSION" PATH "$PATH"
tmux set-environment -t "$SESSION" HOME "$SCRATCH_HOME"
tmux set-environment -t "$SESSION" EXO_TEST_STATUS_DIR "$STATUS_DIR"

# --- Phase 3: Spawn a dev child ---

echo ">>> [Phase 3] Spawning a dev child (spawn_dev)..."

CHILD_NAME="devkid"
SPAWN_ARGS='{"name":"devkid","task":"E2E test task: create a file named E2E_MARKER with any content and commit it with `git add E2E_MARKER` (a specific path — never `git add .` or `git add -A`), then stop. Do nothing else."}'

if ! SPAWN_JSON="$(python3 "$MCP_CLIENT" "$EXO_BIN" "$ROOT_PAPERS" "$REPO_DIR" spawn_dev "$SPAWN_ARGS")"; then
  fail "spawn_dev tool call failed"
fi
echo "  spawn_dev result: $SPAWN_JSON"

NAME="$(python3 -c "import json,sys; print(json.loads(sys.argv[1])['data']['spawned'])" "$SPAWN_JSON")"
[[ "$NAME" == "$CHILD_NAME" ]] || fail "expected spawned child '$CHILD_NAME', got '$NAME'"

# --- Phase 3 assertions: spawn produced a real worktree + branch + ledger record ---

CHILD_DIR="$REPO_DIR/.exo/worktrees/$NAME"
EXPECTED_BRANCH="root.$NAME"

[[ -d "$CHILD_DIR" ]] || fail "child worktree does not exist: $CHILD_DIR"
echo "  [ok] worktree exists at $CHILD_DIR"

ACTUAL_BRANCH="$(git -C "$CHILD_DIR" rev-parse --abbrev-ref HEAD)"
[[ "$ACTUAL_BRANCH" == "$EXPECTED_BRANCH" ]] \
  || fail "child worktree on branch '$ACTUAL_BRANCH', expected '$EXPECTED_BRANCH'"
echo "  [ok] worktree is on branch $EXPECTED_BRANCH"

LEDGER="$REPO_DIR/.exo/children.jsonl"
[[ -f "$LEDGER" ]] || fail "children.jsonl does not exist: $LEDGER"
SPAWNED_LINE="$(jq -c "select(.record==\"spawned\" and .child==\"$NAME\")" "$LEDGER" | tail -1)"
[[ -n "$SPAWNED_LINE" ]] || fail "no 'spawned' record for '$NAME' in $LEDGER"
echo "  [ok] children.jsonl has a Spawned record: $SPAWNED_LINE"

CHILD_PANE="$(echo "$SPAWNED_LINE" | jq -r .pane)"
[[ -n "$CHILD_PANE" && "$CHILD_PANE" != "null" ]] || fail "Spawned record missing a pane id"
echo "  [ok] Spawned record carries pane $CHILD_PANE"

# NOTE: `ChildRecord::Started` (the child's own boot check-in) is defined in
# rust/exo-caps/src/lifecycle.rs but nothing in the current runtime/exo-node ever appends
# one (verified: no `ChildRecord::Started` construction outside tests) — so we deliberately
# do NOT assert a Started record here. This is a real gap in the v2 loop, not a shim
# limitation; flagged in this scenario's notify_parent report.

if [[ "${E2E_INJECT_FAILURE:-}" == "1" ]]; then
  fail "injected failure (E2E_INJECT_FAILURE=1) — verifying the cleanup trap runs unconditionally"
fi

# --- Phase 4: Wait for the child to commit ---

echo ">>> [Phase 4] Waiting for the dev child to commit..."

MARKER="$CHILD_DIR/E2E_MARKER"
for i in $(seq 1 60); do
  [[ -f "$MARKER" ]] && break
  sleep 0.5
  if [[ "$i" -eq 60 ]]; then
    echo "--- $STATUS_DIR/shim.log ---" >&2
    cat "$STATUS_DIR/shim.log" >&2 2>/dev/null || true
    fail "child never committed $MARKER within timeout"
  fi
done

COMMIT_MSG="$(git -C "$CHILD_DIR" log -1 --format=%s)"
[[ "$COMMIT_MSG" == "e2e: dev marker commit" ]] \
  || fail "unexpected child commit message: '$COMMIT_MSG'"
echo "  [ok] child committed: $COMMIT_MSG"

# --- Phase 5: Parent merges the child's branch ---

echo ">>> [Phase 5] Merging the child's branch (merge)..."

MERGE_ARGS="$(python3 -c "import json,sys; print(json.dumps({'branch': 'root.' + sys.argv[1], 'child': sys.argv[1]}))" "$NAME")"
if ! MERGE_JSON="$(python3 "$MCP_CLIENT" "$EXO_BIN" "$ROOT_PAPERS" "$REPO_DIR" merge "$MERGE_ARGS")"; then
  fail "merge tool call failed"
fi
echo "  merge result: $MERGE_JSON"

# --- Phase 5 assertions: merge landed the commit + tore the child down ---

[[ -f "$REPO_DIR/E2E_MARKER" ]] || fail "merge did not bring E2E_MARKER into root's working tree"
echo "  [ok] E2E_MARKER present in root's working tree"

git -C "$REPO_DIR" log --oneline | grep -q "e2e: dev marker commit" \
  || fail "child's commit is not reachable from root's branch"
echo "  [ok] child's commit is reachable from root's branch"

[[ ! -d "$CHILD_DIR" ]] || fail "child worktree directory still exists after merge: $CHILD_DIR"
echo "  [ok] child worktree directory reclaimed"

if tmux list-panes -a -F '#{pane_id}' 2>/dev/null | grep -qx "$CHILD_PANE"; then
  fail "child pane $CHILD_PANE is still alive after merge"
fi
echo "  [ok] child pane $CHILD_PANE is dead"

echo ""
echo "============================================"
echo "  v2-loop E2E: PASS"
echo "============================================"
