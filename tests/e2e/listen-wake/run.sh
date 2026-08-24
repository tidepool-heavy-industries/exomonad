#!/usr/bin/env bash
set -euo pipefail

# E2E: v2 listen wake channel (queue-until-armed -> drain -> live delivery -> re-arm redelivery).
#
# Boots a real v2 root (`exo init` for the papers), runs a persistent root sidecar
# (`exo node`), and exercises the delivery last hop end to end with NO real Claude:
#
#   1. A message appended to root's inbox BEFORE the sidecar even boots (the spawn->boot
#      window) is queued, not dropped: cursor-init-at-0 replays it.
#   2. With the sidecar up but NO `exo listen` client attached, the message stays queued
#      (cursor pinned) — nothing is delivered into the void.
#   3. Arming `exo listen` drains the backlog immediately (listener-attach pings the
#      inbound wake) — the queued message appears on the client's stdout, rendered with
#      its `[from: X, kind: Y]` header.
#   4. A message sent while the listener is live is delivered promptly.
#   5. Killing the listener re-queues: a message sent while unarmed is NOT delivered, the
#      cursor stays pinned, and a fresh listener receives it (with its ORIGINAL id —
#      at-least-once redelivery, ids are reference-only).
#
# The bus is the filesystem, so "a child sends a message" is an append of the same
# IngestionEntry line Bus::deliver writes — this script appends those lines directly
# instead of spawning a child, keeping the scenario focused on the delivery half.
# See tests/e2e/CLAUDE.md for the harness pattern this follows (offline, asserts, exits).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
E2E_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$E2E_DIR/../.." && pwd)"

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

# --- Phase 0: Preconditions ---

echo ">>> [Phase 0] Checking preconditions..."

# Prefer the workspace build over an installed `exo` — this scenario tests the code in the
# tree, and a stale ~/.cargo/bin/exo (predating the listen channel) would fail confusingly.
EXO_BIN=""
if [[ -x "$PROJECT_ROOT/target/debug/exo" ]]; then
  EXO_BIN="$PROJECT_ROOT/target/debug/exo"
elif command -v exo &>/dev/null; then
  EXO_BIN="$(command -v exo)"
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

# Short on purpose: the scratch HOME prefixes every UDS path
# (~/.claude/exo/sockets/{uuid-run-id}/pane-N.listen.sock), and a Unix socket path caps at
# SUN_LEN (108 bytes) — a long template here made the sidecar's socket binds fail outright.
WORK_DIR="$(mktemp -d /tmp/exo-lw.XXXXXX)"
SESSION="e2e-lwake-$$"
REPO_DIR="$WORK_DIR/repo"
SCRATCH_HOME="$WORK_DIR/home"
BIN_DIR="$WORK_DIR/bin"

mkdir -p "$REPO_DIR" "$SCRATCH_HOME" "$BIN_DIR"

SIDECAR_PID=""
LISTENER_PID=""
cleanup() {
  local rc=$?
  echo ""
  echo ">>> [Cleanup] Tearing down (exit code: $rc)..."
  [[ -n "$LISTENER_PID" ]] && kill "$LISTENER_PID" 2>/dev/null || true
  [[ -n "$SIDECAR_PID" ]] && kill "$SIDECAR_PID" 2>/dev/null || true
  tmux kill-session -t "$SESSION" 2>/dev/null && echo "  Killed tmux session $SESSION" \
    || echo "  No tmux session $SESSION to kill"
  rm -rf "$WORK_DIR"
  echo "  Removed $WORK_DIR"
  echo ">>> Done."
}
trap cleanup EXIT

echo "  Work dir: $WORK_DIR"
echo "  Session:  $SESSION"

# Scratch HOME keeps papers/inboxes/sockets off the real ~/.claude; the shim PATH-shadows
# any `claude` the root pane's launch command would invoke (it just idles — root's sidecar
# is run by this script, not by a conversational agent).
cp "$E2E_DIR/v2-loop/claude-shim.sh" "$BIN_DIR/claude"
chmod +x "$BIN_DIR/claude"

export HOME="$SCRATCH_HOME"
export PATH="$BIN_DIR:$PATH"

cd "$REPO_DIR"
git init -q -b main
git config user.name "Exomonad E2E"
git config user.email "e2e@example.com"
echo "base" >README.md
git add README.md
git commit -q -m "base"

# --- Phase 2: Root papers via `exo init` ---

echo ">>> [Phase 2] Booting v2 root ('exo init') to mint the papers..."

set +e
(cd "$REPO_DIR" && timeout 20 "$EXO_BIN" init --session "$SESSION" --recreate) \
  </dev/null >"$WORK_DIR/root-init.log" 2>&1
INIT_RC=$?
set -e
echo "  exo init exit code: $INIT_RC (non-zero is expected: the final tmux-attach has no tty)"

ROOT_PAPERS=""
for i in $(seq 1 20); do
  found="$(find "$REPO_DIR/.exo/node" -maxdepth 2 -name root.json 2>/dev/null | head -1 || true)"
  [[ -n "$found" ]] && { ROOT_PAPERS="$found"; break; }
  sleep 0.5
done
[[ -n "$ROOT_PAPERS" ]] || { cat "$WORK_DIR/root-init.log" >&2; fail "root papers never appeared"; }
RUN_ID="$(basename "$(dirname "$ROOT_PAPERS")")"
ROOT_PANE="$(jq -r .pane "$ROOT_PAPERS")"
PANE_N="${ROOT_PANE#\%}"
INBOX="$SCRATCH_HOME/.claude/exo/inboxes/$RUN_ID/pane-$PANE_N.jsonl"
CURSOR="$INBOX.cursor"
LISTEN_SOCK="$SCRATCH_HOME/.claude/exo/sockets/$RUN_ID/pane-$PANE_N.listen.sock"
STATUS_FILE="$SCRATCH_HOME/.claude/exo/status/$RUN_ID/pane-$PANE_N.json"
echo "  Root papers: $ROOT_PAPERS (pane $ROOT_PANE, run $RUN_ID)"

export EXOMONAD_SWARM_RUN_ID="$RUN_ID"
export EXOMONAD_TMUX_SESSION="$SESSION"

# Append one IngestionEntry line to root's inbox — the same wire line Bus::deliver writes
# (the filesystem IS the bus, so this is a real send, minus the child process).
append_entry() { # args: id summary text
  python3 - "$1" "$2" "$3" >>"$INBOX" <<'EOF'
import json, sys, datetime
ident, summary, text = sys.argv[1:4]
print(json.dumps({
    "v": 1,
    "ts": datetime.datetime.now(datetime.timezone.utc).isoformat().replace("+00:00", "Z"),
    "from": {"agent": "e2e-sender"},
    "id": ident,
    "kind": "chat",
    "summary": summary,
    "text": text,
}))
EOF
}

# --- Phase 3: A message lands BEFORE the sidecar boots (the spawn->boot window) ---

echo ">>> [Phase 3] Appending msg-1 before the sidecar exists..."
mkdir -p "$(dirname "$INBOX")"
append_entry "e2e-msg-1" "queued msg one" "sent before the sidecar booted"
echo "  [ok] msg-1 appended to $INBOX"

# --- Phase 4: Start the persistent root sidecar ---

echo ">>> [Phase 4] Starting the root sidecar ('exo node')..."

# `sleep infinity` holds the sidecar's stdin (the outbound MCP stdio anchor) open for the
# test's lifetime; no MCP handshake is needed for the delivery loops to run.
(cd "$REPO_DIR" && sleep infinity | "$EXO_BIN" node --papers "$ROOT_PAPERS") \
  >"$WORK_DIR/sidecar.log" 2>&1 &
SIDECAR_PID=$!

for i in $(seq 1 40); do
  [[ -S "$LISTEN_SOCK" ]] && break
  kill -0 "$SIDECAR_PID" 2>/dev/null || { cat "$WORK_DIR/sidecar.log" >&2; fail "sidecar died at boot"; }
  sleep 0.25
  if [[ "$i" -eq 40 ]]; then
    echo "--- sidecar.log ---" >&2; cat "$WORK_DIR/sidecar.log" >&2 || true
    fail "listen socket never appeared: $LISTEN_SOCK"
  fi
done
echo "  [ok] sidecar up, listen socket bound: $LISTEN_SOCK"

# --- Phase 5: Unarmed => queued, not delivered ---

echo ">>> [Phase 5] Asserting msg-1 queues while no listener is attached..."
sleep 2
[[ ! -f "$CURSOR" ]] || [[ "$(tr -d '[:space:]' <"$CURSOR")" == "0" ]] \
  || fail "cursor advanced with no listener attached: $(cat "$CURSOR")"
echo "  [ok] cursor pinned (msg-1 queued durably)"

# --- Phase 6: Arming the listener drains the backlog ---

echo ">>> [Phase 6] Arming 'exo listen' — the queued message must drain..."

"$EXO_BIN" listen --papers "$ROOT_PAPERS" >"$WORK_DIR/listener1.out" 2>"$WORK_DIR/listener1.err" &
LISTENER_PID=$!

for i in $(seq 1 40); do
  grep -q "queued msg one" "$WORK_DIR/listener1.out" 2>/dev/null && break
  sleep 0.25
  if [[ "$i" -eq 40 ]]; then
    echo "--- listener1.out ---" >&2; cat "$WORK_DIR/listener1.out" >&2 || true
    echo "--- sidecar.log ---" >&2; tail -30 "$WORK_DIR/sidecar.log" >&2 || true
    fail "queued msg-1 never drained to the listener"
  fi
done
grep -q "\[from: e2e-sender, kind: chat, id: e2e-msg-1\]" "$WORK_DIR/listener1.out" \
  || fail "msg-1 missing its rendered header: $(cat "$WORK_DIR/listener1.out")"
echo "  [ok] boot-window msg-1 drained on arm, with header + id"

for i in $(seq 1 20); do
  [[ -f "$CURSOR" && "$(tr -d '[:space:]' <"$CURSOR")" != "0" ]] && break
  sleep 0.25
  [[ "$i" -eq 20 ]] && fail "cursor did not advance after acked delivery"
done
echo "  [ok] cursor advanced after the acked delivery"

# --- Phase 7: Live delivery while armed ---

echo ">>> [Phase 7] Live delivery while the listener is armed..."
append_entry "e2e-msg-2" "live msg two" "sent while armed"
for i in $(seq 1 40); do
  grep -q "live msg two" "$WORK_DIR/listener1.out" 2>/dev/null && break
  sleep 0.25
  [[ "$i" -eq 40 ]] && fail "msg-2 not delivered while armed"
done
echo "  [ok] msg-2 delivered live"

# --- Phase 8: The status snapshot shows the armed listener ---

echo ">>> [Phase 8] Status snapshot shows listener_connected..."
for i in $(seq 1 30); do
  [[ -f "$STATUS_FILE" ]] && [[ "$(jq -r .listener_connected "$STATUS_FILE")" == "true" ]] && break
  sleep 0.5
  [[ "$i" -eq 30 ]] && fail "status never showed listener_connected=true: $(cat "$STATUS_FILE" 2>/dev/null)"
done
echo "  [ok] listener_connected=true in $STATUS_FILE"

# --- Phase 9: Kill the listener -> re-queue -> re-arm -> redelivery with the ORIGINAL id ---

echo ">>> [Phase 9] Killing the listener; a new message must queue, then redeliver on re-arm..."
kill "$LISTENER_PID"
wait "$LISTENER_PID" 2>/dev/null || true
LISTENER_PID=""
sleep 1

CURSOR_BEFORE="$(tr -d '[:space:]' <"$CURSOR")"
append_entry "e2e-msg-3" "queued msg three" "sent while unarmed"
sleep 3
CURSOR_AFTER="$(tr -d '[:space:]' <"$CURSOR")"
[[ "$CURSOR_BEFORE" == "$CURSOR_AFTER" ]] \
  || fail "cursor advanced while unarmed ($CURSOR_BEFORE -> $CURSOR_AFTER): msg-3 fell into the void"
echo "  [ok] msg-3 queued (cursor pinned at $CURSOR_AFTER)"

"$EXO_BIN" listen --papers "$ROOT_PAPERS" >"$WORK_DIR/listener2.out" 2>"$WORK_DIR/listener2.err" &
LISTENER_PID=$!
for i in $(seq 1 40); do
  grep -q "\[from: e2e-sender, kind: chat, id: e2e-msg-3\]" "$WORK_DIR/listener2.out" 2>/dev/null && break
  sleep 0.25
  if [[ "$i" -eq 40 ]]; then
    echo "--- listener2.out ---" >&2; cat "$WORK_DIR/listener2.out" >&2 || true
    echo "--- sidecar.log ---" >&2; tail -30 "$WORK_DIR/sidecar.log" >&2 || true
    fail "msg-3 never redelivered to the re-armed listener"
  fi
done
echo "  [ok] msg-3 redelivered to the fresh listener with its ORIGINAL id"

echo ""
echo "============================================"
echo "  listen-wake E2E: PASS"
echo "============================================"
