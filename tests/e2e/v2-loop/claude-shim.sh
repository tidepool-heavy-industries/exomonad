#!/usr/bin/env bash
# Fake `claude` binary for the v2-loop e2e test.
#
# A real node-mode child is launched as `claude --mcp-config <path> ... "<task>"`, where the
# real `claude` opens a conversational session and itself spawns `exo node --papers <papers>`
# as an MCP stdio subprocess (per <mcp-config>). This shim stands in for that whole
# conversational agent so the test never launches a real LLM or needs network/API keys.
#
# Role is read from the node's own papers (found via --mcp-config, never the positional
# task string) — deterministic, no parsing of natural-language instructions.
set -uo pipefail

STATUS_DIR="${EXO_TEST_STATUS_DIR:?EXO_TEST_STATUS_DIR not set}"

MCP_CONFIG=""
prev=""
for arg in "$@"; do
  if [[ "$prev" == "--mcp-config" ]]; then
    MCP_CONFIG="$arg"
  fi
  prev="$arg"
done

echo "$(date -Iseconds) shim invoked: pwd=$PWD mcp_config=$MCP_CONFIG" >>"$STATUS_DIR/shim.log"

ROLE="unknown"
PAPERS=""
if [[ -n "$MCP_CONFIG" && -f "$MCP_CONFIG" ]]; then
  PAPERS=$(python3 -c "
import json
d = json.load(open('$MCP_CONFIG'))
a = d['mcpServers']['exomonad']['args']
print(a[a.index('--papers') + 1])
" 2>>"$STATUS_DIR/shim.log") || PAPERS=""
  if [[ -n "$PAPERS" && -f "$PAPERS" ]]; then
    ROLE=$(python3 -c "import json; print(json.load(open('$PAPERS'))['role'])" \
      2>>"$STATUS_DIR/shim.log") || ROLE="unknown"
  fi
fi
echo "role=$ROLE papers=$PAPERS" >>"$STATUS_DIR/shim.log"

case "$ROLE" in
dev)
  # The one piece of "real work" a dev leaf does: a specific-path commit (never
  # `git add .` / `git add -A` — see .claude/rules/exomonad.md antipatterns).
  echo "e2e dev marker" >E2E_MARKER
  git add E2E_MARKER
  git commit -q -m "e2e: dev marker commit"
  echo ok >"$STATUS_DIR/dev.done"
  ;;
*)
  # root (and anything else): idle no-op. All spawn/merge orchestration for this test is
  # driven externally by run.sh's mcp_call.py, calling straight into this node's own papers
  # file — a live "conversational" process in the pane isn't needed for that, since each MCP
  # tool call is a fresh, stateless `exo node --papers <path>` subprocess.
  echo idle >"$STATUS_DIR/${ROLE}.idle"
  ;;
esac

exit 0
