#!/usr/bin/env bash
# exomonad-bootstrap: Set up exomonad in any git repository.
# Called inside the container after cloning a target repo.
set -euo pipefail

cd "${1:-.}"

# ---- Pre-flight checks ----
if ! git rev-parse --is-inside-work-tree &>/dev/null; then
    echo "ERROR: $(pwd) is not a git repository" >&2
    exit 1
fi

if ! command -v exomonad &>/dev/null; then
    echo "ERROR: exomonad not found on PATH" >&2
    exit 1
fi

if ! command -v claude &>/dev/null; then
    echo "ERROR: claude not found on PATH" >&2
    exit 1
fi

if ! command -v gemini &>/dev/null; then
    echo "WARNING: gemini not found on PATH (Gemini agents will not work)" >&2
fi

# ---- Ensure Claude Code settings survive the ~/.claude/ mount ----
# The host's ~/.claude/ is bind-mounted (rw), which may not have
# skipDangerousModePermissionPrompt. Without it, the permissions dialog
# hangs inside tmux (stdin deadlock during Ink component transition).
SETTINGS="$HOME/.claude/settings.json"
if [ -f "$SETTINGS" ]; then
    # Merge the flag into existing settings
    tmp=$(jq '. + {"skipDangerousModePermissionPrompt":true}' "$SETTINGS")
    echo "$tmp" > "$SETTINGS"
else
    echo '{"skipDangerousModePermissionPrompt":true}' > "$SETTINGS"
fi

# ---- Copy pre-built WASM into project ----
mkdir -p .exo/wasm
cp /opt/exomonad/wasm/wasm-guest-devswarm.wasm .exo/wasm/
echo "Copied WASM plugin to .exo/wasm/"

# ---- Authenticate gh if token available ----
if [ -n "${GITHUB_TOKEN:-}" ]; then
    echo "$GITHUB_TOKEN" | gh auth login --with-token 2>/dev/null
    echo "Authenticated gh CLI"
fi

# ---- Git identity (required for worktree operations) ----
git config user.name "ExoMonad"
git config user.email "exile@exomonad.dev"

# ---- Launch exomonad ----
echo "Starting exomonad session..."
exec exomonad init
