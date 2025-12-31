#!/bin/bash
# claude-multi.sh - Run multiple Claude Code instances in tmux panes with git worktrees
#
# Usage: ./claude-multi.sh "branch1:promptfile1" "branch2:promptfile2" ...
#
# Each argument creates:
#   - A new git branch off main
#   - A worktree at ~/dev/worktrees/$REPO_NAME/$BRANCH_NAME
#   - A tmux pane running claude with the prompt from the file

set -e

SESSION_NAME="claude-multi"
HISTORY_LIMIT=50000
REPO_NAME=$(basename "$(git rev-parse --show-toplevel)")
WORKTREE_BASE="$HOME/dev/worktrees/$REPO_NAME"

# Detect WSL distro name
WSL_DISTRO="${WSL_DISTRO_NAME:-Ubuntu}"

if [ $# -eq 0 ]; then
    echo "Usage: $0 \"branch1:promptfile1\" \"branch2:promptfile2\" ..."
    echo "Each argument creates a worktree + branch + pane with claude."
    echo "Format: branch-name:/path/to/prompt.md"
    exit 1
fi

# Ensure worktree base exists
mkdir -p "$WORKTREE_BASE"

# Kill existing session if it exists
tmux kill-session -t "$SESSION_NAME" 2>/dev/null || true

# Parse first argument
first_arg="$1"
shift
first_branch="${first_arg%%:*}"
first_promptfile="${first_arg#*:}"
first_worktree="$WORKTREE_BASE/$first_branch"

# Validate prompt file
if [ ! -f "$first_promptfile" ]; then
    echo "Error: Prompt file not found: $first_promptfile"
    exit 1
fi
first_promptfile=$(realpath "$first_promptfile")

# Clean up existing worktree if present
git worktree remove --force "$first_worktree" 2>/dev/null || true
git branch -D "$first_branch" 2>/dev/null || true

# Create worktree with new branch
git worktree add -b "$first_branch" "$first_worktree" main
echo "Created worktree: $first_worktree (branch: $first_branch)"

# Create tmux session with first pane
# Source ghcup env and add .local/bin to PATH, then run claude
tmux new-session -d -s "$SESSION_NAME" -c "$first_worktree" \
    "source ~/.ghcup/env 2>/dev/null; export PATH=\"\$HOME/.local/bin:\$PATH\"; claude 'Read and execute the task in $first_promptfile'; read -p 'Press enter to close...'"

# Set options for the session
tmux set-option -t "$SESSION_NAME" history-limit "$HISTORY_LIMIT"
tmux set-option -t "$SESSION_NAME" mouse on

# Create additional panes for remaining arguments
for arg in "$@"; do
    branch="${arg%%:*}"
    promptfile="${arg#*:}"
    worktree="$WORKTREE_BASE/$branch"

    # Validate prompt file
    if [ ! -f "$promptfile" ]; then
        echo "Error: Prompt file not found: $promptfile"
        exit 1
    fi
    promptfile=$(realpath "$promptfile")

    # Clean up existing worktree if present
    git worktree remove --force "$worktree" 2>/dev/null || true
    git branch -D "$branch" 2>/dev/null || true

    # Create worktree with new branch
    git worktree add -b "$branch" "$worktree" main
    echo "Created worktree: $worktree (branch: $branch)"

    tmux split-window -t "$SESSION_NAME" -c "$worktree" \
        "source ~/.ghcup/env 2>/dev/null; export PATH=\"\$HOME/.local/bin:\$PATH\"; claude 'Read and execute the task in $promptfile'; read -p 'Press enter to close...'"

    # Rebalance after each split to avoid "no space for new pane" errors
    tmux select-layout -t "$SESSION_NAME" tiled
done

# Final layout adjustment
tmux select-layout -t "$SESSION_NAME" tiled

# Select first pane
tmux select-pane -t "$SESSION_NAME:0.0"

echo ""
echo "Created tmux session '$SESSION_NAME' with $(tmux list-panes -t $SESSION_NAME | wc -l) panes"
echo "Worktrees at: $WORKTREE_BASE/"

# Open new Windows Terminal window attached to the tmux session
wt.exe -w new wsl.exe -d "$WSL_DISTRO" -- tmux attach -t "$SESSION_NAME"

echo "Opened Windows Terminal for supervision"
