#!/bin/bash
# Wrapper script to start Claude Code with OAuth key
# Zellij layouts can't expand env vars reliably, so we use a script

export TERM=xterm-256color
export SHELL=/bin/bash
export ANTHROPIC_API_KEY="$CLAUDE_CODE_OAUTH_KEY"

cd /worktrees
exec claude --dangerously-skip-permissions
