# Gemini CLI SessionStart Hook Setup

## Overview

This directory contains Gemini CLI configuration for Tidepool SessionStart hook support, equivalent to `.claude/settings.json` for Claude Code.

## Configuration

The `settings.json` file configures the `startup` hook (Gemini's hook name) to invoke:
```bash
mantle-agent hook session-start --runtime=gemini
```

## Key Differences from Claude Code

| Aspect | Claude Code | Gemini CLI |
|--------|-------------|-----------|
| Settings directory | `.claude/` | `.gemini/` |
| Hook name | `SessionStart` | `startup` |
| Hook command | `mantle-agent hook session-start` | `mantle-agent hook session-start --runtime=gemini` |
| On error exit code | 1 | 2 |

## How It Works

1. Gemini CLI starts a session and fires the `startup` hook
2. Hook calls `mantle-agent hook session-start --runtime=gemini`
3. mantle-agent forwards to control-server via Unix socket with `runtime=Gemini`
4. control-server:
   - Detects current git branch (should be `gh-{number}/*` for active issue)
   - Fetches issue details from GitHub
   - Renders SessionStart context template with issue info
   - Returns markdown as `additionalContext`
5. Gemini CLI injects the context into the session automatically

## SessionStart Behavior

The SessionStart handler is **identical for both Claude and Gemini**:
- Parses branch name to extract issue number
- Fetches issue info if on a `gh-*` branch
- Renders Jinja template with context
- Returns rendered markdown to inject into session

The only difference is exit code behavior on errors (1 vs 2), but the SessionStart handler doesn't use runtime for conditional logic—it always succeeds or returns silently.

## Testing

To test Gemini SessionStart hook:

1. Ensure you have Gemini CLI installed and in PATH
2. Ensure you're on a `gh-{number}/*` branch with an active issue
3. Start a Gemini session:
   ```bash
   gemini
   ```
4. Verify issue context appears at the top of the session
5. Commands for workflow should be visible (git status, commit, push)

## Session Close Protocol

When ending a Gemini session, follow the session close protocol from the injected context:

```
[ ] 1. git status              (check what changed)
[ ] 2. git add <files>         (stage code changes)
[ ] 3. git commit -m "..."     (commit code)
[ ] 4. git push                (push to remote)
```

This ensures all work is properly tracked in git before the session ends.