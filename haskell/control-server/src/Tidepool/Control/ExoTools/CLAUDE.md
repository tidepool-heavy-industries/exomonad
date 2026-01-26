# Exo Tools - External Orchestration

MCP tools for external orchestration: GitHub issues, git, subprocess operations, and parallel agent spawning.

## When to Read This

Read this if you're:
- Using spawn_agents to dispatch parallel agents in Zellij
- Working with GitHub issue integration
- Understanding the exo_* MCP tools
- Debugging worktree creation or agent orchestration

## Tools Overview

| Tool | Purpose | Tier |
|------|---------|------|
| `spawn_agents` | Create worktrees and launch parallel agents in Zellij tabs | Tier 3 |
| `exo_status` | Get current issue context, git status, and PR info | Tier 3 |
| `file_pr` | File GitHub PR with issue context | Tier 3 |

---

# spawn_agents - Parallel Agent Orchestration

Create git worktrees for multiple issues and launch isolated agent sessions in Zellij tabs.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ User (in Zellij session)                                        │
│  Calls: spawn_agents({ issue_numbers: ["123", "456"] })         │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ spawn_agents Handler                                            │
│  1. Validate Zellij environment                                 │
│  2. Discover Hangar root                                        │
│  3. For each issue number:                                      │
│     a. Fetch issue from GitHub                                  │
│     b. Create git worktree                                      │
│     c. Bootstrap .tidepool/ directory                           │
│     d. Generate process-compose.yaml                            │
│     e. Write .env with merged environment                       │
│     f. Write backend config (.claude/ or .gemini/)              │
│     g. Write issue context to .claude/context/issue.md          │
│     h. Launch Zellij tab with layout                            │
└─────────────────┬───────────────────────────────────────────────┘
                  │
                  ▼
┌──────────────────────────────────────────────────────────────────┐
│ Hangar Worktrees Structure                                       │
│                                                                  │
│  ~/hangars/tidepool/                                             │
│    ├── Hangar.toml                                               │
│    ├── runtime/bin/                                              │
│    │   ├── tidepool-control-server                              │
│    │   ├── mantle-agent                                          │
│    │   └── tui-sidebar                                           │
│    └── worktrees/                                                │
│        ├── gh-123-fix-socket-deadlock/                           │
│        │   ├── .tidepool/logs/                                   │
│        │   ├── .claude/                                          │
│        │   │   ├── settings.local.json (hooks)                   │
│        │   │   └── context/issue.md                              │
│        │   ├── .env (merged environment)                         │
│        │   └── process-compose.yaml (generated)                  │
│        ├── gh-456-update-spawn-agents-paths/                     │
│                                                                  │
│  /tmp/tidepool-123/                                              │
│    ├── control.sock                                              │
│    └── tui.sock                                                  │
└──────────────────────────────────────────────────────────────────┘
```

## Request Schema

```json
{
  "issue_numbers": ["123", "456"],
  "backend": "claude"  // Optional: "claude" or "gemini", defaults to "claude"
}
```

**Field Details:**

- `issue_numbers`: Array of issue numbers as strings.
  - For git branch, worktree, and socket naming the **original provided value** is used:
    - Branch name: `gh-<id>/<slug>` (e.g., `gh-123/fix-bug`)
    - Worktree directory: `gh-<id>-<slug>` (e.g., `gh-123-fix-bug`)
    - Socket directory: `/tmp/tidepool-<id>/...` (e.g., `/tmp/tidepool-123/control.sock`)

- `backend`: Backend CLI to use
  - `"claude"`: Launches `claude --debug --verbose`
  - `"gemini"`: Launches `gemini --debug`
  - Default: `"claude"`

## Response Schema

```json
{
  "worktrees": [
    ["123", "/path/to/worktrees/gh-123-fix-bug"],
    ["456", "/path/to/worktrees/gh-456-feature"]
  ],
  "tabs": [
    ["123", "123"],
    ["456", "456"]
  ],
  "failed": [
    ["789", "Issue not found: 789"]
  ]
}
```

**Field Details:**

- `worktrees`: Successfully created or reused worktrees (id, path)
- `tabs`: Successfully launched Zellij tabs (id, tabId)
- `failed`: Failed operations (id, reason)

**Idempotency:** If a worktree already exists for an issue, it will be reused (no new worktree created, but bootstrap and tab launch still run).

## How It Works

### 1. Environment Validation

Checks that we're running inside a Zellij session:

```haskell
mZellijSession <- checkZellijEnv
case mZellijSession of
  Nothing -> fail "Not running in Zellij session"
  Just _ -> proceed
```

### 2. Hangar Root Discovery

Finds the hangar root directory for shared binaries:

1. Check `HANGAR_ROOT` environment variable
2. If not set, walk up from repo root looking for `Hangar.toml`
3. Falls back to repo root if no hangar found

**Worktree Target Directory:**
- If hangar found: `<hangar>/worktrees/`
- If no hangar: `<repo>/.worktrees/tidepool/`

### 3. Per-Issue Processing

For each issue number:

#### a. Fetch Issue Info
```haskell
mIssue <- getIssue repo num False
```

**Validation checks:**
- Issue ID contains no path separators (prevent traversal)
- Binary exists at `<hangar>/runtime/bin/tidepool-control-server`
- Issue is not closed

#### b. Create Git Worktree

```haskell
let branchName = "gh-123/fix-bug"
    targetPath = "/hangars/tidepool/worktrees/gh-123-fix-bug"
    spec = WorktreeSpec
      { wsBaseName = "gh-123"
      , wsFromBranch = Just "origin/main"
      , wsBranchName = Just branchName
      , wsPath = Just targetPath
      }
createWorktree spec
```

#### c. Bootstrap .tidepool/ Directory

Creates runtime directory structure:

**Socket Directory** (in `/tmp` to avoid path length limits):
```
/tmp/tidepool-123/
  ├── control.sock
  └── tui.sock
```

**Logs Directory** (in worktree):
```
<worktree>/.tidepool/logs/
  └── process-compose.log
```

#### d. Generate process-compose.yaml

**Type-safe generation** from Haskell types (no templates).

#### e. Write .env File

Merges root environment with subagent-specific overrides.

#### f. Write Backend Configuration

Writes `.claude/settings.local.json` or `.gemini/settings.json` with appropriate hooks.

#### g. Write Issue Context

Creates `.claude/context/issue.md` with issue title, body, and labels.

#### h. Launch Zellij Tab

Launches a new Zellij tab with the subagent layout.

## Error Handling and MCP Error Codes

| Code   | Name              | Example (tool-level failure)                                   |
|--------|-------------------|-----------------------------------------------------------------|
| -32001 | NotFound          | Required resource missing                                       |
| -32002 | InvalidInput      | Request arguments failed validation or JSON could not be parsed |
| -32003 | ExternalFailure   | Underlying `git`/`zellij` command crashed unexpectedly         |
| -32004 | StateError        | Unexpected internal state                                       |
| -32005 | EnvironmentError  | Server environment misconfigured                                |

## Troubleshooting

### "Binary missing: /path/to/tidepool-control-server"

**Cause:** Pre-built binaries don't exist in hangar.

**Fix:** Build binaries in `runtime/tidepool` and copy to `runtime/bin/`.

### Worktree Reuse (Idempotency)

`spawn_agents` is idempotent: if a worktree already exists for an issue, it will be reused instead of creating a new one. The tool will:
1. Skip worktree creation
2. Re-run bootstrap (updates configs if needed)
3. Launch a new Zellij tab/Docker container for the existing worktree

This allows re-spawning agents after a tab was closed without manually cleaning up.