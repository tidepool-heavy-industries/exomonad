# Tool Surface

## Coordination Tools (TL role)

### `spawn_subtree`

Fork a worktree node off your current branch. Use when decomposing work into sub-problems that may need further decomposition — the child gets full coordination tools and can spawn its own children. Prefer this when the task has 3+ distinct components or requires architectural decisions.

```
Arguments:
  task: string              — Description of the subtree's responsibility
  branch_name: string       — Branch suffix (prefixed with current branch, e.g. "auth" → "main.feature.auth")
  parent_session_id: string — Your $CLAUDE_SESSION_ID (enables context inheritance via --fork-session)
```

**What happens:**
1. Creates git worktree at `.exomonad/worktrees/{slug}/`
2. Creates branch `{current_branch}.{slug}`
3. Launches Claude with `--resume <parent_session_id> --fork-session -p '<task>'`
4. Child inherits parent's full conversation context
5. Registers parent as event target for child lifecycle events
6. Child gets TL role (can spawn workers and further subtrees)

**Prompt guidance:** Prepend worktree context warning automatically:
> You are now in worktree {path} on branch {branch}. All file paths from your inherited context are STALE — use relative paths only and re-read files before editing.

### `spawn_workers`

Spawn multiple Gemini leaf agents, each in their own worktree branch. Use when work is concrete enough to execute without further decomposition — each worker gets a focused, bounded task with code snippets and exact commands.

```
Arguments:
  specs: array of { name: string, prompt: string }
```

Each worker:
- Gets its own branch (`{current_branch}.{name}`)
- Gets its own worktree (isolation from siblings)
- Has dev role (no spawn tools)
- Files PR against parent branch when done
- Iterates with Copilot autonomously

### `file_pr`

File a pull request against the parent branch. Auto-detects base branch from the `.` naming convention. Triggers Copilot review + CI automatically.

```
Arguments:
  title: string
  body: string
  draft: bool (default: false)
```

**Already implemented.** Base branch detection: strip last `.segment` from current branch name.

### `merge_pr`

Merge a child's pull request into the current branch. Parent-only operation.

```
Arguments:
  pr_number: int
  strategy: "squash" | "merge" (default: "squash" for workers, "merge" for subtrees)
```

**What happens after merge:**
1. Merges the PR on GitHub
2. Pulls the merge into the local worktree
3. Siblings detect the update on their next stop-hook check

### `answer_question`

Answer a pending question from a child agent. The answer is injected into the child's Zellij pane as a synthetic user message.

```
Arguments:
  agent: string   — Child agent identifier
  message: string — The answer
```

**Already implemented** (messaging system). Needs adaptation for Zellij injection delivery.

## Child Tools (dev role)

### `note`

Send a fire-and-forget message to the parent. Parent sees it as a `[NOTE]` event.

```
Arguments:
  message: string
```

### `question`

Send a blocking question to the parent. Child goes dormant until answer arrives (via Zellij injection).

```
Arguments:
  message: string
```

**Delivery:** Server injects question into parent's pane. Parent's `answer_question` injects response into child's pane. Child wakes and continues.

### `file_pr`

Same tool, available to both roles. Children file PRs against their parent branch.

## System Tools (not user-facing)

### GitHub Poller

Background task in the MCP server that polls GitHub API for PR status changes on tracked PRs.

- Tracks all PRs filed by agents in the tree
- Polls every 30s for PRs with pending reviews, 5m otherwise
- On status change, renders template and injects into relevant agent's pane
- Events: Copilot review posted, CI status change, PR merged externally

### Stop-Hook Rebase Check

Added to the existing stop-hook chain. Before a child exits or at natural stopping points:

1. `git fetch origin {parent_branch}`
2. Check if parent is ahead of current branch
3. If ahead: `git rebase origin/{parent_branch}`
4. If conflict: attempt resolution, send question to parent if stuck

### Event Router

Server-side component that resolves event targets from the branch hierarchy:

- Child branch `main.feature.auth` → parent session `main.feature`
- Looks up parent's Zellij pane ID from spawn-time registration
- Renders event template → `zellij action write-chars`

## Deprecated

### `spawn_agents` / `spawn_gemini_teammate`

Replaced by `spawn_subtree` + `spawn_workers`. Internal worktree machinery is shared code.

## Tool Description Philosophy

Tool descriptions are **instructive** — 2-3 sentences with decision criteria. The agent's choice between `spawn_subtree` and `spawn_workers` IS the anamorphism base case decision. The descriptions teach this:

- `spawn_subtree`: "...when decomposing work into sub-problems that may need further decomposition"
- `spawn_workers`: "...when work is concrete enough to execute without further decomposition"

No separate "should I decompose?" reasoning step. The tool choice embodies the recursion decision.
