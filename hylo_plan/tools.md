# Tool Surface

## New Tools

### `spawn_subtree`

Fork a worktree node off your current branch. Use when decomposing work into sub-problems that may need further decomposition. The child gets full coordination tools.

```
Arguments:
  task: string        — description of the subtree's responsibility
  branch_name: string — branch name for the child (branched off current)
  context?: string    — decomposition notes, architectural guidance
```

Implementation: creates worktree off current branch, launches Claude with tl role (has spawn tools). Context delivered via GitHub issue or inline prompt.

### `spawn_leaf`

Spawn a Gemini agent for a focused, bounded task. Use when the work is concrete enough to execute without further decomposition. The child works and exits.

```
Arguments:
  name: string     — human-readable name
  prompt: string   — full task spec with code snippets
  worktree?: bool  — own worktree (default) or parent's worktree
```

Implementation: evolution of `spawn_gemini_teammate`. Gemini with dev role (no spawn tools). Stop hooks enforce commit/push/PR before exit.

### `file_pr`

File a pull request against parent branch. Triggers Copilot review + CI automatically.

```
Arguments:
  title: string
  body: string
  base_branch?: string  — defaults to parent branch (auto-detected from branch naming)
```

Implementation: wraps `gh pr create --base {parent_branch}`. Already partially exists in stop hook flow.

### `get_messages` (modified)

Long-poll for messages from child nodes.

```
Arguments:
  timeout_secs?: int  — max wait time (default: 300)
```

Returns immediately when a message arrives. Returns empty after timeout. One tool call per idle period instead of polling.

## Deprecated

### `spawn_agents`

Replaced by `spawn_subtree` + `spawn_leaf`. The low-level worktree/branch machinery becomes a shared internal, not a tool.

### `spawn_gemini_teammate`

Replaced by `spawn_leaf`.

## Unchanged

All existing tools (git_*, github_*, messaging, tasks) remain. They compose naturally with the new spawn tools.

## Tool Description as Prompt Engineering

The tool descriptions teach the agent the anamorphism base case:

- "Use `spawn_subtree` when the work needs further decomposition"
- "Use `spawn_leaf` when the work is concrete enough to execute directly"

The agent's choice of tool IS the recursion decision. No separate "should I decompose?" step needed.
