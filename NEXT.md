# Hylo Model: Status & Next Steps

## Phase 1: COMPLETE — Ready to Dogfood

### What's Working
- [x] `spawn_subtree` — worktree off current branch, TL role, Claude/Gemini support
- [x] `spawn_worker` — in-place Gemini pane, no worktree (WIP, functional)
- [x] `file_pr` — auto-detects base branch from `.` hierarchy convention
- [x] Stop hook auto-files PR on exit if none exists
- [x] Long-poll `get_messages` with timeout
- [x] Messaging: `note`, `question`, `answer_question`
- [x] Copilot review integration
- [x] Agent-type-aware config (Claude: `.mcp.json`, Gemini: `.gemini/settings.json`)
- [x] Per-agent MCP identity routing (`/agents/{name}/mcp`)
- [x] Branch naming uses `.` separator (avoids git ref collisions, enables PR base detection)
- [x] Integration tests for hierarchy + PR detection

### Known Gaps (Acceptable for Phase 1)
- **Rebase notification** — handled via prompting. Parent sends `note` to siblings after merge; they rebase manually. No tooling needed.
- **spawn_worker registration** — workers don't appear in `list_agents`. Ephemeral by design for now.
- **Depth limit** — convention only, no enforcement. Tool descriptions guide the agent.

### Stale Docs
These reference old `spawn_leaf` naming and `/` branch convention:
- `hylo_plan/README.md`
- `hylo_plan/tools.md`
- `hylo_plan/phase-1.md`
- `hylo_plan/pr-lifecycle.md`

Update after dogfooding confirms the model works.

## Phase 2: Plan AFTER Dogfooding

Do not plan phase 2 until real usage reveals what matters. Candidate topics to evaluate based on experience:
- jj migration (automatic rebase propagation)
- Automated rebase notification (post-merge hook)
- spawn_worker lifecycle (registration, cleanup, tracking)
- Depth limits (if agents recurse too deep in practice)
- Cloud MCP server
- Researcher node type (read-only, no write access to tree)
- Agent evolution / learned facts
