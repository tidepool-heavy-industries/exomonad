# Session Handoff — 2026-02-10

## What Was Done

Two sessions of Codex audit bug fixes + infrastructure work, dispatching to Gemini subagents.

### Commits (most recent first)
- `f025c219` — Remove plugin bottom-bar from agent tabs + spawn teammates as panes
- `f08b3eda` — Coordination backend + SpawnSpec compiler + messaging collapse
- `641bee2f` — Wire WorkspaceTopology into Rust service layer
- `9a79b4cb` — WorkspaceTopology proto enum + nix develop shell wrapping + TL praxis docs
- `3b0eedf8` — Unified WASM + migrate TL messaging tools to effect system
- `0033f2cd` — KV store effect + PostToolUse hook for team name capture
- `1ba35f1c` — Teams infrastructure + audit cleanup (single code path, fail fast)

### Key Changes
- **Coordination backend**: In-process task CRUD + messaging mirroring Claude Teams tool signatures (`proto/effects/coordination.proto`, Rust handler+service, Haskell effects+tools+records)
- **SpawnSpec compiler**: Typed Haskell DSL for generating Gemini spawn prompts (`haskell/wasm-guest/src/ExoMonad/Guest/SpawnSpec/`)
- **Messaging collapse**: `send_question` now uses QuestionRegistry oneshot channels instead of polling loop
- **WorkspaceTopology**: Explicit proto enum (WORKTREE_PER_AGENT, SHARED_DIR) plumbed through Rust service layer
- **Nix develop wrapping**: `new_zellij_tab` detects `flake.nix` and wraps in `nix develop -c sh -c '...'`
- **Pane-not-tab**: `spawn_gemini_teammate` creates panes in current tab via `zellij action new-pane` (one tab per worktree model)
- **Bottom bar removal**: exomonad-plugin pane deleted from agent tab KDL layout

## What Remains

The active plan is at `.claude/plans/radiant-crafting-zebra.md`. Remaining waves:

### Wave 2: Activate Unified WASM for Serve Mode

The unified WASM exists (`.exomonad/roles/unified/`) but isn't wired into serve pipeline.

| Task | Description | Files |
|------|-------------|-------|
| 2a | Sync unified TLRole.hs with current tl/Role.hs | `.exomonad/roles/unified/TLRole.hs` |
| 2b | Serve mode loads unified WASM (not per-role) | `rust/exomonad/src/main.rs` |
| 2c | Rust WASM bridge passes `role` in every call | `rust/exomonad-core/src/mcp/tools.rs`, `handler.rs` |
| 2d | Remove Rust-side role filtering (`TL_ONLY_TOOLS`) | `rust/exomonad-core/src/mcp/handler.rs` |
| 2e | Fix Gemini agent httpUrl to `/agents/{name}/mcp` | `rust/exomonad-core/src/services/agent_control.rs` |
| 2f | Per-agent endpoint sets dev role | `rust/exomonad/src/main.rs` |
| 2g | Build pipeline: `just wasm unified` | `justfile` |

### Wave 3: Migrate Last Direct Rust Tools to WASM

`get_agent_messages` and `answer_question` are still direct Rust MCP tools bypassing WASM. Need to go through the effect system like everything else.

| Task | Description | Files |
|------|-------------|-------|
| 3a | Proto: add TL messaging effects | `proto/effects/messaging.proto` + vendor copy |
| 3b | Rust handler: implement in MessagingHandler | `rust/exomonad-core/src/handlers/messaging.rs` |
| 3c | Haskell effects | `haskell/wasm-guest/src/ExoMonad/Effects/Messaging.hs` |
| 3d | Haskell tools | `haskell/wasm-guest/src/ExoMonad/Guest/Tools/Messaging.hs` |
| 3e | TLMessagingTools record | `haskell/wasm-guest/src/ExoMonad/Guest/Records/TLMessaging.hs` (NEW) |
| 3f | Add to TL role | `.exomonad/roles/tl/Role.hs`, `.exomonad/roles/unified/TLRole.hs` |
| 3g | Delete direct Rust tools | `rust/exomonad-core/src/mcp/handler.rs` |

### Wave 4: Documentation

Update CLAUDE.md files to reflect all changes.

## Verification Needed

`cargo test --workspace` has NOT been run on the final commit (`f025c219`). Run it first thing on the remote machine. Expected: 312+ tests pass, one possible flaky (`mcp_clean_shutdown` — timing-dependent, passes on retry).

## TL Dispatch Patterns (for Gemini subagent work)

These patterns were proven this session and should be used for remaining waves:

### Spec Structure
```
1. READ FIRST     — exact file paths
2. STEPS          — numbered, each step = one action with code snippets
3. VERIFY         — exact commands with env vars
4. DONE CRITERIA  — acceptance tests
5. BOUNDARY       — "Do NOT commit"
```

### Intelligence Gradient
- **Opus**: architecture, proto shapes, type decisions, review, commit
- **Gemini**: file creation, boilerplate, build verification, mechanical plumbing

### Sequencing
1. Plan the wave (identify independent work units)
2. Write protos / make architectural decisions yourself
3. Run `just proto-gen`
4. Spawn agents in parallel on non-overlapping file sets
5. Review diffs when agents finish
6. Run tests yourself (don't trust "tests pass" claims)
7. Commit all work in one shot

### Anti-Patterns
- Vague tasks ("implement the identity system") — Gemini overengineers
- Missing code snippets — Gemini invents wrong patterns
- Spawning before proto-gen — agent guesses type names
- Overlapping file ownership — merge conflicts between agents

## Key Architecture Context

- **All MCP tools must be in Haskell WASM** — Rust handles I/O only
- **Single code path rule** — never two paths doing the same thing
- **Proto-first** — write proto, run `just proto-gen`, then implement
- `register_builtin_handlers` returns `(RuntimeBuilder, Arc<QuestionRegistry>)` tuple
- `EffectError::timeout(msg)` constructor, NOT struct literal
- `services/inbox.rs` is THE inbox implementation
- Per-agent Gemini settings: `.exomonad/agents/{name}/settings.json` with `httpUrl` pointing to `/agents/{name}/mcp`
- rmcp `StreamableHttpService` dispatches by HTTP method, not path — safe to forward

## Files to Read First on Remote

1. `CLAUDE.md` — project overview + TL praxis section
2. `.claude/plans/radiant-crafting-zebra.md` — the active plan
3. `proto/CLAUDE.md` — proto structure and codegen
4. `rust/CLAUDE.md` — Rust workspace overview
5. This file
