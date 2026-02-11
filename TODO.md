# Worker Stop Hook: Auto-Report Back to Parent

## Problem

Worker agents (spawned via `spawn_worker`) currently have no way to report completion back to their parent. They just exit silently. The stop hook is checking for dirty files and PR status, which isn't relevant for workers — they share the parent's worktree and don't file PRs.

Additionally, the hook is failing because `exomonad` binary path is wrong (macOS path on Linux system).

## Goal

When a worker agent exits, it should automatically send a completion message to the parent agent that spawned it. This lets the parent know the worker finished and can trigger parent review/merge of the worker's changes.

## Current Hook System

**Location of hook config generation:**
- Rust: `rust/exomonad-core/src/services/agent_control.rs` — `write_context_files()` method
- This runs during agent spawn and writes `.gemini/settings.json` with `afterAgent` hook config

**Current hook config for workers:**
```json
{
  "afterAgent": [
    {
      "type": "command",
      "command": "exomonad",
      "args": ["hook", "stop-check"]
    }
  ]
}
```

**What the hook does now:**
- File: `haskell/wasm-guest/src/ExoMonad/Guest/Tool/Runtime.hs` (search for `HookStopCheck` or similar)
- Checks: uncommitted changes, unpushed commits, PR status, Copilot review
- Returns: allow/deny decision + warnings

## Task: Implement Auto-Report Hook for Workers

### Step 1: Understand Current Messaging

**Read these files first:**
- `rust/exomonad-core/src/services/inbox.rs` — how messages are written to disk
- `rust/exomonad-core/src/handlers/messaging.rs` — `send_note` effect handler
- `haskell/wasm-guest/src/ExoMonad/Guest/Effects/Messaging.hs` — Haskell side of messaging effects

**Key questions to answer:**
1. Where are agent inboxes stored? (Path format: `.exomonad/agents/{name}/inbox/` ?)
2. How does `send_note` determine the recipient? (From `EXOMONAD_AGENT_ID` env var? From MCP endpoint URL?)
3. What's the message format on disk? (JSON with timestamp, sender, body?)

### Step 2: Create New Hook Handler in Haskell

**File to modify:** `haskell/wasm-guest/src/ExoMonad/Guest/Tool/Runtime.hs`

Add a new hook handler for workers that:
1. Detects if the agent is a worker (no git worktree? has `EXOMONAD_AGENT_ID` with `-gemini` suffix?)
2. Determines the parent agent ID (remove `-gemini` suffix? read from spawn metadata?)
3. Calls `sendNote` effect with completion message
4. Returns `HookOutput { allow_action: true, warnings: [] }`

**Message content should include:**
- "Worker {name} completed"
- Summary of what was done (file paths touched? commit message if any?)
- Exit status (success/failure)

**Proto changes needed:**
- Check `proto/exomonad/hook.proto` — does `HookInput` have enough context?
- May need to add `agent_type` or `parent_agent_id` fields

### Step 3: Wire the Hook in Rust

**File to modify:** `rust/exomonad-core/src/services/agent_control.rs`

In `write_context_files()`, when writing `.gemini/settings.json` for workers:
- Change the `afterAgent` command from `exomonad hook stop-check` to `exomonad hook worker-exit` (or similar)
- Ensure `EXOMONAD_AGENT_ID` env var is set correctly in the worker's Zellij pane config

**Files to check:**
- `rust/exomonad-core/src/services/agent_control.rs` lines ~750-800 (spawn_worker method)
- Look for where `env_vars.insert("EXOMONAD_AGENT_ID", ...)` is called

### Step 4: Add Hook Handler Dispatch in Rust

**File to modify:** `rust/exomonad/src/main.rs` (or `rust/exomonad/src/cli.rs`)

In the `hook` subcommand handling:
- Add a new variant `worker-exit` alongside `pre-tool-use`, `stop-check`, etc.
- Route it to WASM `handle_worker_exit` (or whatever you name the Haskell handler)

**Pattern to follow:**
```rust
"worker-exit" => {
    let output = plugin.handle_worker_exit(input)?;
    println!("{}", serde_json::to_string(&output)?);
}
```

### Step 5: Fix the Binary Path Issue

The hook is trying to call `/Users/inannamalick/.cargo/bin/exomonad` but this is Linux, not macOS.

**Root cause:** The path is probably hardcoded somewhere in the hook config generation.

**Fix:** In `write_context_files()`, use `which exomonad` or just `exomonad` (rely on PATH) instead of hardcoding a home directory path.

### Step 6: Test

1. Rebuild everything:
   ```bash
   just install-all-dev
   exomonad init --recreate
   ```

2. Spawn a worker:
   ```bash
   # In TL tab
   spawn_worker name="test-worker" prompt="Create a file called /tmp/test.txt with content 'hello'"
   ```

3. Let it exit, check for message:
   ```bash
   get_agent_messages  # Should show completion message from test-worker
   ```

4. Verify no errors in Gemini pane logs

## Files to Read/Modify Summary

| File | Action | Why |
|------|--------|-----|
| `rust/exomonad-core/src/services/inbox.rs` | Read | Understand message storage format |
| `rust/exomonad-core/src/handlers/messaging.rs` | Read | Understand `send_note` implementation |
| `haskell/wasm-guest/src/ExoMonad/Guest/Effects/Messaging.hs` | Read | Understand Haskell messaging API |
| `haskell/wasm-guest/src/ExoMonad/Guest/Tool/Runtime.hs` | Modify | Add `handleWorkerExit` hook handler |
| `rust/exomonad-core/src/services/agent_control.rs` | Modify | Change worker hook config, fix binary path |
| `rust/exomonad/src/main.rs` or `cli.rs` | Modify | Add `worker-exit` hook dispatch |
| `proto/exomonad/hook.proto` | Maybe modify | If HookInput needs more fields |

## Success Criteria

- [x] Worker agents send completion message on exit
- [x] Message appears in parent's `get_agent_messages` output
- [x] No hook execution errors in Gemini logs
- [x] Hook uses correct binary path (just `exomonad`, not hardcoded home dir)
- [x] Existing stop-check hook for subtree agents still works (don't break it)

## Notes

- Workers don't file PRs, so don't check for PR status
- Workers don't have separate branches, so don't check for unpushed commits
- Workers might have uncommitted changes — that's fine, parent can review via `git diff`
- Keep the existing `stop-check` hook for subtree agents (they DO need PR validation)
- This is phase 1 — the goal is "good enough to dogfood", not perfect
