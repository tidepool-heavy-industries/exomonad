# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added
- **FixesPushed event**: Poller fires `fixes_pushed` event when leaf addresses Copilot review and pushes fixes. Copilot does NOT re-review — this is the actionable signal for the TL to merge.
- **Dual timeout**: 15 minutes for initial Copilot review, 5 minutes after leaf addresses changes (since Copilot won't re-review).
- **Event handler dispatch**: Third dispatch category alongside tools and hooks. GitHub poller calls WASM `handle_event` for PR review events (reviews, approvals, timeouts, fixes pushed) and sibling merge events.
- **CI status change events**: Route CI status transitions through WASM event handlers.
- **Sibling merge notification**: Event when a sibling PR merges (rebase may be needed).
- **Pragma corruption guard**: PreToolUse hook blocks edits that corrupt Haskell `#-}` LANGUAGE pragma closings.
- **Bidirectional messaging**: `send_message` tool for arbitrary agent-to-agent messaging (routes via Teams inbox, ACP, UDS, or Zellij).
- **ACP messaging**: Structured JSON-RPC messaging via Agent Client Protocol for Gemini agents.
- **HTTP-over-UDS delivery**: `notify_parent` → POST to `.exo/agents/{name}/notify.sock` for custom binary agents.
- **Coordination mutexes**: In-memory `MutexRegistry` with FIFO wait queues and TTL auto-expiry for parallel agents.
- **KV store**: Persistent key-value store via `.exo/kv/` for cross-agent state.
- **Claude session registry**: Track Claude session UUIDs for `--fork-session` context inheritance.
- **`exomonad reload`**: Clear WASM plugin cache (hot reload on next call).
- **`exomonad shutdown`**: Graceful server shutdown.

### Changed
- **Teams inbox delivery** replaces Zellij stdin injection as primary delivery mechanism. `notify_parent` → Teams inbox → native `<teammate-message>` in parent conversation.
- **`notify_parent` is a message bus**, not a completion signal. Used for status updates, failure escalation, and structured results.
- **Event-driven PR review flow**: Stop hook and prompts updated for event handler architecture. Leaf agents no longer need to manually call `notify_parent` after Copilot review — the event system handles it.

## [0.1.0] - 2026-02-24

### Added
- Agent orchestration: `spawn_subtree` (Claude), `spawn_leaf_subtree` (Gemini), `spawn_workers` (Gemini panes)
- PR workflow: `file_pr`, `merge_pr`, `notify_parent`
- Haskell WASM effect system with typed effects and Rust host handlers
- Hot reload for WASM tools in serve mode
- Zellij plugin for agent status display and interactive popup UI
- Event logging (JSONL) and GitHub poller for CI/review status
- Role system: TL, Dev, Worker roles with permission cascades
- `exomonad init` for session bootstrap (server tab + TL tab)
