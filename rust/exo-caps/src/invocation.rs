//! Single source of truth for how a parent constructs a child's `exo` invocation —
//! the MCP sidecar args and the CC hook commands written into the child's worktree.
//!
//! These literals MUST match the clap command surface in `exo/src/main.rs`
//! (`Commands::{Node, Hook}`) and the hook event names rendered by `HookEventType`'s
//! `value_enum`. Defined once here so the spawner and the binary can't drift: rename a
//! subcommand in `main.rs` and this is the only place to update.

/// The binary name (resolved on `PATH`).
pub const BIN: &str = "exo";

const NODE: &str = "node";
const HOOK: &str = "hook";
const PAPERS_FLAG: &str = "--papers";

/// CC hook events the node wires, in the order the spawner emits them.
/// Each must match a `HookEventType` `value_enum` rendering.
pub const PRE_TOOL_USE: &str = "pre-tool-use";
pub const STOP: &str = "stop";
pub const SESSION_START: &str = "session-start";

/// MCP sidecar args for a child's `.mcp.json` (the `"command"` is [`BIN`]):
/// `exo node --papers <papers>`.
pub fn node_args(papers: &str) -> [String; 3] {
    [NODE.into(), PAPERS_FLAG.into(), papers.into()]
}

/// A full CC hook command string for `settings.local.json`:
/// `exo hook <event> --papers <papers>`.
///
/// `event` is one of [`PRE_TOOL_USE`]/[`STOP`]/[`SESSION_START`]; `papers_escaped` is the
/// papers path **already shell-escaped** by the caller (it is pasted into a shell).
pub fn hook_command(event: &str, papers_escaped: &str) -> String {
    format!("{BIN} {HOOK} {event} {PAPERS_FLAG} {papers_escaped}")
}
