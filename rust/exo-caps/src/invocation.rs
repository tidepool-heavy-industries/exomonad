//! Single source of truth for how a parent constructs a child's `exo` invocation —
//! the MCP sidecar args and the CC hook commands written into the child's worktree.
//!
//! These literals MUST match the clap command surface in `exo/src/main.rs`
//! (`Commands::{Node, Hook, Listen}`) and the hook event names rendered by `HookEventType`'s
//! `value_enum`. Defined once here so the spawner and the binary can't drift: rename a
//! subcommand in `main.rs` and this is the only place to update.

/// The binary name (resolved on `PATH`).
pub const BIN: &str = "exo";

const NODE: &str = "node";
const HOOK: &str = "hook";
const LISTEN: &str = "listen";
const PAPERS_FLAG: &str = "--papers";

/// CC hook events the node wires, in the order the spawner emits them.
/// Each must match a `HookEventType` `value_enum` rendering.
///
/// There used to be a `STOP` const here too — a node's settings no longer register a `Stop` hook
/// (see `rust/exo/CLAUDE.md` for why).
pub const PRE_TOOL_USE: &str = "pre-tool-use";
pub const SESSION_START: &str = "session-start";

/// MCP sidecar args for a child's `.mcp.json` (the `"command"` is [`BIN`]):
/// `exo node --papers <papers>`.
pub fn node_args(papers: &str) -> [String; 3] {
    [NODE.into(), PAPERS_FLAG.into(), papers.into()]
}

/// A full CC hook command string for `settings.local.json`:
/// `exo hook <event> --papers <papers>`.
///
/// `event` is one of [`PRE_TOOL_USE`]/[`SESSION_START`]; `papers_escaped` is the
/// papers path **already shell-escaped** by the caller (it is pasted into a shell).
pub fn hook_command(event: &str, papers_escaped: &str) -> String {
    format!("{BIN} {HOOK} {event} {PAPERS_FLAG} {papers_escaped}")
}

/// The wake-channel command a node arms under Claude Code's `Monitor` tool:
/// `exo listen --papers <papers>`.
///
/// `papers_escaped` is the papers path **already shell-escaped and absolute** by the caller —
/// Monitor runs the command in a shell whose cwd is the agent's worktree, and an inline
/// worker's cwd is its parent's dir, so a relative path would resolve wrong.
pub fn listen_command(papers_escaped: &str) -> String {
    format!("{BIN} {LISTEN} {PAPERS_FLAG} {papers_escaped}")
}
