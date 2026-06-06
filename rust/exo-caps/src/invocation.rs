//! Single source of truth for how a parent constructs a child's `exomonad` invocation —
//! the MCP sidecar args and the CC hook commands written into the child's worktree.
//!
//! These literals MUST match the clap command surface in `exomonad/src/main.rs`
//! (`Commands::Experimental` → `Node`/`Hook`) and the hook event names rendered by
//! `HookEventType`'s `value_enum`. Defined once here so the spawner and the binary can't
//! drift: rename a subcommand in `main.rs` and this is the only place to update.

/// The binary name (resolved on `PATH`).
pub const BIN: &str = "exomonad";

const EXPERIMENTAL: &str = "experimental";
const NODE: &str = "node";
const HOOK: &str = "hook";
const PAPERS_FLAG: &str = "--papers";

/// CC hook events the experimental node wires, in the order the spawner emits them.
/// Each must match a `HookEventType` `value_enum` rendering.
pub const PRE_TOOL_USE: &str = "pre-tool-use";
pub const STOP: &str = "stop";
pub const SESSION_START: &str = "session-start";

/// Gemini CLI `settings.json` hook KEYS — the harness event names, distinct from the CLI event
/// args above ([`PRE_TOOL_USE`]/[`STOP`]/[`SESSION_START`]). `AfterAgent` is Gemini's turn-end
/// (the `Stop` equivalent); `BeforeTool` is its pre-tool event; `SessionStart` mirrors Claude.
/// A Gemini hook entry maps one of these keys to the same `exomonad experimental hook <arg>`
/// command — so `AfterAgent` runs `... hook stop`, `BeforeTool` runs `... hook pre-tool-use`.
pub const GEMINI_AFTER_AGENT: &str = "AfterAgent";
pub const GEMINI_BEFORE_TOOL: &str = "BeforeTool";
pub const GEMINI_SESSION_START: &str = "SessionStart";

/// MCP sidecar args for a child's `.mcp.json` (the `"command"` is [`BIN`]):
/// `exomonad experimental node --papers <papers>`.
pub fn node_args(papers: &str) -> [String; 4] {
    [
        EXPERIMENTAL.into(),
        NODE.into(),
        PAPERS_FLAG.into(),
        papers.into(),
    ]
}

/// A full CC hook command string for `settings.local.json`:
/// `exomonad experimental hook <event> --papers <papers>`.
///
/// `event` is one of [`PRE_TOOL_USE`]/[`STOP`]/[`SESSION_START`]; `papers_escaped` is the
/// papers path **already shell-escaped** by the caller (it is pasted into a shell).
pub fn hook_command(event: &str, papers_escaped: &str) -> String {
    format!("{BIN} {EXPERIMENTAL} {HOOK} {event} {PAPERS_FLAG} {papers_escaped}")
}
