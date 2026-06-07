//! Generic hook-RPC wire types — the contract for the per-agent UDS hook channel.
//!
//! A short-lived `exo hook` **client** forwards a hook invocation to the
//! node's **sidecar** over `.exo/agents/{name}/hook.sock`; the sidecar runs the role's hook fn
//! against its live runtime and replies with the exact stdout to print. The channel is
//! deliberately generic: adding a hook is a new [`HookEvent`] variant + a role hook fn — the
//! transport never changes.

use serde::{Deserialize, Serialize};

/// A harness hook event the sidecar can handle. The canonical event identifier shared by the
/// client and server halves of the channel.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HookEvent {
    /// Before a tool call (CC `PreToolUse` / Gemini `BeforeTool`).
    PreToolUse,
    /// The agent finished a turn and is yielding control (CC `Stop` / Gemini `AfterAgent`).
    Stop,
    /// Session bootstrap (CC `SessionStart`). Handled one-shot, NOT over the socket — included
    /// here only so the client can recognise and route it.
    SessionStart,
}

/// One hook invocation forwarded from the thin client to the sidecar.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookRequest {
    pub event: HookEvent,
    /// The raw payload the harness wrote to the hook process's stdin, verbatim. The sidecar
    /// parses only what it needs; it's carried whole so later refinement can use any field.
    pub stdin_json: String,
}

/// The sidecar's reply: the exact stdout the thin client prints to the harness, already shaped
/// for the node's `agent_type` (Claude vs Gemini verdict JSON). The client is dumb — it prints
/// `stdout` and exits 0.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookVerdict {
    pub stdout: String,
}
