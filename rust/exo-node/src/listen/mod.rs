//! `listen` (N6) — the per-agent wake channel: sidecar → Monitor client → harness notification.
//!
//! The last hop of message delivery. The agent arms Claude Code's `Monitor` tool with
//! `exo listen --papers <path>` (persistent); that thin client connects to this node's
//! **listen socket** (`paths::listen_sock` — a second UDS beside the hooksock, because the
//! hooksock protocol is one-shot EOF-framed and this one streams), receives each dispatched
//! message as a [`ListenFrame`], writes it to stdout (each write becomes a harness notification
//! that wakes the agent between turns), and acks with a [`ListenAck`] **after** flushing.
//!
//! The ack is what lets `dispatch` return `Ok` and the inbound cursor advance: an acked frame
//! is known to have reached the harness's notification stream. With no client attached (or a
//! dead one), `dispatch` errs, the cursor stays pinned, and messages **queue durably** in the
//! inbox — the bus itself is the replay buffer for the spawn→arm window. A client attaching
//! pings the inbound wake so the backlog drains immediately.
//!
//! Framing: newline-delimited JSON in both directions. Ack correlation is by connection-local
//! `seq`, NOT message id — ids are reference-only (never a dedup or correlation key; a
//! redelivered entry keeps its original id), and `seq` stays unambiguous when the same logical
//! message crosses the channel twice.

pub mod client;
pub mod server;

pub use server::{serve, ListenDeliverError, ListenerSlot};

use serde::{Deserialize, Serialize};

/// Sidecar → client: one delivered message. `text` is the rendered payload the client must
/// write to stdout verbatim (may contain newlines — JSON-escaped inside the single frame line;
/// one buffered stdout write keeps all its lines in one Monitor notification batch).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ListenFrame {
    /// Connection-local monotonic sequence — the ack correlation key.
    pub seq: u64,
    pub text: String,
}

/// Client → sidecar: "I have written frame `seq` to stdout and flushed it."
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct ListenAck {
    pub seq: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frame_roundtrips_with_newlines() {
        let f = ListenFrame {
            seq: 7,
            text: "[from: a, kind: chat]\n\nline one\nline two".into(),
        };
        let json = serde_json::to_string(&f).unwrap();
        // The frame itself must stay one wire line.
        assert!(!json.contains('\n'));
        assert_eq!(serde_json::from_str::<ListenFrame>(&json).unwrap(), f);
    }

    #[test]
    fn ack_roundtrips() {
        let a = ListenAck { seq: 42 };
        let json = serde_json::to_string(&a).unwrap();
        assert_eq!(serde_json::from_str::<ListenAck>(&json).unwrap(), a);
    }
}
