//! `hooksock` (N5) — the per-agent UDS hook-RPC channel.
//!
//! The third sidecar loop. The sidecar binds `paths::hook_sock` and serves hook invocations
//! against its **live** [`Runtime`](exo_runtime::Runtime); the short-lived
//! `exomonad experimental hook` client connects and forwards `{event, stdin}`, gets back the
//! exact verdict stdout. Synchronous request/response — the dual of the async jsonl bus.
//!
//! This is the **generic** hook abstraction: a new hook is a new
//! [`HookEvent`](exo_caps::HookEvent) variant + a role hook fn; the transport here is unchanged.
//! SessionStart is the one event NOT served here (the client handles it one-shot — it needs no
//! live state and must survive a cold-start race before the socket is listening).
//!
//! **Status: Wave-0 scaffold.** Signatures are frozen; `server` (leaf A1) and `client` (leaf A2)
//! fill the bodies in Wave 1.

pub mod client;
pub mod server;

pub use client::client_request;
pub use server::serve;
