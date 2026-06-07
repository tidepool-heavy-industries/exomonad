//! ExoMonad shared seam.
//!
//! The lean code both architectures need, with **no dependency on classic**:
//!
//! - `domain` — validated newtypes (agent names, branches, paths, …).
//! - `protocol` — control-envelope wire types (`Runtime`, `HookEventType`, hook/mcp/service).
//! - `error`, `util`, `ffi`, `hooks`, `logging` — shared plumbing.
//! - `services::{tmux_ipc, resilience, agent_control}` — tmux IPC, retry primitives,
//!   and the agent-launch types + command builder.
//!
//! Classic (`exomonad-core`) depends on this crate and re-exports these modules at
//! their historical paths, so classic code resolves `crate::domain::X` unchanged.
//! Node mode (`exo-runtime`) depends on this crate **instead of** classic.

pub mod domain;
pub mod error;
pub mod ffi;
pub mod hooks;
pub mod logging;
pub mod protocol;
pub mod services;
pub mod util;
