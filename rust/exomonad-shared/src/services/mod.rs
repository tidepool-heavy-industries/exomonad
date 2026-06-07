//! Services shared by classic mode and node mode.
//!
//! These are the IO/plumbing pieces with no dependency on classic-only services
//! (no `Services`/`Has*` context, no GitHub/poller/registries): tmux IPC, the
//! retry/health primitives it builds on, and the agent-launch seam.

pub mod agent_control;
pub mod resilience;
pub mod tmux_ipc;
