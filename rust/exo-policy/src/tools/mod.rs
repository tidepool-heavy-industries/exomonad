//! The MCP tools — one module per tool file (the type-per-tool layout). Each tool is a
//! **type** with an `Args` struct (deriving `Deserialize + JsonSchema`), a generic-over-caps
//! `run` whose cap bounds *are* its least-privilege spec, and a hand-written `Tool<R>` adapter
//! (NO macro). Each ships mock-cap unit tests in the same file. See `docs/design/swarm/04-policy.md`.
//!
//! **Status: Wave-3 scaffold.** The leaves (P1–P5) populate one file each, conflict-free:
//! `messaging` (notify_parent / send_message), `tasks` (task_list / get / update), `spawn`
//! (the three per-op spawn tools), `file_pr`, `merge_pr`. P7 wires the resulting tool types
//! into [`role_def`](crate::roles::role_def).

pub mod messaging;
pub mod tasks;
pub mod spawn;
pub mod file_pr;
pub mod merge_pr;
