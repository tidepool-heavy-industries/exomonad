//! `exo-policy` — the Bucket-C decision logic (tools / roles / hooks).
//!
//! Written generic over the [`exo_caps`] capability traits it needs (no `dyn Caps`), so
//! least-privilege is compiler-checked and every tool is unit-testable against mock caps
//! with zero IO. No phases, no DSL, no macros.
//!
//! ## Shape
//! - [`tool`] — the [`Tool<R>`](tool::Tool) trait + the JSON-edge adapter helpers + [`ToolOutput`](tool::ToolOutput).
//! - [`caps`] — the [`PolicyCaps`](caps::PolicyCaps) static bound-union for the dispatch boundary.
//! - [`hooks`] — the decision enums (`pre_tool_use` nudges, `stop` clean-gate, `session_start`).
//! - [`roles`] — [`RoleDef<R>`](roles::RoleDef) + the [`role_def`](roles::role_def) `NodeKind` table.
//! - [`tools`] — one module per MCP tool (a type + `Args` + generic-over-caps `run` + adapter).
//! - [`testing`] — the shared [`MockRuntime`](testing::MockRuntime) every leaf unit-tests against.

pub mod caps;
pub mod hooks;
pub mod roles;
pub mod tool;
pub mod tools;

#[cfg(test)]
pub mod testing;

pub use caps::PolicyCaps;
pub use hooks::{HookDecision, HookInput, SessionStartOutput, StopDecision};
pub use roles::{role_def, RoleDef};
pub use tool::{ok_json, parse, schema_json, BoxFuture, Tool, ToolOutput};
