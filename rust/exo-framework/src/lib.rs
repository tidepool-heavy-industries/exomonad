//! `exo-framework` — the reusable node-mode **engine abstractions**.
//!
//! The policy *contract*: the [`Tool<R>`](tool::Tool) trait +
//! JSON-edge helpers, [`RoleDef<R>`](roles::RoleDef) + its hook fn-pointer aliases, the hook
//! decision enums ([`hooks`]), the [`PolicyCaps`](caps::PolicyCaps) static bound-union for the
//! dispatch boundary, and the [`RoleRegistry`](registry::RoleRegistry) that lets the engine
//! resolve roles **injected by the binary** rather than depending on the concrete domain.
//!
//! Everything here is generic over the concrete runtime `R` and written against the
//! [`exo_caps`] trait seam — no IO, no concrete tools/roles/gates. Those live in the `exo`
//! usage crate, the minimal domain that wires this framework together.
//!
//! ## Shape
//! - [`tool`] — the [`Tool<R>`](tool::Tool) trait + the JSON-edge adapter helpers + [`ToolOutput`](tool::ToolOutput).
//! - [`caps`] — the [`PolicyCaps`](caps::PolicyCaps) static bound-union for the dispatch boundary.
//! - [`hooks`] — the decision enums (`pre_tool_use` nudges, `stop` clean-gate, `session_start`).
//! - [`roles`] — [`RoleDef<R>`](roles::RoleDef) + the hook fn-pointer type aliases.
//! - [`registry`] — [`RoleRegistry<R>`](registry::RoleRegistry), the injected role resolver (being
//!   superseded by [`exomonad`]).
//! - [`exomonad`] — the [`Exomonad`](exomonad::Exomonad) trait: the engine as generic machinery
//!   over a domain TYPE (four associated types + `role_def` + `handle_system`). The seam traits its
//!   associated types are bound by ([`RoleKind`](exo_caps::RoleKind) etc.) live in `exo-caps`.

pub mod caps;
pub mod exomonad;
pub mod hooks;
pub mod registry;
pub mod roles;
pub mod tool;

pub use caps::PolicyCaps;
pub use exomonad::{Exomonad, SystemCtx, SystemOutcome};
pub use hooks::{HookDecision, HookInput, SessionStartOutput, StopDecision};
pub use registry::RoleRegistry;
pub use roles::{PreToolUseFn, RoleDef, SessionStartFn, StopFn};
pub use tool::{ok_json, parse, schema_json, BoxFuture, Tool, ToolOutput};
