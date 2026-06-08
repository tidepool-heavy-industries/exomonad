//! `exo` — the minimal domain usage of [`exo_framework`].
//!
//! This is the Rust analog of Classic's Haskell-WASM "config DSL": the framework is the big
//! reusable engine; `exo` is the small concrete usage of it. It holds the genuinely
//! domain-specific Bucket-C logic — the MCP tool set, the per-role roster, and the CC hook
//! gates — written **generic over the [`exo_caps`] traits** (no `dyn Caps`), so least-privilege
//! is compiler-checked and every tool is unit-testable against mock caps with zero IO.
//!
//! The engine never depends on this crate. The binary builds a
//! [`RoleRegistry`](exo_framework::RoleRegistry) from [`roster`] and injects it into `exo-node`;
//! that injection is the whole point of the framework/domain split.
//!
//! ## Shape
//! - [`tools`] — one module per MCP tool (a type + `Args` + generic-over-caps `run` + adapter).
//! - [`gates`] — the concrete hook bodies (`pre_tool_use` nudge, `stop` clean-gate, the per-role
//!   `stop` variants, `session_start`).
//! - [`roles`] — [`role_def`](roles::role_def) (the `NodeKind` table the domain's [`Exomonad`]
//!   impl resolves through).
//! - [`review`] — the domain's inter-node behavior: [`ReviewSystem`](review::ReviewSystem)
//!   (`D::System`) + the relocated review-gate logic ([`handle_review_system`](review::handle_review_system)).
//! - [`spawn`] — [`ExoSpawn`](spawn::ExoSpawn), the domain's `D::Spawn`.
//! - [`testing`] — the shared [`MockRuntime`](testing::MockRuntime) every tool unit-tests against.
//!
//! [`Exomonad`]: exo_framework::Exomonad

pub mod gates;
pub mod protocol;
pub mod review;
pub mod roles;
pub mod spawn;
pub mod tools;

#[cfg(test)]
pub mod testing;

pub use review::{handle_review_system, ReviewSystem};
pub use roles::{role_def, ExoRole};
pub use spawn::ExoSpawn;
