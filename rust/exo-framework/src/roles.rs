//! The role **abstraction**: [`RoleDef<R>`] bundles a role's served tools + its three shared
//! hook fns, and the fn-pointer type aliases ([`PreToolUseFn`]/[`StopFn`]/[`SessionStartFn`])
//! the table stores them as. A role *reads* like declarative config but is plain, greppable,
//! unit-testable Rust: a list of tool **types** plus three fn-pointers. NO `dyn Caps` — the
//! table is parameterized by the concrete runtime `R`.
//!
//! The concrete roster (the `match` over the domain's role enum that names each role's tools +
//! hooks) is domain code and lives in the `exo` usage crate; the engine resolves it through
//! [`Exomonad::role_def`](crate::exomonad::Exomonad::role_def), monomorphized at the binary.

use crate::hooks::{HookDecision, HookInput, SessionStartOutput, StopDecision};
use crate::tool::{BoxFuture, Tool};

/// A hook is an async fn over the concrete runtime `R`. Stored as a plain fn-pointer so the
/// role table stays a greppable struct literal; the `BoxFuture` return lets the body do
/// async cap IO (the `stop` gate reads `git status` live). The generic bound lives on the
/// fn's own definition (e.g. `fn stop<R: Git + Log>(…)`); a role's `R: PolicyCaps` guarantees
/// every cap is present, so any role's hooks coerce to these pointer types.
pub type PreToolUseFn<R> = for<'a> fn(&'a R, &'a HookInput) -> BoxFuture<'a, HookDecision>;
pub type StopFn<R> = for<'a> fn(&'a R) -> BoxFuture<'a, StopDecision>;
pub type SessionStartFn<R> = for<'a> fn(&'a R) -> BoxFuture<'a, SessionStartOutput>;

/// A role: its served tools + its three shared hook fns. `dyn Tool<R>` is dyn over the
/// CONCRETE `R`, not over `Caps`.
pub struct RoleDef<R: Send + Sync> {
    pub tools: Vec<Box<dyn Tool<R>>>,
    pub pre_tool_use: PreToolUseFn<R>,
    pub stop: StopFn<R>,
    pub session_start: SessionStartFn<R>,
}
