//! Roles — [`RoleDef<R>`] bundles a role's tools + its three shared hook fns, and
//! [`role_def`] is the hand-written `match NodeKind` table. A role *reads* like declarative
//! config but is plain, greppable, unit-testable Rust: a list of tool **types** plus three
//! fn-pointers (hooks compose by pointing several roles at the same fn). NO `dyn Caps` — the
//! table is parameterized by the concrete runtime `R`. See `docs/design/swarm/04-policy.md`.
//!
//! **Status: Wave-3 scaffold.** The `RoleDef` shape + the fn-pointer signatures are frozen;
//! P7 fills the `role_def` arms with real tool lists + hook wiring once P1–P6 land their
//! tool/hook types. Until then each arm returns an empty-but-valid `RoleDef` so the crate
//! compiles and downstream (the sidecar) can already call `role_def`.

use crate::caps::PolicyCaps;
use crate::events::{EventAction, WorldEvent};
use crate::hooks::{HookDecision, HookInput, SessionStartOutput, StopDecision};
use crate::tool::{BoxFuture, Tool};
use exo_caps::NodeKind;

/// A hook is an async fn over the concrete runtime `R`. Stored as a plain fn-pointer so the
/// role table stays a greppable struct literal; the `BoxFuture` return lets the body do
/// async cap IO (the `stop` gate queries `GitHub` live). The generic bound lives on the
/// fn's own definition (e.g. `fn dev_stop<R: GitHub>(…)`); here `R: PolicyCaps` guarantees
/// every cap is present, so any role's hooks coerce to these pointer types.
pub type PreToolUseFn<R> = for<'a> fn(&'a R, &'a HookInput) -> BoxFuture<'a, HookDecision>;
pub type StopFn<R> = for<'a> fn(&'a R) -> BoxFuture<'a, StopDecision>;
pub type SessionStartFn<R> = for<'a> fn(&'a R) -> BoxFuture<'a, SessionStartOutput>;
pub type OnEventFn<R> = for<'a> fn(&'a R, &'a WorldEvent) -> BoxFuture<'a, EventAction>;

/// A role: its served tools + its three shared hook fns. `dyn Tool<R>` is dyn over the
/// CONCRETE `R`, not over `Caps`.
pub struct RoleDef<R: Send + Sync> {
    pub tools: Vec<Box<dyn Tool<R>>>,
    pub pre_tool_use: PreToolUseFn<R>,
    pub stop: StopFn<R>,
    pub session_start: SessionStartFn<R>,
    pub on_event: OnEventFn<R>,
}

// Default no-op hooks — the scaffold's wiring so the table compiles before P6/P7 supply the
// real fns. P7 replaces these per-role with the ported guard/stop/event logic.
fn allow_all<R: PolicyCaps>(_ctx: &R, _input: &HookInput) -> BoxFuture<'_, HookDecision> {
    Box::pin(async { HookDecision::Allow })
}
fn allow_stop<R: PolicyCaps>(_ctx: &R) -> BoxFuture<'_, StopDecision> {
    Box::pin(async { StopDecision::Allow })
}
fn no_context<R: PolicyCaps>(_ctx: &R) -> BoxFuture<'_, SessionStartOutput> {
    Box::pin(async { SessionStartOutput::default() })
}
fn no_event<R: PolicyCaps>(_ctx: &R, _e: &WorldEvent) -> BoxFuture<'_, EventAction> {
    Box::pin(async { EventAction::NoAction })
}

/// The per-role policy table. Hand-written `match` — the single place a role's tool list +
/// hooks are named. P1–P6 add tool types to the `tools` vecs; P7 swaps the no-op hooks for
/// the real per-role fns.
pub fn role_def<R: PolicyCaps>(kind: NodeKind) -> RoleDef<R> {
    // Every arm currently shares the no-op hooks + an empty toolset. Distinct arms are kept
    // so P1–P7 fill each independently without restructuring.
    let base = || RoleDef::<R> {
        tools: Vec::new(),
        pre_tool_use: allow_all::<R>,
        stop: allow_stop::<R>,
        session_start: no_context::<R>,
        on_event: no_event::<R>,
    };
    match kind {
        NodeKind::Root => base(),
        NodeKind::Tl => base(),
        NodeKind::Dev => base(),
        NodeKind::Worker => base(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockRuntime;

    #[tokio::test]
    async fn every_role_builds_and_hooks_default_to_allow() {
        for kind in [NodeKind::Root, NodeKind::Tl, NodeKind::Dev, NodeKind::Worker] {
            let rd = role_def::<MockRuntime>(kind);
            let ctx = MockRuntime::default();
            let input = HookInput {
                tool_name: "x".into(),
                tool_input: serde_json::Value::Null,
            };
            assert_eq!((rd.pre_tool_use)(&ctx, &input).await, HookDecision::Allow);
            assert_eq!((rd.stop)(&ctx).await, StopDecision::Allow);
        }
    }
}
