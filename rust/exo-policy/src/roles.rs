//! Roles — [`RoleDef<R>`] bundles a role's tools + its three shared hook fns, and
//! [`role_def`] is the hand-written `match NodeKind` table. A role *reads* like declarative
//! config but is plain, greppable, unit-testable Rust: a list of tool **types** plus three
//! fn-pointers (hooks compose by pointing several roles at the same fn). NO `dyn Caps` — the
//! table is parameterized by the concrete runtime `R`.
//!
//! **Status: Wave-3 scaffold.** The `RoleDef` shape + the fn-pointer signatures are frozen;
//! P7 fills the `role_def` arms with real tool lists + hook wiring once P1–P6 land their
//! tool/hook types. Until then each arm returns an empty-but-valid `RoleDef` so the crate
//! compiles and downstream (the sidecar) can already call `role_def`.

use crate::caps::PolicyCaps;
use crate::hooks::{
    pre_tool_use, session_start, stop, stop_allow, HookDecision, HookInput, SessionStartOutput,
    StopDecision,
};
use crate::tool::{BoxFuture, Tool};
use crate::tools::merge::Merge;
use crate::tools::messaging::{NotifyParent, SendMessage};
use crate::tools::spawn::{ForkWave, SpawnGemini, SpawnWorker};
use crate::tools::submit::SubmitBranch;
use exo_caps::NodeKind;

/// A hook is an async fn over the concrete runtime `R`. Stored as a plain fn-pointer so the
/// role table stays a greppable struct literal; the `BoxFuture` return lets the body do
/// async cap IO (the `stop` gate reads `git status` live). The generic bound lives on the
/// fn's own definition (e.g. `fn stop<R: Git + Log>(…)`); here `R: PolicyCaps` guarantees
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

/// The per-role policy table. Hand-written `match` — the single place a role's tool list +
/// hooks are named. Convergence is on-disk (v2): a TL folds a finished child with the local
/// `merge` tool (no PR, no GitHub); leaves just commit to their branch.
pub fn role_def<R: PolicyCaps>(kind: NodeKind) -> RoleDef<R> {
    match kind {
        // Root is the human-facing top: no parent (so no `notify_parent`). It spawns children
        // and folds them by merging their branches locally; that's it.
        NodeKind::Root => RoleDef {
            tools: vec![
                Box::new(ForkWave),
                Box::new(SpawnGemini),
                Box::new(SpawnWorker),
                Box::new(Merge),
                Box::new(SendMessage),
            ],
            pre_tool_use,
            // Root has nothing to fold upward — never gate its exit (blocking it bricks the session).
            stop: stop_allow,
            session_start,
        },
        // A spawned TL spawns + folds its own subtree, then submits its own branch up to its
        // parent when done (and notifies for status/failure).
        NodeKind::Tl => RoleDef {
            tools: vec![
                Box::new(ForkWave),
                Box::new(SpawnGemini),
                Box::new(SpawnWorker),
                Box::new(Merge),
                Box::new(NotifyParent),
                Box::new(SendMessage),
                Box::new(SubmitBranch),
            ],
            pre_tool_use,
            stop,
            session_start,
        },
        // A dev leaf works on its own branch and submits it for the parent to merge.
        NodeKind::Dev => RoleDef {
            tools: vec![Box::new(NotifyParent), Box::new(SubmitBranch)],
            pre_tool_use,
            stop,
            session_start,
        },
        // A worker is an inline child sharing the parent's worktree — it has no own branch to
        // submit, so it only reports back.
        NodeKind::Worker => RoleDef {
            tools: vec![Box::new(NotifyParent)],
            pre_tool_use,
            // Workers are ephemeral and commit nothing to fold — nothing to gate on.
            stop: stop_allow,
            session_start,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockRuntime;

    #[tokio::test]
    async fn every_role_builds_non_empty_tools() {
        for kind in [
            NodeKind::Root,
            NodeKind::Tl,
            NodeKind::Dev,
            NodeKind::Worker,
        ] {
            let rd = role_def::<MockRuntime>(kind);
            assert!(!rd.tools.is_empty(), "Role {:?} should have tools", kind);

            // Verify hooks are wired (pointers are non-null by definition of fn pointers in Rust)
            assert_eq!(
                rd.pre_tool_use as usize,
                pre_tool_use::<MockRuntime> as *const () as usize
            );
            // Root/Worker never file a PR → no gate (stop_allow); Tl/Dev gate via `stop`.
            let expected_stop = match kind {
                NodeKind::Root | NodeKind::Worker => {
                    stop_allow::<MockRuntime> as *const () as usize
                }
                NodeKind::Tl | NodeKind::Dev => stop::<MockRuntime> as *const () as usize,
            };
            assert_eq!(rd.stop as usize, expected_stop, "Role {:?} stop fn", kind);
            assert_eq!(
                rd.session_start as usize,
                session_start::<MockRuntime> as *const () as usize
            );
        }
    }

    #[tokio::test]
    async fn test_role_stop_gate_blocks_when_dirty() {
        let rd = role_def::<MockRuntime>(NodeKind::Dev);
        let ctx = MockRuntime {
            is_clean: false,
            ..Default::default()
        };

        match (rd.stop)(&ctx).await {
            StopDecision::Block { reason } => {
                assert!(reason.contains("Uncommitted changes"));
            }
            _ => panic!("Should be blocked by uncommitted changes"),
        }
    }
}
