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
use crate::events::{on_world_event, EventAction, WorldEvent};
use crate::hooks::{pre_tool_use, session_start, stop, HookDecision, HookInput, SessionStartOutput, StopDecision};
use crate::tool::{BoxFuture, Tool};
use crate::tools::file_pr::FilePr;
use crate::tools::merge_pr::MergePr;
use crate::tools::messaging::{NotifyParent, SendMessage};
use crate::tools::spawn::{ForkWave, SpawnGemini, SpawnWorker};
use crate::tools::tasks::{TaskGet, TaskList, TaskUpdate};
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

/// The per-role policy table. Hand-written `match` — the single place a role's tool list +
/// hooks are named. P1–P6 add tool types to the `tools` vecs; P7 swaps the no-op hooks for
/// the real per-role fns.
pub fn role_def<R: PolicyCaps>(kind: NodeKind) -> RoleDef<R> {
    match kind {
        NodeKind::Root | NodeKind::Tl => RoleDef {
            tools: vec![
                Box::new(ForkWave),
                Box::new(SpawnGemini),
                Box::new(SpawnWorker),
                Box::new(FilePr),
                Box::new(MergePr),
                Box::new(NotifyParent),
                Box::new(SendMessage),
            ],
            pre_tool_use,
            stop,
            session_start,
            on_event: on_world_event,
        },
        NodeKind::Dev => RoleDef {
            tools: vec![
                Box::new(FilePr),
                Box::new(NotifyParent),
                Box::new(SendMessage),
                Box::new(TaskList),
                Box::new(TaskGet),
                Box::new(TaskUpdate),
            ],
            pre_tool_use,
            stop,
            session_start,
            on_event: on_world_event,
        },
        NodeKind::Worker => RoleDef {
            tools: vec![
                Box::new(NotifyParent),
                Box::new(SendMessage),
                Box::new(TaskList),
                Box::new(TaskGet),
                Box::new(TaskUpdate),
            ],
            pre_tool_use,
            stop,
            session_start,
            on_event: on_world_event,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockRuntime;

    #[tokio::test]
    async fn every_role_builds_non_empty_tools() {
        for kind in [NodeKind::Root, NodeKind::Tl, NodeKind::Dev, NodeKind::Worker] {
            let rd = role_def::<MockRuntime>(kind);
            assert!(!rd.tools.is_empty(), "Role {:?} should have tools", kind);
            
            // Verify hooks are wired (pointers are non-null by definition of fn pointers in Rust)
            assert_eq!(rd.pre_tool_use as usize, pre_tool_use::<MockRuntime> as *const () as usize);
            assert_eq!(rd.stop as usize, stop::<MockRuntime> as *const () as usize);
            assert_eq!(rd.session_start as usize, session_start::<MockRuntime> as *const () as usize);
            assert_eq!(rd.on_event as usize, on_world_event::<MockRuntime> as *const () as usize);
        }
    }

    #[tokio::test]
    async fn test_role_stop_gate_blocks_when_needed() {
        let rd = role_def::<MockRuntime>(NodeKind::Dev);
        let ctx = MockRuntime {
            pr_for_branch: Some(123),
            has_unaddressed_changes: true,
            ..Default::default()
        };
        
        match (rd.stop)(&ctx).await {
            StopDecision::Block { reason } => {
                assert!(reason.contains("Open PR #123 has unaddressed ChangesRequested"));
            }
            _ => panic!("Should be blocked by unaddressed changes"),
        }
    }
}
