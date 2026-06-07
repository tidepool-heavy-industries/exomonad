//! Roles — the concrete roster. [`role_def`] is the hand-written `match NodeKind` table (the
//! single place a role's tool list + hooks are named), and [`roster`] wraps it as the
//! [`RoleRegistry`](exo_framework::RoleRegistry) the binary injects into the engine. A role
//! *reads* like declarative config but is plain, greppable, unit-testable Rust: a list of tool
//! **types** plus three fn-pointers (hooks compose by pointing several roles at the same fn). NO
//! `dyn Caps` — the table is parameterized by the concrete runtime `R`. The [`RoleDef<R>`] shape
//! and the fn-pointer aliases are the framework contract ([`exo_framework::roles`]).

use crate::gates::{pre_tool_use, session_start, stop, stop_allow, stop_notify, stop_reviewer};
use crate::tools::merge::Merge;
use crate::tools::messaging::{NotifyParent, SendMessage};
use crate::tools::spawn::{ForkWave, SpawnGemini, SpawnWorker};
use crate::tools::submit::SubmitBranch;
use crate::tools::tree::Tree;
use crate::tools::verdict::Verdict;
use exo_caps::NodeKind;
use exo_framework::{PolicyCaps, RoleDef, RoleRegistry};

/// Build the [`RoleRegistry`] the binary injects into the engine — the domain's whole
/// public surface to `exo-node`. Monomorphized at the binary's concrete runtime `R`.
pub fn roster<R: PolicyCaps>() -> RoleRegistry<R> {
    RoleRegistry::new(role_def::<R>)
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
                Box::new(Tree),
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
                Box::new(Tree),
            ],
            pre_tool_use,
            stop,
            session_start,
        },
        // A dev leaf works on its own branch and submits it for the parent to merge. It NEVER blocks at
        // stop (Gemini #20426); the committed-before-fold guarantee is enforced by submit_branch.
        NodeKind::Dev => RoleDef {
            tools: vec![Box::new(NotifyParent), Box::new(SubmitBranch)],
            pre_tool_use,
            stop: stop_notify,
            session_start,
        },
        // A worker is an inline child sharing the parent's worktree — no own branch to submit, so it
        // only reports back, but it still signals the parent when it yields control.
        NodeKind::Worker => RoleDef {
            tools: vec![Box::new(NotifyParent)],
            pre_tool_use,
            stop: stop_notify,
            session_start,
        },
        // A reviewer reads the under-review branch and emits a `verdict`, then exits. It does not
        // submit or merge; `notify_parent` is its colleague back-channel ("why'd you do this?").
        NodeKind::Reviewer => RoleDef {
            tools: vec![Box::new(Verdict), Box::new(NotifyParent)],
            pre_tool_use,
            // Ephemeral; it exits after the verdict — nothing to fold, so don't gate (would only
            // risk wedging on stray review artifacts).
            stop: stop_reviewer,
            session_start,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockRuntime;
    use exo_framework::StopDecision;

    #[tokio::test]
    async fn every_role_builds_non_empty_tools() {
        for kind in [
            NodeKind::Root,
            NodeKind::Tl,
            NodeKind::Dev,
            NodeKind::Worker,
            NodeKind::Reviewer,
        ] {
            let rd = role_def::<MockRuntime>(kind);
            assert!(!rd.tools.is_empty(), "Role {:?} should have tools", kind);

            // Verify hooks are wired (pointers are non-null by definition of fn pointers in Rust)
            assert_eq!(
                rd.pre_tool_use as usize,
                pre_tool_use::<MockRuntime> as *const () as usize
            );
            // Root never yields work to fold → stop_allow. Reviewer (Gemini) signals ReviewAborted
            // if no verdict → stop_reviewer. Dev/Worker (Gemini) notify the parent then allow
            // (never block). Tl keeps the dirty-gate (stop).
            let expected_stop = match kind {
                NodeKind::Root => stop_allow::<MockRuntime> as *const () as usize,
                NodeKind::Reviewer => stop_reviewer::<MockRuntime> as *const () as usize,
                NodeKind::Dev | NodeKind::Worker => {
                    stop_notify::<MockRuntime> as *const () as usize
                }
                NodeKind::Tl => stop::<MockRuntime> as *const () as usize,
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
        let rd = role_def::<MockRuntime>(NodeKind::Tl);
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
