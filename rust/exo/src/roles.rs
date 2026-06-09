//! Roles — the concrete roster. [`ExoRole`] is the domain's role enum; [`role_def`] is the
//! hand-written `match ExoRole` table (the
//! single place a role's tool list + hooks are named); the domain's [`Exomonad`](exo_framework::Exomonad)
//! impl resolves a role's [`RoleDef`] through it (replacing the deleted fn-pointer `RoleRegistry`).
//! A role *reads* like declarative config but is plain, greppable, unit-testable Rust: a list of
//! tool **types** plus three fn-pointers (hooks compose by pointing several roles at the same fn).
//! NO `dyn Caps` — the table is parameterized by the concrete runtime `R`. The [`RoleDef<R>`] shape
//! and the fn-pointer aliases are the framework contract ([`exo_framework::roles`]).

use crate::gates::{pre_tool_use, session_start, stop, stop_allow, stop_notify, stop_reviewer};
use crate::tools::merge::Merge;
use crate::tools::messaging::{NotifyParent, SendMessage};
use crate::tools::spawn::{ForkWave, SpawnGemini, SpawnWorker};
use crate::tools::submit::SubmitBranch;
use crate::tools::tree::Tree;
use crate::tools::verdict::Verdict;
use exo_caps::{AgentType, RoleKind};
use exo_framework::{PolicyCaps, RoleDef};
use serde::{Deserialize, Serialize};

/// The `exo` domain's role enum — the closed set of node archetypes (its `D::Role`). Owned by the
/// domain (was the engine's `NodeKind` before the trait refactor — leak #1), reached by the engine
/// only through the [`RoleKind`] seam. `agent_type` is the role→backend mapping (leak #2): a domain
/// maps each role onto the engine-owned launchable backend set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ExoRole {
    Root,
    Tl,
    Dev,
    Worker,
    /// A short-lived Gemini spawned by a submitting node to review its branch. Works in its own
    /// worktree off the under-review code and emits a `verdict`. Not a tree-building archetype.
    Reviewer,
}

impl RoleKind for ExoRole {
    fn all() -> &'static [Self] {
        &[
            ExoRole::Root,
            ExoRole::Tl,
            ExoRole::Dev,
            ExoRole::Worker,
            ExoRole::Reviewer,
        ]
    }
    fn agent_type(&self) -> AgentType {
        match self {
            ExoRole::Root | ExoRole::Tl => AgentType::Claude,
            ExoRole::Dev | ExoRole::Worker | ExoRole::Reviewer => AgentType::Gemini,
        }
    }
    fn role_str(&self) -> &'static str {
        match self {
            ExoRole::Root => "root",
            ExoRole::Tl => "tl",
            ExoRole::Dev => "dev",
            ExoRole::Worker => "worker",
            ExoRole::Reviewer => "reviewer",
        }
    }
    fn protocol(&self) -> &'static str {
        match self {
            ExoRole::Root => crate::protocol::ROOT,
            ExoRole::Tl => crate::protocol::TL,
            ExoRole::Dev => crate::protocol::DEV,
            ExoRole::Worker => crate::protocol::WORKER,
            ExoRole::Reviewer => crate::protocol::REVIEWER,
        }
    }
}

/// The per-role policy table. Hand-written `match` — the single place a role's tool list +
/// hooks are named. Convergence is on-disk (v2): a TL folds a finished child with the local
/// `merge` tool (no PR, no GitHub); leaves just commit to their branch.
pub fn role_def<R: PolicyCaps>(kind: ExoRole) -> RoleDef<R> {
    match kind {
        // Root is the human-facing top: no parent (so no `notify_parent`). It spawns children
        // and folds them by merging their branches locally; that's it.
        ExoRole::Root => RoleDef {
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
        ExoRole::Tl => RoleDef {
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
        ExoRole::Dev => RoleDef {
            tools: vec![Box::new(NotifyParent), Box::new(SubmitBranch)],
            pre_tool_use,
            stop: stop_notify,
            session_start,
        },
        // A worker is an inline child sharing the parent's worktree — no own branch to submit, so it
        // only reports back, but it still signals the parent when it yields control.
        ExoRole::Worker => RoleDef {
            tools: vec![Box::new(NotifyParent)],
            pre_tool_use,
            stop: stop_notify,
            session_start,
        },
        // A reviewer reads the under-review branch and emits a `verdict`, then exits. It does not
        // submit or merge; `notify_parent` is its colleague back-channel ("why'd you do this?").
        ExoRole::Reviewer => RoleDef {
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
            ExoRole::Root,
            ExoRole::Tl,
            ExoRole::Dev,
            ExoRole::Worker,
            ExoRole::Reviewer,
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
                ExoRole::Root => stop_allow::<MockRuntime> as *const () as usize,
                ExoRole::Reviewer => stop_reviewer::<MockRuntime> as *const () as usize,
                ExoRole::Dev | ExoRole::Worker => stop_notify::<MockRuntime> as *const () as usize,
                ExoRole::Tl => stop::<MockRuntime> as *const () as usize,
            };
            assert_eq!(rd.stop as usize, expected_stop, "Role {:?} stop fn", kind);
            assert_eq!(
                rd.session_start as usize,
                session_start::<MockRuntime> as *const () as usize
            );
        }
    }

    #[test]
    fn test_role_protocol_maps_per_variant() {
        // Each ExoRole returns its own protocol const (the override of the empty RoleKind default).
        assert!(ExoRole::Root.protocol().contains("Root TL Protocol"));
        assert!(ExoRole::Tl.protocol().contains("Spawned TL Protocol"));
        assert!(ExoRole::Dev.protocol().contains("Dev Agent Protocol"));
        assert!(ExoRole::Worker.protocol().contains("Worker Agent Protocol"));
        assert!(ExoRole::Reviewer.protocol().contains("Reviewer Protocol"));
        // T1.3: intent-cue anchoring lives in the protocol too, not just the spawn task.
        assert!(ExoRole::Reviewer.protocol().contains("do NOT lower the bar"));
        // v2-accurate: no classic plumbing leaked into the steering prose.
        for kind in ExoRole::all() {
            let p = kind.protocol();
            assert!(!p.contains("file_pr"), "{kind:?} mentions classic file_pr");
            assert!(!p.contains("Copilot"), "{kind:?} mentions Copilot");
        }
    }

    #[tokio::test]
    async fn role_tool_matrix() {
        for kind in ExoRole::all() {
            let rd = role_def::<MockRuntime>(*kind);
            let mut names: Vec<String> = rd.tools.iter().map(|t| t.name().to_string()).collect();
            names.sort();
            let expected = match kind {
                ExoRole::Root => vec![
                    "fork_wave",
                    "merge",
                    "send_message",
                    "spawn_gemini",
                    "spawn_worker",
                    "tree",
                ],
                ExoRole::Tl => vec![
                    "fork_wave",
                    "merge",
                    "notify_parent",
                    "send_message",
                    "spawn_gemini",
                    "spawn_worker",
                    "submit_branch",
                    "tree",
                ],
                ExoRole::Dev => vec!["notify_parent", "submit_branch"],
                ExoRole::Worker => vec!["notify_parent"],
                ExoRole::Reviewer => vec!["notify_parent", "verdict"],
            };
            assert_eq!(names, expected, "Tool matrix mismatch for {:?}", kind);
        }
    }

    #[test]
    fn exo_role_metadata() {
        use std::collections::HashSet;
        assert_eq!(ExoRole::all().len(), 5);
        let mut strs = HashSet::new();
        for kind in ExoRole::all() {
            strs.insert(kind.role_str());
            let agent = kind.agent_type();
            match kind {
                ExoRole::Root | ExoRole::Tl => assert_eq!(agent, AgentType::Claude),
                ExoRole::Dev | ExoRole::Worker | ExoRole::Reviewer => {
                    assert_eq!(agent, AgentType::Gemini)
                }
            }
        }
        assert_eq!(strs.len(), 5, "role_str must be unique");
    }

    #[tokio::test]
    async fn test_role_stop_gate_blocks_when_dirty() {
        let rd = role_def::<MockRuntime>(ExoRole::Tl);
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
