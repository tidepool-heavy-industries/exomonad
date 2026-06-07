//! The review gate — the `exo` domain's inter-node behavior, relocated out of the engine.
//!
//! [`ReviewSystem`] is the domain's [`DomainSystem`](exo_caps::DomainSystem) payload (the verdict a
//! reviewer emits, rides the bus erased as `MessageKind::Domain`). [`handle_review_system`] is the
//! relocated `apply_verdict` — the logic the submitter's sidecar runs on a verdict. It operates
//! purely through the engine's [`SystemCtx`] seam (no caps, no IO of its own), so it is unit-testable
//! against a mock context and the engine stays domain-agnostic. The one lifecycle action it can't do
//! itself — tearing down the one-shot reviewer — is returned as [`SystemOutcome::ReclaimSender`] for
//! the engine to perform (the engine owns `kill_pane`/`reclaim_worktree`).

use exo_caps::{Branch, CapResult, Message, MessageBody, MessageKind, Persona, Summary};
use exo_framework::{SystemCtx, SystemOutcome};
use serde::{Deserialize, Serialize};

/// The review verdict a reviewer emits to its submitter (the `exo` domain's `D::System`). Serde-tagged
/// on `type`; rides the bus erased as `MessageKind::Domain` (via `deliver_domain`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ReviewSystem {
    /// The reviewer approved `branch@sha`. The submitter's sidecar auto-escalates `[READY]`
    /// upward (no LLM turn) iff `sha` still matches the submitter's HEAD.
    ReviewApproved { branch: Branch, sha: String },
    /// The reviewer rejected with feedback. Rendered + delivered to the submitter's LLM to address.
    ReviewDenied {
        branch: Branch,
        sha: String,
        message: String,
    },
    /// The reviewer committed a counter-proposal to `changes_branch`. Rendered + delivered to the
    /// submitter's LLM to `merge` + re-submit.
    ReviewChanges {
        branch: Branch,
        sha: String,
        changes_branch: Branch,
        message: String,
    },
    /// A reviewer ended its turn WITHOUT producing a verdict (its `stop` hook sent this so the
    /// failure is LOUD instead of a silent forever-stall).
    ReviewAborted { reason: String },
}

/// Apply a review verdict (the relocated `apply_verdict`). Escalates `[READY]` to the parent on a
/// matching approval; wakes this node's LLM on deny/changes/aborted. Always asks the engine to
/// reclaim the one-shot reviewer (the sender) afterward — a reviewer is done the moment it votes.
pub async fn handle_review_system<C: SystemCtx + ?Sized>(
    ctx: &C,
    _from: &Persona,
    system: &ReviewSystem,
) -> CapResult<SystemOutcome> {
    match system {
        ReviewSystem::ReviewApproved { branch, sha } => {
            // The approval must be for THIS node's branch at its CURRENT commit. A mismatched
            // branch (right sha) must not escalate [READY] for my branch; a stale sha (work
            // committed after the review) needs a fresh review. Either way the reviewer is done.
            let my_branch = ctx.own_branch().clone();
            if branch.as_str() != my_branch.as_str() {
                tracing::warn!(
                    "approval names branch {} but my branch is {} — ignoring",
                    branch.as_str(),
                    my_branch.as_str()
                );
                return Ok(SystemOutcome::ReclaimSender);
            }
            let head = ctx.head_sha().await?;
            if &head != sha {
                tracing::warn!(
                    "stale approval for {} @ {} (HEAD is {}) — ignoring",
                    branch.as_str(),
                    sha,
                    head
                );
                return Ok(SystemOutcome::ReclaimSender);
            }
            let text = format!(
                "[READY] branch `{}` was approved by review and is ready for merge.",
                my_branch.as_str()
            );
            let summary = format!("[READY] {}", my_branch.as_str());
            let msg = Message {
                text: MessageBody::new(text)?,
                summary: Summary::new(summary)?,
                kind: MessageKind::Chat,
            };
            ctx.deliver_parent(msg).await?;
            tracing::info!(
                outcome = "escalated_ready",
                branch = %my_branch.as_str(),
                "review approved for {} — escalated [READY] to parent",
                my_branch.as_str()
            );
            Ok(SystemOutcome::ReclaimSender)
        }
        ReviewSystem::ReviewDenied { message, .. } => {
            ctx.deliver_to_self(
                "reviewer",
                "[REVIEW]",
                &format!(
                    "[REVIEW: changes requested] Your branch was not approved. Address this feedback, commit, then call submit_branch again:\n{message}"
                ),
            )
            .await?;
            Ok(SystemOutcome::ReclaimSender)
        }
        ReviewSystem::ReviewChanges {
            changes_branch,
            message,
            ..
        } => {
            ctx.deliver_to_self(
                "reviewer",
                "[REVIEW]",
                &format!(
                    "[REVIEW: proposed changes] The reviewer committed improvements on branch `{}`. Merge it with the `merge` tool to incorporate, then call submit_branch again:\n{message}",
                    changes_branch.as_str()
                ),
            )
            .await?;
            Ok(SystemOutcome::ReclaimSender)
        }
        ReviewSystem::ReviewAborted { reason } => {
            ctx.deliver_to_self(
                "reviewer",
                "[REVIEW]",
                &format!(
                    "[REVIEW ABORTED] Your reviewer exited without producing a verdict ({reason}). No approval was recorded — re-run `submit_branch` to spawn a fresh reviewer."
                ),
            )
            .await?;
            Ok(SystemOutcome::ReclaimSender)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use std::sync::Mutex;

    /// A mock [`SystemCtx`] recording deliveries — proves the gate logic without a live runtime.
    struct MockCtx {
        branch: Branch,
        head: String,
        parent: Mutex<Vec<String>>,
        to_self: Mutex<Vec<String>>,
    }

    #[async_trait]
    impl SystemCtx for MockCtx {
        fn own_branch(&self) -> &Branch {
            &self.branch
        }
        async fn head_sha(&self) -> CapResult<String> {
            Ok(self.head.clone())
        }
        async fn deliver_parent(&self, msg: Message) -> CapResult<()> {
            self.parent.lock().unwrap().push(msg.summary.as_str().to_string());
            Ok(())
        }
        async fn deliver_to_self(&self, _from: &str, summary: &str, _text: &str) -> CapResult<()> {
            self.to_self.lock().unwrap().push(summary.to_string());
            Ok(())
        }
    }

    fn mock(branch: &str, head: &str) -> MockCtx {
        MockCtx {
            branch: Branch::new(branch.into()).unwrap(),
            head: head.into(),
            parent: Mutex::new(vec![]),
            to_self: Mutex::new(vec![]),
        }
    }

    fn from() -> Persona {
        Persona::Agent(exo_caps::AgentName::new("reviewer-0".into()).unwrap())
    }

    #[tokio::test]
    async fn matching_approval_escalates_ready_and_reclaims() {
        let ctx = mock("root.dev-0", "abc");
        let sys = ReviewSystem::ReviewApproved {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "abc".into(),
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(ctx.parent.lock().unwrap().iter().any(|s| s.contains("[READY]")));
    }

    #[tokio::test]
    async fn stale_sha_does_not_escalate_but_reclaims() {
        let ctx = mock("root.dev-0", "newsha");
        let sys = ReviewSystem::ReviewApproved {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "oldsha".into(),
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(ctx.parent.lock().unwrap().is_empty());
    }

    #[tokio::test]
    async fn denied_wakes_llm_and_reclaims() {
        let ctx = mock("root.dev-0", "abc");
        let sys = ReviewSystem::ReviewDenied {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "abc".into(),
            message: "fix it".into(),
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(!ctx.to_self.lock().unwrap().is_empty());
    }
}
