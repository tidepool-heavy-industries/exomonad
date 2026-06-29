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
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// The severity of a review finding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    /// A critical issue that MUST be addressed before the branch can be merged.
    Error,
    /// A notable issue or stylistic concern that should be addressed but does not block merge.
    Warning,
    /// Purely informational feedback.
    Info,
    /// A minor suggestion or "nice-to-have" improvement.
    Hint,
}

impl Severity {
    /// Returns true if this severity level blocks a merge.
    pub fn blocks(self) -> bool {
        matches!(self, Severity::Error)
    }
}

/// A structured finding from a code review.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct Finding {
    /// The file path where the issue was found.
    pub file: String,
    /// The line number (1-indexed) where the issue was found.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub line: Option<u32>,
    /// The severity of the finding.
    pub severity: Severity,
    /// A description of the issue.
    pub body: String,
    /// An optional suggested fix or replacement code.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub suggestion: Option<String>,
}

/// The review verdict a reviewer emits to its submitter (the `exo` domain's `D::System`). Serde-tagged
/// on `type`; rides the bus erased as `MessageKind::Domain` (via `deliver_domain`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ReviewSystem {
    /// The reviewer completed the review of `branch@sha`. The decision (approve vs request changes)
    /// is derived from the `findings`: any `Severity::Error` blocks the merge.
    Reviewed {
        branch: Branch,
        sha: String,
        summary: String,
        findings: Vec<Finding>,
    },
    /// A reviewer ended its turn WITHOUT producing a verdict (its `stop` hook sent this so the
    /// failure is LOUD instead of a silent forever-stall).
    ReviewAborted { reason: String },
}

/// A single round of review persisted to the log.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReviewRound {
    /// The 1-indexed round number.
    pub round: u32,
    /// The commit SHA reviewed in this round.
    pub sha: String,
    /// The high-level summary of the review.
    pub summary: String,
    /// The detailed findings.
    pub findings: Vec<Finding>,
    /// Whether this round blocked the merge (derived from findings).
    pub blocked: bool,
}

/// The durable log of all review rounds for a branch.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReviewLog {
    /// The branch name.
    pub branch: String,
    /// The history of review rounds, oldest to newest.
    pub rounds: Vec<ReviewRound>,
}

/// Sanitize a branch name to a safe filename: [A-Za-z0-9_-], join with `.`.
pub fn safe_branch(branch: &str) -> String {
    branch
        .split('.')
        .map(|seg| {
            seg.chars()
                .map(|c| {
                    if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                        c
                    } else {
                        '-'
                    }
                })
                .collect::<String>()
        })
        .collect::<Vec<_>>()
        .join(".")
}

/// Apply a review verdict (the relocated `apply_verdict`). Escalates `[READY]` to the parent on a
/// matching approval (no blocking findings); wakes this node's LLM on blocked/aborted reviews.
/// Always asks the engine to reclaim the one-shot reviewer (the sender) afterward — a reviewer is
/// done the moment it votes.
pub async fn handle_review_system<C: SystemCtx + ?Sized>(
    ctx: &C,
    _from: &Persona,
    system: &ReviewSystem,
) -> CapResult<SystemOutcome> {
    match system {
        ReviewSystem::Reviewed {
            branch,
            sha,
            summary,
            findings,
        } => {
            // The approval must be for THIS node's branch at its CURRENT commit. A mismatched
            // branch (right sha) must not escalate [READY] for my branch; a stale sha (work
            // committed after the review) needs a fresh review. Either way the reviewer is done.
            let my_branch = ctx.own_branch().clone();
            if branch.as_str() != my_branch.as_str() {
                tracing::warn!(
                    "verdict names branch {} but my branch is {} — ignoring",
                    branch.as_str(),
                    my_branch.as_str()
                );
                return Ok(SystemOutcome::ReclaimSender);
            }
            let head = ctx.head_sha().await?;
            if &head != sha {
                tracing::warn!(
                    "stale verdict for {} @ {} (HEAD is {}) — ignoring",
                    branch.as_str(),
                    sha,
                    head
                );
                return Ok(SystemOutcome::ReclaimSender);
            }

            let blocked = findings.iter().any(|f| f.severity.blocks());
            if !blocked {
                let nit_count = findings.len();
                let text = if nit_count > 0 {
                    format!(
                        "[READY] branch `{}` passed review ({} non-blocking nits). Summary: {}",
                        my_branch.as_str(),
                        nit_count,
                        summary
                    )
                } else {
                    format!(
                        "[READY] branch `{}` passed review with no findings. Summary: {}",
                        my_branch.as_str(),
                        summary
                    )
                };
                let msg_summary = format!("[READY] {}", my_branch.as_str());
                let msg = Message {
                    text: MessageBody::new(text)?,
                    summary: Summary::new(msg_summary)?,
                    kind: MessageKind::Chat,
                };
                ctx.deliver_parent(msg).await?;
                tracing::info!(
                    outcome = "escalated_ready",
                    branch = %my_branch.as_str(),
                    "review approved for {} — escalated [READY] to parent",
                    my_branch.as_str()
                );
            } else {
                ctx.deliver_to_self("reviewer", "[REVIEW]", &render_findings(summary, findings))
                    .await?;
            }

            // BEST-EFFORT: Persist the review round to the durable log.
            let safe = safe_branch(my_branch.as_str());
            let path = std::path::PathBuf::from(format!(".exo/reviews/{safe}.json"));

            let mut log = match ctx.read_file(&path).await {
                Ok(Some(bytes)) => {
                    serde_json::from_slice::<ReviewLog>(&bytes).unwrap_or_else(|e| {
                        tracing::warn!("failed to parse review log at {:?}: {e}", path);
                        ReviewLog {
                            branch: my_branch.as_str().to_string(),
                            rounds: vec![],
                        }
                    })
                }
                Ok(None) => ReviewLog {
                    branch: my_branch.as_str().to_string(),
                    rounds: vec![],
                },
                Err(e) => {
                    tracing::warn!("failed to read review log at {:?}: {e}", path);
                    ReviewLog {
                        branch: my_branch.as_str().to_string(),
                        rounds: vec![],
                    }
                }
            };

            log.rounds.push(ReviewRound {
                round: (log.rounds.len() as u32) + 1,
                sha: sha.clone(),
                summary: summary.clone(),
                findings: findings.clone(),
                blocked,
            });

            if let Ok(bytes) = serde_json::to_vec(&log) {
                if let Err(e) = ctx.write_file(&path, &bytes).await {
                    tracing::warn!("failed to persist review log at {:?}: {e}", path);
                }
            }

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

/// Renders a structured list of findings into a human-readable string.
pub fn render_findings(summary: &str, findings: &[Finding]) -> String {
    use std::collections::BTreeMap;

    let mut out = String::new();
    let blocked = findings.iter().any(|f| f.severity.blocks());

    if blocked {
        out.push_str("[REVIEW: changes requested] Your branch was not approved. Address the blocking Error findings, commit, then call submit_branch again.\n\n");
    } else {
        out.push_str("[REVIEW: approved with nits] Your branch was approved for merge, but the reviewer left some optional feedback.\n\n");
    }

    out.push_str("SUMMARY: ");
    out.push_str(summary);
    out.push_str("\n\n");

    if findings.is_empty() {
        out.push_str("No findings.\n");
    } else {
        // Group by file
        let mut by_file: BTreeMap<&str, Vec<&Finding>> = BTreeMap::new();
        for f in findings {
            by_file.entry(&f.file).or_default().push(f);
        }

        for (file, file_findings) in by_file {
            out.push_str("FILE: ");
            out.push_str(file);
            out.push('\n');

            let mut sorted_findings = file_findings;
            sorted_findings.sort_by_key(|f| f.line);

            for f in sorted_findings {
                let sev = format!("{:?}", f.severity).to_uppercase();
                out.push_str(&format!("  {:<7} ", sev));
                if let Some(line) = f.line {
                    out.push_str(&format!("L{:<4} ", line));
                } else {
                    out.push_str("      ");
                }
                out.push_str(&f.body);
                if let Some(sug) = &f.suggestion {
                    out.push_str("\n          Suggestion: ");
                    out.push_str(sug);
                }
                out.push('\n');
            }
            out.push('\n');
        }
    }

    if blocked {
        out.push_str("INSTRUCTION: Address every Error finding above, commit your changes, then call `submit_branch` again to request a new review.");
    } else {
        out.push_str("INSTRUCTION: These findings are non-blocking. You may address them if you wish, or proceed with merge if your parent allows.");
    }

    out
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
        persisted: Mutex<Vec<(std::path::PathBuf, Vec<u8>)>>,
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
            self.parent
                .lock()
                .unwrap()
                .push(msg.summary.as_str().to_string());
            Ok(())
        }
        async fn deliver_to_self(&self, _from: &str, summary: &str, _text: &str) -> CapResult<()> {
            self.to_self.lock().unwrap().push(summary.to_string());
            Ok(())
        }
        async fn read_file(&self, path: &std::path::Path) -> CapResult<Option<Vec<u8>>> {
            Ok(self
                .persisted
                .lock()
                .unwrap()
                .iter()
                .find(|(p, _)| p == path)
                .map(|(_, b)| b.clone()))
        }
        async fn write_file(&self, path: &std::path::Path, bytes: &[u8]) -> CapResult<()> {
            self.persisted
                .lock()
                .unwrap()
                .push((path.to_path_buf(), bytes.to_vec()));
            Ok(())
        }
    }

    fn mock(branch: &str, head: &str) -> MockCtx {
        MockCtx {
            branch: Branch::new(branch.into()).unwrap(),
            head: head.into(),
            parent: Mutex::new(vec![]),
            to_self: Mutex::new(vec![]),
            persisted: Mutex::new(vec![]),
        }
    }

    fn from() -> Persona {
        Persona::Agent(exo_caps::AgentName::new("reviewer-0".into()).unwrap())
    }

    #[tokio::test]
    async fn matching_reviewed_no_errors_escalates_ready_and_reclaims() {
        let ctx = mock("root.dev-0", "abc");
        let sys = ReviewSystem::Reviewed {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "abc".into(),
            summary: "looks good".into(),
            findings: vec![Finding {
                file: "src/lib.rs".into(),
                line: Some(10),
                severity: Severity::Hint,
                body: "nit".into(),
                suggestion: None,
            }],
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(ctx
            .parent
            .lock()
            .unwrap()
            .iter()
            .any(|s| s.contains("[READY]")));
    }

    #[tokio::test]
    async fn matching_reviewed_with_errors_blocks_and_reclaims() {
        let ctx = mock("root.dev-0", "abc");
        let sys = ReviewSystem::Reviewed {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "abc".into(),
            summary: "needs work".into(),
            findings: vec![Finding {
                file: "src/lib.rs".into(),
                line: Some(10),
                severity: Severity::Error,
                body: "bug".into(),
                suggestion: None,
            }],
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(ctx.parent.lock().unwrap().is_empty());
        assert!(ctx
            .to_self
            .lock()
            .unwrap()
            .iter()
            .any(|s| s.contains("[REVIEW]")));
    }

    #[tokio::test]
    async fn stale_sha_does_not_escalate_but_reclaims() {
        let ctx = mock("root.dev-0", "newsha");
        let sys = ReviewSystem::Reviewed {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "oldsha".into(),
            summary: "too late".into(),
            findings: vec![],
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(ctx.parent.lock().unwrap().is_empty());
    }

    #[tokio::test]
    async fn aborted_wakes_llm_and_reclaims() {
        let ctx = mock("root.dev-0", "abc");
        let sys = ReviewSystem::ReviewAborted {
            reason: "exited".into(),
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(!ctx.to_self.lock().unwrap().is_empty());
    }

    #[tokio::test]
    async fn branch_mismatch_does_not_escalate_but_reclaims() {
        let ctx = mock("root.dev-0", "abc");
        let sys = ReviewSystem::Reviewed {
            branch: Branch::new("root.other-9".into()).unwrap(),
            sha: "abc".into(),
            summary: "wrong branch".into(),
            findings: vec![],
        };
        let outcome = handle_review_system(&ctx, &from(), &sys).await.unwrap();
        assert_eq!(outcome, SystemOutcome::ReclaimSender);
        assert!(ctx.parent.lock().unwrap().is_empty());
    }

    #[test]
    fn severity_blocks_correctly() {
        assert!(Severity::Error.blocks());
        assert!(!Severity::Warning.blocks());
        assert!(!Severity::Info.blocks());
        assert!(!Severity::Hint.blocks());
    }

    #[tokio::test]
    async fn persistence_appends_rounds() {
        let ctx = mock("root.dev-0", "abc");
        let sys = ReviewSystem::Reviewed {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "abc".into(),
            summary: "round 1".into(),
            findings: vec![Finding {
                file: "src/lib.rs".into(),
                line: Some(10),
                severity: Severity::Error,
                body: "bug".into(),
                suggestion: None,
            }],
        };

        // First round
        handle_review_system(&ctx, &from(), &sys).await.unwrap();
        {
            let p = ctx.persisted.lock().unwrap();
            assert_eq!(p.len(), 1);
            let (_, bytes) = &p[0];
            let log: ReviewLog = serde_json::from_slice(bytes).unwrap();
            assert_eq!(log.rounds.len(), 1);
            assert_eq!(log.rounds[0].round, 1);
            assert_eq!(log.rounds[0].summary, "round 1");
            assert!(log.rounds[0].blocked);
        }

        // Second round (different sha/summary, approved)
        let ctx2 = mock("root.dev-0", "def");
        // Pre-fill ctx2 with the first round
        let first_round_bytes = ctx.persisted.lock().unwrap()[0].1.clone();
        let path = std::path::PathBuf::from(".exo/reviews/root.dev-0.json");
        ctx2.persisted
            .lock()
            .unwrap()
            .push((path.clone(), first_round_bytes));

        let sys2 = ReviewSystem::Reviewed {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "def".into(),
            summary: "round 2 looks good".into(),
            findings: vec![],
        };

        handle_review_system(&ctx2, &from(), &sys2).await.unwrap();
        {
            let p = ctx2.persisted.lock().unwrap();
            // Should have read-then-pushed a NEW version
            assert_eq!(p.len(), 2);
            let (_, bytes) = &p[1];
            let log: ReviewLog = serde_json::from_slice(bytes).unwrap();
            assert_eq!(log.rounds.len(), 2);
            assert_eq!(log.rounds[1].round, 2);
            assert_eq!(log.rounds[1].sha, "def");
            assert!(!log.rounds[1].blocked);
        }
    }

    #[test]
    fn safe_branch_sanitizes() {
        assert_eq!(safe_branch("root.dev-0"), "root.dev-0");
        assert_eq!(safe_branch("root/dev-0"), "root-dev-0");
        assert_eq!(safe_branch("root.feat/bug"), "root.feat-bug");
    }
}
