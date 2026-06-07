//! `verdict` — the reviewer's one output. A reviewer (spawned by a submitting node off the
//! under-review branch) reads the diff against the acceptance criteria, then calls this with
//! one of three decisions. It packages the decision into a [`ReviewSystem`] and delivers it to
//! its **parent** (the submitter — a real tree edge) over the erased domain wire
//! ([`deliver_domain`]); the submitter's *sidecar* acts on it (approve → auto-escalate `[READY]`;
//! deny/changes → wake the LLM). The reviewer then exits.

use crate::review::ReviewSystem;
use exo_caps::{deliver_domain, Addressee, Branch, Bus, CapError, CapResult, Kv};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use exo_framework::{ok_json, parse, schema_json, Tool, ToolOutput};

/// The reviewer's decision.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Decision {
    /// The branch meets the bar — the submitter may escalate.
    Approve,
    /// The branch does not meet the bar — `message` explains what to fix.
    Deny,
    /// You committed improvements to your OWN branch — the submitter should merge `changes_branch`.
    Changes,
}

/// Arguments for `verdict`.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct VerdictArgs {
    /// approve / deny / changes.
    pub decision: Decision,
    /// The branch you reviewed (as given in your review task).
    pub branch: String,
    /// The commit sha you reviewed (as given in your review task).
    pub sha: String,
    /// Feedback for the submitter (required for deny / changes).
    #[serde(default)]
    pub message: String,
    /// For `changes`: your own branch carrying the committed counter-proposal.
    #[serde(default)]
    pub changes_branch: Option<String>,
}

/// The `verdict` tool.
pub struct Verdict;

impl Verdict {
    pub async fn run<C: Bus + Kv>(ctx: &C, args: VerdictArgs) -> CapResult<ToolOutput> {
        let branch = Branch::new(args.branch.clone())?;
        let system = match args.decision {
            Decision::Approve => ReviewSystem::ReviewApproved {
                branch,
                sha: args.sha.clone(),
            },
            Decision::Deny => {
                if args.message.trim().is_empty() {
                    return Err(CapError::invalid(
                        "verdict",
                        "decision=deny requires a non-empty message explaining what to fix",
                    ));
                }
                ReviewSystem::ReviewDenied {
                    branch,
                    sha: args.sha.clone(),
                    message: args.message.clone(),
                }
            }
            Decision::Changes => {
                if args.message.trim().is_empty() {
                    return Err(CapError::invalid(
                        "verdict",
                        "decision=changes requires a non-empty message describing the change",
                    ));
                }
                let changes_branch = match args.changes_branch.clone() {
                    Some(b) => Branch::new(b)?,
                    None => {
                        return Err(CapError::invalid(
                            "verdict",
                            "decision=changes requires changes_branch",
                        ))
                    }
                };
                ReviewSystem::ReviewChanges {
                    branch,
                    sha: args.sha.clone(),
                    changes_branch,
                    message: args.message.clone(),
                }
            }
        };

        // A short human-readable rendering rides text/summary (used in logs, and shown to the
        // submitter's LLM when the sidecar delivers a deny/changes); the typed payload is erased
        // into the domain wire by `deliver_domain`.
        let summary = format!("[VERDICT] {} {}", verb(&system), args.branch);
        let text = render(&system, &args);
        deliver_domain(ctx, Addressee::Parent, &summary, &text, &system).await?;

        // Record that a verdict was produced this turn so the reviewer's stop hook stays silent
        // (the verdict is the done-signal). Best-effort — a kv failure at worst causes a spurious
        // re-submit, never a silent stall.
        let _ = ctx.set("verdict_produced", "true").await;

        Ok(ToolOutput::with_data(
            "verdict delivered to parent".to_string(),
            json!({ "branch": args.branch, "sha": args.sha }),
        ))
    }
}

fn verb(s: &ReviewSystem) -> &'static str {
    match s {
        ReviewSystem::ReviewApproved { .. } => "approve",
        ReviewSystem::ReviewDenied { .. } => "deny",
        ReviewSystem::ReviewChanges { .. } => "changes",
        // The verdict tool only ever builds the three decisions above; ReviewAborted is emitted by
        // the reviewer's stop hook, never here.
        ReviewSystem::ReviewAborted { .. } => unreachable!("verdict never produces ReviewAborted"),
    }
}

fn render(s: &ReviewSystem, args: &VerdictArgs) -> String {
    match s {
        ReviewSystem::ReviewApproved { branch, sha } => {
            format!(
                "[VERDICT approve] branch `{}` @ {} approved.",
                branch.as_str(),
                sha
            )
        }
        ReviewSystem::ReviewDenied { branch, .. } => format!(
            "[VERDICT deny] branch `{}` rejected: {}",
            branch.as_str(),
            args.message
        ),
        ReviewSystem::ReviewChanges {
            branch,
            changes_branch,
            ..
        } => format!(
            "[VERDICT changes] branch `{}` — merge `{}` to incorporate: {}",
            branch.as_str(),
            changes_branch.as_str(),
            args.message
        ),
        ReviewSystem::ReviewAborted { .. } => {
            unreachable!("verdict never produces ReviewAborted")
        }
    }
}

#[async_trait::async_trait]
impl<R: Bus + Kv + Send + Sync> Tool<R> for Verdict {
    fn name(&self) -> &str {
        "verdict"
    }
    fn description(&self) -> &str {
        "Submit your review verdict on the branch you were spawned to review, then end your turn. \
         `approve` (meets the bar), `deny` + `message` (what to fix), or `changes` + `changes_branch` \
         + `message` (you committed a fix to your own branch for the submitter to merge). Pass the \
         `branch` and `sha` from your review task."
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(VerdictArgs))
    }
    async fn call(&self, ctx: &R, j: serde_json::Value) -> CapResult<serde_json::Value> {
        ok_json(Self::run(ctx, parse(j)?).await?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};

    #[tokio::test]
    async fn approve_delivers_system_message_to_parent() {
        let mock = MockRuntime::default();
        Verdict::run(
            &mock,
            VerdictArgs {
                decision: Decision::Approve,
                branch: "main.dev-0".into(),
                sha: "abc123".into(),
                message: String::new(),
                changes_branch: None,
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        assert!(calls.iter().any(|c| matches!(c, Call::BusDeliver { to, msg }
            if *to == Addressee::Parent
                && matches!(&msg.kind, exo_caps::MessageKind::Domain(p)
                    if matches!(serde_json::from_str::<ReviewSystem>(p.0.get()),
                        Ok(ReviewSystem::ReviewApproved { branch, sha })
                        if branch.as_str() == "main.dev-0" && sha == "abc123")))));
    }

    #[tokio::test]
    async fn changes_requires_changes_branch() {
        let mock = MockRuntime::default();
        let res = Verdict::run(
            &mock,
            VerdictArgs {
                decision: Decision::Changes,
                branch: "main.dev-0".into(),
                sha: "abc123".into(),
                message: "fixed the bug".into(),
                changes_branch: None,
            },
        )
        .await;
        assert!(res.is_err());
    }
}
