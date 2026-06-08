//! `verdict` — the reviewer's one output. A reviewer (spawned by a submitting node off the
//! under-review branch) reads the diff against the acceptance criteria, then calls this with
//! one of three decisions. It packages the decision into a [`ReviewSystem`] and delivers it to
//! its **parent** (the submitter — a real tree edge) over the erased domain wire
//! ([`deliver_domain`]); the submitter's *sidecar* acts on it (approve → auto-escalate `[READY]`;
//! deny/changes → wake the LLM). The reviewer then exits.

use crate::review::{Finding, ReviewSystem, Severity};
use exo_caps::{deliver_domain, Addressee, Branch, Bus, CapError, CapResult, Kv};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use exo_framework::{ok_json, parse, schema_json, Tool, ToolOutput};

/// Arguments for `verdict`.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct VerdictArgs {
    /// The branch you reviewed (as given in your review task).
    pub branch: String,
    /// The commit sha you reviewed (as given in your review task).
    pub sha: String,
    /// A high-level summary of your review.
    pub summary: String,
    /// A structured list of findings. Any Severity::Error will block the merge.
    #[serde(default)]
    pub findings: Vec<Finding>,
}

/// The `verdict` tool.
pub struct Verdict;

impl Verdict {
    pub async fn run<C: Bus + Kv>(ctx: &C, args: VerdictArgs) -> CapResult<ToolOutput> {
        if args.summary.trim().is_empty() {
            return Err(CapError::invalid(
                "verdict",
                "summary must not be empty",
            ));
        }

        for finding in &args.findings {
            if finding.severity == Severity::Error && finding.body.trim().is_empty() {
                return Err(CapError::invalid(
                    "verdict",
                    format!("Error-severity finding for file {} must have a non-empty body", finding.file),
                ));
            }
        }

        let branch = Branch::new(args.branch.clone())?;
        let system = ReviewSystem::Reviewed {
            branch,
            sha: args.sha.clone(),
            summary: args.summary.clone(),
            findings: args.findings.clone(),
        };

        let blocked = args.findings.iter().any(|f| f.severity.blocks());
        let outcome_label = if blocked { "CHANGES REQUESTED" } else { "APPROVED" };
        let verdict_summary = format!("[VERDICT] {} {}", outcome_label, args.branch);
        let text = crate::review::render_findings(&args.summary, &args.findings);

        deliver_domain(ctx, Addressee::Parent, &verdict_summary, &text, &system).await?;

        // Record that a verdict was produced this turn so the reviewer's stop hook stays silent
        // (the verdict is the done-signal). Best-effort — a kv failure at worst causes a spurious
        // re-submit, never a silent stall.
        let _ = ctx.set("verdict_produced", "true").await;

        Ok(ToolOutput::with_data(
            format!("verdict delivered to parent: {}", outcome_label),
            json!({ "branch": args.branch, "sha": args.sha, "blocked": blocked }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Bus + Kv + Send + Sync> Tool<R> for Verdict {
    fn name(&self) -> &str {
        "verdict"
    }
    fn description(&self) -> &str {
        "Submit your review verdict on the branch you were spawned to review, then end your turn. \
         Provide a high-level `summary` and a list of structured `findings`. Each finding has a \
         `file`, `line`, `severity` (error, warning, info, hint), `body`, and optional `suggestion`. \
         Any `error` severity finding will block the merge and require the submitter to address it. \
         The decision (approve vs request changes) is derived automatically from your findings."
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
    async fn approved_delivers_system_message_to_parent() {
        let mock = MockRuntime::default();
        Verdict::run(
            &mock,
            VerdictArgs {
                branch: "main.dev-0".into(),
                sha: "abc123".into(),
                summary: "LGTM".into(),
                findings: vec![Finding {
                    file: "src/lib.rs".into(),
                    line: Some(10),
                    severity: Severity::Hint,
                    body: "nit".into(),
                    suggestion: None,
                }],
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        assert!(calls.iter().any(|c| matches!(c, Call::BusDeliver { to, msg }
            if *to == Addressee::Parent
                && matches!(&msg.kind, exo_caps::MessageKind::Domain(p)
                    if matches!(serde_json::from_str::<ReviewSystem>(&p.0),
                        Ok(ReviewSystem::Reviewed { branch, sha, findings, .. })
                        if branch.as_str() == "main.dev-0" && sha == "abc123" && findings.len() == 1)))));
    }

    #[tokio::test]
    async fn blocked_delivers_system_message_to_parent() {
        let mock = MockRuntime::default();
        Verdict::run(
            &mock,
            VerdictArgs {
                branch: "main.dev-0".into(),
                sha: "abc123".into(),
                summary: "needs work".into(),
                findings: vec![Finding {
                    file: "src/lib.rs".into(),
                    line: Some(10),
                    severity: Severity::Error,
                    body: "bug".into(),
                    suggestion: None,
                }],
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        assert!(calls.iter().any(|c| matches!(c, Call::BusDeliver { to, msg }
            if *to == Addressee::Parent
                && matches!(&msg.kind, exo_caps::MessageKind::Domain(p)
                    if matches!(serde_json::from_str::<ReviewSystem>(&p.0),
                        Ok(ReviewSystem::Reviewed { branch, sha, findings, .. })
                        if branch.as_str() == "main.dev-0" && sha == "abc123" && findings.len() == 1)))));

        assert_eq!(
            mock.kv
                .lock()
                .unwrap()
                .get("verdict_produced")
                .map(|s| s.as_str()),
            Some("true")
        );
    }

    #[tokio::test]
    async fn requires_non_empty_summary() {
        let mock = MockRuntime::default();
        let res = Verdict::run(
            &mock,
            VerdictArgs {
                branch: "main.dev-0".into(),
                sha: "abc123".into(),
                summary: "  ".into(),
                findings: vec![],
            },
        )
        .await;
        assert!(res.is_err());
    }

    #[tokio::test]
    async fn error_finding_requires_body() {
        let mock = MockRuntime::default();
        let res = Verdict::run(
            &mock,
            VerdictArgs {
                branch: "main.dev-0".into(),
                sha: "abc123".into(),
                summary: "broken".into(),
                findings: vec![Finding {
                    file: "src/lib.rs".into(),
                    line: Some(10),
                    severity: Severity::Error,
                    body: "".into(),
                    suggestion: None,
                }],
            },
        )
        .await;
        assert!(res.is_err());
    }
}
