//! `submit_branch` — the leaf's "done, ready for review / merge" signal: v2's local analogue of
//! filing a PR. The leaf commits its work, then calls this; it runs an **ordered list of
//! preconditions** and, on pass, delivers a structured `[READY]` message to the parent, which
//! then folds the branch with the `merge` tool. No PR, no remote — convergence is on-disk.
//!
//! The checks are a structured, extensible list (modeled like the role hook fn-pointers), so
//! adding a gate later — ahead-of-base, tests-pass, a reviewer verdict — is one entry, not a
//! rewrite. v1 has a single check: the worktree must be clean (work committed), because a parent
//! merges the BRANCH off disk and uncommitted changes would be invisible to that merge.

use exo_caps::{
    Addressee, Bus, CapError, CapResult, Git, Message, MessageBody, MessageKind, Process, Summary,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::tool::{ok_json, parse, schema_json, BoxFuture, Tool, ToolOutput};

#[derive(serde::Deserialize)]
struct CheckResult {
    pass: bool,
    #[serde(default)]
    detail: String,
}

/// Arguments for `submit_branch`.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SubmitBranchArgs {
    /// What you did / what the parent should review before merging. One or two sentences.
    pub note: String,
}

/// One submit precondition. Ordered; the first failure blocks the submit with its reason. A
/// named async fn-pointer (like the `RoleDef` hook fn-pointers) so the gate stays a greppable,
/// extensible list rather than a hardcoded sequence of calls.
struct Check<C> {
    name: &'static str,
    run: for<'a> fn(&'a C) -> BoxFuture<'a, Result<(), String>>,
}

/// v1 gate: the worktree must be clean. A parent merges the branch off disk, so uncommitted
/// work would silently not be merged — refuse until it's committed.
fn committed<C: Git + Sync>(ctx: &C) -> BoxFuture<'_, Result<(), String>> {
    Box::pin(async move {
        match ctx.is_clean().await {
            Ok(true) => Ok(()),
            Ok(false) => Err(
                "you have uncommitted changes — commit your work first (your parent \
                              merges your branch off disk; uncommitted changes won't be merged)"
                    .into(),
            ),
            Err(e) => Err(format!("could not read git status: {e}")),
        }
    })
}

/// Run every script in `.exo/checks/pre-merge/*` (relative to the node's worktree). Each must
/// print a JSON line `{"pass": bool, "detail": "..."}`; any non-pass (or non-zero exit with no
/// JSON) blocks the submit. Absent dir / no scripts = pass (no gate).
fn pre_merge_checks<C: Process + Sync>(ctx: &C) -> BoxFuture<'_, Result<(), String>> {
    Box::pin(async move {
        let dir = std::path::Path::new(".exo/checks/pre-merge");
        let mut scripts: Vec<std::path::PathBuf> = match std::fs::read_dir(dir) {
            Ok(rd) => rd
                .filter_map(|e| e.ok().map(|e| e.path()))
                .filter(|p| p.is_file())
                .collect(),
            Err(_) => return Ok(()),
        };
        scripts.sort();
        let mut failures = Vec::new();
        for script in scripts {
            let path = script.to_string_lossy().to_string();
            let out = match ctx.run(&path, &[]).await {
                Ok(o) => o,
                Err(e) => {
                    failures.push(format!("{path}: failed to run: {e}"));
                    continue;
                }
            };
            let stdout = String::from_utf8_lossy(&out.stdout);
            match serde_json::from_str::<CheckResult>(stdout.trim()) {
                Ok(r) if r.pass => {}
                Ok(r) => failures.push(format!("{path}: {}", r.detail)),
                Err(_) => {
                    if !out.status.success() {
                        failures.push(format!("{path}: exited non-zero with no JSON output"));
                    }
                }
            }
        }
        if failures.is_empty() {
            Ok(())
        } else {
            Err(format!(
                "pre-merge checks failed:\n- {}",
                failures.join("\n- ")
            ))
        }
    })
}

/// The ordered precondition list. Append here to add a gate.
fn checks<C: Git + Process + Sync>() -> Vec<Check<C>> {
    vec![
        Check {
            name: "committed",
            run: committed::<C>,
        },
        Check {
            name: "pre_merge_checks",
            run: pre_merge_checks::<C>,
        },
    ]
}

/// The `submit_branch` tool.
pub struct SubmitBranch;

impl SubmitBranch {
    pub async fn run<C: Git + Bus + Process + Sync>(
        ctx: &C,
        args: SubmitBranchArgs,
    ) -> CapResult<ToolOutput> {
        // Run the ordered preconditions; first failure blocks (surfaced as a tool error so the
        // agent sees the reason and can fix it before retrying).
        for check in checks::<C>() {
            if let Err(reason) = (check.run)(ctx).await {
                return Err(CapError::invalid(
                    "submit_branch",
                    format!("{}: {}", check.name, reason),
                ));
            }
        }

        let branch = ctx.current_branch().await?;
        let text = format!(
            "[READY] branch `{}` is committed and ready for review / merge. {}",
            branch.as_str(),
            args.note
        );
        let msg = Message {
            text: MessageBody::new(text)?,
            summary: Summary::new(format!("[READY] {}", branch.as_str()))?,
            kind: MessageKind::Chat,
        };
        ctx.deliver(Addressee::Parent, msg).await?;

        Ok(ToolOutput::with_data(
            format!("submitted branch {} for review/merge", branch.as_str()),
            json!({ "branch": branch.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Git + Bus + Process + Send + Sync> Tool<R> for SubmitBranch {
    fn name(&self) -> &str {
        "submit_branch"
    }
    fn description(&self) -> &str {
        "Mark your branch DONE — committed and ready for review / merge. This is how you request \
         your parent merge your work: the local-merge analogue of filing a PR (there is NO \
         file_pr, no PR, no remote). Commit everything first — it refuses if your worktree has \
         uncommitted changes. After calling it, end your turn; your parent reviews and merges."
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(SubmitBranchArgs))
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
    async fn submits_when_clean() {
        let mock = MockRuntime::default(); // is_clean = true, branch = dev.policy-claude
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("dev.policy-claude"));
        let calls = mock.calls_made();
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::BusDeliver { to, msg }
            if *to == Addressee::Parent
                && msg.summary.as_str().contains("[READY]")
                && msg.text.as_str().contains("dev.policy-claude"))));
    }

    #[tokio::test]
    async fn blocks_when_dirty() {
        let mock = MockRuntime {
            is_clean: false,
            ..Default::default()
        };
        let res = SubmitBranch::run(&mock, SubmitBranchArgs { note: "x".into() }).await;
        assert!(res.is_err());
        // The gate blocks BEFORE any delivery.
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::BusDeliver { .. })));
    }
}
