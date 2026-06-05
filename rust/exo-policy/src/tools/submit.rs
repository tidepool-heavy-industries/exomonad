//! `submit_branch` — the leaf's "done, ready for review / merge" signal: v2's local analogue of
//! filing a PR. The leaf commits its work, then calls this; it runs an **ordered list of
//! preconditions** and, on pass, delivers a structured `[READY]` message to the parent, which
//! then folds the branch with the `merge` tool. No PR, no remote — convergence is on-disk.
//!
//! The checks are a structured, extensible list (modeled like the role hook fn-pointers), so
//! adding a gate later — ahead-of-base, tests-pass, a reviewer verdict — is one entry, not a
//! rewrite. v1 has a single check: the worktree must be clean (work committed), because a parent
//! merges the BRANCH off disk and uncommitted changes would be invisible to that merge.

use exo_caps::{CapError, CapResult, Fs, GeminiSpec, Git, Process, Spawner};
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
            // A missing dir = no gate. Any OTHER error (permissions, IO) must NOT silently
            // disable the gate — fail the submit.
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(e) => return Err(format!("could not read .exo/checks/pre-merge: {e}")),
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
                // A check that doesn't honour the `{"pass":bool,"detail":...}` contract is
                // misconfigured/broken — fail closed rather than silently passing the gate.
                Err(_) => failures.push(format!(
                    "{path}: did not emit valid {{\"pass\":bool,\"detail\":...}} JSON (exit {})",
                    out.status
                )),
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
    pub async fn run<C: Git + Process + Spawner + Fs + Sync>(
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
        let sha = ctx.head_sha().await?;
        // The diff base is the parent branch (this branch minus its last dot-segment).
        let base = branch
            .as_str()
            .rsplit_once('.')
            .map(|(p, _)| p)
            .unwrap_or("main")
            .to_string();
        // The reviewer's bar: this node's spawn prompt + acceptance criteria, persisted at birth.
        let acceptance = match ctx.read(std::path::Path::new(".exo/acceptance.md")).await {
            Ok(bytes) => String::from_utf8_lossy(&bytes).to_string(),
            Err(_) => "(no acceptance criteria recorded for this branch)".to_string(),
        };

        // Spawn a reviewer in its own worktree off this branch. We do NOT deliver `[READY]` here —
        // the ONLY path that escalates is the sidecar reacting to an approve-verdict for this sha
        // (see exo-node `handle_system`). That makes the gate structural: the LLM has no tool that
        // can skip review.
        let review_task = format!(
            "You are a code reviewer. Review branch `{branch}` (commit {sha}). Run \
             `git diff {base}...HEAD` to see exactly what changed; you may build / test / experiment \
             freely in your own worktree (changes here never touch the reviewed code). Judge the \
             work against the ACCEPTANCE CRITERIA below and the project's conventions, then call the \
             `verdict` tool with branch=`{branch}`, sha=`{sha}`, and one of:\n\
             - approve  (it meets the bar)\n\
             - deny + message  (what must change)\n\
             - changes + message + changes_branch=<your own branch>  (you committed a concrete fix \
             to your own branch for the submitter to merge)\n\n\
             Note from the submitter: {note}\n\n\
             ACCEPTANCE CRITERIA\n{acceptance}",
            branch = branch.as_str(),
            note = args.note,
        );
        let spec = GeminiSpec {
            name: None,
            task: review_task,
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
        };
        let reviewer = ctx.spawn_reviewer(spec).await?;

        Ok(ToolOutput::with_data(
            format!(
                "Review requested for branch {branch}: reviewer `{reviewer}` spawned. STOP now — do \
                 nothing further and end your turn. You will be woken automatically: on approval \
                 your `[READY]` is escalated to your parent with no action from you; on deny / \
                 changes you'll receive the reviewer's feedback to address.",
                branch = branch.as_str(),
                reviewer = reviewer.as_str(),
            ),
            json!({ "branch": branch.as_str(), "sha": sha, "reviewer": reviewer.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Git + Process + Spawner + Fs + Send + Sync> Tool<R> for SubmitBranch {
    fn name(&self) -> &str {
        "submit_branch"
    }
    fn description(&self) -> &str {
        "Request review of your branch. Commit everything first (it refuses on uncommitted changes \
         or failing `.exo/checks/pre-merge` scripts), then it spawns a reviewer of your work and \
         returns. Do NOT expect to merge yourself: on approval the sidecar escalates `[READY]` to \
         your parent automatically; on deny / changes you'll be woken with feedback to address and \
         re-submit. After calling it, STOP and end your turn."
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
    async fn submits_spawns_reviewer_when_clean() {
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
        // It spawns a reviewer...
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::SpawnReviewer { .. })));
        // ...and NEVER delivers [READY] itself — only the sidecar does, on an approve-verdict.
        assert!(!calls.iter().any(|c| matches!(c, Call::BusDeliver { .. })));
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
