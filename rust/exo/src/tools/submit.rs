//! `submit_branch` — the leaf's "done, ready for review / merge" signal: v2's local analogue of
//! filing a PR. The leaf commits its work, then calls this; it runs an **ordered list of
//! preconditions** and, on pass, delivers a structured `[READY]` message to the parent, which
//! then folds the branch with the `merge` tool. No PR, no remote — convergence is on-disk.
//!
//! The checks are a structured, extensible list (modeled like the role hook fn-pointers), so
//! adding a gate later — ahead-of-base, tests-pass, a reviewer verdict — is one entry, not a
//! rewrite. v1 has a single check: the worktree must be clean (work committed), because a parent
//! merges the BRANCH off disk and uncommitted changes would be invisible to that merge.

use crate::branching::{child_name, parent_branch};
use crate::roles::ExoRole;
use crate::spawn::ExoSpawn;
use exo_caps::{
    Addressee, Bus, CapError, CapResult, ChildKind, Fs, Git, Kv, Message, MessageBody, MessageKind,
    NodePapers, Process, Spawner, Summary,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use exo_framework::{BoxFuture, Tool, ToolOutput};

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
    /// DANGER: skip the reviewer and forward `[READY]` straight to your parent (it will NOT be
    /// auto-merged — your parent still decides). Only set this when you're confident the change is
    /// trivial/safe; your parent is told the review was skipped and to be extra suspicious. Default
    /// false (review required).
    #[serde(default)]
    pub dangerously_skip_reviewer: bool,
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

/// Rebase gate: refuse to submit a branch that's behind its parent's current commit. In the
/// fold-up model a parent merges children *sequentially*, so a sibling submitting after an earlier
/// sibling was already merged is now behind the parent branch — and the parent's `git merge` of it
/// would conflict, forcing the parent to hand-resolve (violating "the TL never touches child
/// code"). Push the update to the child, which has the most context: prompt it to rebase onto the
/// parent's current commit and re-submit, so the parent's eventual merge is trivial. Fails open —
/// an unresolvable parent name (a direct child of root derives `root`, which is never a live git
/// branch) reads as "not behind" via `Git::is_behind`, never a block.
fn needs_rebase<C: Git + Sync>(ctx: &C) -> BoxFuture<'_, Result<(), String>> {
    Box::pin(async move {
        let branch = match ctx.current_branch().await {
            Ok(b) => b,
            // Can't read our own branch → skip the gate (fail-open, never wedge a submit).
            Err(e) => {
                tracing::warn!("rebase gate: current_branch failed, skipping: {e}");
                return Ok(());
            }
        };
        let parent = parent_branch(&branch);
        match ctx.is_behind(parent).await {
            Ok(false) => Ok(()),
            Ok(true) => Err(format!(
                "your branch `{branch}` is behind its parent `{parent}` (the parent advanced since \
                 you forked — most likely a sibling was merged into it). Rebase onto the parent's \
                 current commit before submitting, so the parent's merge of your work is clean and \
                 conflict-free: run `git rebase {parent}`, resolve any conflicts, commit, then call \
                 submit_branch again.",
                branch = branch.as_str(),
            )),
            // A git error resolving the parent → fail-open (same posture as is_behind's own).
            Err(e) => {
                tracing::warn!("rebase gate: is_behind({parent}) failed, skipping: {e}");
                Ok(())
            }
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
                // misconfigured/broken — fail closed rather than silently passing the gate. Include
                // the script's stderr so the misconfiguration is debuggable, not just "(exit N)".
                Err(_) => {
                    let stderr = String::from_utf8_lossy(&out.stderr);
                    let stderr = stderr.trim();
                    let tail = if stderr.is_empty() {
                        String::new()
                    } else {
                        format!("; stderr: {stderr}")
                    };
                    failures.push(format!(
                        "{path}: did not emit valid {{\"pass\":bool,\"detail\":...}} JSON (exit {}){tail}",
                        out.status
                    ));
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

/// Whether this node's tree has reviewers turned on — `.exo/node.json`'s `review_enabled`,
/// inherited down the tree from the root's config (see `exo-runtime`'s `own_launch_policy`, which
/// stamps the same field onto every child's papers at birth). Fails safe: any read/parse error
/// (missing file, corrupt JSON, pre-field papers) defaults to the same off-by-default posture as
/// the flag itself — reviewers are opt-in, never assumed.
async fn own_review_enabled<C: Fs + Sync>(ctx: &C) -> bool {
    match ctx.read(std::path::Path::new(".exo/node.json")).await {
        Ok(bytes) => serde_json::from_slice::<NodePapers>(&bytes)
            .map(|p| p.review_enabled)
            .unwrap_or(NodePapers::DEFAULT_REVIEW_ENABLED),
        Err(_) => NodePapers::DEFAULT_REVIEW_ENABLED,
    }
}

/// The ordered precondition list. Append here to add a gate. Order matters: `committed` first (a
/// clean tree is a precondition for a sane rebase check), then `needs_rebase` (don't waste a review
/// / forward a stale `[READY]` on a branch that must be rebased first), then the project's scripts.
fn checks<C: Git + Process + Sync>() -> Vec<Check<C>> {
    vec![
        Check {
            name: "committed",
            run: committed::<C>,
        },
        Check {
            name: "needs_rebase",
            run: needs_rebase::<C>,
        },
        Check {
            name: "pre_merge_checks",
            run: pre_merge_checks::<C>,
        },
    ]
}

/// The `submit_branch` tool.
pub struct SubmitBranch;

#[async_trait::async_trait]
impl<R: Git + Process + Spawner + Fs + Bus + Kv + Send + Sync> Tool<R> for SubmitBranch {
    const NAME: &'static str = "submit_branch";
    const DESCRIPTION: &'static str =
        "Request review of your branch (if reviewers are enabled for this project — see \
         `review_enabled` in `.exo/config.toml`; off by default). Commit everything first: it \
         refuses on uncommitted changes, on a branch that's behind its parent (it'll tell you to \
         `git rebase` onto the parent first, so the parent's merge stays clean), or on failing \
         `.exo/checks/pre-merge` scripts. When reviewers are enabled, it spawns a reviewer of your \
         work and returns — do NOT expect to merge yourself: on approval the sidecar escalates \
         `[READY]` to your parent automatically; on deny / changes you'll be woken with feedback to \
         address and re-submit. Set `dangerously_skip_reviewer: true` to force-skip review even when \
         reviewers are enabled (only for a trivial/safe change) — forwards `[READY]` straight to \
         your parent, flagged as unreviewed. After calling it, STOP and end your turn.";
    type Args = SubmitBranchArgs;

    async fn run(ctx: &R, args: SubmitBranchArgs) -> CapResult<ToolOutput> {
        // Run the ordered preconditions; first failure blocks (surfaced as a tool error so the
        // agent sees the reason and can fix it before retrying). These run regardless of whether
        // review is skipped — committed + pre-merge checks are non-negotiable.
        for check in checks::<R>() {
            if let Err(reason) = (check.run)(ctx).await {
                return Err(CapError::invalid(
                    "submit_branch",
                    format!("{}: {}", check.name, reason),
                ));
            }
        }

        let branch = ctx.current_branch().await?;
        let sha = ctx.head_sha().await?;

        // Two ways to end up here: the agent explicitly opted out (loud, "dangerous"), or this
        // project simply doesn't have reviewers turned on (quiet, the normal off-by-default case —
        // reviewers aren't a fully-cooked feature yet; see `rust/exo/CLAUDE.md`). Either way the gate
        // stays structural: the LLM has no tool that can fabricate a `[READY]` other than through
        // this one path, and a config-disabled project is told so plainly, not scared into
        // suspicion for something the project chose.
        let reviewer_configured = own_review_enabled(ctx).await;
        if args.dangerously_skip_reviewer || !reviewer_configured {
            let text = if args.dangerously_skip_reviewer {
                format!(
                    "[READY — REVIEWER SKIPPED] branch `{}` @ {} is committed and ready for you to \
                     merge. The submitting node chose to skip review (dangerously_skip_reviewer): NO \
                     reviewer vetted this. Inspect the diff yourself before merging — be more suspicious \
                     than usual. Note from the submitter: {}",
                    branch.as_str(),
                    sha,
                    args.note
                )
            } else {
                format!(
                    "[READY] branch `{}` @ {} is committed and ready for you to merge. Reviewers are \
                     disabled for this project (set `review_enabled = true` in `.exo/config.toml` to \
                     turn them on) — this was not reviewed. Note from the submitter: {}",
                    branch.as_str(),
                    sha,
                    args.note
                )
            };
            let summary = if args.dangerously_skip_reviewer {
                format!("[READY skipped] {}", branch.as_str())
            } else {
                format!("[READY] {}", branch.as_str())
            };
            let msg = Message {
                text: MessageBody::new(text)?,
                summary: Summary::new(summary)?,
                kind: MessageKind::Chat,
            };
            ctx.deliver(Addressee::Parent, msg).await?;
            return Ok(ToolOutput::with_data(
                format!(
                    "Forwarded [READY] to your parent for branch {} WITHOUT review. Your parent \
                     will decide whether to merge. STOP now and end your turn.",
                    branch.as_str()
                ),
                json!({ "branch": branch.as_str(), "sha": sha, "reviewer_skipped": true }),
            ));
        }

        // Resolve a REAL diff base for the reviewer: the fork point off the parent branch. The
        // derived parent *name* may not be a live git ref (a direct child of root derives "root",
        // which the human session never checks out), so resolve to a merge-base SHA, trying the
        // derived parent then the repo's default branch. A SHA always resolves in `git diff`.
        let base_sha = match ctx.fork_point().await? {
            Some(fp) => Some(fp),
            None => {
                let derived_parent = parent_branch(&branch);
                let mut b = None;
                for candidate in [derived_parent, "main", "master"] {
                    if let Some(found) = ctx.merge_base(candidate).await? {
                        b = Some(found);
                        break;
                    }
                }
                b
            }
        };
        let diff_instruction = match &base_sha {
            Some(b) => format!("Run `git diff {b}...HEAD` to see exactly what changed"),
            None => {
                "Inspect the change with `git log` / `git show` (no diff base could be resolved)"
                    .to_string()
            }
        };

        // The reviewer's bar: this node's spawn prompt + acceptance criteria, persisted at birth.
        let acceptance = match ctx.read(std::path::Path::new(".exo/acceptance.md")).await {
            Ok(bytes) => String::from_utf8_lossy(&bytes).to_string(),
            Err(_) => "(no acceptance criteria recorded for this branch)".to_string(),
        };

        // BEST-EFFORT: Read prior review rounds for continuity.
        let safe = crate::review::safe_branch(branch.as_str());
        let review_log_path = std::path::PathBuf::from(format!(".exo/reviews/{safe}.json"));
        let mut prior_round_context = String::new();
        if let Ok(bytes) = ctx.read(&review_log_path).await {
            if let Ok(log) = serde_json::from_slice::<crate::review::ReviewLog>(&bytes) {
                if let Some(last) = log.rounds.last() {
                    let unresolved: Vec<_> = last
                        .findings
                        .iter()
                        .filter(|f| f.severity.blocks())
                        .collect();
                    if !unresolved.is_empty() {
                        prior_round_context = format!(
                            "\n\nPRIOR ROUND — verify these were addressed; do not re-raise resolved items\nSummary: {}\nFindings:\n",
                            last.summary
                        );
                        for f in unresolved {
                            let line = f
                                .line
                                .map(|l| format!("L{l}"))
                                .unwrap_or_else(|| "     ".to_string());
                            prior_round_context
                                .push_str(&format!("- {} {}: {}\n", f.file, line, f.body));
                        }
                    }
                }
            }
        }

        // Spawn a reviewer in its own worktree off this branch. We do NOT deliver `[READY]` here —
        // the ONLY path that escalates is the sidecar reacting to an approve-verdict for this sha
        // (see exo-node `handle_system`). That makes the gate structural: the LLM has no tool that
        // can skip review (the explicit opt-out above is the one exception, and it's loud).
        let review_task = format!(
            "You are a code reviewer. Review branch `{branch}` (commit {sha}). {diff_instruction}. \
             READ ONLY: judge the diff by reading it, not by running it — do NOT run the build, the \
             test suite, or any other long-running command. You have a wall-clock abandonment \
             timeout (30 minutes); a cold build/test run routinely exceeds that on its own and wastes \
             the whole review round for nothing but a compile check. If you must sanity-check a small, \
             fast, specific thing (e.g. one unit test), keep it to seconds, not a full suite. Judge the \
             work against the ACCEPTANCE CRITERIA below and the project's conventions, then call the \
             `verdict` tool with branch=`{branch}`, sha=`{sha}`, a high-level `summary`, and a list of \
             structured `findings` (file, line, severity, body, suggestion). \
             Use the following severity rubric:\n\
             - error: correctness, security, or missed spec. This BLOCKS the merge.\n\
             - warning / info / hint: non-blocking nits or suggestions.\n\
             Intent labels in code or commits (\"throwaway\", \"WIP\", \"probe\", \"experimental\") \
             do NOT lower the bar — review every diff as production code.\n\n\
             Note from the submitter: {note}\n\n\
             ACCEPTANCE CRITERIA\n{acceptance}{prior}",
            branch = branch.as_str(),
            note = args.note,
            prior = prior_round_context,
        );
        // Spawn a reviewer in its own worktree off the under-review branch (role fixed here, the
        // domain tool boundary). It reads the diff + acceptance criteria, emits a `verdict`, exits.
        // Name it after the branch under review — its last `.`-segment is the submitter's name — so a
        // tree of reviewers is legible (`oauth-dev-rev-0`, not a wall of `reviewer-0`). Auto-increment
        // tags the re-review rounds (`-0`, `-1`).
        let review_prefix = format!("{}-rev", child_name(&branch));
        let spec = ExoSpawn {
            role: ExoRole::Reviewer,
            kind: ChildKind::Worktree,
            name: None,
            name_prefix: review_prefix,
            task: review_task,
            fork_session: false,
        };
        let reviewer = ctx.spawn(spec).await?;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_framework::Tool;

    /// Minimal valid `NodePapers` JSON for seeding `mock.files` at `.exo/node.json` — the shape
    /// `own_review_enabled` reads. Only `review_enabled` varies across tests; the rest is filler.
    fn papers_json(review_enabled: bool) -> Vec<u8> {
        format!(
            r#"{{"path":["root","dev-node"],"branch":"dev.policy-claude","role":"dev","pane":"%1","parent_inbox":null,"review_enabled":{review_enabled}}}"#
        )
        .into_bytes()
    }

    /// A mock with reviewers turned on (`review_enabled: true` in its own papers) — the shared
    /// setup for every test exercising the normal reviewer-spawn path.
    fn mock_with_reviews_enabled() -> MockRuntime {
        let mock = MockRuntime::default(); // is_clean = true, branch = dev.policy-claude
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json(true));
        mock
    }

    #[tokio::test]
    async fn submits_spawns_reviewer_when_clean() {
        let mock = mock_with_reviews_enabled();
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("dev.policy-claude"));
        let calls = mock.calls_made();
        // It spawns a reviewer...
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::Spawn { role, .. } if role == "reviewer")));
        // ...and NEVER delivers [READY] itself — only the sidecar does, on an approve-verdict.
        assert!(!calls.iter().any(|c| matches!(c, Call::BusDeliver { .. })));
    }

    #[tokio::test]
    async fn skip_reviewer_forwards_ready_without_spawning() {
        let mock = MockRuntime::default();
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "trivial typo fix".into(),
                dangerously_skip_reviewer: true,
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("WITHOUT review"));
        let calls = mock.calls_made();
        // No reviewer is spawned...
        assert!(!calls.iter().any(|c| matches!(c, Call::Spawn { .. })));
        // ...and it forwards a SKIPPED [READY] to the parent.
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::BusDeliver { to, msg }
            if *to == exo_caps::Addressee::Parent
                && msg.summary.as_str().contains("[READY skipped]"))));
    }

    #[tokio::test]
    async fn reviews_disabled_by_default_forwards_ready_without_spawning() {
        // No `.exo/node.json` seeded — the real-world "config never set" case. Reviewers must be
        // off by default (not a fully-cooked feature), so this must behave like
        // `dangerously_skip_reviewer`, minus the "dangerous" framing.
        let mock = MockRuntime::default();
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("WITHOUT review"));
        let calls = mock.calls_made();
        assert!(!calls.iter().any(|c| matches!(c, Call::Spawn { .. })));
        let delivered = calls.iter().find_map(|c| match c {
            Call::BusDeliver {
                to: exo_caps::Addressee::Parent,
                msg,
            } => Some(msg),
            _ => None,
        });
        let msg = delivered.expect("should forward [READY] to parent");
        // Plain wording — no "dangerously skipped" / "be more suspicious" scare language, since
        // this wasn't the agent's choice.
        assert!(msg.text.as_str().contains("Reviewers are disabled for this project"));
        assert!(!msg.text.as_str().contains("dangerously_skip_reviewer"));
    }

    #[tokio::test]
    async fn reviews_explicitly_disabled_in_papers_forwards_ready_without_spawning() {
        let mock = MockRuntime::default();
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json(false));
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("WITHOUT review"));
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn spawns_reviewer_with_diff_instruction() {
        let mock = mock_with_reviews_enabled();
        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        let spawn = calls
            .iter()
            .find_map(|c| {
                if let Call::Spawn { role, task, .. } = c {
                    if role == "reviewer" {
                        return Some(task);
                    }
                }
                None
            })
            .expect("reviewer should be spawned");

        assert!(spawn.contains("git diff basebasebasebasebasebasebasebasebasebase...HEAD"));
        assert!(spawn.contains("dev.policy-claude"));
        // T1.3: intent-cue anchoring — labels like "WIP"/"throwaway" must not lower the bar.
        assert!(spawn.contains("do NOT lower the bar"));
    }

    #[tokio::test]
    async fn spawns_reviewer_preferring_fork_point() {
        let mock = MockRuntime {
            fork_point: Some("forkforkforkforkforkforkforkforkforkfork".into()),
            ..Default::default()
        };
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json(true));
        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        let spawn = calls
            .iter()
            .find_map(|c| {
                if let Call::Spawn { role, task, .. } = c {
                    if role == "reviewer" {
                        return Some(task);
                    }
                }
                None
            })
            .expect("reviewer should be spawned");

        assert!(spawn.contains("git diff forkforkforkforkforkforkforkforkforkfork...HEAD"));
    }

    #[tokio::test]
    async fn spawns_reviewer_with_fallback_when_no_base() {
        let mock = MockRuntime {
            merge_base: None,
            ..Default::default()
        };
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json(true));
        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        let spawn = calls
            .iter()
            .find_map(|c| {
                if let Call::Spawn { role, task, .. } = c {
                    if role == "reviewer" {
                        return Some(task);
                    }
                }
                None
            })
            .expect("reviewer should be spawned");

        assert!(spawn.contains("no diff base could be resolved"));
    }

    #[tokio::test]
    async fn blocks_when_dirty() {
        let mock = MockRuntime {
            is_clean: false,
            ..Default::default()
        };
        let res = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "x".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await;
        assert!(res.is_err());
        // The gate blocks BEFORE any delivery.
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::BusDeliver { .. })));
    }

    #[tokio::test]
    async fn blocks_and_prompts_rebase_when_behind_parent() {
        // Reviewers ON so we'd normally spawn — but the rebase gate must fire first, before either
        // the review-spawn OR the skip-forward path.
        let mut mock = mock_with_reviews_enabled();
        mock.is_behind = true;
        let res = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await;
        let err = res.expect_err("submit must block when the branch is behind its parent");
        let msg = err.to_string();
        // The prompt names the gate, the parent branch (`dev`, from `dev.policy-claude`), and the fix.
        assert!(msg.contains("needs_rebase"), "err should name the gate: {msg}");
        assert!(msg.contains("git rebase dev"), "err should prompt the rebase: {msg}");
        // Neither a reviewer nor a [READY] delivery happens — the gate is before both.
        let calls = mock.calls_made();
        assert!(!calls.iter().any(|c| matches!(c, Call::Spawn { .. })));
        assert!(!calls.iter().any(|c| matches!(c, Call::BusDeliver { .. })));
    }

    #[tokio::test]
    async fn behind_parent_blocks_even_with_reviewer_skipped() {
        // The rebase gate is a precondition — it runs even when review is being skipped, since a
        // stale branch's merge conflicts regardless of whether it was reviewed.
        let mock = MockRuntime {
            is_behind: true,
            ..Default::default()
        };
        let res = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "trivial".into(),
                dangerously_skip_reviewer: true,
            },
        )
        .await;
        assert!(res.is_err());
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::BusDeliver { .. })));
    }

    #[tokio::test]
    async fn submit_folds_prior_round_findings() {
        let mock = mock_with_reviews_enabled();
        let safe = crate::review::safe_branch("dev.policy-claude");
        let path = format!(".exo/reviews/{safe}.json");

        let log = crate::review::ReviewLog {
            branch: "dev.policy-claude".into(),
            rounds: vec![crate::review::ReviewRound {
                round: 1,
                sha: "oldsha".into(),
                summary: "failed first round".into(),
                findings: vec![crate::review::Finding {
                    file: "broken.rs".into(),
                    line: Some(5),
                    severity: crate::review::Severity::Error,
                    body: "fix me".into(),
                    suggestion: None,
                }],
                blocked: true,
            }],
        };
        mock.files
            .lock()
            .unwrap()
            .insert(path, serde_json::to_vec(&log).unwrap());

        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "retrying".into(),
                dangerously_skip_reviewer: false,
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        let spawn = calls
            .iter()
            .find_map(|c| {
                if let Call::Spawn { role, task, .. } = c {
                    if role == "reviewer" {
                        return Some(task);
                    }
                }
                None
            })
            .expect("reviewer should be spawned");

        assert!(spawn.contains("PRIOR ROUND"));
        assert!(spawn.contains("failed first round"));
        assert!(spawn.contains("broken.rs L5: fix me"));
    }
}
