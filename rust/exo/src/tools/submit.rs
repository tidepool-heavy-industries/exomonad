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
use crate::receipts::{self, render_receipts_summary, Receipts, TransferProof};
use crate::roles::ExoRole;
use crate::spawn::ExoSpawn;
use exo_caps::{
    Addressee, Bus, CapError, CapResult, ChildKind, Fs, Git, Kv, Lifecycle, Message, MessageBody,
    MessageKind, NodePapers, Process, Spawner, Summary,
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
    /// Structured receipts for your parent — what you actually did, checkable rather than taken on
    /// faith. `commit_tested` is the sha you LAST RAN YOUR VERIFICATION AT (e.g. the commit your
    /// `cargo test` was green on, not the commit you're submitting) — it is checked against HEAD to
    /// surface any gap between what you tested and what you're handing up (a rebase, a follow-up
    /// fix, "one more small thing"). Every string field has a size cap
    /// (`receipts::MAX_FIELD_BYTES`); an oversized field is rejected loudly, never silently
    /// trimmed. Entirely optional — omit for a plain, unreceipted submit.
    #[serde(default)]
    pub receipts: Option<Receipts>,
}

// Hand-written, not `#[derive(Default)]`: a derive would silently pick a wrong default for a
// field added later without thought (e.g. an `Option` that should start `Some`).
#[allow(clippy::derivable_impls)]
impl Default for SubmitBranchArgs {
    fn default() -> Self {
        Self {
            note: String::new(),
            dangerously_skip_reviewer: false,
            receipts: None,
        }
    }
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
/// parent's current commit and re-submit, so the parent's eventual merge is trivial.
///
/// The parent branch comes from our OWN papers (`.exo/node.json`'s `parent_branch`, stamped at
/// birth by the spawner from ITS OWN current branch) — NOT the dot-derived tree-address
/// coordinate (a direct child of root derives the literal `root`, which is root's exo IDENTITY,
/// not the branch the human's root session is actually on; `Git::is_behind("root")` then fails
/// open unconditionally, so the gate never fired for root's direct children — the most common
/// case). Fails open on `None`/unreadable/corrupt papers or an `is_behind` git error — same fail-
/// open posture as before, now reserved for the cases that are actually unresolvable (the root
/// itself, or a genuinely missing/broken papers file), not the common case.
fn needs_rebase<C: Git + Fs + Sync>(ctx: &C) -> BoxFuture<'_, Result<(), String>> {
    Box::pin(async move {
        let branch = match ctx.current_branch().await {
            Ok(b) => b,
            // Can't read our own branch → skip the gate (fail-open, never wedge a submit).
            Err(e) => {
                tracing::warn!("rebase gate: current_branch failed, skipping: {e}");
                return Ok(());
            }
        };
        let parent = match own_parent_branch(ctx).await {
            Some(p) => p,
            None => return Ok(()),
        };
        match ctx.is_behind(parent.as_str()).await {
            Ok(false) => Ok(()),
            Ok(true) => Err(format!(
                "your branch `{branch}` is behind its parent `{parent}` (the parent advanced since \
                 you forked — most likely a sibling was merged into it). Rebase onto the parent's \
                 current commit before submitting, so the parent's merge of your work is clean and \
                 conflict-free: run `git rebase {parent}`, resolve any conflicts, commit, then call \
                 submit_branch again.",
                branch = branch.as_str(),
                parent = parent.as_str(),
            )),
            // A git error resolving the parent → fail-open (same posture as is_behind's own).
            Err(e) => {
                tracing::warn!(
                    "rebase gate: is_behind({}) failed, skipping: {e}",
                    parent.as_str()
                );
                Ok(())
            }
        }
    })
}

/// This node's real parent git branch — `.exo/node.json`'s `parent_branch`, birth-stamped by the
/// spawner from ITS OWN current branch. `None` covers the root (no parent), an unreadable papers
/// file (not yet written / IO error), a corrupt one, and older papers predating this field — all
/// fail the rebase gate open, warning loudly only for the corrupt/unreadable cases (the other two
/// are legitimate, expected states, not surprises worth a warn).
async fn own_parent_branch<C: Fs + Sync>(ctx: &C) -> Option<exo_caps::Branch> {
    match ctx.read(std::path::Path::new(".exo/node.json")).await {
        Ok(bytes) => match serde_json::from_slice::<NodePapers>(&bytes) {
            Ok(papers) => papers.parent_branch,
            Err(e) => {
                tracing::warn!(
                    "rebase gate: .exo/node.json failed to parse ({e}); skipping (fail-open)"
                );
                None
            }
        },
        Err(exo_caps::FsError::At { source, .. } | exo_caps::FsError::Io(source))
            if source.kind() == std::io::ErrorKind::NotFound =>
        {
            None
        }
        Err(e) => {
            tracing::warn!(
                "rebase gate: could not read .exo/node.json ({e}); skipping (fail-open)"
            );
            None
        }
    }
}

/// Run every script in `.exo/checks/pre-merge/*` (relative to the node's worktree). Each must
/// print a JSON line `{"pass": bool, "detail": "..."}`; any non-pass (or non-zero exit with no
/// JSON) blocks the submit. Absent dir / no scripts = pass (no gate).
fn pre_merge_checks<C: Process + Fs + Sync>(ctx: &C) -> BoxFuture<'_, Result<(), String>> {
    Box::pin(async move {
        let dir = std::path::Path::new(".exo/checks/pre-merge");
        let mut scripts: Vec<std::path::PathBuf> = match ctx.read_dir(dir).await {
            Ok(entries) => entries.into_iter().filter(|p| p.is_file()).collect(),
            // A missing dir = no gate. Any OTHER error (permissions, IO) must NOT silently
            // disable the gate — fail the submit.
            Err(exo_caps::FsError::At { source, .. } | exo_caps::FsError::Io(source))
                if source.kind() == std::io::ErrorKind::NotFound =>
            {
                return Ok(())
            }
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
/// stamps the same field onto every child's papers at birth). Fails safe (defaults to the
/// off-by-default posture), but distinguishes WHY: a missing file is the legitimate "papers not
/// written yet" case and stays silent; corrupt/unparseable papers is unexpected and warns loudly,
/// so a corrupted `.exo/node.json` is never mistaken for "reviewers just aren't configured".
async fn own_review_enabled<C: Fs + Sync>(ctx: &C) -> bool {
    match ctx.read(std::path::Path::new(".exo/node.json")).await {
        Ok(bytes) => match serde_json::from_slice::<NodePapers>(&bytes) {
            Ok(papers) => papers.review_enabled,
            Err(e) => {
                tracing::warn!(
                    "own_review_enabled: .exo/node.json failed to parse ({e}); defaulting to \
                     review_enabled={}",
                    NodePapers::DEFAULT_REVIEW_ENABLED
                );
                NodePapers::DEFAULT_REVIEW_ENABLED
            }
        },
        Err(exo_caps::FsError::At { source, .. } | exo_caps::FsError::Io(source))
            if source.kind() == std::io::ErrorKind::NotFound =>
        {
            NodePapers::DEFAULT_REVIEW_ENABLED
        }
        Err(e) => {
            tracing::warn!(
                "own_review_enabled: could not read .exo/node.json ({e}); defaulting to \
                 review_enabled={}",
                NodePapers::DEFAULT_REVIEW_ENABLED
            );
            NodePapers::DEFAULT_REVIEW_ENABLED
        }
    }
}

/// The ordered precondition list. Append here to add a gate. Order matters: `committed` first (a
/// clean tree is a precondition for a sane rebase check), then `needs_rebase` (don't waste a review
/// / forward a stale `[READY]` on a branch that must be rebased first), then the project's scripts.
fn checks<C: Git + Process + Fs + Sync>() -> Vec<Check<C>> {
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

/// Reject any receipts field over [`receipts::MAX_FIELD_BYTES`], naming the offending field so the
/// agent knows what to trim. Runs before any precondition check or delivery — an oversized receipt
/// is rejected even on the skip path, where it wouldn't otherwise be rendered.
fn validate_receipts_size(r: &Receipts) -> CapResult<()> {
    let check = |field: String, s: &str| -> CapResult<()> {
        if s.len() > receipts::MAX_FIELD_BYTES {
            Err(CapError::invalid(
                "submit_branch",
                format!(
                    "receipts.{field} is {} bytes, exceeds the {}-byte cap — trim it",
                    s.len(),
                    receipts::MAX_FIELD_BYTES
                ),
            ))
        } else {
            Ok(())
        }
    };
    if let Some(t) = &r.commit_tested {
        check("commit_tested".to_string(), t)?;
    }
    for (i, c) in r.verify_commands_run.iter().enumerate() {
        check(format!("verify_commands_run[{i}]"), c)?;
    }
    for (i, m) in r.metrics.iter().enumerate() {
        check(format!("metrics[{i}].label"), &m.label)?;
        check(format!("metrics[{i}].value"), &m.value)?;
    }
    for (i, d) in r.deviations.iter().enumerate() {
        check(format!("deviations[{i}]"), d)?;
    }
    Ok(())
}

/// The real `git diff` base for this branch: the fork point off its ancestry, falling back to a
/// merge-base against the derived parent name, then `main`/`master`. A SHA always resolves in
/// `git diff`, unlike a branch *name* that may not be a live ref (e.g. a direct child of root
/// derives `root`, which the human session never checks out).
async fn resolve_diff_base<C: Git + Sync>(
    ctx: &C,
    branch: &exo_caps::Branch,
) -> CapResult<Option<String>> {
    Ok(match ctx.fork_point().await? {
        Some(fp) => Some(fp),
        None => {
            let derived_parent = parent_branch(branch);
            let mut b = None;
            for candidate in [derived_parent, "main", "master"] {
                if let Some(found) = ctx.merge_base(candidate).await? {
                    b = Some(found);
                    break;
                }
            }
            b
        }
    })
}

/// Build the [`TransferProof`] between the sha the submitter last tested at and the sha it is
/// actually submitting. See [`receipts`] for why this is the load-bearing part of a receipt.
async fn transfer_proof<C: Git + Sync>(
    ctx: &C,
    tested: &str,
    head: &str,
    branch: &exo_caps::Branch,
) -> TransferProof {
    let tested_lower = tested.to_lowercase();
    let head_lower = head.to_lowercase();
    let (shorter, longer) = if tested_lower.len() <= head_lower.len() {
        (&tested_lower, &head_lower)
    } else {
        (&head_lower, &tested_lower)
    };
    if shorter.len() >= 7 && longer.starts_with(shorter.as_str()) {
        return TransferProof::AtHead {
            sha: head.to_string(),
        };
    }

    let commits = match ctx.commits_between(tested, "HEAD").await {
        Ok(c) => c,
        Err(e) => {
            return TransferProof::Unverifiable {
                tested: tested.to_string(),
                head: head.to_string(),
                reason: e.to_string(),
            }
        }
    };

    let overlap = match resolve_diff_base(ctx, branch).await {
        Ok(Some(base)) => match ctx.commits_between(&base, "HEAD").await {
            Ok(diff_commits) => {
                let diff_files: std::collections::BTreeSet<String> = diff_commits
                    .iter()
                    .flat_map(|c| c.files.iter().cloned())
                    .collect();
                let moved_files: std::collections::BTreeSet<String> = commits
                    .iter()
                    .flat_map(|c| c.files.iter().cloned())
                    .collect();
                Some(
                    moved_files
                        .intersection(&diff_files)
                        .cloned()
                        .collect::<Vec<_>>(),
                )
            }
            Err(_) => None,
        },
        _ => None,
    };

    TransferProof::Moved {
        tested: tested.to_string(),
        head: head.to_string(),
        commits,
        overlap,
    }
}

/// A small serde_json rendering of a [`TransferProof`] for `ToolOutput::data`. Built inline
/// (rather than a `Serialize` derive on `TransferProof`) because it holds [`exo_caps::CommitFiles`],
/// which is not `Serialize`.
fn transfer_proof_json(proof: Option<&TransferProof>) -> serde_json::Value {
    match proof {
        None => serde_json::Value::Null,
        Some(TransferProof::AtHead { sha }) => json!({ "kind": "at_head", "sha": sha }),
        Some(TransferProof::Moved {
            tested,
            head,
            commits,
            overlap,
        }) => json!({
            "kind": "moved",
            "tested": tested,
            "head": head,
            "commits": commits
                .iter()
                .map(|c| json!({ "sha": c.sha, "files": c.files }))
                .collect::<Vec<_>>(),
            "overlap": overlap,
        }),
        Some(TransferProof::Unverifiable {
            tested,
            head,
            reason,
        }) => json!({ "kind": "unverifiable", "tested": tested, "head": head, "reason": reason }),
    }
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
         your parent, flagged as unreviewed. Pass `receipts` so your parent can CHECK your claim \
         instead of taking it on faith: the commands you actually ran, any counts worth handing up, \
         and anywhere you knowingly departed from your spec. Set `receipts.commit_tested` to the sha \
         you last verified at — it is checked against HEAD, and any commits you added since are \
         named to your parent along with whether they touch the diff it's about to merge. \
         After calling it, STOP and end your turn.";
    type Args = SubmitBranchArgs;

    async fn run(ctx: &R, args: SubmitBranchArgs) -> CapResult<ToolOutput> {
        // Oversized receipt fields are rejected loudly, before any precondition check or
        // delivery — even on the skip path, where they wouldn't otherwise be rendered.
        if let Some(receipts) = &args.receipts {
            validate_receipts_size(receipts)?;
        }

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

        // The transfer proof: how far HEAD has moved past the sha the submitter last tested at.
        // Computed unconditionally (used on the skip path below; the reviewer-spawn path owns its
        // own rendering).
        let proof = match args
            .receipts
            .as_ref()
            .and_then(|r| r.commit_tested.as_deref())
        {
            Some(tested) => Some(transfer_proof(ctx, tested, &sha, &branch).await),
            None => None,
        };

        // Persist the FULL, untruncated receipts to our own worktree, at submit time, regardless
        // of which path below is taken — the rendered `[READY]` text only ever carries a truncated
        // summary, so the parent needs a byte-exact copy to read once it's folded (via its own
        // sidecar copying these bytes on the `Lifecycle::Submitted` event). Best-effort: a receipts
        // write failing must never block a submission the agent otherwise did correctly.
        if let Some(receipts) = &args.receipts {
            match serde_json::to_vec_pretty(receipts) {
                Ok(bytes) => {
                    if let Err(e) = ctx
                        .write_atomic(std::path::Path::new(".exo/receipts-submitted.json"), &bytes)
                        .await
                    {
                        tracing::warn!(
                            "failed to persist full receipts to .exo/receipts-submitted.json: {e}"
                        );
                    }
                }
                Err(e) => {
                    tracing::warn!("failed to serialize receipts to JSON: {e}");
                }
            }
        }

        // Two ways to end up here: the agent explicitly opted out (loud, "dangerous"), or this
        // project simply doesn't have reviewers turned on (quiet, the normal off-by-default case —
        // reviewers aren't a fully-cooked feature yet; see `rust/exo/CLAUDE.md`). Either way the gate
        // stays structural: the LLM has no tool that can fabricate a `[READY]` other than through
        // this one path, and a config-disabled project is told so plainly, not scared into
        // suspicion for something the project chose.
        let reviewer_configured = own_review_enabled(ctx).await;
        if args.dangerously_skip_reviewer || !reviewer_configured {
            let flag_line = if args.dangerously_skip_reviewer {
                format!(
                    "[READY] branch `{}` @ {} — review: SKIPPED-BY-AGENT (dangerously_skip_reviewer; \
                     inspect the diff yourself, be more suspicious than usual)",
                    branch.as_str(),
                    sha,
                )
            } else {
                format!("[READY] branch `{}` @ {}", branch.as_str(), sha)
            };
            let mut text = format!("{flag_line}\nnote: {}", args.note);
            let receipts_block = args
                .receipts
                .as_ref()
                .map(|r| render_receipts_summary(r, proof.as_ref()))
                .unwrap_or_default();
            if !receipts_block.is_empty() {
                if receipts_block.len() > receipts::MAX_RENDERED_BYTES {
                    return Err(CapError::invalid(
                        "submit_branch",
                        format!(
                            "rendered receipts block is {} bytes, exceeds the {}-byte cap — trim \
                             your receipts (fewer/shorter verify_commands_run, metrics, or \
                             deviations)",
                            receipts_block.len(),
                            receipts::MAX_RENDERED_BYTES
                        ),
                    ));
                }
                text.push('\n');
                text.push_str(&receipts_block);
            }
            if args.receipts.is_some() {
                text.push('\n');
                text.push_str(&format!(
                    "full receipts: .exo/receipts/{}.json",
                    crate::review::safe_branch(branch.as_str())
                ));
            }
            let summary = if args.dangerously_skip_reviewer {
                format!("[READY skipped] {}", branch.as_str())
            } else {
                format!("[READY] {}", branch.as_str())
            };
            let msg = Message {
                text: MessageBody::new(text)?,
                summary: Summary::new(summary)?,
                kind: MessageKind::Lifecycle(Lifecycle::Submitted {
                    branch: branch.clone(),
                    sha: sha.clone(),
                    reviewed: false,
                }),
                reply_to: None,
            };
            ctx.deliver(Addressee::Parent, msg).await?;
            let wake_note =
                crate::tools::messaging::wake_note(ctx.wake_status(&Addressee::Parent).await)
                    .map(|n| format!("\n{n}"))
                    .unwrap_or_default();
            return Ok(ToolOutput::with_data(
                format!(
                    "Forwarded [READY] to your parent for branch {} WITHOUT review. Your parent \
                     will decide whether to merge. STOP now and end your turn.{wake_note}",
                    branch.as_str()
                ),
                json!({
                    "branch": branch.as_str(),
                    "sha": sha,
                    "reviewer_skipped": true,
                    "receipts": args.receipts,
                    "transfer_proof": transfer_proof_json(proof.as_ref()),
                }),
            ));
        }

        // Resolve a REAL diff base for the reviewer: the fork point off the parent branch. The
        // derived parent *name* may not be a live git ref (a direct child of root derives "root",
        // which the human session never checks out), so resolve to a merge-base SHA, trying the
        // derived parent then the repo's default branch. A SHA always resolves in `git diff`.
        let base_sha = resolve_diff_base(ctx, &branch).await?;
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
        let review_log_path = crate::review::review_log_path(branch.as_str());
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
        let directives = crate::directives::load_directives(ctx).await?;
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
        let review_task = directives.apply(review_task);
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
            model_override: None,
            directives_hash: directives.hash(),
            review_override: None,
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
    use crate::receipts::LabeledValue;
    use crate::testing::{Call, MockRuntime};
    use exo_caps::CommitFiles;
    use exo_framework::Tool;

    /// Minimal valid `NodePapers` JSON for seeding `mock.files` at `.exo/node.json` — the shape
    /// `own_review_enabled`/`own_parent_branch` read. `review_enabled` and `parent_branch` vary
    /// across tests; the rest is filler. `parent_branch: None` omits the field entirely (the
    /// absent-from-older-papers case), matching real serde `#[serde(default)]` behavior.
    fn papers_json(review_enabled: bool, parent_branch: Option<&str>) -> Vec<u8> {
        let parent_branch_field = match parent_branch {
            Some(p) => format!(r#","parent_branch":"{p}""#),
            None => String::new(),
        };
        format!(
            r#"{{"path":["root","dev-node"],"branch":"dev.policy-claude","role":"dev","pane":"%1","parent_inbox":null,"review_enabled":{review_enabled}{parent_branch_field}}}"#
        )
        .into_bytes()
    }

    /// A mock with reviewers turned on (`review_enabled: true` in its own papers) and a real
    /// parent branch (`dev`, matching `current_branch = dev.policy-claude`) — the shared setup
    /// for every test exercising the normal reviewer-spawn path.
    fn mock_with_reviews_enabled() -> MockRuntime {
        let mock = MockRuntime::default(); // is_clean = true, branch = dev.policy-claude
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json(true, Some("dev")));
        mock
    }

    #[tokio::test]
    async fn submits_spawns_reviewer_when_clean() {
        let mock = mock_with_reviews_enabled();
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                ..Default::default()
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
                ..Default::default()
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
    async fn skip_reviewer_delivers_typed_lifecycle_submitted() {
        let mock = MockRuntime::default();
        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "trivial typo fix".into(),
                dangerously_skip_reviewer: true,
                ..Default::default()
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        let msg = calls
            .iter()
            .find_map(|c| match c {
                Call::BusDeliver {
                    to: exo_caps::Addressee::Parent,
                    msg,
                } => Some(msg),
                _ => None,
            })
            .expect("should forward [READY] to parent");
        match &msg.kind {
            exo_caps::MessageKind::Lifecycle(exo_caps::Lifecycle::Submitted {
                branch,
                sha,
                reviewed,
            }) => {
                assert_eq!(branch.as_str(), "dev.policy-claude");
                assert_eq!(sha, &mock.head_sha);
                assert!(!reviewed);
            }
            other => panic!("expected Lifecycle::Submitted, got {other:?}"),
        }
        // Prose is unchanged — same wording as before the typed kind was introduced.
        assert!(msg.summary.as_str().contains("[READY skipped]"));
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
                ..Default::default()
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
        // Plain flag line — no "review: disabled (config)" noise (pure noise when review is off
        // by default, the common case) and no "SKIPPED-BY-AGENT" / "be more suspicious" scare
        // language, since this wasn't the agent's choice.
        assert!(msg
            .text
            .as_str()
            .contains("[READY] branch `dev.policy-claude`"));
        assert!(!msg.text.as_str().contains("review: disabled"));
        assert!(!msg.text.as_str().contains("dangerously_skip_reviewer"));
    }

    #[tokio::test]
    async fn reviews_explicitly_disabled_in_papers_forwards_ready_without_spawning() {
        let mock = MockRuntime::default();
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json(false, None));
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                ..Default::default()
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
                ..Default::default()
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
            .insert(".exo/node.json".to_string(), papers_json(true, Some("dev")));
        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                ..Default::default()
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
            .insert(".exo/node.json".to_string(), papers_json(true, Some("dev")));
        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                ..Default::default()
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
                ..Default::default()
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
                ..Default::default()
            },
        )
        .await;
        let err = res.expect_err("submit must block when the branch is behind its parent");
        let msg = err.to_string();
        // The prompt names the gate, the parent branch (`dev`, from `dev.policy-claude`), and the fix.
        assert!(
            msg.contains("needs_rebase"),
            "err should name the gate: {msg}"
        );
        assert!(
            msg.contains("git rebase dev"),
            "err should prompt the rebase: {msg}"
        );
        // Neither a reviewer nor a [READY] delivery happens — the gate is before both.
        let calls = mock.calls_made();
        assert!(!calls.iter().any(|c| matches!(c, Call::Spawn { .. })));
        assert!(!calls.iter().any(|c| matches!(c, Call::BusDeliver { .. })));
    }

    #[tokio::test]
    async fn behind_parent_blocks_even_with_reviewer_skipped() {
        // The rebase gate is a precondition — it runs even when review is being skipped, since a
        // stale branch's merge conflicts regardless of whether it was reviewed. Needs a papers
        // file recording a real `parent_branch` — the gate no longer derives one from the branch
        // name — so seed it explicitly here rather than relying on `MockRuntime::default()`.
        let mock = MockRuntime {
            is_behind: true,
            ..Default::default()
        };
        mock.files.lock().unwrap().insert(
            ".exo/node.json".to_string(),
            papers_json(false, Some("dev")),
        );
        let res = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "trivial".into(),
                dangerously_skip_reviewer: true,
                ..Default::default()
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
    async fn rebase_gate_passes_open_when_parent_branch_is_none() {
        // Root (or any papers with no recorded parent) — the gate must NOT block even when
        // `is_behind` would report true, since there's no real parent branch to compare against.
        let mock = MockRuntime {
            is_behind: true,
            ..Default::default()
        };
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json(false, None));
        let res = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "trivial".into(),
                dangerously_skip_reviewer: true,
                ..Default::default()
            },
        )
        .await;
        assert!(
            res.is_ok(),
            "a None parent_branch must fail the rebase gate open: {res:?}"
        );
    }

    #[tokio::test]
    async fn rebase_gate_passes_open_when_papers_unreadable() {
        // No `.exo/node.json` seeded at all (papers not yet written, or genuinely absent) — must
        // fail open exactly like an explicit `None`.
        let mock = MockRuntime {
            is_behind: true,
            ..Default::default()
        };
        let res = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "trivial".into(),
                dangerously_skip_reviewer: true,
                ..Default::default()
            },
        )
        .await;
        assert!(
            res.is_ok(),
            "unreadable papers must fail the rebase gate open: {res:?}"
        );
    }

    #[tokio::test]
    async fn own_review_enabled_absent_papers_defaults_silently() {
        // No `.exo/node.json` at all — the legitimate "not written yet" case.
        let mock = MockRuntime::default();
        assert_eq!(
            own_review_enabled(&mock).await,
            NodePapers::DEFAULT_REVIEW_ENABLED
        );
    }

    #[tokio::test]
    async fn own_review_enabled_corrupt_papers_defaults_but_is_distinguishable() {
        // Papers present but not valid JSON — corruption, not absence. Must still default safely
        // (fail-safe), but this is the case the corrupt-vs-absent split exists to make loud (via
        // tracing::warn!, asserted here only by exercising the parse-error branch without panicking).
        let mock = MockRuntime::default();
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), b"not valid json".to_vec());
        assert_eq!(
            own_review_enabled(&mock).await,
            NodePapers::DEFAULT_REVIEW_ENABLED
        );
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
                ..Default::default()
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

    /// Run `submit_branch` on the quiet no-reviewer-configured path (reviewers off by default,
    /// no `.exo/node.json` seeded) with the given receipts, and return the `[READY]` message
    /// delivered to the parent. Shared by the receipts tests below.
    async fn submit_and_capture_ready(mock: &MockRuntime, receipts: Option<Receipts>) -> Message {
        SubmitBranch::run(
            mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                receipts,
                ..Default::default()
            },
        )
        .await
        .unwrap();
        mock.calls_made()
            .into_iter()
            .find_map(|c| match c {
                Call::BusDeliver {
                    to: exo_caps::Addressee::Parent,
                    msg,
                } => Some(msg),
                _ => None,
            })
            .expect("should forward [READY] to parent")
    }

    #[tokio::test]
    async fn receipts_commit_tested_matching_head_renders_at_head() {
        let mock = MockRuntime::default(); // head_sha is all zeros
        let receipts = Receipts {
            commit_tested: Some("0000000".into()),
            ..Default::default()
        };
        let msg = submit_and_capture_ready(&mock, Some(receipts)).await;
        assert!(
            msg.text.as_str().contains("tested@HEAD"),
            "{}",
            msg.text.as_str()
        );
    }

    #[tokio::test]
    async fn receipts_commit_tested_differing_renders_commits_between_and_overlap() {
        let mock = MockRuntime {
            commits_between: vec![CommitFiles {
                sha: "c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1".into(),
                files: vec!["a.rs".into()],
            }],
            ..Default::default()
        };
        let receipts = Receipts {
            commit_tested: Some("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef".into()),
            ..Default::default()
        };
        let msg = submit_and_capture_ready(&mock, Some(receipts)).await;
        let text = msg.text.as_str();
        assert!(text.contains("commits between"), "{text}");
        assert!(text.contains("overlap your diff"), "{text}");
    }

    #[tokio::test]
    async fn receipts_commit_tested_differing_with_no_overlap() {
        // Commits with no touched files at all: the moved-file set and the diff-file set are both
        // empty, so their intersection is the real, reassuring "none" answer, not "unknown".
        let mock = MockRuntime {
            commits_between: vec![CommitFiles {
                sha: "c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1".into(),
                files: vec![],
            }],
            ..Default::default()
        };
        let receipts = Receipts {
            commit_tested: Some("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef".into()),
            ..Default::default()
        };
        let msg = submit_and_capture_ready(&mock, Some(receipts)).await;
        assert!(
            msg.text.as_str().contains("none overlap your diff"),
            "{}",
            msg.text.as_str()
        );
    }

    #[tokio::test]
    async fn receipts_commit_tested_unresolvable_renders_untested_transfer() {
        let mock = MockRuntime::default();
        *mock.fail.lock().unwrap() = Some("commits_between");
        let receipts = Receipts {
            commit_tested: Some("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef".into()),
            ..Default::default()
        };
        let msg = submit_and_capture_ready(&mock, Some(receipts)).await;
        assert!(
            msg.text.as_str().contains("treat as untested transfer"),
            "{}",
            msg.text.as_str()
        );
    }

    #[tokio::test]
    async fn oversized_receipt_field_is_rejected_before_delivery() {
        let mock = MockRuntime::default();
        let receipts = Receipts {
            commit_tested: Some("x".repeat(receipts::MAX_FIELD_BYTES + 1)),
            ..Default::default()
        };
        let res = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                receipts: Some(receipts),
                ..Default::default()
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
    async fn full_receipts_appear_untruncated_in_tool_output_data() {
        let mock = MockRuntime::default();
        let receipts = Receipts {
            commit_tested: Some("0000000".into()),
            verify_commands_run: vec!["cargo test -p exo".into()],
            metrics: vec![LabeledValue {
                label: "tests passed".into(),
                value: "412".into(),
            }],
            deviations: vec!["used an enum for TransferProof".into()],
        };
        let out = SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                receipts: Some(receipts.clone()),
                ..Default::default()
            },
        )
        .await
        .unwrap();
        let data = out.data.expect("skip path always carries data");
        assert_eq!(data["receipts"], serde_json::to_value(&receipts).unwrap());
    }

    #[tokio::test]
    async fn realistic_receipts_payload_stays_under_message_body_max_len() {
        let mock = MockRuntime::default();
        let receipts = Receipts {
            commit_tested: Some("0000000".into()),
            verify_commands_run: vec![
                "cargo test -p exo".into(),
                "cargo clippy --workspace --all-targets".into(),
            ],
            metrics: vec![
                LabeledValue {
                    label: "tests passed".into(),
                    value: "412".into(),
                },
                LabeledValue {
                    label: "wall".into(),
                    value: "1m41s".into(),
                },
            ],
            deviations: vec!["used an enum for TransferProof".into()],
        };
        let msg = submit_and_capture_ready(&mock, Some(receipts)).await;
        assert!(msg.text.as_str().len() <= exo_caps::MessageBody::MAX_LEN);
    }

    #[tokio::test]
    async fn lifecycle_submitted_unchanged_when_receipts_present() {
        let mock = MockRuntime::default();
        let receipts = Receipts {
            commit_tested: Some("0000000".into()),
            ..Default::default()
        };
        let msg = submit_and_capture_ready(&mock, Some(receipts)).await;
        match &msg.kind {
            exo_caps::MessageKind::Lifecycle(exo_caps::Lifecycle::Submitted {
                branch,
                sha,
                reviewed,
            }) => {
                assert_eq!(branch.as_str(), "dev.policy-claude");
                assert_eq!(sha, &mock.head_sha);
                assert!(!reviewed);
            }
            other => panic!("expected Lifecycle::Submitted, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn receipts_present_writes_full_receipts_file() {
        let mock = MockRuntime::default();
        let receipts = Receipts {
            commit_tested: Some("0000000".into()),
            verify_commands_run: vec!["cargo test -p exo".into()],
            metrics: vec![],
            deviations: vec!["used an enum for TransferProof".into()],
        };
        submit_and_capture_ready(&mock, Some(receipts.clone())).await;

        let calls = mock.calls_made();
        assert!(
            calls.iter().any(
                |c| matches!(c, Call::FsWrite { path } if path == ".exo/receipts-submitted.json")
            ),
            "should write the full receipts to .exo/receipts-submitted.json: {calls:?}"
        );
        let written = mock
            .files
            .lock()
            .unwrap()
            .get(".exo/receipts-submitted.json")
            .cloned()
            .expect("receipts file should be written");
        let parsed: Receipts = serde_json::from_slice(&written).unwrap();
        assert_eq!(
            serde_json::to_value(&parsed).unwrap(),
            serde_json::to_value(&receipts).unwrap()
        );
    }

    #[tokio::test]
    async fn receipts_present_writes_full_receipts_file_on_reviewer_path() {
        // The write must happen at submit time REGARDLESS of which path is taken afterward — the
        // reviewer-spawn path doesn't render receipts into a message at all today, so without this
        // the file would never land for a reviewed submission.
        let mock = mock_with_reviews_enabled();
        let receipts = Receipts {
            commit_tested: Some("0000000".into()),
            ..Default::default()
        };
        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                receipts: Some(receipts),
                ..Default::default()
            },
        )
        .await
        .unwrap();

        let calls = mock.calls_made();
        assert!(
            calls.iter().any(
                |c| matches!(c, Call::FsWrite { path } if path == ".exo/receipts-submitted.json")
            ),
            "should write the full receipts even on the reviewer-spawn path: {calls:?}"
        );
    }

    #[tokio::test]
    async fn receipts_absent_does_not_write_receipts_file() {
        let mock = MockRuntime::default();
        submit_and_capture_ready(&mock, None).await;

        let calls = mock.calls_made();
        assert!(
            !calls.iter().any(
                |c| matches!(c, Call::FsWrite { path } if path == ".exo/receipts-submitted.json")
            ),
            "must not write a receipts file when no receipts were passed: {calls:?}"
        );
    }

    #[tokio::test]
    async fn ready_text_names_parent_side_receipts_path_when_receipts_present() {
        let mock = MockRuntime::default(); // branch = dev.policy-claude
        let receipts = Receipts {
            commit_tested: Some("0000000".into()),
            ..Default::default()
        };
        let msg = submit_and_capture_ready(&mock, Some(receipts)).await;
        assert!(
            msg.text
                .as_str()
                .contains("full receipts: .exo/receipts/dev.policy-claude.json"),
            "{}",
            msg.text.as_str()
        );
    }

    #[tokio::test]
    async fn ready_text_omits_receipts_path_when_receipts_absent() {
        let mock = MockRuntime::default();
        let msg = submit_and_capture_ready(&mock, None).await;
        assert!(
            !msg.text.as_str().contains("full receipts:"),
            "{}",
            msg.text.as_str()
        );
    }

    #[tokio::test]
    async fn submit_injects_standing_directives_into_reviewer_task() {
        let mock = mock_with_reviews_enabled();
        mock.dirs.lock().unwrap().insert(
            crate::directives::DIRECTIVES_DIR.to_string(),
            vec![std::path::PathBuf::from(".exo/directives/a.md")],
        );
        mock.files.lock().unwrap().insert(
            ".exo/directives/a.md".to_string(),
            b"reject any new unwrap() in library code".to_vec(),
        );

        SubmitBranch::run(
            &mock,
            SubmitBranchArgs {
                note: "did the thing".into(),
                ..Default::default()
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

        assert!(spawn.contains("reject any new unwrap() in library code"));
    }
}
