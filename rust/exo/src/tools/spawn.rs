//! **P3 leaf.** The three **per-op** spawn tools over the [`Spawner`] cap: `spawn_worker`
//! (→ Inline/Worker), `spawn_dev` (→ Worktree/Dev), `fork_wave` (→ Worktree/Tl). Every spawn is a
//! Claude instance — the role's model is what varies (leaves on Sonnet, TLs on the session
//! default; see [`ExoRole::model`](crate::ExoRole)). Each tool is a thin wrapper type: an `Args`
//! carrying task content plus an optional per-spawn `model` override (the `(role, kind)` pair is
//! fixed by which op, never a caller field), a generic-over-caps `run<C: Spawner>`, and a `Tool<R>`
//! adapter. Ships mock-cap unit tests (assert the right `Spawner` method recorded) in this file.

use crate::directives::{copy_directives, load_directives, Directives};
use crate::roles::ExoRole;
use crate::spawn::{render_spec_prompt, write_acceptance, ExoSpawn};
use exo_caps::{
    AgentName, AgentType, CapError, CapResult, ChildKind, Fs, Git, NodePapers, Spawner,
};
use exo_framework::{Tool, ToolOutput};
use schemars::JsonSchema;
use serde::Deserialize;

/// Model tiers a spawned TL may override to. Anything outside this list is a loud [`CapError`] —
/// no silent pass-through, no defaulting.
const TL_MODEL_ALLOWLIST: &[&str] = &["opus", "sonnet", "haiku"];

/// Validate a per-spawn `model` override against the calling node's own role tier.
///
/// A spawned TL runs on Opus; without a cap it could spawn a subtree on an even more expensive
/// tier or a name the launcher doesn't understand (which fails late, at pane-launch time, not at
/// the tool call). Root is the human's own interactive session and is trusted to name any tier —
/// only its shape is checked, to catch shell injection and typos, not to restrict choice.
fn validate_model(spawner: ExoRole, backend: AgentType, model: &str) -> CapResult<()> {
    let shape_ok = !model.is_empty()
        && model
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-'));
    if !shape_ok {
        return Err(CapError::invalid(
            "model",
            format!(
                "`{model}` must be a single bare token (letters, digits, `.`, `_`, `-` only) and \
                 must NOT contain shell metacharacters or whitespace"
            ),
        ));
    }
    // Codex model ids are not a small fixed tier vocabulary and are already passed structurally
    // to the harness. Keep the injection guard, but let the harness validate availability.
    if backend == AgentType::Codex {
        return Ok(());
    }
    match spawner {
        ExoRole::Root => Ok(()),
        ExoRole::Tl => {
            if TL_MODEL_ALLOWLIST.contains(&model) {
                Ok(())
            } else {
                Err(CapError::invalid(
                    "model",
                    format!(
                        "unknown model `{model}` — a spawned TL may only override to one of: \
                         opus, sonnet, haiku"
                    ),
                ))
            }
        }
        // Dev/Worker/Reviewer have no spawn tools, so this arm is unreachable in practice —
        // reject defensively rather than silently accepting an override that could never
        // legitimately arrive here.
        ExoRole::Dev | ExoRole::Worker | ExoRole::Reviewer => Err(CapError::invalid(
            "model",
            format!("role {spawner:?} cannot set a model override on a spawned child"),
        )),
    }
}

/// This node's own role, read from its birth papers (`.exo/node.json`). Three outcomes:
///
/// - Papers absent (`NotFound`) → [`ExoRole::Root`], silently. The root's own papers live OUTSIDE
///   the cwd, under `~/.claude/exo` — their absence in the cwd IS the root signature.
/// - Papers present but unreadable, unparseable, or the role field doesn't type as `ExoRole` →
///   [`ExoRole::Tl`], with a loud `tracing::warn!`. A spuriously-capped TL fails loud and
///   actionably ("use opus/sonnet/haiku"); a spuriously-uncapped one would silently burn tokens on
///   the wrong tier — the conservative guess is the safe one.
/// - Papers present and readable → the recorded role.
async fn own_identity<C: Fs + Sync>(ctx: &C) -> (ExoRole, AgentType) {
    let conservative = |context: &str, detail: &dyn std::fmt::Display| {
        tracing::warn!(
            "own_role: {context} ({detail}); capping as Tl — a spuriously-capped TL fails loud \
             and actionably, a spuriously-uncapped one silently burns tokens on the wrong tier"
        );
        (ExoRole::Tl, AgentType::Claude)
    };
    match ctx.read(std::path::Path::new(".exo/node.json")).await {
        Ok(bytes) => match serde_json::from_slice::<NodePapers>(&bytes) {
            Ok(papers) => match papers.role.typed::<ExoRole>() {
                Ok(role) => (role, papers.agent_type),
                Err(e) => conservative("role field failed to type as ExoRole", &e),
            },
            Err(e) => conservative(".exo/node.json failed to parse", &e),
        },
        Err(exo_caps::FsError::At { source, .. } | exo_caps::FsError::Io(source))
            if source.kind() == std::io::ErrorKind::NotFound =>
        {
            (ExoRole::Root, AgentType::Codex)
        }
        Err(e) => conservative("could not read .exo/node.json", &e),
    }
}

/// Refuse to spawn a worktree child whose `read_first` names a path that isn't tracked at HEAD.
/// A worktree child forks from the spawner's current COMMIT, not its working directory — a file
/// that merely exists on disk but was never `git add`ed/committed is invisible to the child, and
/// today that's discovered mid-flight, at the cost of a full spawn round-trip. Empty `read_first`
/// skips the check entirely (nothing to validate).
async fn require_tracked_read_first<C: Git + Sync>(
    ctx: &C,
    read_first: &[String],
) -> CapResult<()> {
    let missing = untracked_read_first(ctx, read_first).await?;
    if missing.is_empty() {
        Ok(())
    } else {
        Err(missing_read_first_error(&missing))
    }
}

/// The subset of `read_first` that isn't tracked at HEAD. Empty input short-circuits to empty
/// output without a git call.
async fn untracked_read_first<C: Git + Sync>(
    ctx: &C,
    read_first: &[String],
) -> CapResult<Vec<String>> {
    if read_first.is_empty() {
        return Ok(Vec::new());
    }
    Ok(ctx.tracked_at_head(read_first).await?)
}

fn missing_read_first_error(missing: &[String]) -> CapError {
    CapError::invalid(
        "read_first",
        format!(
            "not tracked at HEAD — children fork from your COMMIT, so an untracked or \
             uncommitted file is invisible to them even though it exists on disk; commit the \
             file first: {}",
            missing.join(", ")
        ),
    )
}

/// `read_first` entries that fall outside `file_boundary` — a WARNING for the parent, never a
/// refusal: reading outside your own edit scope is legitimate (context), so this only flags a
/// possible authoring slip. Empty either input skips the check.
fn read_first_outside_boundary(read_first: &[String], file_boundary: &[String]) -> Vec<String> {
    if read_first.is_empty() || file_boundary.is_empty() {
        return Vec::new();
    }
    read_first
        .iter()
        .filter(|rf| !crate::boundary::matches(file_boundary, rf))
        .cloned()
        .collect()
}

fn boundary_warning_line(outside: &[String]) -> String {
    format!(
        "note: read_first outside file_boundary (fine for read-only context, check it isn't an \
         authoring slip): {}",
        outside.join(", ")
    )
}

/// Refuse to spawn a worktree child from a dirty tree. Children fork from the spawner's CURRENT
/// COMMIT — uncommitted state is invisible to them and they'll build against a foundation the TL
/// won't find missing until integration. Fails CLOSED on a git error.
async fn require_clean_worktree<C: Git + Sync>(ctx: &C) -> CapResult<()> {
    let dirty = ctx.status_porcelain().await?;
    if dirty.is_empty() {
        Ok(())
    } else {
        Err(CapError::invalid(
            "worktree",
            format!(
                "worktree not clean — commit or stash before spawning worktree children \
                 (children fork from your current commit; uncommitted state would be invisible \
                 to them):\n  {}",
                dirty.join("\n  ")
            ),
        ))
    }
}

/// Internal fields shared by all three spawn tools. Callers convert their specific Args into this
/// and pass it to [`build_spawn`].
struct SpawnArgs {
    name: Option<String>,
    task: String,
    steps: Vec<String>,
    verify: Vec<String>,
    done_criteria: Vec<String>,
    context: Option<String>,
    boundary: Vec<String>,
    file_boundary: Vec<String>,
    read_first: Vec<String>,
    fork_session: bool,
    model: Option<String>,
    review: Option<bool>,
}

/// Resolve the name, render the spec prompt (with standing directives applied), and assemble an
/// [`ExoSpawn`]. The `(role, kind, name_prefix)` triple is fixed by the calling tool — callers
/// provide only the task content.
fn build_spawn(
    role: ExoRole,
    kind: ChildKind,
    name_prefix: &str,
    args: SpawnArgs,
    directives: &Directives,
) -> CapResult<ExoSpawn> {
    let name = match args.name {
        Some(n) => Some(AgentName::new(n)?),
        None => None,
    };
    Ok(ExoSpawn {
        role,
        kind,
        name,
        name_prefix: name_prefix.into(),
        task: directives.apply(render_spec_prompt(
            &args.task,
            &args.read_first,
            &args.steps,
            &args.verify,
            &args.boundary,
            &args.file_boundary,
            args.context.as_ref(),
            &args.done_criteria,
        )),
        fork_session: args.fork_session,
        model_override: args.model,
        directives_hash: directives.hash(),
        review_override: args.review,
    })
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SpawnWorkerArgs {
    pub name: Option<String>,
    pub task: String,
    #[serde(default)]
    pub steps: Vec<String>,
    #[serde(default)]
    pub verify: Vec<String>,
    #[serde(default)]
    pub done_criteria: Vec<String>,
    #[serde(default)]
    pub context: Option<String>,
    /// Prose rules injected into the child's task under "CONSTRAINTS" — known
    /// failure modes to avoid. This is NOT an allowed-paths list and nothing checks it
    /// mechanically; for that, worktree tools take `file_boundary` instead.
    #[serde(default)]
    pub boundary: Vec<String>,
    #[serde(default)]
    pub read_first: Vec<String>,
    /// Per-spawn model override for this child, replacing the role's default. Capped at the
    /// spawning node's own tier. IGNORED for a role redirected by a launch profile — that
    /// profile's proxy serves exactly one model, so overriding it would 404.
    #[serde(default)]
    pub model: Option<String>,
}

pub struct SpawnWorker;

#[async_trait::async_trait]
impl<R: Spawner + Fs + Send + Sync> Tool<R> for SpawnWorker {
    const NAME: &'static str = "spawn_worker";
    const DESCRIPTION: &'static str =
        "Spawn an ephemeral Sonnet worker in a pane inside YOUR worktree (no own branch, no \
         review). PREFER DELEGATING OVER DOING WORK YOURSELF — a Sonnet leaf costs far less than \
         your own tokens, so every line you implement yourself is wasted budget. Give it \
         acceptance criteria, key file paths, and anti-patterns — not line-by-line code. For \
         research or non-conflicting in-place edits; it reports back with `notify_parent`. There \
         is nothing to merge — for work that should land on its own branch, use `spawn_dev`. \
         Set `model` to override this child's model tier for this one spawn, capped at your own \
         role's tier; ignored if this role is redirected by a launch profile, since that \
         profile's proxy serves exactly one model. Never poll after spawning: events are pushed. \
         Continue useful non-overlapping work, or yield when none remains.";
    type Args = SpawnWorkerArgs;

    async fn run(ctx: &R, args: SpawnWorkerArgs) -> CapResult<ToolOutput> {
        if let Some(m) = args.model.as_deref() {
            let (role, backend) = own_identity(ctx).await;
            validate_model(role, backend, m)?;
        }
        let directives = load_directives(ctx).await?;
        // The tool fixes the (role, kind): an ephemeral inline worker (Sonnet Claude).
        let spec = build_spawn(
            ExoRole::Worker,
            ChildKind::Inline,
            "worker",
            SpawnArgs {
                name: args.name,
                task: args.task,
                steps: args.steps,
                verify: args.verify,
                done_criteria: args.done_criteria,
                context: args.context,
                boundary: args.boundary,
                file_boundary: Vec::new(),
                read_first: args.read_first,
                fork_session: false,
                model: args.model,
                review: None,
            },
            &directives,
        )?;
        let spawned = ctx.spawn(spec).await?;
        Ok(ToolOutput::with_data(
            format!("Spawned worker {}", spawned.as_str()),
            serde_json::json!({ "spawned": spawned.as_str() }),
        ))
    }
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SpawnDevArgs {
    pub name: Option<String>,
    pub task: String,
    #[serde(default)]
    pub steps: Vec<String>,
    #[serde(default)]
    pub verify: Vec<String>,
    #[serde(default)]
    pub done_criteria: Vec<String>,
    #[serde(default)]
    pub context: Option<String>,
    /// Prose rules injected into the child's task under "CONSTRAINTS" — known
    /// failure modes to avoid. This is NOT an allowed-paths list — for that, use `file_boundary`,
    /// which the child also sees (under "ALLOWED PATHS") and which is additionally checked
    /// mechanically at merge time.
    #[serde(default)]
    pub boundary: Vec<String>,
    #[serde(default)]
    pub read_first: Vec<String>,
    /// Per-spawn model override for this child, replacing the role's default. Capped at the
    /// spawning node's own tier. IGNORED for a role redirected by a launch profile — that
    /// profile's proxy serves exactly one model, so overriding it would 404.
    #[serde(default)]
    pub model: Option<String>,
    /// Per-spawn override of this child's review gate. Omitted/`None` inherits your own
    /// `review_enabled` exactly as today. `Some(true)`/`Some(false)` stamps the child's papers
    /// directly, and that value inherits onward to its own children — turn review ON for a
    /// subtree doing subtle cross-cutting work, or OFF for mechanical leaf work where receipts +
    /// the typed unreviewed flag already carry the audit trail.
    #[serde(default)]
    pub review: Option<bool>,
    /// Allowed file paths / directory prefixes for this child's diff. Rendered into the child's
    /// spec under "ALLOWED PATHS" so it actually knows where it may write, AND checked
    /// mechanically by `merge` against the child's real diff before folding. An entry matches a
    /// changed file if it equals it exactly or is a directory prefix (the file path starts with
    /// the entry + `/`). Empty (default) = unrestricted, no section rendered, no check at merge
    /// time.
    #[serde(default)]
    pub file_boundary: Vec<String>,
}

pub struct SpawnDev;

#[async_trait::async_trait]
impl<R: Spawner + Fs + Git + Send + Sync> Tool<R> for SpawnDev {
    const NAME: &'static str = "spawn_dev";
    const DESCRIPTION: &'static str =
        "Spawn a Sonnet dev leaf in its OWN worktree + branch with a self-contained spec. PREFER \
         DELEGATING OVER DOING WORK YOURSELF — a Sonnet leaf costs far less than your own tokens; \
         every line you implement yourself is wasted budget. Use the structured fields \
         (steps, verify, boundary, read_first) for precise specs — give it acceptance criteria \
         and file paths, not line-by-line code. It commits to that branch and calls \
         `submit_branch` when ready; a one-shot reviewer checks it, then you `merge` the branch \
         locally. No PRs, no remote — convergence is on-disk. Refuses to run on a dirty \
         worktree — the child forks from your current commit, so uncommitted state would be \
         invisible to it. Set `model` to override this child's model tier for this one spawn, \
         capped at your own role's tier; ignored if this role is redirected by a launch profile. \
         Duplicate `name` (including a previously reaped one) is refused before any resource is \
         created — on an ambiguous spawn error, check `tree` before retrying; never blind-respawn. \
         Never poll after spawning: [READY] is pushed. Continue useful non-overlapping work, or \
         yield when none remains.";
    type Args = SpawnDevArgs;

    async fn run(ctx: &R, args: SpawnDevArgs) -> CapResult<ToolOutput> {
        if let Some(m) = args.model.as_deref() {
            let (role, backend) = own_identity(ctx).await;
            validate_model(role, backend, m)?;
        }
        require_tracked_read_first(ctx, &args.read_first).await?;
        require_clean_worktree(ctx).await?;
        let directives = load_directives(ctx).await?;
        let boundary_warning = read_first_outside_boundary(&args.read_first, &args.file_boundary);
        // The tool fixes the (role, kind): a Sonnet dev leaf in its own worktree.
        let spec = build_spawn(
            ExoRole::Dev,
            ChildKind::Worktree,
            "dev",
            SpawnArgs {
                name: args.name,
                task: args.task,
                steps: args.steps,
                verify: args.verify,
                done_criteria: args.done_criteria,
                context: args.context,
                boundary: args.boundary,
                file_boundary: args.file_boundary.clone(),
                read_first: args.read_first,
                fork_session: false,
                model: args.model,
                review: args.review,
            },
            &directives,
        )?;
        let task = spec.task.clone();
        let spawned = ctx.spawn(spec).await?;
        // Persist the child's spec as its acceptance bar (relocated out of the runtime birth).
        write_acceptance(ctx, &spawned, &task).await;
        if !args.file_boundary.is_empty() {
            let boundary = crate::boundary::FileBoundary {
                allowed: args.file_boundary,
            };
            crate::boundary::write_boundary(ctx, &spawned, &boundary).await;
        }
        // Untracked files don't materialize through `git worktree add` — copy the directives into
        // the child's own worktree so it can pass them further down its own subtree.
        copy_directives(ctx, &spawned, &directives).await;
        let mut text = format!("Spawned dev {}", spawned.as_str());
        if !boundary_warning.is_empty() {
            text.push('\n');
            text.push_str(&boundary_warning_line(&boundary_warning));
        }
        Ok(ToolOutput::with_data(
            text,
            serde_json::json!({ "spawned": spawned.as_str(), "spec": task }),
        ))
    }
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ForkChildArgs {
    pub name: Option<String>,
    pub task: String,
    #[serde(default)]
    pub steps: Vec<String>,
    #[serde(default)]
    pub verify: Vec<String>,
    #[serde(default)]
    pub done_criteria: Vec<String>,
    #[serde(default)]
    pub context: Option<String>,
    /// Prose rules injected into the child's task under "CONSTRAINTS" — known
    /// failure modes to avoid. This is NOT an allowed-paths list — for that, use `file_boundary`,
    /// which the child also sees (under "ALLOWED PATHS") and which is additionally checked
    /// mechanically at merge time.
    #[serde(default)]
    pub boundary: Vec<String>,
    #[serde(default)]
    pub read_first: Vec<String>,
    /// Opt-in (default false): inherit this TL session's context by launching the child
    /// Claude with `--resume --fork-session <this-session-uuid>`. Default false — the
    /// scaffold commit + spec is the primary context channel, and forking a stale/compacted
    /// parent context often hurts.
    #[serde(default)]
    pub fork_session: bool,
    /// Per-spawn model override for this child, replacing the role's default. Capped at the
    /// spawning node's own tier. IGNORED for a role redirected by a launch profile — that
    /// profile's proxy serves exactly one model, so overriding it would 404.
    #[serde(default)]
    pub model: Option<String>,
    /// Per-spawn override of this child's review gate. Omitted/`None` inherits your own
    /// `review_enabled` exactly as today. `Some(true)`/`Some(false)` stamps the child's papers
    /// directly, and that value inherits onward to its own children — turn review ON for a
    /// subtree doing subtle cross-cutting work, or OFF for mechanical leaf work where receipts +
    /// the typed unreviewed flag already carry the audit trail.
    #[serde(default)]
    pub review: Option<bool>,
    /// Allowed file paths / directory prefixes for this child's diff. Rendered into the child's
    /// spec under "ALLOWED PATHS" so it actually knows where it may write, AND checked
    /// mechanically by `merge` against the child's real diff before folding. An entry matches a
    /// changed file if it equals it exactly or is a directory prefix (the file path starts with
    /// the entry + `/`). Empty (default) = unrestricted, no section rendered, no check at merge
    /// time.
    #[serde(default)]
    pub file_boundary: Vec<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ForkWaveArgs {
    pub children: Vec<ForkChildArgs>,
    /// Render every child's fully assembled spec (directives applied, birth preamble included)
    /// and spawn nothing — so you can check a wave before committing to it. Works even on a
    /// dirty tree, since nothing is actually spawned. Default false.
    #[serde(default)]
    pub preview: bool,
}

pub struct ForkWave;

#[async_trait::async_trait]
impl<R: Spawner + Fs + Git + Send + Sync> Tool<R> for ForkWave {
    const NAME: &'static str = "fork_wave";
    const DESCRIPTION: &'static str =
        "Fork a wave of parallel Claude TL children, each in its own worktree + branch. Each runs \
         scaffold-fork-converge on its subtree and calls `submit_branch` when its branch is \
         ready; you then `merge` it locally — no PRs, no remote, convergence is on-disk. \
         Decompose and delegate aggressively: every token you spend on work a child could do is \
         wasted. Requires a clean worktree — children fork from your current commit, so \
         uncommitted state would be invisible to them. Set `model` per child to override that \
         child's model tier for this one spawn, capped at your own role's tier; ignored for a \
         role redirected by a launch profile. Set `preview: true` to render every child's final \
         assembled spec and spawn nothing — works even on a dirty tree. Duplicate `name` \
         (including a previously reaped one) is refused before any resource is created — on an \
         ambiguous spawn error, check `tree` before retrying; never blind-respawn. Never poll \
         after spawning: child events are pushed. Continue useful non-overlapping work, or yield \
         when none remains.";
    type Args = ForkWaveArgs;

    async fn run(ctx: &R, args: ForkWaveArgs) -> CapResult<ToolOutput> {
        let directives = load_directives(ctx).await?;

        // Resolve the spawner's own role ONCE for the whole wave, not per child.
        let needs_role = args.children.iter().any(|c| c.model.is_some());
        if needs_role {
            let (spawner_role, backend) = own_identity(ctx).await;
            for child in &args.children {
                if let Some(m) = child.model.as_deref() {
                    validate_model(spawner_role, backend, m)?;
                }
            }
        }

        // Validate ALL children's read_first before spawning ANY — consistent with the existing
        // clean-gate's all-or-nothing posture. One batched `tracked_at_head` call over the union
        // of every child's read_first, then attributed back per child.
        let all_read_first: Vec<String> = args
            .children
            .iter()
            .flat_map(|c| c.read_first.iter().cloned())
            .collect();
        let missing: std::collections::HashSet<String> = untracked_read_first(ctx, &all_read_first)
            .await?
            .into_iter()
            .collect();
        let missing_by_child: Vec<(String, Vec<String>)> = args
            .children
            .iter()
            .filter_map(|c| {
                let name = c.name.clone().unwrap_or_else(|| "tl-<auto>".to_string());
                let child_missing: Vec<String> = c
                    .read_first
                    .iter()
                    .filter(|p| missing.contains(*p))
                    .cloned()
                    .collect();
                (!child_missing.is_empty()).then_some((name, child_missing))
            })
            .collect();

        if !args.preview && !missing_by_child.is_empty() {
            let detail = missing_by_child
                .iter()
                .map(|(name, paths)| format!("{name}: {}", paths.join(", ")))
                .collect::<Vec<_>>()
                .join("; ");
            return Err(missing_read_first_error(&[detail]));
        }

        let mut specs = Vec::with_capacity(args.children.len());
        // Keep the rendered tasks and display names parallel to `specs` so we can persist each
        // spawned child's acceptance bar after the wave returns (the results are positional), and
        // so preview can label each child before anything is spawned.
        let mut tasks = Vec::with_capacity(args.children.len());
        let mut display_names = Vec::with_capacity(args.children.len());
        let mut file_boundaries = Vec::with_capacity(args.children.len());
        let mut boundary_warnings = Vec::new();
        for child in args.children {
            let display_name = child
                .name
                .clone()
                .unwrap_or_else(|| "tl-<auto>".to_string());
            let file_boundary = child.file_boundary.clone();
            let outside = read_first_outside_boundary(&child.read_first, &file_boundary);
            if !outside.is_empty() {
                boundary_warnings.push(format!("{display_name}: {}", outside.join(", ")));
            }
            // The tool fixes the (role, kind): a Claude TL child in its own worktree.
            let spec = build_spawn(
                ExoRole::Tl,
                ChildKind::Worktree,
                "tl",
                SpawnArgs {
                    name: child.name,
                    task: child.task,
                    steps: child.steps,
                    verify: child.verify,
                    done_criteria: child.done_criteria,
                    context: child.context,
                    boundary: child.boundary,
                    file_boundary: child.file_boundary,
                    read_first: child.read_first,
                    fork_session: child.fork_session,
                    model: child.model,
                    review: child.review,
                },
                &directives,
            )?;
            tasks.push(spec.task.clone());
            display_names.push(display_name);
            file_boundaries.push(file_boundary);
            specs.push(spec);
        }

        if args.preview {
            // A pure render: no clean gate, no spawn, no acceptance/directives writes. The
            // reproduced preamble path is PREDICTED — the real one is absolute and resolved at
            // birth — but the content matches what the child will actually see.
            let mut out = String::new();
            if !missing_by_child.is_empty() {
                out.push_str(
                    "WOULD REFUSE: read_first not tracked at HEAD — children fork from your \
                     COMMIT, so an untracked or uncommitted file is invisible to them even \
                     though it exists on disk; commit the file first:\n",
                );
                for (name, paths) in &missing_by_child {
                    out.push_str(&format!("  {name}: {}\n", paths.join(", ")));
                }
                out.push('\n');
            }
            for (spec, name) in specs.iter().zip(display_names.iter()) {
                out.push_str(&format!("=== {name} ===\n"));
                out.push_str(&exo_caps::birth_preamble(
                    ChildKind::Worktree,
                    std::path::Path::new(&format!(".exo/worktrees/{name}")),
                ));
                out.push_str(&spec.task);
                out.push('\n');
            }
            if !boundary_warnings.is_empty() {
                out.push('\n');
                out.push_str(&boundary_warning_line(&boundary_warnings));
            }
            return Ok(ToolOutput::text(out));
        }

        require_clean_worktree(ctx).await?;

        let results = ctx.fork_wave(specs).await;

        let mut spawned = Vec::new();
        let mut errors = Vec::new();
        for ((res, task), file_boundary) in results
            .into_iter()
            .zip(tasks.iter())
            .zip(file_boundaries.iter())
        {
            match res {
                Ok(name) => {
                    write_acceptance(ctx, &name, task).await;
                    if !file_boundary.is_empty() {
                        let boundary = crate::boundary::FileBoundary {
                            allowed: file_boundary.clone(),
                        };
                        crate::boundary::write_boundary(ctx, &name, &boundary).await;
                    }
                    copy_directives(ctx, &name, &directives).await;
                    spawned.push(serde_json::json!({ "name": name.as_str(), "spec": task }));
                }
                Err(e) => errors.push(e.to_string()),
            }
        }

        let total = spawned.len() + errors.len();
        let mut text = format!(
            "Forked {} children ({} succeeded, {} failed)",
            total,
            spawned.len(),
            errors.len()
        );
        if !boundary_warnings.is_empty() {
            text.push('\n');
            text.push_str(&boundary_warning_line(&boundary_warnings));
        }
        Ok(ToolOutput::with_data(
            text,
            serde_json::json!({
                "spawned": spawned,
                "errors": errors
            }),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn codex_model_ids_are_shape_checked_not_claude_tier_capped() {
        assert!(validate_model(ExoRole::Tl, AgentType::Codex, "gpt-5.6-sol").is_ok());
        assert!(validate_model(ExoRole::Tl, AgentType::Codex, "bad model").is_err());
    }
    use crate::testing::{Call, MockRuntime};
    use exo_framework::Tool;

    /// Minimal valid `NodePapers` JSON with a controllable `role`, for seeding `mock.files` at
    /// `.exo/node.json` — the shape `own_role` reads.
    fn papers_json_with_role(role: &str) -> Vec<u8> {
        format!(
            r#"{{"path":["root","node"],"branch":"root.node","role":"{role}","pane":"%1","parent_inbox":null}}"#
        )
        .into_bytes()
    }

    #[tokio::test]
    async fn test_spawn_worker() {
        let mock = MockRuntime::default();
        let args = SpawnWorkerArgs {
            name: Some("worker-1".to_string()),
            task: "do something".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
        };
        let out = SpawnWorker::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Spawned worker"));
        let calls = mock.calls_made();
        // Exactly one spawn (directives-loading also touches Fs, so don't assert total count).
        let spawns: Vec<_> = calls
            .iter()
            .filter(|c| matches!(c, Call::Spawn { .. }))
            .collect();
        assert_eq!(spawns.len(), 1);
        match spawns[0] {
            Call::Spawn { role, task, .. } => {
                assert_eq!(role, "worker");
                assert!(task.contains("do something"));
            }
            _ => panic!("wrong call"),
        }
    }

    #[tokio::test]
    async fn test_spawn_worker_structured() {
        let mock = MockRuntime::default();
        let args = SpawnWorkerArgs {
            name: Some("worker-1".to_string()),
            task: "do something".to_string(),
            steps: vec!["step 1".into()],
            verify: vec!["verify 1".into()],
            done_criteria: vec![],
            context: None,
            boundary: vec!["boundary 1".into()],
            read_first: vec![],
            model: None,
        };
        let _ = SpawnWorker::run(&mock, args).await.unwrap();
        let calls = mock.calls_made();
        let spawn = calls
            .iter()
            .find(|c| matches!(c, Call::Spawn { .. }))
            .expect("spawn recorded");
        match spawn {
            Call::Spawn { task, .. } => {
                // The structured fields are rendered into the single task body by the domain.
                assert!(task.contains("STEPS (if useful):\n1. step 1"));
                assert!(task.contains("CONSTRAINTS:\n- boundary 1"));
            }
            _ => panic!("wrong call"),
        }
    }

    #[tokio::test]
    async fn test_spawn_dev() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do something else".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        let out = SpawnDev::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Spawned dev"));
        let calls = mock.calls_made();
        // The spawn, then the acceptance.md write (relocated into the domain tool).
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::Spawn { role, task, .. }
            if role == "dev" && task.contains("do something else"))));
        assert!(calls.iter().any(|c| matches!(c, Call::FsWrite { path }
            if path.contains("dev-1") && path.ends_with(".exo/acceptance.md"))));
    }

    #[tokio::test]
    async fn spawn_dev_output_data_carries_the_rendered_spec() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "distinctive dev task".to_string(),
            steps: vec!["step 1".into()],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        let out = SpawnDev::run(&mock, args).await.unwrap();
        let data = out.data.expect("spawn_dev always carries data");
        let spec = data["spec"].as_str().expect("data.spec is a string");
        // Byte-exact: the same rendered task the child was actually spawned with.
        let spawned_task = mock
            .calls_made()
            .iter()
            .find_map(|c| match c {
                Call::Spawn { task, .. } => Some(task.clone()),
                _ => None,
            })
            .expect("spawn recorded");
        assert_eq!(spec, spawned_task);
        assert!(spec.contains("distinctive dev task"));
        assert!(spec.contains("STEPS (if useful):\n1. step 1"));
    }

    #[tokio::test]
    async fn fork_wave_output_data_carries_per_child_spec() {
        let mock = MockRuntime::default();
        let args = ForkWaveArgs {
            children: vec![ForkChildArgs {
                name: Some("child-1".to_string()),
                task: "distinctive tl task".to_string(),
                steps: vec![],
                verify: vec![],
                done_criteria: vec![],
                context: None,
                boundary: vec![],
                read_first: vec![],
                fork_session: false,
                model: None,
                review: None,
                file_boundary: vec![],
            }],
            preview: false,
        };
        let out = ForkWave::run(&mock, args).await.unwrap();
        let data = out.data.expect("fork_wave always carries data");
        let spawned = data["spawned"].as_array().expect("spawned is an array");
        assert_eq!(spawned.len(), 1);
        assert_eq!(spawned[0]["name"], "child-1");
        assert!(spawned[0]["spec"]
            .as_str()
            .expect("spec is a string")
            .contains("distinctive tl task"));
    }

    #[tokio::test]
    async fn test_fork_wave() {
        let mock = MockRuntime::default();
        let args = ForkWaveArgs {
            children: vec![
                ForkChildArgs {
                    name: Some("child-1".to_string()),
                    task: "task 1".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec![],
                    fork_session: false,
                    model: None,
                    review: None,
                    file_boundary: vec![],
                },
                ForkChildArgs {
                    name: Some("child-2".to_string()),
                    task: "task 2".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec![],
                    fork_session: false,
                    model: None,
                    review: None,
                    file_boundary: vec![],
                },
            ],
            preview: false,
        };
        let out = ForkWave::run(&mock, args).await.unwrap();
        assert!(out
            .text
            .contains("Forked 2 children (2 succeeded, 0 failed)"));
        let calls = mock.calls_made();
        // One fork_wave call recording the wave size...
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::ForkWave { n } if *n == 2)));
        // ...and an acceptance.md write per spawned child.
        let writes = calls
            .iter()
            .filter(|c| matches!(c, Call::FsWrite { path } if path.ends_with(".exo/acceptance.md")))
            .count();
        assert_eq!(writes, 2);
    }

    #[tokio::test]
    async fn tier_cap_accepts_allowed_model_for_tl() {
        let mock = MockRuntime::default();
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json_with_role("tl"));
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: Some("sonnet".to_string()),
            review: None,
            file_boundary: vec![],
        };
        assert!(SpawnDev::run(&mock, args).await.is_ok());
        assert!(mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn tier_cap_rejects_unknown_model_for_tl() {
        let mock = MockRuntime::default();
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), papers_json_with_role("tl"));
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: Some("fable".to_string()),
            review: None,
            file_boundary: vec![],
        };
        let err = SpawnDev::run(&mock, args).await.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("opus"), "err should name opus: {msg}");
        assert!(msg.contains("sonnet"), "err should name sonnet: {msg}");
        assert!(msg.contains("haiku"), "err should name haiku: {msg}");
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn corrupt_papers_caps_as_tl() {
        let mock = MockRuntime::default();
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/node.json".to_string(), b"not valid json".to_vec());
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: Some("fable".to_string()),
            review: None,
            file_boundary: vec![],
        };
        assert!(SpawnDev::run(&mock, args).await.is_err());
    }

    #[tokio::test]
    async fn missing_papers_is_root_uncapped() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: Some("gpt-nonsense-4".to_string()),
            review: None,
            file_boundary: vec![],
        };
        assert!(SpawnDev::run(&mock, args).await.is_ok());
    }

    #[tokio::test]
    async fn root_shape_rejects_shell_metacharacters() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: Some("sonnet; rm -rf /".to_string()),
            review: None,
            file_boundary: vec![],
        };
        assert!(SpawnDev::run(&mock, args).await.is_err());
    }

    #[tokio::test]
    async fn spawn_dev_refuses_dirty_worktree() {
        let mock = MockRuntime {
            dirty_files: vec![" M rust/exo/src/lib.rs".into(), "?? junk.txt".into()],
            ..Default::default()
        };
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        let err = SpawnDev::run(&mock, args).await.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("rust/exo/src/lib.rs"));
        assert!(msg.contains("junk.txt"));
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn fork_wave_refuses_dirty_worktree() {
        let mock = MockRuntime {
            dirty_files: vec![" M rust/exo/src/lib.rs".into(), "?? junk.txt".into()],
            ..Default::default()
        };
        let args = ForkWaveArgs {
            children: vec![ForkChildArgs {
                name: Some("child-1".to_string()),
                task: "task 1".to_string(),
                steps: vec![],
                verify: vec![],
                done_criteria: vec![],
                context: None,
                boundary: vec![],
                read_first: vec![],
                fork_session: false,
                model: None,
                review: None,
                file_boundary: vec![],
            }],
            preview: false,
        };
        let err = ForkWave::run(&mock, args).await.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("rust/exo/src/lib.rs"));
        assert!(msg.contains("junk.txt"));
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::ForkWave { .. })));
    }

    #[tokio::test]
    async fn spawn_worker_not_gated_on_dirty_worktree() {
        let mock = MockRuntime {
            dirty_files: vec!["?? junk.txt".into()],
            ..Default::default()
        };
        let args = SpawnWorkerArgs {
            name: Some("worker-1".to_string()),
            task: "do something".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
        };
        assert!(SpawnWorker::run(&mock, args).await.is_ok());
        assert!(mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn git_failure_fails_closed() {
        let mock = MockRuntime::failing("status_porcelain");
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        assert!(SpawnDev::run(&mock, args).await.is_err());
    }

    #[tokio::test]
    async fn directives_injected_into_spawned_task() {
        let mock = MockRuntime::default();
        mock.dirs.lock().unwrap().insert(
            crate::directives::DIRECTIVES_DIR.to_string(),
            vec![std::path::PathBuf::from(".exo/directives/rules.md")],
        );
        mock.files.lock().unwrap().insert(
            ".exo/directives/rules.md".to_string(),
            b"always be kind".to_vec(),
        );
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        SpawnDev::run(&mock, args).await.unwrap();
        let calls = mock.calls_made();
        let task = calls
            .iter()
            .find_map(|c| match c {
                Call::Spawn { task, .. } => Some(task.clone()),
                _ => None,
            })
            .expect("spawn recorded");
        assert!(task.contains("always be kind"));
    }

    #[tokio::test]
    async fn directives_copied_to_worktree_child() {
        let mock = MockRuntime::default();
        mock.dirs.lock().unwrap().insert(
            crate::directives::DIRECTIVES_DIR.to_string(),
            vec![std::path::PathBuf::from(".exo/directives/rules.md")],
        );
        mock.files.lock().unwrap().insert(
            ".exo/directives/rules.md".to_string(),
            b"always be kind".to_vec(),
        );
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        SpawnDev::run(&mock, args).await.unwrap();
        assert!(mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::FsWrite { path }
            if path.starts_with(".exo/worktrees/") && path.ends_with(".exo/directives/rules.md"))));
    }

    #[test]
    fn build_spawn_directives_hash_present_and_absent() {
        let base_args = || SpawnArgs {
            name: None,
            task: "t".into(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            file_boundary: vec![],
            read_first: vec![],
            fork_session: false,
            model: None,
            review: None,
        };

        let nonempty = Directives {
            files: vec![("a.md".to_string(), "alpha".to_string())],
        };
        let spawn = build_spawn(
            ExoRole::Dev,
            ChildKind::Worktree,
            "dev",
            base_args(),
            &nonempty,
        )
        .unwrap();
        assert!(spawn.directives_hash.is_some());

        let empty = Directives::default();
        let spawn2 = build_spawn(
            ExoRole::Dev,
            ChildKind::Worktree,
            "dev",
            base_args(),
            &empty,
        )
        .unwrap();
        assert!(spawn2.directives_hash.is_none());
    }

    #[test]
    fn build_spawn_model_rides_the_spec() {
        let spawn = build_spawn(
            ExoRole::Tl,
            ChildKind::Worktree,
            "tl",
            SpawnArgs {
                name: None,
                task: "t".into(),
                steps: vec![],
                verify: vec![],
                done_criteria: vec![],
                context: None,
                boundary: vec![],
                file_boundary: vec![],
                read_first: vec![],
                fork_session: false,
                model: Some("opus".to_string()),
                review: None,
            },
            &Directives::default(),
        )
        .unwrap();
        assert_eq!(spawn.model_override.as_deref(), Some("opus"));
    }

    #[tokio::test]
    async fn fork_wave_preview_renders_without_spawning() {
        let mock = MockRuntime {
            dirty_files: vec!["?? junk.txt".into()],
            ..Default::default()
        };
        let args = ForkWaveArgs {
            children: vec![ForkChildArgs {
                name: Some("child-1".to_string()),
                task: "distinctive task text".to_string(),
                steps: vec![],
                verify: vec![],
                done_criteria: vec![],
                context: None,
                boundary: vec![],
                read_first: vec![],
                fork_session: false,
                model: None,
                review: None,
                file_boundary: vec![],
            }],
            preview: true,
        };
        let out = ForkWave::run(&mock, args).await.unwrap();
        assert!(out.text.contains("distinctive task text"));
        assert!(out.text.contains("ISOLATED git worktree"));
        let calls = mock.calls_made();
        assert!(!calls.iter().any(|c| matches!(c, Call::ForkWave { .. })));
        assert!(!calls.iter().any(|c| matches!(c, Call::Spawn { .. })));
        assert!(!calls.iter().any(|c| matches!(c, Call::FsWrite { .. })));
    }

    #[tokio::test]
    async fn model_none_is_byte_identical_to_no_override() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do something else".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        SpawnDev::run(&mock, args).await.unwrap();
        let calls = mock.calls_made();
        let task = calls
            .iter()
            .find_map(|c| match c {
                Call::Spawn { task, .. } => Some(task.clone()),
                _ => None,
            })
            .expect("spawn recorded");
        // No directives seeded, no model override — the task must be exactly the plain rendered
        // spec prompt, with no directives section appended.
        assert_eq!(
            task,
            render_spec_prompt("do something else", &[], &[], &[], &[], &[], None, &[])
        );
    }

    #[test]
    fn review_absent_from_json_parses_as_none() {
        let dev: SpawnDevArgs =
            serde_json::from_str(r#"{"name":"dev-1","task":"do work"}"#).unwrap();
        assert_eq!(dev.review, None);

        let child: ForkChildArgs =
            serde_json::from_str(r#"{"name":"child-1","task":"do work"}"#).unwrap();
        assert_eq!(child.review, None);
    }

    #[test]
    fn review_explicit_true_false_round_trips_through_json() {
        let dev: SpawnDevArgs =
            serde_json::from_str(r#"{"name":"dev-1","task":"t","review":true}"#).unwrap();
        assert_eq!(dev.review, Some(true));

        let dev: SpawnDevArgs =
            serde_json::from_str(r#"{"name":"dev-1","task":"t","review":false}"#).unwrap();
        assert_eq!(dev.review, Some(false));
    }

    #[tokio::test]
    async fn spawn_dev_review_override_rides_into_the_spawned_spec() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: Some(true),
            file_boundary: vec![],
        };
        SpawnDev::run(&mock, args).await.unwrap();
        // build_spawn threads `review` straight onto ExoSpawn.review_override — covered directly by
        // `build_spawn_review_override_rides_the_spec` below; here we only assert the tool call
        // succeeds end-to-end with the field set (MockRuntime's `Call::Spawn` doesn't record it).
        assert!(mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[test]
    fn build_spawn_review_override_rides_the_spec() {
        let base_args = |review| SpawnArgs {
            name: None,
            task: "t".into(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            file_boundary: vec![],
            read_first: vec![],
            fork_session: false,
            model: None,
            review,
        };

        let spawn = build_spawn(
            ExoRole::Dev,
            ChildKind::Worktree,
            "dev",
            base_args(Some(true)),
            &Directives::default(),
        )
        .unwrap();
        assert_eq!(spawn.review_override, Some(true));

        let spawn = build_spawn(
            ExoRole::Dev,
            ChildKind::Worktree,
            "dev",
            base_args(Some(false)),
            &Directives::default(),
        )
        .unwrap();
        assert_eq!(spawn.review_override, Some(false));

        let spawn = build_spawn(
            ExoRole::Dev,
            ChildKind::Worktree,
            "dev",
            base_args(None),
            &Directives::default(),
        )
        .unwrap();
        assert_eq!(spawn.review_override, None);
    }

    #[tokio::test]
    async fn spawn_dev_file_boundary_renders_as_allowed_paths_in_child_task() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec!["do not touch prod config".to_string()],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec!["rust/exo/src/tools/spawn.rs".to_string()],
        };
        SpawnDev::run(&mock, args).await.unwrap();
        let task = mock
            .calls_made()
            .iter()
            .find_map(|c| match c {
                Call::Spawn { task, .. } => Some(task.clone()),
                _ => None,
            })
            .expect("spawn recorded");
        assert!(task.contains("ALLOWED PATHS"));
        assert!(task.contains("- rust/exo/src/tools/spawn.rs"));
        assert!(task.contains("CONSTRAINTS:\n- do not touch prod config"));
    }

    #[tokio::test]
    async fn spawn_dev_writes_boundary_when_non_empty() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec!["rust/exo/src/tools/spawn.rs".to_string()],
        };
        SpawnDev::run(&mock, args).await.unwrap();
        let calls = mock.calls_made();
        let path = crate::boundary::boundary_path("dev-1")
            .display()
            .to_string();
        let write = calls
            .iter()
            .find(|c| matches!(c, Call::FsWrite { path: p } if p == &path))
            .expect("boundary file written");
        match write {
            Call::FsWrite { path } => {
                let bytes = mock.files.lock().unwrap().get(path).cloned().unwrap();
                let boundary: crate::boundary::FileBoundary =
                    serde_json::from_slice(&bytes).unwrap();
                assert_eq!(boundary.allowed, vec!["rust/exo/src/tools/spawn.rs"]);
            }
            _ => unreachable!(),
        }
    }

    #[tokio::test]
    async fn spawn_dev_omits_boundary_write_when_empty() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        SpawnDev::run(&mock, args).await.unwrap();
        let path = crate::boundary::boundary_path("dev-1")
            .display()
            .to_string();
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::FsWrite { path: p } if p == &path)));
    }

    #[tokio::test]
    async fn fork_wave_writes_boundary_per_child_when_non_empty() {
        let mock = MockRuntime::default();
        let args = ForkWaveArgs {
            children: vec![
                ForkChildArgs {
                    name: Some("child-1".to_string()),
                    task: "task 1".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec![],
                    fork_session: false,
                    model: None,
                    review: None,
                    file_boundary: vec!["rust/exo/src/tools".to_string()],
                },
                ForkChildArgs {
                    name: Some("child-2".to_string()),
                    task: "task 2".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec![],
                    fork_session: false,
                    model: None,
                    review: None,
                    file_boundary: vec![],
                },
            ],
            preview: false,
        };
        ForkWave::run(&mock, args).await.unwrap();
        let calls = mock.calls_made();
        let path1 = crate::boundary::boundary_path("child-1")
            .display()
            .to_string();
        let path2 = crate::boundary::boundary_path("child-2")
            .display()
            .to_string();
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::FsWrite { path: p } if p == &path1)));
        assert!(!calls
            .iter()
            .any(|c| matches!(c, Call::FsWrite { path: p } if p == &path2)));
    }

    #[tokio::test]
    async fn spawn_dev_refuses_untracked_read_first() {
        let mock = MockRuntime {
            untracked_paths: vec!["docs/missing.md".to_string()],
            ..Default::default()
        };
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec!["docs/missing.md".to_string()],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        let err = SpawnDev::run(&mock, args).await.unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("docs/missing.md"),
            "err should name the missing path: {msg}"
        );
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn spawn_dev_all_tracked_read_first_passes() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec!["README.md".to_string()],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        assert!(SpawnDev::run(&mock, args).await.is_ok());
    }

    #[tokio::test]
    async fn spawn_dev_empty_read_first_skips_tracked_check_even_when_paths_would_be_missing() {
        // untracked_paths is populated, but read_first is empty — nothing to check.
        let mock = MockRuntime {
            untracked_paths: vec!["docs/missing.md".to_string()],
            ..Default::default()
        };
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
            model: None,
            review: None,
            file_boundary: vec![],
        };
        assert!(SpawnDev::run(&mock, args).await.is_ok());
    }

    #[tokio::test]
    async fn spawn_dev_read_first_outside_boundary_warns_but_does_not_block() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec!["docs/other.md".to_string()],
            model: None,
            review: None,
            file_boundary: vec!["rust/exo/src".to_string()],
        };
        let out = SpawnDev::run(&mock, args).await.unwrap();
        assert!(out.text.contains("note: read_first outside file_boundary"));
        assert!(out.text.contains("docs/other.md"));
        assert!(mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn spawn_dev_read_first_inside_boundary_has_no_warning() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do work".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec!["rust/exo/src/tools/spawn.rs".to_string()],
            model: None,
            review: None,
            file_boundary: vec!["rust/exo/src".to_string()],
        };
        let out = SpawnDev::run(&mock, args).await.unwrap();
        assert!(!out.text.contains("note: read_first outside file_boundary"));
    }

    #[tokio::test]
    async fn fork_wave_refuses_all_or_nothing_on_untracked_read_first() {
        let mock = MockRuntime {
            untracked_paths: vec!["docs/missing.md".to_string()],
            ..Default::default()
        };
        let args = ForkWaveArgs {
            children: vec![
                ForkChildArgs {
                    name: Some("child-1".to_string()),
                    task: "task 1".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec!["docs/missing.md".to_string()],
                    fork_session: false,
                    model: None,
                    review: None,
                    file_boundary: vec![],
                },
                ForkChildArgs {
                    name: Some("child-2".to_string()),
                    task: "task 2".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec![],
                    fork_session: false,
                    model: None,
                    review: None,
                    file_boundary: vec![],
                },
            ],
            preview: false,
        };
        let err = ForkWave::run(&mock, args).await.unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("docs/missing.md"),
            "err should name the missing path: {msg}"
        );
        assert!(
            msg.contains("child-1"),
            "err should name the offending child: {msg}"
        );
        // All-or-nothing: neither child was spawned, even though child-2 had no read_first issue.
        assert!(!mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::ForkWave { .. } | Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn fork_wave_preview_reports_would_refuse_without_spawning() {
        let mock = MockRuntime {
            untracked_paths: vec!["docs/missing.md".to_string()],
            ..Default::default()
        };
        let args = ForkWaveArgs {
            children: vec![ForkChildArgs {
                name: Some("child-1".to_string()),
                task: "task 1".to_string(),
                steps: vec![],
                verify: vec![],
                done_criteria: vec![],
                context: None,
                boundary: vec![],
                read_first: vec!["docs/missing.md".to_string()],
                fork_session: false,
                model: None,
                review: None,
                file_boundary: vec![],
            }],
            preview: true,
        };
        let out = ForkWave::run(&mock, args).await.unwrap();
        assert!(out.text.contains("WOULD REFUSE"));
        assert!(out.text.contains("docs/missing.md"));
        let calls = mock.calls_made();
        assert!(!calls.iter().any(|c| matches!(c, Call::ForkWave { .. })));
        assert!(!calls.iter().any(|c| matches!(c, Call::Spawn { .. })));
    }

    #[tokio::test]
    async fn fork_wave_read_first_outside_boundary_warns_in_result_text() {
        let mock = MockRuntime::default();
        let args = ForkWaveArgs {
            children: vec![ForkChildArgs {
                name: Some("child-1".to_string()),
                task: "task 1".to_string(),
                steps: vec![],
                verify: vec![],
                done_criteria: vec![],
                context: None,
                boundary: vec![],
                read_first: vec!["docs/other.md".to_string()],
                fork_session: false,
                model: None,
                review: None,
                file_boundary: vec!["rust/exo/src".to_string()],
            }],
            preview: false,
        };
        let out = ForkWave::run(&mock, args).await.unwrap();
        assert!(out.text.contains("note: read_first outside file_boundary"));
        assert!(out.text.contains("docs/other.md"));
        assert!(mock
            .calls_made()
            .iter()
            .any(|c| matches!(c, Call::ForkWave { .. })));
    }
}
