//! `impl Spawner for Runtime` — the recursion (birth + teardown). **Race-prone; built by
//! the Spawner sub-TL, decomposed S1/S2/S3 — never one leaf.**
//!
//! Per-op methods each fix their own `(role, agent_type, kind)`; the spec carries only
//! task content. All three ops funnel through one private `birth(BirthCore)` tail:
//!   append `AgentSpawned` record FIRST (so there's never an untracked process)
//!   → (`git worktree add` for a Worktree child — Inline shares the parent's cwd)
//!   → `tmux new_pane`
//!   → write child papers (`node.json`, incl. `parent_inbox` = my inbox)
//!   → launch `exo node --papers <node.json>` in the pane.
//!
//! Decomposition:
//!   - **S1**: safe branch-gen (`Branch::from_path`) + `git worktree add` (Worktree only).
//!   - **S2**: the `birth(BirthCore)` core (record-first ordering is the load-bearing race
//!     guard — log intent before the pane exists).
//!   - **S3**: teardown — `reclaim_worktree` (`git worktree remove`, parent-side at
//!     convergence) + force `kill_pane`.
//!
//! HARD RULE: `tokio::process`/`spawn_blocking`; reuse `Git`/`Tmux` cap impls + the
//! exomonad-core `GitWorktreeService`/`TmuxIpc` — do not re-shell git/tmux by hand where a
//! cap already does it.
//!
//! ## Record-first ordering — the load-bearing race guard (read before editing `birth`)
//!
//! The frozen [`ChildRecord::Spawned`] stores the child's `pane` id, yet the parent must
//! log the spawn **before** an *agent* process exists (so a parent crash never leaves an
//! untracked agent). A pane id doesn't exist until tmux creates the pane — so these are
//! reconciled by **two-phase pane creation**:
//!
//!   1. (Worktree only) `git worktree add` — the child's dir.
//!   2. `Tmux::new_pane(cwd, $SHELL)` — a **holding shell**, NOT the agent. Returns `%N`.
//!   3. Append `Spawned { child, kind, pane: %N, inbox }` to `children.jsonl`. ← THE GUARD.
//!   4. Write the child's `node.json` papers (`parent_inbox` = *my* inbox).
//!   5. `Tmux::paste(%N, "<launch cmd>\n")` — inject the agent command into the holding
//!      shell, starting `claude` (+ its `exo node` sidecar via .mcp.json).
//!
//! The record precedes the **agent** launch (step 3 before step 5). The holding shell
//! (step 2) carries no agent, so a crash before step 5 leaves only a bare shell — nothing
//! untracked. **Do not collapse steps 2+5 into a one-shot `new_pane(cwd, launch_cmd)`** —
//! that reopens the orphan window the two-phase split closes.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{
    fold_children, AgentName, AgentType, Branch, Child, ChildKind, ChildRecord, FsError, InboxPath,
    NodePapers, PaneId, RoleKind, RoleRecord, SpawnError, SpawnSpec, Spawner,
};
use std::collections::BTreeMap;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::io::AsyncWriteExt;

/// Max attempts for a best-effort teardown op (`reclaim_worktree` / `kill_pane`). Bounded —
/// never an unbounded loop. A transient tmux/git hiccup (a pane still settling, a lock briefly
/// held) usually clears within a couple of tries.
const MAX_TEARDOWN_ATTEMPTS: u32 = 3;
/// Linear backoff base between teardown retries (`base * attempt`).
const TEARDOWN_BACKOFF_BASE: Duration = Duration::from_millis(150);

/// Read `$HOME` for deriving a child's inbox/papers/settings path. Behavior-preserving fallback to
/// `"."`, but NOT silent: bootstrap hard-fails on a missing `$HOME` precisely because a `"."`
/// fallback derives an inbox path the parent never writes (the child then silently receives
/// nothing). We can't hard-fail here without a signature change, so at least make the fallback
/// loud — a `warn!` naming the call site so the wrong-path symptom is diagnosable.
fn home_or_warn(site: &str) -> String {
    std::env::var("HOME").unwrap_or_else(|_| {
        tracing::warn!(
            site,
            "$HOME unset; falling back to '.' for path derivation — the child's inbox/papers path \
             may not match what the parent writes, and the child may silently receive nothing"
        );
        ".".to_string()
    })
}

/// Run a best-effort teardown op with **bounded** retry + linear backoff. Each transient failure
/// is logged at `warn`; a final failure after [`MAX_TEARDOWN_ATTEMPTS`] is logged LOUD and
/// structured (`op` + `child` + `attempts`) and the last error is returned.
///
/// **Semantics stay best-effort.** This helper only retries-then-surfaces — it never panics and
/// never escalates. The caller is responsible for keeping a returned `Err` non-fatal to the
/// merge/teardown flow (it logs and proceeds; a lingering worktree/pane self-heals via the
/// liveness reap and auto-incrementing child names).
pub async fn retry_teardown<T, E, F, Fut>(
    op: &'static str,
    child: &str,
    mut attempt: F,
) -> Result<T, E>
where
    E: std::fmt::Display,
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<T, E>>,
{
    let mut last_err: Option<E> = None;
    for n in 1..=MAX_TEARDOWN_ATTEMPTS {
        match attempt().await {
            Ok(v) => {
                if n > 1 {
                    tracing::info!(op, child, attempt = n, "teardown op succeeded on retry");
                }
                return Ok(v);
            }
            Err(e) => {
                tracing::warn!(
                    op,
                    child,
                    attempt = n,
                    max = MAX_TEARDOWN_ATTEMPTS,
                    "teardown op failed: {e}"
                );
                last_err = Some(e);
                if n < MAX_TEARDOWN_ATTEMPTS {
                    tokio::time::sleep(TEARDOWN_BACKOFF_BASE * n).await;
                }
            }
        }
    }
    let err = last_err.expect("retry loop runs at least once");
    tracing::error!(
        op,
        child,
        attempts = MAX_TEARDOWN_ATTEMPTS,
        "teardown FAILED after {MAX_TEARDOWN_ATTEMPTS} attempts (best-effort — flow continues): {err}"
    );
    Err(err)
}

/// Placeholder `ANTHROPIC_AUTH_TOKEN` for a profiled child whose endpoint needs no real key (a local
/// proxy holds the OAuth). Claude requires a **non-empty** token whenever `ANTHROPIC_BASE_URL` is set,
/// so the runtime supplies this when the launch profile carries none — the operator never writes a
/// dummy value into config.
const DEFAULT_PROXY_TOKEN: &str = "exo-local-proxy";

/// A per-role launch redirect resolved from `{prefix}_*` env at `Spawner::spawn`. Points a child's
/// Claude at a non-default Anthropic-compatible endpoint + model (e.g. a local
/// [`claude-code-proxy`](https://github.com/raine/claude-code-proxy) serving Kimi). Runtime-internal
/// and **backend-agnostic** — the optional `auth_token` lives in memory only and is never written to
/// papers; only the non-secret `label` is recorded (window + `tree`).
#[derive(Debug, Clone)]
pub(crate) struct LaunchProfile {
    pub base_url: Option<String>,
    pub model: Option<String>,
    pub auth_token: Option<String>,
    pub label: Option<String>,
}

impl LaunchProfile {
    /// Resolve a role's launch profile from a `{prefix}_*` env lookup. Returns `None` when the role
    /// declares no prefix OR no `{prefix}_BASE_URL` is set — `BASE_URL` is the defining field of a
    /// redirect, so a half-set env (only `MODEL`/`LABEL`, no endpoint) must NOT half-activate it. The
    /// `auth_token` is optional (a local proxy holds the OAuth; the placeholder is supplied at
    /// translation). `getenv` is injected so the gating logic is testable without mutating the
    /// process environment.
    fn resolve(prefix: Option<&str>, getenv: impl Fn(&str) -> Option<String>) -> Option<Self> {
        let prefix = prefix?;
        let base_url = getenv(&format!("{prefix}_BASE_URL"))?;
        Some(LaunchProfile {
            base_url: Some(base_url),
            model: getenv(&format!("{prefix}_MODEL")),
            auth_token: getenv(&format!("{prefix}_AUTH_TOKEN")),
            label: getenv(&format!("{prefix}_LABEL")),
        })
    }
}

/// The fixed triple + identity each op hands to the shared `birth` tail. Constructed by
/// the per-op method (the single place a triple is named); `birth` branches only on `kind`.
#[derive(Debug, Clone)]
pub(crate) struct BirthCore {
    pub kind: ChildKind,
    pub agent_type: AgentType,
    /// The child's role, erased ([`RoleRecord`]) — birth writes it straight into papers, so the
    /// birth tail stays non-generic over the domain role (the domain tool already fixed it).
    pub role: RoleRecord,
    pub name: AgentName,
    pub branch: Branch,
    pub task: String,
    /// The child's resolved role-steering protocol (override-or-const), passed to Claude via
    /// `--append-system-prompt`. Empty ⇒ no steering injected.
    pub protocol: String,
    /// The child's `--model` (from [`RoleKind::model`](exo_caps::RoleKind)). `None` ⇒ inherit the
    /// launcher's default model. `exo` pins its leaf roles (dev/worker/reviewer) to `sonnet`.
    pub model: Option<String>,
    /// Optional per-role launch redirect (from [`RoleKind::launch_profile_env_prefix`]): point this
    /// child's Claude at a non-default Anthropic-compatible endpoint/model (e.g. a local proxy
    /// serving Kimi). `None` ⇒ default Claude launch. Carries the auth token **in memory only**.
    pub launch_profile: Option<LaunchProfile>,
    /// Opt-in context inheritance. When true AND this is a Claude worktree child, the
    /// launch resolves the parent's Claude session UUID (via `exo-scry`) and starts the
    /// child with `--resume --fork-session <uuid>`. Set only by `fork_wave` (from
    /// `ForkSpec::fork_session`); false for every other op. Default-false keeps launch
    /// byte-identical unless explicitly opted in.
    pub fork_session: bool,
}

// ── Shared ledger + inbox-scheme helpers (Spawner-TL scaffold) ───────────────────────────
// Used by S2 (`birth` appends `Spawned`) and S3 (`reclaim_worktree`/`kill_pane` read+fold).
// The ledger helpers are self-contained here. The inbox-path scheme is the canonical
// `exo_caps::paths::inbox_path` (see `child_inbox_path`); `Bus` resolution reads stored
// `InboxPath`s off the ledger/papers, so the derivation lives in exactly one place.
impl Runtime {
    /// This node's parent-local child ledger (`{working_dir}/.exo/children.jsonl`).
    pub(crate) fn children_log_path(&self) -> PathBuf {
        self.working_dir.join(".exo/children.jsonl")
    }

    /// Append one lifecycle record. **Single-writer** (this node owns its ledger), so a
    /// plain `append` is race-free — none of the multi-writer-bus PIPE_BUF dance applies.
    pub(crate) async fn append_child_record(&self, rec: &ChildRecord) -> Result<(), SpawnError> {
        let path = self.children_log_path();
        if let Some(dir) = path.parent() {
            tokio::fs::create_dir_all(dir).await?;
        }
        let mut line = serde_json::to_string(rec).map_err(|e| SpawnError::Failed {
            op: "record_encode",
            child: Some(rec.child().clone()),
            detail: e.to_string(),
        })?;
        line.push('\n');
        let mut f = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
            .await?;
        f.write_all(line.as_bytes()).await?;
        f.sync_all().await?;
        Ok(())
    }

    /// Read + parse the child ledger. **Tolerant of malformed lines** — a torn last record
    /// from a crash mid-append must not block delivery to EVERY child, only fail if the
    /// *target* child can't be found. Mirrors the inbound loop's tolerant parse.
    /// A missing file means no children yet → empty, not an error.
    pub(crate) async fn read_child_records(&self) -> Result<Vec<ChildRecord>, SpawnError> {
        // An inline node shares the parent's worktree, so `children_log_path()` points at the
        // parent's ledger. Inline nodes have no spawn tools and genuinely never have children;
        // reading here would surface the parent's children as phantoms (causing shutdown to defer
        // and the idle gate to misread). Return empty unconditionally instead.
        if self.is_inline() {
            return Ok(Vec::new());
        }
        let path = self.children_log_path();
        let data = match exo_caps::Fs::read(self, &path).await {
            Ok(d) => d,
            Err(FsError::At { source, .. } | FsError::Io(source))
                if source.kind() == std::io::ErrorKind::NotFound =>
            {
                return Ok(Vec::new())
            }
            Err(FsError::At { source, .. } | FsError::Io(source)) => return Err(source.into()),
        };

        let mut records = Vec::new();
        for line in data.split(|&b| b == b'\n') {
            if line.is_empty() {
                continue;
            }
            match serde_json::from_slice::<ChildRecord>(line) {
                Ok(record) => records.push(record),
                Err(e) => {
                    tracing::warn!("skipping malformed children.jsonl line: {e}");
                }
            }
        }
        Ok(records)
    }

    /// Read + parse the child ledger, yielding the folded child set.
    pub(crate) async fn read_children(&self) -> Result<BTreeMap<AgentName, Child>, SpawnError> {
        let records = self.read_child_records().await?;
        Ok(fold_children(&records))
    }

    /// The child's OWN ingestion inbox, derived from its pane + the run-id namespace:
    /// `~/.claude/exo/inboxes/{run_id}/pane-{N}.jsonl`.
    /// Stored in the child's `Spawned` record so the parent can address DOWN to it.
    pub(crate) fn child_inbox_path(&self, pane: &PaneId) -> InboxPath {
        let home = home_or_warn("child_inbox_path");
        exo_caps::paths::inbox_path(Path::new(&home), &self.run_id, pane)
    }

    /// This node's child-launch policy (`yolo`, `wrap_nix`), read from its own papers and
    /// inherited down the tree: `birth` stamps it onto each child's papers and uses it in
    /// the launch command. A node whose papers can't be read (or whose papers predate these
    /// fields — they default on parse) falls back to [`NodePapers`]' behavior-preserving
    /// defaults, so the root and any older node launch children exactly as before.
    /// Papers live at `{working_dir}/.exo/node.json` for a worktree node, or the
    /// run-namespaced `root.json` for the root.
    async fn own_launch_policy(&self) -> (bool, bool) {
        let candidates = [
            self.working_dir.join(".exo/node.json"),
            self.working_dir
                .join(format!(".exo/node/{}/root.json", self.run_id)),
        ];
        for path in candidates {
            match exo_caps::Fs::read(self, &path).await {
                Ok(bytes) => match serde_json::from_slice::<NodePapers>(&bytes) {
                    Ok(p) => return (p.yolo, p.wrap_nix),
                    Err(e) => {
                        tracing::warn!("own papers parse failed ({}): {e}", path.display());
                    }
                },
                Err(_) => continue,
            }
        }
        (NodePapers::DEFAULT_YOLO, NodePapers::DEFAULT_WRAP_NIX)
    }

    /// Resolve a role's steering protocol: the optional on-disk override
    /// (`{working_dir}/.exo/roles/devswarm/context/{role}.md`) if it exists, else the domain's
    /// baked-in const (`RoleKind::protocol`). The compiled const is the source of truth; the file
    /// just overrides it during prompt-tuning. Mirrors the same resolution `exo-node`'s
    /// SessionStart hook applies — the resolved protocol is passed to the child Claude via
    /// `--append-system-prompt`.
    async fn resolve_protocol(&self, role_str: &str, baked: &str) -> String {
        let path = self
            .working_dir
            .join(format!(".exo/roles/devswarm/context/{role_str}.md"));
        match exo_caps::Fs::read(self, &path).await {
            Ok(bytes) => match String::from_utf8(bytes) {
                Ok(s) => s,
                Err(e) => {
                    tracing::warn!(
                        "protocol override {} is not UTF-8 ({e}); using the baked-in const",
                        path.display()
                    );
                    baked.to_string()
                }
            },
            Err(_) => baked.to_string(),
        }
    }

    pub(crate) async fn resolve_child_name(
        &self,
        given: Option<AgentName>,
        prefix: &str,
    ) -> Result<AgentName, SpawnError> {
        let current_set = self.read_children().await?;

        if let Some(name) = given {
            if current_set.contains_key(&name) {
                return Err(SpawnError::Failed {
                    op: "spawn",
                    child: Some(name),
                    detail: "duplicate child name".into(),
                });
            }
            Ok(name)
        } else {
            let mut i = 0;
            loop {
                let name = AgentName::new(format!("{}-{}", prefix, i)).unwrap();
                if !current_set.contains_key(&name) {
                    return Ok(name);
                }
                i += 1;
            }
        }
    }
}

impl Runtime {
    /// The shared birth tail. **S2.** Record-first, then pane, then papers, then launch.
    ///
    /// Birth acquires two external resources — a git worktree (Worktree kind) and a tmux pane —
    /// then *fills* them (record → papers → launch). Cleanup can't ride `Drop` (it's async), so
    /// rollback is explicit **compensation**: each resource has one acquire and one best-effort,
    /// logged release ([`birth_rollback`](Self::birth_rollback)), and a mid-birth failure releases
    /// in reverse (pane, then worktree). The fill phase is grouped into
    /// [`birth_finish`](Self::birth_finish) so there is a single rollback site for it. The
    /// append-only `children.jsonl` `Spawned` record is deliberately NOT compensated — it's
    /// event-sourced, and a stale record self-heals via the liveness (ghost-spawn) reap and
    /// auto-incrementing child names.
    pub(crate) async fn birth(&self, core: BirthCore) -> Result<AgentName, SpawnError> {
        // (a) compute child worktree path
        let child_dir = match core.kind {
            ChildKind::Worktree => self
                .working_dir
                .join(".exo/worktrees")
                .join(core.name.as_str()),
            ChildKind::Inline => self.working_dir.to_path_buf(),
        };

        // (b) acquire the worktree (Worktree kind only).
        if core.kind == ChildKind::Worktree {
            exo_caps::Git::worktree_add(self, &core.branch, &child_dir)
                .await
                .map_err(|e| SpawnError::Failed {
                    op: "worktree_add",
                    child: Some(core.name.clone()),
                    detail: e.to_string(),
                })?;
        }

        // (c) acquire a holding-shell pane (NOT the agent yet). A Worktree child gets its own
        // window (tab — one agent per window, the triad); an Inline worker gets a split pane.
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/bash".into());
        // Name the window after the agent (emoji + slug), not the bare `claude`/shell process.
        let emoji = match core.agent_type {
            AgentType::Claude => "🤖",
            AgentType::Shoal => "🌊",
        };
        // A launch-profiled child (e.g. a Kimi reviewer) is still a 🤖 Claude process; tag the
        // window with its model label so the heterogeneity is legible at a glance.
        let window_name = match core
            .launch_profile
            .as_ref()
            .and_then(|p| p.label.as_deref())
        {
            Some(label) => format!("{} {} ({})", emoji, core.name.as_str(), label),
            None => format!("{} {}", emoji, core.name.as_str()),
        };
        let pane = match core.kind {
            ChildKind::Worktree => {
                exo_caps::Tmux::new_window(self, &window_name, &child_dir, &shell).await
            }
            ChildKind::Inline => exo_caps::Tmux::new_pane(self, &child_dir, &shell).await,
        };
        let pane = match pane {
            Ok(p) => p,
            Err(e) => {
                // Only the worktree was acquired — release it.
                self.birth_rollback(&core, &child_dir, None).await;
                return Err(SpawnError::Failed {
                    op: "new_pane",
                    child: Some(core.name.clone()),
                    detail: e.to_string(),
                });
            }
        };

        // (d–f) fill the worktree+pane: record → papers → launch. On any failure, compensate
        // (kill the pane, remove the worktree) before surfacing the error.
        if let Err(e) = self.birth_finish(&core, &child_dir, &pane).await {
            self.birth_rollback(&core, &child_dir, Some(&pane)).await;
            return Err(e);
        }

        // A freshly launched child starts working — seed its busy bit. The idle gate
        // (`ChildLiveness`) combines this with pane-liveness, so a child that later dies without
        // ever reporting `ChildIdle` still reads idle via its dead pane.
        self.mark_child_busy(&core.name);

        Ok(core.name)
    }

    /// Best-effort compensation for a failed [`birth`]: release acquired resources in reverse
    /// (pane, then worktree). Logged, never fatal — a rollback failure must not mask the original
    /// error. The `children.jsonl` record is intentionally not undone (see [`birth`]).
    async fn birth_rollback(&self, core: &BirthCore, child_dir: &Path, pane: Option<&PaneId>) {
        if let Some(p) = pane {
            if let Err(e) = exo_caps::Tmux::kill_pane(self, p).await {
                tracing::warn!(
                    "birth rollback: kill_pane failed for {}: {e}",
                    core.name.as_str()
                );
            }
        }
        if core.kind == ChildKind::Worktree {
            if let Err(e) = exo_caps::Git::worktree_remove(self, child_dir).await {
                tracing::warn!(
                    "birth rollback: worktree_remove failed for {}: {e}",
                    core.name.as_str()
                );
            }
        }
    }

    /// The fill phase, steps (d)–(f): record-first, then papers, then launch. Extracted so
    /// [`birth`] has a single rollback site for everything after the pane is acquired.
    async fn birth_finish(
        &self,
        core: &BirthCore,
        child_dir: &Path,
        pane: &PaneId,
    ) -> Result<(), SpawnError> {
        // (d) RECORD FIRST — before launching the agent (the load-bearing race guard: the record
        // precedes the *agent*, so a crash never leaves an untracked agent — the pane here is a
        // bare holding shell).
        let inbox = self.child_inbox_path(pane);
        let record = ChildRecord::Spawned {
            child: core.name.clone(),
            kind: core.kind,
            pane: pane.clone(),
            inbox,
            // Non-secret cosmetic tag (e.g. "kimi") so the `tree` tool can show it; never the token.
            model_label: core.launch_profile.as_ref().and_then(|p| p.label.clone()),
        };
        self.append_child_record(&record).await?;

        // (e) Write child papers. The child inherits this node's launch policy (yolo /
        // wrap_nix), so it stamps the same onto its own children — config set on one node
        // flows down its whole subtree.
        let parent_inbox = Some(self.own_inbox());
        let (yolo, wrap_nix) = self.own_launch_policy().await;

        // Struct literal (not `NodePapers::new`, which takes a *typed* role): the role is already
        // erased into a `RoleRecord` on `BirthCore`, so birth stays non-generic over the domain role.
        let papers = NodePapers {
            v: NodePapers::VERSION,
            path: self.node_path().child(&core.name),
            branch: core.branch.clone(),
            role: core.role.clone(),
            pane: pane.clone(),
            parent_inbox,
            yolo,
            wrap_nix,
            kind: core.kind,
        };

        let papers_path = match core.kind {
            ChildKind::Worktree => child_dir.join(".exo/node.json"),
            ChildKind::Inline => {
                let home = home_or_warn("inline_papers_path");
                exo_caps::paths::papers_path(Path::new(&home), &self.run_id, pane)
            }
        };

        // The node's private CC config files (siblings of its papers). `claude` is pointed at them
        // via `--settings`/`--mcp-config` so we NEVER write the shared cwd's config — the inline-worker
        // clobber + the `.mcp.json`-in-repo footgun both vanish (an inline worker's papers live under
        // `~/.claude/exo/papers/`, outside the shared cwd).
        let (settings_path, mcp_config_path) = exo_caps::paths::node_config_paths(&papers_path);

        let papers_json = serde_json::to_vec_pretty(&papers).map_err(|e| SpawnError::Failed {
            op: "serialize_papers",
            child: Some(core.name.clone()),
            detail: e.to_string(),
        })?;
        // Atomic (temp + rename, parent dirs created) via the `Fs` supertrait — the child's
        // bootstrap must never read half-written papers.
        exo_caps::Fs::write_atomic(self, &papers_path, &papers_json)
            .await
            .map_err(|e| SpawnError::Failed {
                op: "write_papers",
                child: Some(core.name.clone()),
                detail: e.to_string(),
            })?;

        // The node's spec (the reviewer's acceptance bar) is persisted to `.exo/acceptance.md` by
        // the spawning DOMAIN tool via the `Fs` cap, NOT here — the runtime no longer knows the
        // review-gate's filename (that domain concept moved out with the Spawner collapse).

        // (f) Launch the agent via exomonad's shared launch builder (reuse over reinvent):
        // the prompt goes in a file (.exo/tmp), never inline — so a multi-line/quote-bearing
        // task can't break shell parsing — and the CLI/flags are the proven ones. The node
        // self-IDs from its papers (.mcp.json → `experimental node --papers`); the only env it
        // needs is the boot context its bootstrap reads, set explicitly (not via inherited
        // session env). `yolo` / `wrap_nix` come from this node's inherited launch policy
        // (see `own_launch_policy`); the defaults launch plain (no nix wrap), like the root.
        let mut env_vars: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        env_vars.insert("EXOMONAD_SWARM_RUN_ID".into(), self.run_id.clone());
        env_vars.insert("EXOMONAD_TMUX_SESSION".into(), self.tmux_session.clone());

        // Propagate every role's launch-profile config DOWN the tree, so a deep node that spawns a
        // profiled child (e.g. a dev calling `submit_branch` → reviewer) still carries it. Opaque
        // `EXO_*_{BASE_URL,MODEL,AUTH_TOKEN,LABEL}` — NEVER `ANTHROPIC_*` — so a non-profiled child
        // is never redirected. Pattern-matched, so a new role/backend needs no edit here.
        const PROFILE_SUFFIXES: [&str; 4] = ["_BASE_URL", "_MODEL", "_AUTH_TOKEN", "_LABEL"];
        for (k, v) in std::env::vars() {
            if k.starts_with("EXO_") && PROFILE_SUFFIXES.iter().any(|s| k.ends_with(s)) {
                env_vars.insert(k, v);
            }
        }

        let agent_type = match core.agent_type {
            AgentType::Claude => {
                crate::node_config::write_node_agent_config(
                    &settings_path,
                    &mcp_config_path,
                    &papers_path,
                )
                .await
                .map_err(|e| SpawnError::Failed {
                    op: "write_node_agent_config",
                    child: Some(core.name.clone()),
                    detail: e.to_string(),
                })?;
                // Enable Claude Code Teams so the Bus→Teams last hop (dispatch.rs) can
                // deliver as a native `<teammate-message>` instead of falling back to paste.
                // Worktree children only — inline workers share the parent's worktree and cwd,
                // so their Teams resolution would land in the parent's team (the leak this PR fixes).
                if core.kind == ChildKind::Worktree {
                    env_vars.insert("CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS".into(), "1".into());
                }

                // Launch-profiled child only: translate the opaque profile to ANTHROPIC_* on THIS
                // claude (the token stays in memory — never in papers). The model is applied below
                // via claude_flags. A single-model proxy (kimi-for-coding) needs the background /
                // small-fast calls mapped to the same model too, or they 404.
                if let Some(p) = &core.launch_profile {
                    if let Some(url) = &p.base_url {
                        env_vars.insert("ANTHROPIC_BASE_URL".into(), url.clone());
                    }
                    // Claude needs a non-empty token whenever a base_url is set; supply a placeholder
                    // when the profile carries none (a local proxy holds the real OAuth).
                    env_vars.insert(
                        "ANTHROPIC_AUTH_TOKEN".into(),
                        p.auth_token
                            .clone()
                            .unwrap_or_else(|| DEFAULT_PROXY_TOKEN.to_string()),
                    );
                    if let Some(m) = &p.model {
                        env_vars.insert("ANTHROPIC_SMALL_FAST_MODEL".into(), m.clone());
                    }
                }
                exomonad_shared::services::agent_control::AgentType::Claude
            }

            AgentType::Shoal => {
                return Err(SpawnError::Failed {
                    op: "launch",
                    child: Some(core.name.clone()),
                    detail: "Shoal is not spawnable as a tree child".into(),
                })
            }
        };

        let preamble = match core.kind {
            ChildKind::Worktree => format!(
                "You are working in an ISOLATED git worktree at `{}` — this is your repo root. ALL file \
                 paths are relative to it. Do NOT read or write files outside this directory (never touch \
                 the parent repository). Commit your work to your branch here.\n\n",
                child_dir.display()
            ),
            ChildKind::Inline => format!(
                "You are working in the repository at `{}`. ALL file paths are relative to it. \
                 Do NOT read or write files outside this directory.\n\n",
                child_dir.display()
            ),
        };

        let worktree_prompt = format!("{}{}", preamble, core.task);

        let prompt_file = exomonad_shared::services::agent_control::launch::write_prompt_file(
            child_dir,
            core.name.as_str(),
            &worktree_prompt,
        )
        .await
        .map_err(|e| SpawnError::Failed {
            op: "write_prompt_file",
            child: Some(core.name.clone()),
            detail: e.to_string(),
        })?;

        // Opt-in context inheritance: ONLY a Claude worktree child with fork_session set.
        // Resolve the parent's Claude session UUID from live OS state (exo-scry — observed,
        // no registry) and link the child's Claude project dir so --fork-session can find
        // the parent's sessions. Any miss (no team / no uuid / resolution error) falls back
        // to a fresh launch (None) — never crashes, never blocks the spawn.
        let fork_session_id: Option<String> = if core.fork_session
            && core.agent_type == AgentType::Claude
            && core.kind == ChildKind::Worktree
        {
            if let Err(e) =
                exomonad_shared::services::agent_control::fork_session::link_parent_project_dir(
                    &self.working_dir,
                    child_dir,
                )
            {
                tracing::warn!(
                    "fork_session: link_parent_project_dir failed for {}: {e}",
                    core.name.as_str()
                );
            }
            match exo_scry::resolve_self_or_portable() {
                Ok(Some(team)) => match team.lead_session_id {
                    Some(uuid) => Some(uuid),
                    None => {
                        tracing::warn!(
                            "fork_session requested for {} but active team has no lead_session_id; launching fresh",
                            core.name.as_str()
                        );
                        None
                    }
                },
                Ok(None) => {
                    tracing::warn!(
                        "fork_session requested for {} but no active team resolved; launching fresh",
                        core.name.as_str()
                    );
                    None
                }
                Err(e) => {
                    tracing::warn!(
                        "fork_session requested for {} but team resolution failed ({e}); launching fresh",
                        core.name.as_str()
                    );
                    None
                }
            }
        } else {
            None
        };

        // The launch model: a launch profile's model (e.g. kimi-for-coding) wins over the role's
        // default (`exo` pins leaves to sonnet); `None` inherits the launcher's default. Threaded
        // via the Claude-spawn flags bag → `build_agent_command`'s `--model`.
        let launch_model = core
            .launch_profile
            .as_ref()
            .and_then(|p| p.model.clone())
            .or_else(|| core.model.clone());
        // Always pass Claude flags: besides the optional `--model`, they carry the node's private
        // config-file paths so the launch emits `--settings`/`--mcp-config` (never the cwd config).
        let claude_flags = exomonad_shared::services::agent_control::ClaudeSpawnFlags {
            model: launch_model,
            settings_path: Some(settings_path.to_string_lossy().into_owned()),
            mcp_config_path: Some(mcp_config_path.to_string_lossy().into_owned()),
            ..Default::default()
        };

        // The protocol string is passed via --append-system-prompt for Claude.
        let launch_cmd = format!(
            "{}\n",
            exomonad_shared::services::agent_control::launch::build_agent_command(
                agent_type,
                Some(&prompt_file),
                fork_session_id.as_deref(),
                &env_vars,
                child_dir,           // cwd (flake detection for wrap_nix)
                Some(&claude_flags), // claude_flags (--model + private config paths)
                yolo,                // yolo (inherited launch policy)
                wrap_nix,            // wrap_nix: nix develop wrap (inherited launch policy)
                Some(&core.protocol),
            )
        );

        exo_caps::Tmux::paste(self, pane, &launch_cmd)
            .await
            .map_err(|e| SpawnError::Failed {
                op: "launch",
                child: Some(core.name.clone()),
                detail: e.to_string(),
            })?;

        Ok(())
    }
}

#[async_trait]
impl Spawner for Runtime {
    async fn spawn<S: SpawnSpec>(&self, spec: S) -> Result<AgentName, SpawnError> {
        let role = spec.role();
        let kind = spec.child_kind();
        let fork_session = spec.fork_session();
        let prefix = spec.name_prefix().to_string();
        let name = self.resolve_child_name(spec.name(), &prefix).await?;
        // The branch: a Worktree child gets its own (safe-generated from its tree address); an
        // Inline child shares the parent's worktree + branch. The agent backend is the role→backend
        // mapping the domain owns (via `RoleKind`); the role itself is recorded erased.
        let branch = match kind {
            ChildKind::Worktree => Branch::from_path(&self.node_path().child(&name)),
            ChildKind::Inline => self.branch().clone(),
        };
        let agent_type = RoleKind::agent_type(&role);
        // The role's launch model (`exo` pins leaves to sonnet); resolved while the role is typed.
        let model = RoleKind::model(&role).map(|m| m.to_string());
        // The role's optional launch profile: resolve `{prefix}_*` from this node's own env while
        // the role is typed. The token lives only here + in the child's launch env (never papers).
        let launch_profile =
            LaunchProfile::resolve(RoleKind::launch_profile_env_prefix(&role), |k| {
                std::env::var(k).ok()
            });
        // Resolve the child's role-steering protocol (override-or-const) while the role is still
        // typed. Threaded onto `BirthCore` and passed to Claude via `--append-system-prompt`.
        let protocol = self
            .resolve_protocol(role.role_str(), role.protocol())
            .await;
        let role = RoleRecord::new(&role).map_err(|e| SpawnError::Failed {
            op: "role_record",
            child: Some(name.clone()),
            detail: e.to_string(),
        })?;
        // The spec carries the fully-rendered prompt (the domain tool rendered it); birth wraps it
        // in the worktree/inline preamble.
        let task = spec.into_task();
        let core = BirthCore {
            kind,
            agent_type,
            role,
            branch,
            name,
            task,
            protocol,
            model,
            launch_profile,
            fork_session,
        };
        self.birth(core).await
    }

    async fn reclaim_worktree(&self, child: &AgentName) -> Result<(), SpawnError> {
        let current_set = self.read_children().await?;
        let record = current_set.get(child).ok_or_else(|| SpawnError::Failed {
            op: "reclaim_worktree",
            child: Some(child.clone()),
            detail: "unknown child".into(),
        })?;

        match record.kind {
            ChildKind::Worktree => {
                let base_path = self.working_dir.join(".exo/worktrees").join(child.as_str());

                // Find all nested worktrees.
                let mut stack = vec![base_path.clone()];
                let mut to_remove = vec![];
                while let Some(dir) = stack.pop() {
                    to_remove.push(dir.clone());
                    let w_dir = dir.join(".exo/worktrees");
                    if let Ok(mut entries) = tokio::fs::read_dir(&w_dir).await {
                        while let Ok(Some(entry)) = entries.next_entry().await {
                            if let Ok(ft) = entry.file_type().await {
                                if ft.is_dir() {
                                    stack.push(entry.path());
                                }
                            }
                        }
                    }
                }

                // Innermost first. Removal is the `Git` supertrait's `worktree_remove`
                // (force/reclaim semantics: the directory's state is discarded, the branch
                // ref — a reviewer's committed fixup — survives).
                for path in to_remove.into_iter().rev() {
                    let child_name = path
                        .file_name()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_string();

                    let res = retry_teardown("reclaim_worktree", &child_name, || {
                        exo_caps::Git::worktree_remove(self, &path)
                    })
                    .await;

                    if let Err(e) = res {
                        if path == base_path {
                            tracing::error!(
                                child = child.as_str(),
                                path = %path.display(),
                                reason = %e,
                                "reclaim_worktree: worktree remove FAILED after {MAX_TEARDOWN_ATTEMPTS} attempts — worktree may be locked, dirty, or nested",
                            );
                            match tokio::process::Command::new("git")
                                .args(["worktree", "prune"])
                                .current_dir(&self.working_dir)
                                .output()
                                .await
                            {
                                Ok(out) => tracing::info!(
                                    exit = ?out.status.code(),
                                    stdout = %String::from_utf8_lossy(&out.stdout).trim(),
                                    stderr = %String::from_utf8_lossy(&out.stderr).trim(),
                                    "git worktree prune (post-reclaim fallback)",
                                ),
                                Err(prune_err) => tracing::warn!(
                                    "git worktree prune failed to launch: {prune_err}"
                                ),
                            }
                            return Err(SpawnError::Failed {
                                op: "reclaim_worktree",
                                child: Some(child.clone()),
                                detail: e.to_string(),
                            });
                        } else {
                            tracing::warn!("nested reclaim failed: {}", e);
                        }
                    }
                }

                Ok(())
            }
            ChildKind::Inline => Ok(()),
        }
    }

    async fn kill_pane(&self, child: &AgentName) -> Result<(), SpawnError> {
        let current_set = self.read_children().await?;
        let record = current_set.get(child).ok_or_else(|| SpawnError::Failed {
            op: "kill_pane",
            child: Some(child.clone()),
            detail: "unknown child".into(),
        })?;

        retry_teardown("kill_pane", child.as_str(), || {
            exo_caps::Tmux::kill_pane(self, &record.pane)
        })
        .await
        .map_err(|e| SpawnError::Failed {
            op: "kill_pane",
            child: Some(child.clone()),
            detail: e.to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, ChildKind, ChildRecord, NodePath, PaneId};
    use tempfile::tempdir;

    fn an(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }

    #[test]
    fn launch_profile_gates_on_base_url_token_optional() {
        let full = std::collections::HashMap::from([
            ("EXO_REVIEWER_AUTH_TOKEN".to_string(), "sk-xxx".to_string()),
            (
                "EXO_REVIEWER_BASE_URL".to_string(),
                "http://localhost:18765".to_string(),
            ),
            (
                "EXO_REVIEWER_MODEL".to_string(),
                "kimi-for-coding".to_string(),
            ),
            ("EXO_REVIEWER_LABEL".to_string(), "kimi".to_string()),
        ]);
        // No prefix (a non-profiled role) → None regardless of env.
        assert!(LaunchProfile::resolve(None, |k: &str| full.get(k).cloned()).is_none());

        // Full env → resolved with every field (an explicit token is preserved).
        let p = LaunchProfile::resolve(Some("EXO_REVIEWER"), |k: &str| full.get(k).cloned())
            .expect("resolves");
        assert_eq!(p.auth_token.as_deref(), Some("sk-xxx"));
        assert_eq!(p.base_url.as_deref(), Some("http://localhost:18765"));
        assert_eq!(p.model.as_deref(), Some("kimi-for-coding"));
        assert_eq!(p.label.as_deref(), Some("kimi"));

        // BASE_URL present but NO token → STILL activates (token is optional; the runtime supplies a
        // placeholder at translation). This is the `reviewer = "kimi"` shorthand case.
        let no_token = std::collections::HashMap::from([
            (
                "EXO_REVIEWER_BASE_URL".to_string(),
                "http://localhost:18765".to_string(),
            ),
            (
                "EXO_REVIEWER_MODEL".to_string(),
                "kimi-for-coding".to_string(),
            ),
            ("EXO_REVIEWER_LABEL".to_string(), "kimi".to_string()),
        ]);
        let p = LaunchProfile::resolve(Some("EXO_REVIEWER"), |k: &str| no_token.get(k).cloned())
            .expect("resolves without a token");
        assert!(p.auth_token.is_none());
        assert_eq!(p.base_url.as_deref(), Some("http://localhost:18765"));

        // Half-set env (MODEL/LABEL but NO base_url) must NOT activate the redirect.
        let no_url = std::collections::HashMap::from([
            (
                "EXO_REVIEWER_MODEL".to_string(),
                "kimi-for-coding".to_string(),
            ),
            ("EXO_REVIEWER_AUTH_TOKEN".to_string(), "sk-xxx".to_string()),
        ]);
        assert!(
            LaunchProfile::resolve(Some("EXO_REVIEWER"), |k: &str| no_url.get(k).cloned())
                .is_none()
        );
    }

    #[tokio::test]
    async fn test_ledger_append_and_read() {
        let tmp = tempdir().unwrap();
        let rt = Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            tmp.path().to_path_buf(),
            None,
            "test-run".into(),
            "test-session".into(),
            PaneId::new("%100".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let pane = PaneId::new("%1".into()).unwrap();
        let record = ChildRecord::Spawned {
            child: an("worker-1"),
            kind: ChildKind::Inline,
            pane: pane.clone(),
            inbox: rt.child_inbox_path(&pane),
            model_label: None,
        };

        rt.append_child_record(&record).await.unwrap();

        let records = rt.read_child_records().await.unwrap();
        assert_eq!(records.len(), 1);
        assert_eq!(records[0], record);

        let kids = exo_caps::fold_children(&records);
        assert!(kids.contains_key(&an("worker-1")));
        assert_eq!(kids[&an("worker-1")].pane, pane);
    }

    #[tokio::test]
    async fn test_resolve_child_name() {
        let tmp = tempdir().unwrap();
        let rt = Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            tmp.path().to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%100".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        // 1. Unnamed → worker-0
        let name0 = rt.resolve_child_name(None, "worker").await.unwrap();
        assert_eq!(name0.as_str(), "worker-0");

        // 2. Add worker-0 to ledger
        let pane = PaneId::new("%1".into()).unwrap();
        rt.append_child_record(&ChildRecord::Spawned {
            child: name0.clone(),
            kind: ChildKind::Inline,
            pane: pane.clone(),
            inbox: rt.child_inbox_path(&pane),
            model_label: None,
        })
        .await
        .unwrap();

        // 3. Unnamed again → worker-1
        let name1 = rt.resolve_child_name(None, "worker").await.unwrap();
        assert_eq!(name1.as_str(), "worker-1");

        // 4. Explicit duplicate → Err
        let res = rt.resolve_child_name(Some(name0), "worker").await;
        match res {
            Err(SpawnError::Failed { detail, .. }) => assert!(detail.contains("duplicate")),
            _ => panic!("expected duplicate error"),
        }

        // 5. Explicit unique → Ok
        let name_unique = rt
            .resolve_child_name(Some(an("custom")), "worker")
            .await
            .unwrap();
        assert_eq!(name_unique.as_str(), "custom");
    }

    #[tokio::test]
    async fn test_child_inbox_path_derivation() {
        let tmp = tempdir().unwrap();
        let rt = Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            tmp.path().to_path_buf(),
            None,
            "run-42".into(),
            "session".into(),
            PaneId::new("%100".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let path = rt.child_inbox_path(&PaneId::new("%317".into()).unwrap());
        let s = path.as_path().to_string_lossy();
        assert!(s.contains("run-42"));
        assert!(s.contains("pane-317.jsonl"));
    }

    #[tokio::test]
    async fn test_retry_teardown_success_on_first_try() {
        let mut calls = 0;
        let res: Result<u32, &str> = retry_teardown("test", "child", || {
            calls += 1;
            async move { Ok(42) }
        })
        .await;

        assert_eq!(res.unwrap(), 42);
        assert_eq!(calls, 1);
    }

    #[tokio::test]
    async fn test_retry_teardown_success_on_retry() {
        let mut calls = 0;
        let res: Result<u32, &str> = retry_teardown("test", "child", || {
            calls += 1;
            async move {
                if calls < 2 {
                    Err("transient")
                } else {
                    Ok(42)
                }
            }
        })
        .await;

        assert_eq!(res.unwrap(), 42);
        assert_eq!(calls, 2);
    }

    #[tokio::test]
    async fn test_retry_teardown_failure_after_max_attempts() {
        let mut calls = 0;
        let res: Result<u32, &str> = retry_teardown("test", "child", || {
            calls += 1;
            async move { Err("persistent") }
        })
        .await;

        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), "persistent");
        assert_eq!(calls, MAX_TEARDOWN_ATTEMPTS as usize);
    }

    #[test]
    fn is_inline_reflects_own_kind() {
        let tmp = tempdir().unwrap();

        let rt_worktree = Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            tmp.path().to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );
        assert!(!rt_worktree.is_inline());

        let rt_inline = Runtime::new(
            NodePath::new(vec![an("root"), an("w")]).unwrap(),
            Branch::new("root".into()).unwrap(),
            tmp.path().to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%2".into()).unwrap(),
            exo_caps::ChildKind::Inline,
        );
        assert!(rt_inline.is_inline());
    }

    #[tokio::test]
    async fn inline_node_read_child_records_returns_empty() {
        let tmp = tempdir().unwrap();

        // Parent (worktree) writes a child record into its ledger.
        let rt_parent = Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            tmp.path().to_path_buf(),
            None,
            "test-run".into(),
            "test-session".into(),
            PaneId::new("%100".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );
        let pane = PaneId::new("%1".into()).unwrap();
        rt_parent
            .append_child_record(&ChildRecord::Spawned {
                child: an("worker-1"),
                kind: ChildKind::Inline,
                pane: pane.clone(),
                inbox: rt_parent.child_inbox_path(&pane),
                model_label: None,
            })
            .await
            .unwrap();

        // Inline worker uses the SAME working_dir but is tagged Inline.
        let rt_inline = Runtime::new(
            NodePath::new(vec![an("root"), an("worker-1")]).unwrap(),
            Branch::new("root".into()).unwrap(),
            tmp.path().to_path_buf(),
            None,
            "test-run".into(),
            "test-session".into(),
            pane.clone(),
            exo_caps::ChildKind::Inline,
        );

        // Inline node must report no children even though the ledger (the parent's) has records.
        let records = rt_inline.read_child_records().await.unwrap();
        assert!(
            records.is_empty(),
            "inline node must not read the parent's ledger"
        );

        // The parent still reads its own ledger correctly.
        let parent_records = rt_parent.read_child_records().await.unwrap();
        assert_eq!(parent_records.len(), 1);
    }
}
