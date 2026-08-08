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
use std::collections::{BTreeMap, HashSet};
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

/// Parse a child ledger's raw bytes into records. **Tolerant of malformed lines** — a torn
/// last record from a crash mid-append must not block the whole ledger, only the affected
/// record. Shared by [`Runtime::read_child_records`] (this node's own ledger) and
/// [`Runtime::read_child_records_at`] (an arbitrary enclosing directory's ledger).
pub(crate) fn parse_child_ledger(data: &[u8], ledger_path: &Path) -> Vec<ChildRecord> {
    let mut records = Vec::new();
    for line in data.split(|&b| b == b'\n') {
        if line.is_empty() {
            continue;
        }
        match serde_json::from_slice::<ChildRecord>(line) {
            Ok(record) => records.push(record),
            Err(e) => {
                tracing::warn!(
                    path = %ledger_path.display(),
                    "skipping malformed children.jsonl line: {e}"
                );
            }
        }
    }
    records
}

/// The pure decision behind [`Runtime::detect_child_deaths`]: which of `candidates` (assumed
/// already filtered to non-terminal) are missing from the probed alive set. `alive = None` means
/// the pane probe failed — unknown, never "no panes exist" — so it yields nothing rather than
/// treating every candidate as dead. Split out so the truth table is testable without tmux (mirrors
/// `liveness.rs`'s `any_busy`).
fn missing_from_alive<'a>(
    candidates: &'a [Child],
    alive: Option<&HashSet<String>>,
) -> Vec<&'a Child> {
    match alive {
        None => Vec::new(),
        Some(set) => candidates
            .iter()
            .filter(|c| !set.contains(c.pane.as_str()))
            .collect(),
    }
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
    /// The child's `--model` (from [`RoleKind::model`](exo_caps::RoleKind), overridden by
    /// [`SpawnSpec::model_override`] when the domain sets one). `None` ⇒ inherit the launcher's
    /// default model. `exo` pins its leaf roles (dev/worker/reviewer) to `sonnet`.
    pub model: Option<String>,
    /// A hash of the directives bundle this child was launched with ([`SpawnSpec::directives_hash`]).
    /// `None` when nothing computes one yet — the runtime records whatever it's given.
    pub directives_hash: Option<String>,
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
    ///
    /// `pub` (not a cap-trait method) so the sidecar's `Submitted`-record writer can reach it
    /// directly — it stays an INHERENT method on `Runtime`, never a `Bus`/`Spawner` trait method,
    /// so a policy tool can never reach the ledger.
    pub async fn append_child_record(&self, rec: &ChildRecord) -> Result<(), SpawnError> {
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
        self.read_child_records_at(&self.working_dir).await
    }

    /// Read + parse the child ledger rooted at an arbitrary directory (`{dir}/.exo/children.jsonl`),
    /// not necessarily this node's own `working_dir`. Used by nested-worktree reclaim: a nested
    /// child's `Spawned` record lives in its immediate ENCLOSING worktree's ledger, which may not
    /// be `self`'s. Shares the same tolerant-parse discipline as [`read_child_records`].
    pub(crate) async fn read_child_records_at(
        &self,
        dir: &Path,
    ) -> Result<Vec<ChildRecord>, SpawnError> {
        let path = dir.join(".exo/children.jsonl");
        let data = match exo_caps::Fs::read(self, &path).await {
            Ok(d) => d,
            Err(FsError::At { source, .. } | FsError::Io(source))
                if source.kind() == std::io::ErrorKind::NotFound =>
            {
                return Ok(Vec::new())
            }
            Err(FsError::At { source, .. } | FsError::Io(source)) => return Err(source.into()),
        };
        Ok(parse_child_ledger(&data, &path))
    }

    /// Best-effort teardown of a nested worktree's recorded pane, read from its ENCLOSING
    /// directory's ledger (not `self`'s — a nested child was spawned by the enclosing node, not by
    /// this one). Missing ledger / missing record / kill failure are all logged at `warn!` and
    /// swallowed — the caller proceeds with `git worktree remove` regardless, so a torn-down pane
    /// is best-effort, never a precondition for reclaiming the directory.
    pub(crate) async fn kill_nested_pane_before_removal(
        &self,
        enclosing_dir: &Path,
        nested_child_name: &str,
    ) {
        let name = match AgentName::new(nested_child_name.to_string()) {
            Ok(n) => n,
            Err(e) => {
                tracing::warn!(
                    enclosing = %enclosing_dir.display(),
                    child = nested_child_name,
                    error = %e,
                    "reclaim_worktree: nested dir name is not a valid AgentName; skipping pane-kill"
                );
                return;
            }
        };
        let records = match self.read_child_records_at(enclosing_dir).await {
            Ok(r) => r,
            Err(e) => {
                tracing::warn!(
                    enclosing = %enclosing_dir.display(),
                    child = name.as_str(),
                    error = %e,
                    "reclaim_worktree: could not read enclosing ledger; proceeding without a pane-kill"
                );
                return;
            }
        };
        let Some(record) = fold_children(&records).get(&name).cloned() else {
            tracing::warn!(
                enclosing = %enclosing_dir.display(),
                child = name.as_str(),
                "reclaim_worktree: nested child has no record in the enclosing ledger; proceeding without a pane-kill"
            );
            return;
        };
        if let Err(e) = retry_teardown("reclaim_nested_kill_pane", name.as_str(), || {
            exo_caps::Tmux::kill_pane(self, &record.pane)
        })
        .await
        {
            tracing::warn!(
                enclosing = %enclosing_dir.display(),
                child = name.as_str(),
                pane = record.pane.as_str(),
                error = %e,
                "reclaim_worktree: nested pane-kill failed after retries; proceeding with removal anyway"
            );
        }
    }

    /// Read + parse the child ledger, yielding the folded child set.
    pub(crate) async fn read_children(&self) -> Result<BTreeMap<AgentName, Child>, SpawnError> {
        let records = self.read_child_records().await?;
        Ok(fold_children(&records))
    }

    /// Record `Reaped` for `child` if the ledger still shows it non-terminal. Called by the
    /// runtime's own teardown paths (`kill_pane` after a successful kill, `reclaim_worktree` after
    /// a successful reclaim) — never by a tool. Re-folding and checking [`ChildState::is_terminal`]
    /// first is what makes the normal kill-then-reclaim sequence append exactly one `Reaped` record
    /// (the second call finds the child already terminal and does nothing).
    ///
    /// A child's cooperative self-reap and the forced shutdown cascade never go through
    /// `Spawner::kill_pane` on the parent side, so those children are NOT recorded as `Reaped`
    /// here — they surface as `Died` on the parent's next watchdog tick instead. That is intended,
    /// not a gap.
    ///
    /// Any failure (ledger read or append) is logged and swallowed — a teardown must never fail
    /// because a ledger write failed.
    pub async fn record_reaped_if_active(&self, child: &AgentName) {
        let records = match self.read_child_records().await {
            Ok(r) => r,
            Err(e) => {
                tracing::warn!(
                    child = child.as_str(),
                    error = %e,
                    "record_reaped_if_active: ledger read failed; not recording"
                );
                return;
            }
        };
        let Some(entry) = fold_children(&records).get(child).cloned() else {
            return;
        };
        if entry.state.is_terminal() {
            return;
        }
        if let Err(e) = self
            .append_child_record(&ChildRecord::Reaped {
                child: child.clone(),
                at: Some(chrono::Utc::now()),
            })
            .await
        {
            tracing::warn!(
                child = child.as_str(),
                error = %e,
                "record_reaped_if_active: append failed"
            );
        }
    }

    /// Scan for non-terminal children whose recorded pane no longer exists in tmux and record
    /// `Died` for each. **Once-only is structural**: a `Died` child folds terminal and is excluded
    /// from every later scan — no separate "already reported" set needed.
    ///
    /// A [`exo_caps::Tmux::list_panes`] probe failure means "could not tell", never "no panes
    /// exist" — it returns an empty vec and records nothing, rather than treating every non-
    /// terminal child as dead.
    pub async fn detect_child_deaths(&self) -> Vec<Child> {
        let records = match self.read_child_records().await {
            Ok(r) => r,
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "detect_child_deaths: ledger read failed; recording nothing"
                );
                return Vec::new();
            }
        };
        let candidates: Vec<Child> = fold_children(&records)
            .into_values()
            .filter(|c| !c.state.is_terminal())
            .collect();
        if candidates.is_empty() {
            return Vec::new();
        }

        let alive = match exo_caps::Tmux::list_panes(self).await {
            Ok(set) => Some(set),
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "detect_child_deaths: pane probe failed; recording nothing"
                );
                None
            }
        };

        let mut died = Vec::new();
        for child in missing_from_alive(&candidates, alive.as_ref()) {
            if let Err(e) = self
                .append_child_record(&ChildRecord::Died {
                    child: child.name.clone(),
                    pane: child.pane.clone(),
                    at: Some(chrono::Utc::now()),
                })
                .await
            {
                tracing::warn!(
                    child = child.name.as_str(),
                    error = %e,
                    "detect_child_deaths: append failed"
                );
                continue;
            }
            died.push(child.clone());
        }
        died
    }

    /// The child's OWN ingestion inbox, derived from its pane + the run-id namespace:
    /// `~/.claude/exo/inboxes/{run_id}/pane-{N}.jsonl`.
    /// Stored in the child's `Spawned` record so the parent can address DOWN to it.
    pub(crate) fn child_inbox_path(&self, pane: &PaneId) -> InboxPath {
        let home = home_or_warn("child_inbox_path");
        exo_caps::paths::inbox_path(Path::new(&home), &self.run_id, pane)
    }

    /// This node's child-launch policy (`yolo`, `wrap_nix`, `review_enabled`), read from its own
    /// papers and inherited down the tree: `birth` stamps it onto each child's papers and uses it
    /// in the launch command. A node whose papers can't be read (or whose papers predate these
    /// fields — they default on parse) falls back to [`NodePapers`]' behavior-preserving
    /// defaults, so the root and any older node launch children exactly as before.
    /// Papers live at `{working_dir}/.exo/node.json` for a worktree node, or the
    /// run-namespaced `root.json` for the root.
    async fn own_launch_policy(&self) -> (bool, bool, bool) {
        let candidates = [
            self.working_dir.join(".exo/node.json"),
            self.working_dir
                .join(format!(".exo/node/{}/root.json", self.run_id)),
        ];
        for path in candidates {
            match exo_caps::Fs::read(self, &path).await {
                Ok(bytes) => match serde_json::from_slice::<NodePapers>(&bytes) {
                    Ok(p) => {
                        tracing::info!(
                            path = %path.display(),
                            yolo = p.yolo,
                            wrap_nix = p.wrap_nix,
                            review_enabled = p.review_enabled,
                            "own_launch_policy: loaded from papers"
                        );
                        return (p.yolo, p.wrap_nix, p.review_enabled);
                    }
                    Err(e) => {
                        tracing::warn!(
                            path = %path.display(),
                            "own_launch_policy: papers parse failed ({e}); trying next candidate"
                        );
                    }
                },
                Err(_) => continue,
            }
        }
        tracing::info!(
            yolo = NodePapers::DEFAULT_YOLO,
            wrap_nix = NodePapers::DEFAULT_WRAP_NIX,
            review_enabled = NodePapers::DEFAULT_REVIEW_ENABLED,
            "own_launch_policy: no readable papers found; using behavior-preserving defaults"
        );
        (
            NodePapers::DEFAULT_YOLO,
            NodePapers::DEFAULT_WRAP_NIX,
            NodePapers::DEFAULT_REVIEW_ENABLED,
        )
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
        //
        // The launch model: a launch profile's model (e.g. kimi-for-coding) wins over the spec's
        // resolved model (role default or explicit override) — profiles are proxy-backed and serve
        // exactly one model, so pointing a profiled child at any other model 404s. Resolved here,
        // before the record append, and reused below at the `ClaudeInvocation` site so there is one
        // binding, not two independently-computed ones.
        let launch_model = core
            .launch_profile
            .as_ref()
            .and_then(|p| p.model.clone())
            .or_else(|| core.model.clone());
        let inbox = self.child_inbox_path(pane);
        let record = ChildRecord::Spawned {
            child: core.name.clone(),
            kind: core.kind,
            pane: pane.clone(),
            inbox,
            // Non-secret cosmetic tag (e.g. "kimi") so the `tree` tool can show it; never the token.
            model_label: core.launch_profile.as_ref().and_then(|p| p.label.clone()),
            model: launch_model.clone(),
            directives_hash: core.directives_hash.clone(),
        };
        self.append_child_record(&record).await?;

        // (e) Write child papers. The child inherits this node's launch policy (yolo /
        // wrap_nix / review_enabled), so it stamps the same onto its own children — config set
        // on one node flows down its whole subtree.
        let parent_inbox = Some(self.own_inbox());
        let (yolo, wrap_nix, review_enabled) = self.own_launch_policy().await;

        // The child's birth-identity parent branch: THIS node's own real git branch (the parent
        // is, by definition, on the branch the child forks from) — never the child's own
        // dot-derived tree-address coordinate, which for a direct child of root would be the
        // literal `root` (root's exo IDENTITY, not a live git ref). A failure to read our own
        // branch degrades to `None`, which fails `submit_branch`'s rebase gate open — never
        // blocks the spawn.
        let parent_branch = match exo_caps::Git::current_branch(self).await {
            Ok(b) => Some(b),
            Err(e) => {
                tracing::warn!(
                    child = core.name.as_str(),
                    "birth: could not read own current_branch for child's parent_branch papers \
                     field ({e}); child's rebase gate will fail open"
                );
                None
            }
        };

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
            review_enabled,
            parent_branch,
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

        let worktree_prompt = format!(
            "{}{}",
            exo_caps::birth_preamble(core.kind, child_dir),
            core.task
        );

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

        // Build the typed launch invocation. The ordered render enforces the structural invariant:
        // --mcp-config (variadic) is always capped by --append-system-prompt/--model before the
        // positional prompt, so it can never swallow the prompt as a config path.
        let launch_cmd = format!(
            "{}\n",
            exomonad_shared::services::agent_control::launch::ClaudeInvocation {
                agent_type,
                cwd: child_dir.to_path_buf(),
                permission_mode: None, // node spawns always use --dangerously-skip-permissions
                allowed_tools: vec![],
                disallowed_tools: vec![],
                settings_path: Some(settings_path.to_string_lossy().into_owned()),
                mcp_config_path: Some(mcp_config_path.to_string_lossy().into_owned()),
                // Cap flags — rendered after --mcp-config, before the positional prompt.
                append_system_prompt: Some(core.protocol.clone()),
                model: launch_model,
                prompt_file: Some(prompt_file),
                fork_session_id,
                env_vars,
                yolo,
                wrap_nix,
                resume: false,
            }
            .render()
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

impl Runtime {
    /// The nested-worktree reclaim walk: discover every `.exo/worktrees/**` nested inside
    /// `base_path`, kill each nested child's recorded pane (best-effort — see
    /// [`kill_nested_pane_before_removal`](Self::kill_nested_pane_before_removal)), then
    /// `git worktree remove --force` innermost-first, `base_path` last. **The single
    /// implementation of this walk** — shared by [`Spawner::reclaim_worktree`] (parent-side,
    /// ledger-driven) and `exo doctor --fix` (root-side, `git worktree list`-driven), so there is
    /// exactly one place that knows how to safely tear down a worktree subtree. `child` labels
    /// errors only (kept as an [`AgentName`] to match [`SpawnError::Failed`]'s field, not read
    /// from the ledger by this fn) — the caller resolves it however it discovered `base_path`.
    ///
    /// A nested dir that `git worktree remove` can't reclaim (never registered, e.g. a
    /// crash-orphaned dir) is logged and does not block `base_path`'s own removal, but is
    /// surfaced as an `Err` so the caller never reports a clean reclaim over a leftover.
    pub async fn reclaim_worktree_tree(
        &self,
        child: &AgentName,
        base_path: &Path,
    ) -> Result<(), SpawnError> {
        // Find all nested worktrees. `enclosing` maps a discovered nested path to the
        // directory it was found under — the enclosing worktree whose OWN ledger records
        // the nested child's pane (a nested child was spawned by that enclosing node, not
        // by `self`). `base_path` has no entry (its pane-kill is the caller's job — both
        // `merge` and the reviewer-verdict teardown call `Spawner::kill_pane` before
        // `reclaim_worktree`; `exo doctor` has no live pane to kill for the top-level path
        // it's given since it discovers worktrees independent of any ledger).
        let mut stack = vec![base_path.to_path_buf()];
        let mut to_remove = vec![];
        let mut enclosing: BTreeMap<PathBuf, PathBuf> = BTreeMap::new();
        while let Some(dir) = stack.pop() {
            to_remove.push(dir.clone());
            let w_dir = dir.join(".exo/worktrees");
            match tokio::fs::read_dir(&w_dir).await {
                Ok(mut entries) => loop {
                    match entries.next_entry().await {
                        Ok(Some(entry)) => match entry.file_type().await {
                            Ok(ft) if ft.is_dir() => {
                                let path = entry.path();
                                enclosing.insert(path.clone(), dir.clone());
                                stack.push(path);
                            }
                            Ok(_) => {}
                            Err(e) => tracing::warn!(
                                path = %entry.path().display(),
                                error = %e,
                                "reclaim_worktree_tree: file_type() failed during nested-worktree discovery; entry skipped"
                            ),
                        },
                        Ok(None) => break,
                        Err(e) => {
                            tracing::warn!(
                                dir = %w_dir.display(),
                                error = %e,
                                "reclaim_worktree_tree: next_entry() failed during nested-worktree discovery; remaining entries in this dir skipped"
                            );
                            break;
                        }
                    }
                },
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                Err(e) => tracing::warn!(
                    dir = %w_dir.display(),
                    error = %e,
                    "reclaim_worktree_tree: read_dir() failed during nested-worktree discovery; this subtree's nested worktrees may be missed"
                ),
            }
        }

        // Innermost first. Removal is the `Git` supertrait's `worktree_remove`
        // (force/reclaim semantics: the directory's state is discarded, the branch
        // ref — a reviewer's committed fixup — survives).
        let mut failed_nested: Vec<PathBuf> = vec![];
        for path in to_remove.into_iter().rev() {
            let child_name = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();

            // A live nested agent's pane must die BEFORE its cwd is force-removed out from
            // under it — kill the recorded pane first (best-effort; see
            // `kill_nested_pane_before_removal`).
            if let Some(enclosing_dir) = enclosing.get(&path) {
                self.kill_nested_pane_before_removal(enclosing_dir, &child_name)
                    .await;
            }

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
                        "reclaim_worktree_tree: worktree remove FAILED after {MAX_TEARDOWN_ATTEMPTS} attempts — worktree may be locked, dirty, or nested",
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
                        Err(prune_err) => {
                            tracing::warn!("git worktree prune failed to launch: {prune_err}")
                        }
                    }
                    return Err(SpawnError::Failed {
                        op: "reclaim_worktree",
                        child: Some(child.clone()),
                        detail: e.to_string(),
                    });
                } else {
                    tracing::warn!("nested reclaim failed: {}", e);
                    failed_nested.push(path);
                }
            }
        }

        if !failed_nested.is_empty() {
            let paths = failed_nested
                .iter()
                .map(|p| p.display().to_string())
                .collect::<Vec<_>>()
                .join(", ");
            tracing::error!(
                child = child.as_str(),
                paths = %paths,
                "reclaim_worktree_tree: nested worktrees left behind after retries — caller must not report a clean reclaim"
            );
            return Err(SpawnError::Failed {
                op: "reclaim_nested",
                child: Some(child.clone()),
                detail: format!("nested worktrees left behind: {paths}"),
            });
        }

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
        // The role's launch model (`exo` pins leaves to sonnet), unless the spec names an explicit
        // override — resolved while the role is typed AND before `spec.into_task()` consumes it.
        let model = spec
            .model_override()
            .or_else(|| RoleKind::model(&role).map(String::from));
        let directives_hash = spec.directives_hash();
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
            directives_hash,
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

        let result = match record.kind {
            ChildKind::Worktree => {
                let base_path = self.working_dir.join(".exo/worktrees").join(child.as_str());
                self.reclaim_worktree_tree(child, &base_path).await
            }
            ChildKind::Inline => Ok(()),
        };

        if result.is_ok() {
            self.record_reaped_if_active(child).await;
        }
        result
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
        })?;

        self.record_reaped_if_active(child).await;
        Ok(())
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
            model: None,
            directives_hash: None,
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
            model: None,
            directives_hash: None,
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
                model: None,
                directives_hash: None,
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

    fn run_git(dir: &std::path::Path, args: &[&str]) {
        let ok = std::process::Command::new("git")
            .current_dir(dir)
            .args(args)
            .status()
            .unwrap()
            .success();
        assert!(ok, "git {:?} failed in {}", args, dir.display());
    }

    fn init_git_repo(dir: &std::path::Path) {
        run_git(dir, &["init", "-q", "-b", "main"]);
        run_git(dir, &["config", "user.email", "t@t"]);
        run_git(dir, &["config", "user.name", "t"]);
        std::fs::write(dir.join("f.txt"), "base\n").unwrap();
        run_git(dir, &["add", "f.txt"]);
        run_git(dir, &["commit", "-q", "-m", "base"]);
    }

    fn root_runtime(repo: &std::path::Path) -> Runtime {
        Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            repo.to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        )
    }

    /// A nested worktree with a ledger-recorded (but nonexistent) pane still gets a best-effort
    /// kill attempt before removal, and that best-effort failure does NOT block reclaim: the
    /// nested and outer worktrees are both removed, and `reclaim_worktree` returns `Ok`.
    #[tokio::test]
    async fn reclaim_worktree_attempts_nested_pane_kill_and_survives_a_dead_pane() {
        let tmp = tempdir().unwrap();
        let repo = tmp.path();
        init_git_repo(repo);
        let rt = root_runtime(repo);

        let outer_name = an("outer");
        let outer_path = repo.join(".exo/worktrees/outer");
        run_git(
            repo,
            &[
                "worktree",
                "add",
                "-b",
                "outer-branch",
                outer_path.to_str().unwrap(),
            ],
        );
        let outer_pane = PaneId::new("%10".into()).unwrap();
        rt.append_child_record(&ChildRecord::Spawned {
            child: outer_name.clone(),
            kind: ChildKind::Worktree,
            pane: outer_pane.clone(),
            inbox: rt.child_inbox_path(&outer_pane),
            model_label: None,
            model: None,
            directives_hash: None,
        })
        .await
        .unwrap();

        // A nested worktree (e.g. a reviewer spawned inside the leaf), registered with git so
        // removal succeeds, but with a pane id that doesn't correspond to any real tmux pane.
        let nested_path = outer_path.join(".exo/worktrees/nested");
        run_git(
            repo,
            &[
                "worktree",
                "add",
                "-b",
                "nested-branch",
                nested_path.to_str().unwrap(),
            ],
        );
        let nested_pane = PaneId::new("%99999".into()).unwrap();
        let nested_record = ChildRecord::Spawned {
            child: an("nested"),
            kind: ChildKind::Worktree,
            pane: nested_pane.clone(),
            inbox: rt.child_inbox_path(&nested_pane),
            model_label: None,
            model: None,
            directives_hash: None,
        };
        let nested_ledger = outer_path.join(".exo/children.jsonl");
        tokio::fs::create_dir_all(nested_ledger.parent().unwrap())
            .await
            .unwrap();
        let mut line = serde_json::to_string(&nested_record).unwrap();
        line.push('\n');
        tokio::fs::write(&nested_ledger, line).await.unwrap();

        let res = exo_caps::Spawner::reclaim_worktree(&rt, &outer_name).await;
        assert!(
            res.is_ok(),
            "a best-effort nested pane-kill failure must not fail reclaim: {:?}",
            res
        );
        assert!(!nested_path.exists(), "nested worktree should be removed");
        assert!(!outer_path.exists(), "outer worktree should be removed");
    }

    /// A nested directory that git never registered as a worktree (e.g. left over from a prior
    /// crash) cannot be removed by `git worktree remove`. `reclaim_worktree` must still attempt
    /// (and succeed at) removing the OUTERMOST worktree, but surface the leftover nested dir as an
    /// `Err` so the caller (the `merge` tool) doesn't report a clean reclaim.
    #[tokio::test]
    async fn reclaim_worktree_errs_on_leftover_nested_dir_but_still_removes_outer() {
        let tmp = tempdir().unwrap();
        let repo = tmp.path();
        init_git_repo(repo);
        let rt = root_runtime(repo);

        let outer_name = an("outer2");
        let outer_path = repo.join(".exo/worktrees/outer2");
        run_git(
            repo,
            &[
                "worktree",
                "add",
                "-b",
                "outer2-branch",
                outer_path.to_str().unwrap(),
            ],
        );
        let outer_pane = PaneId::new("%11".into()).unwrap();
        rt.append_child_record(&ChildRecord::Spawned {
            child: outer_name.clone(),
            kind: ChildKind::Worktree,
            pane: outer_pane.clone(),
            inbox: rt.child_inbox_path(&outer_pane),
            model_label: None,
            model: None,
            directives_hash: None,
        })
        .await
        .unwrap();

        // A plain directory under the discovery path, never registered via `git worktree add` —
        // `git worktree remove` on it always fails.
        let ghost = outer_path.join(".exo/worktrees/ghost");
        tokio::fs::create_dir_all(&ghost).await.unwrap();

        let res = exo_caps::Spawner::reclaim_worktree(&rt, &outer_name).await;
        assert!(
            res.is_err(),
            "a leftover nested worktree must surface as an Err"
        );
        assert!(
            !outer_path.exists(),
            "outer removal must still be attempted and succeed despite the nested failure"
        );
    }

    /// Guarantees a throwaway tmux session is killed even if the test body panics.
    struct TmuxSessionGuard(String);
    impl Drop for TmuxSessionGuard {
        fn drop(&mut self) {
            let _ = std::process::Command::new("tmux")
                .args(["kill-session", "-t", &self.0])
                .status();
        }
    }

    /// A real kill-then-reclaim sequence appends exactly ONE `Reaped` record — the fold-check in
    /// `record_reaped_if_active` is what prevents the second call from double-reaping.
    #[tokio::test]
    async fn reap_records_exactly_once() {
        let tmp = tempdir().unwrap();
        let repo = tmp.path();
        init_git_repo(repo);

        let session = format!("exo-test-reap-{}", std::process::id());
        assert!(
            std::process::Command::new("tmux")
                .args(["new-session", "-d", "-s", &session, "-x", "80", "-y", "24"])
                .status()
                .unwrap()
                .success(),
            "failed to create throwaway tmux session for the test"
        );
        let _guard = TmuxSessionGuard(session.clone());

        let rt = Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            repo.to_path_buf(),
            None,
            "run".into(),
            session,
            PaneId::new("%0".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let child = an("leaf");
        let branch = Branch::new("leaf-branch".into()).unwrap();
        let child_dir = repo.join(".exo/worktrees/leaf");
        exo_caps::Git::worktree_add(&rt, &branch, &child_dir)
            .await
            .unwrap();

        let pane = exo_caps::Tmux::new_pane(&rt, &child_dir, "/bin/sh")
            .await
            .unwrap();
        rt.append_child_record(&ChildRecord::Spawned {
            child: child.clone(),
            kind: ChildKind::Worktree,
            pane: pane.clone(),
            inbox: rt.child_inbox_path(&pane),
            model_label: None,
            model: None,
            directives_hash: None,
        })
        .await
        .unwrap();

        exo_caps::Spawner::kill_pane(&rt, &child).await.unwrap();
        exo_caps::Spawner::reclaim_worktree(&rt, &child)
            .await
            .unwrap();

        let records = rt.read_child_records().await.unwrap();
        let reaped = records
            .iter()
            .filter(|r| matches!(r, ChildRecord::Reaped { child: c, .. } if c == &child))
            .count();
        assert_eq!(
            reaped, 1,
            "kill_pane + reclaim_worktree must append exactly one Reaped record: {records:?}"
        );
    }

    /// `detect_child_deaths` records `Died` for a child whose pane no longer exists, exactly once
    /// — a second scan finds it already terminal and appends nothing.
    #[tokio::test]
    async fn death_scan_appends_died_once() {
        let tmp = tempdir().unwrap();
        let rt = root_runtime_no_git(tmp.path());

        let child = an("worker-1");
        // A pane id that (with overwhelming probability) is not live in any real tmux session —
        // `list_panes` is a genuine `tmux list-panes -a` call.
        let pane = PaneId::new("%99999999".into()).unwrap();
        rt.append_child_record(&ChildRecord::Spawned {
            child: child.clone(),
            kind: ChildKind::Inline,
            pane: pane.clone(),
            inbox: rt.child_inbox_path(&pane),
            model_label: None,
            model: None,
            directives_hash: None,
        })
        .await
        .unwrap();

        let died = rt.detect_child_deaths().await;
        assert_eq!(died.len(), 1);
        assert_eq!(died[0].name, child);

        let records = rt.read_child_records().await.unwrap();
        let died_records = records
            .iter()
            .filter(|r| matches!(r, ChildRecord::Died { .. }))
            .count();
        assert_eq!(died_records, 1);

        // Second scan: the child is now terminal, so it's excluded from the candidate set —
        // nothing new is appended, and the scan reports nothing.
        let died_again = rt.detect_child_deaths().await;
        assert!(died_again.is_empty());
        let records2 = rt.read_child_records().await.unwrap();
        assert_eq!(records2.len(), records.len(), "no new record appended");
    }

    fn root_runtime_no_git(dir: &std::path::Path) -> Runtime {
        Runtime::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            dir.to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        )
    }

    fn child(name: &str, pane: &str) -> Child {
        Child {
            name: an(name),
            kind: ChildKind::Inline,
            pane: PaneId::new(pane.into()).unwrap(),
            inbox: InboxPath::new(format!("/tmp/{name}.jsonl").into()),
            model_label: None,
            model: None,
            state: exo_caps::ChildState::Live,
        }
    }

    #[test]
    fn missing_from_alive_probe_failure_yields_nothing() {
        let candidates = vec![child("a", "%1"), child("b", "%2")];
        assert!(missing_from_alive(&candidates, None).is_empty());
    }

    #[test]
    fn missing_from_alive_finds_dead_panes() {
        let candidates = vec![child("a", "%1"), child("b", "%2")];
        let alive: HashSet<String> = ["%1".to_string()].into_iter().collect();
        let missing = missing_from_alive(&candidates, Some(&alive));
        assert_eq!(missing.len(), 1);
        assert_eq!(missing[0].name.as_str(), "b");
    }

    #[test]
    fn missing_from_alive_none_missing_when_all_alive() {
        let candidates = vec![child("a", "%1")];
        let alive: HashSet<String> = ["%1".to_string()].into_iter().collect();
        assert!(missing_from_alive(&candidates, Some(&alive)).is_empty());
    }
}
