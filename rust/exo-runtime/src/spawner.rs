//! `impl Spawner for Runtime` — the recursion (birth + teardown). **Race-prone; built by
//! the Spawner sub-TL, decomposed S1/S2/S3 — never one leaf.** See docs 03/04, plan 06.
//!
//! Per-op methods each fix their own `(role, agent_type, kind)`; the spec carries only
//! task content. All three ops funnel through one private `birth(BirthCore)` tail:
//!   append `AgentSpawned` record FIRST (so there's never an untracked process)
//!   → (`git worktree add` for a Worktree child — Inline shares the parent's cwd)
//!   → `tmux new_pane`
//!   → write child papers (`node.json`, incl. `parent_inbox` = my inbox)
//!   → launch `exomonad experimental node --papers <node.json>` in the pane.
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
//!      shell, starting `claude`/`gemini` (+ its `exomonad experimental node` sidecar via .mcp.json).
//!
//! The record precedes the **agent** launch (step 3 before step 5). The holding shell
//! (step 2) carries no agent, so a crash before step 5 leaves only a bare shell — nothing
//! untracked. **Do not collapse steps 2+5 into a one-shot `new_pane(cwd, launch_cmd)`** —
//! that reopens the orphan window the two-phase split closes.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{
    AgentName, AgentType, Branch, ChildKind, ChildRecord, ForkSpec, GeminiSpec, InboxPath,
    NodeKind, NodePapers, PaneId, SpawnError, Spawner, WorkerSpec,
};
use std::path::{Path, PathBuf};
use tokio::io::AsyncWriteExt;

/// The fixed triple + identity each op hands to the shared `birth` tail. Constructed by
/// the per-op method (the single place a triple is named); `birth` branches only on `kind`.
#[derive(Debug, Clone)]
pub(crate) struct BirthCore {
    pub kind: ChildKind,
    pub agent_type: AgentType,
    pub role: NodeKind,
    pub name: AgentName,
    pub branch: Branch,
    pub task: String,
}

// ── Shared ledger + inbox-scheme helpers (Spawner-TL scaffold) ───────────────────────────
// Used by S2 (`birth` appends `Spawned`) and S3 (`reclaim_worktree`/`kill_pane` read+fold).
// Self-contained in this file so leaves edit only `spawner.rs`. The inbox-path scheme is
// duplicated by the `Bus` leaf (R4) for resolution; hoisting to a shared module is a
// Runtime-TL converge concern, deliberately not done here (would touch a sibling's file).
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

    /// Read + parse the child ledger (fold with [`exo_caps::fold_children`] for the current
    /// child set). A missing file means no children yet → empty, not an error.
    pub(crate) async fn read_child_records(&self) -> Result<Vec<ChildRecord>, SpawnError> {
        let path = self.children_log_path();
        let content = match tokio::fs::read_to_string(&path).await {
            Ok(c) => c,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
            Err(e) => return Err(e.into()),
        };
        content
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| {
                serde_json::from_str::<ChildRecord>(l).map_err(|e| SpawnError::Failed {
                    op: "record_decode",
                    child: None,
                    detail: e.to_string(),
                })
            })
            .collect()
    }

    /// The child's OWN ingestion inbox, derived from its pane + the run-id namespace:
    /// `~/.claude/exo/inboxes/{run_id}/pane-{N}.jsonl`.
    /// Stored in the child's `Spawned` record so the parent can address DOWN to it.
    pub(crate) fn child_inbox_path(&self, pane: &PaneId) -> InboxPath {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
        exo_caps::paths::inbox_path(Path::new(&home), &self.run_id, pane)
    }

    pub(crate) async fn resolve_child_name(
        &self,
        given: Option<AgentName>,
        prefix: &str,
    ) -> Result<AgentName, SpawnError> {
        let records = self.read_child_records().await?;
        let current_set = exo_caps::fold_children(&records);

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

    fn render_spec_prompt(
        task: &str,
        read_first: &[String],
        steps: &[String],
        verify: &[String],
        boundary: &[String],
        context: Option<&String>,
        done_criteria: &[String],
    ) -> String {
        let mut prompt = task.to_string();

        if !boundary.is_empty() {
            prompt.push_str("\n\nBOUNDARY (DO NOT):");
            for b in boundary {
                prompt.push_str(&format!("\n- {}", b));
            }
        }

        if !read_first.is_empty() {
            prompt.push_str("\n\nREAD FIRST:");
            for rf in read_first {
                prompt.push_str(&format!("\n- {}", rf));
            }
        }

        if !steps.is_empty() {
            prompt.push_str("\n\nSTEPS:");
            for (i, step) in steps.iter().enumerate() {
                prompt.push_str(&format!("\n{}. {}", i + 1, step));
            }
        }

        if !verify.is_empty() {
            prompt.push_str("\n\nVERIFY:");
            for v in verify {
                prompt.push_str(&format!("\n- {}", v));
            }
        }

        if let Some(ctx) = context {
            if !ctx.is_empty() {
                prompt.push_str("\n\nCONTEXT:\n");
                prompt.push_str(ctx);
            }
        }

        if !done_criteria.is_empty() {
            prompt.push_str("\n\nDONE CRITERIA:");
            for d in done_criteria {
                prompt.push_str(&format!("\n- {}", d));
            }
        }

        prompt
    }
}

impl Runtime {
    /// The shared birth tail. **S2.** Record-first, then pane, then papers, then launch.
    pub(crate) async fn birth(&self, core: BirthCore) -> Result<AgentName, SpawnError> {
        // (a) compute child worktree path
        let child_dir = match core.kind {
            ChildKind::Worktree => self
                .working_dir
                .join(".exo/worktrees")
                .join(core.name.as_str()),
            ChildKind::Inline => self.working_dir.to_path_buf(),
        };

        // (b) If core.kind == ChildKind::Worktree: call self.worktree_add
        if core.kind == ChildKind::Worktree {
            exo_caps::Git::worktree_add(self, &core.branch, &child_dir)
                .await
                .map_err(|e| SpawnError::Failed {
                    op: "worktree_add",
                    child: Some(core.name.clone()),
                    detail: e.to_string(),
                })?;
        }

        // (c) Two-phase birth: open a holding shell, capture its pane id. A Worktree child
        // gets its own window (tab — one agent per window, the triad); an Inline worker gets
        // a split pane in the parent's window.
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/bash".into());
        let pane = match core.kind {
            ChildKind::Worktree => exo_caps::Tmux::new_window(self, &child_dir, &shell).await,
            ChildKind::Inline => exo_caps::Tmux::new_pane(self, &child_dir, &shell).await,
        }
        .map_err(|e| SpawnError::Failed {
            op: "new_pane",
            child: Some(core.name.clone()),
            detail: e.to_string(),
        })?;

        // (d) RECORD FIRST — BEFORE launching the agent
        let inbox = self.child_inbox_path(&pane);
        let record = ChildRecord::Spawned {
            child: core.name.clone(),
            kind: core.kind,
            pane: pane.clone(),
            inbox: inbox.clone(),
        };
        self.append_child_record(&record).await?;

        // (e) Write child papers
        let parent_inbox = Some(self.own_inbox());

        let papers = NodePapers::new(
            self.node_path().child(&core.name),
            core.branch.clone(),
            core.role,
            pane.clone(),
            parent_inbox,
        );

        let papers_path = match core.kind {
            ChildKind::Worktree => child_dir.join(".exo/node.json"),
            ChildKind::Inline => {
                let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
                exo_caps::paths::papers_path(Path::new(&home), &self.run_id, &pane)
            }
        };

        if let Some(parent) = papers_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let papers_json = serde_json::to_vec_pretty(&papers).map_err(|e| SpawnError::Failed {
            op: "serialize_papers",
            child: Some(core.name.clone()),
            detail: e.to_string(),
        })?;
        tokio::fs::write(&papers_path, papers_json).await?;

        // Every value interpolated into `launch_cmd` is pasted into a shell, so each is
        // shell-escaped — a name/path with a space or metachar must not break the command.
        let esc = |s: &str| shell_escape::escape(s.to_string().into()).into_owned();

        // (f) Launch the agent.
        // The child self-IDs from its papers (via `.mcp.json` → `experimental node --papers`),
        // so the only env it needs is the boot context its `bootstrap` reads. Set these
        // explicitly rather than relying on inherited session env (the set-environment-timing
        // bug that bit the root). `EXOMONAD_AGENT_ID/ROLE` are mcp-stdio-era and unused here.
        let mut env_prefix = format!(
            "EXOMONAD_SWARM_RUN_ID={} EXOMONAD_TMUX_SESSION={} ",
            esc(&self.run_id),
            esc(&self.tmux_session),
        );

        let agent_bin = match core.agent_type {
            AgentType::Claude => {
                crate::node_config::write_node_agent_config(&child_dir, &papers_path)
                    .await
                    .map_err(|e| SpawnError::Failed {
                        op: "write_node_agent_config",
                        child: Some(core.name.clone()),
                        detail: e.to_string(),
                    })?;

                "claude --dangerously-skip-permissions"
            }
            AgentType::Gemini => {
                let mcp_config = serde_json::json!({
                    "mcpServers": {
                        "exomonad": {
                            "type": "stdio",
                            "command": "exomonad",
                            "args": exo_caps::invocation::node_args(&papers_path.to_string_lossy())
                        }
                    }
                });

                // Gemini child hooks are not wired (not supported in the same command-type way)
                let settings_path = papers_path.with_file_name("settings.json");
                let settings = mcp_config.clone();
                tokio::fs::write(
                    &settings_path,
                    serde_json::to_vec_pretty(&settings).map_err(|e| SpawnError::Failed {
                        op: "write_gemini_settings",
                        child: Some(core.name.clone()),
                        detail: e.to_string(),
                    })?,
                )
                .await?;
                env_prefix.push_str(&format!(
                    "GEMINI_CLI_SYSTEM_SETTINGS_PATH={} ",
                    esc(&settings_path.to_string_lossy())
                ));
                "gemini --yolo"
            }
            AgentType::Shoal => {
                return Err(SpawnError::Failed {
                    op: "launch",
                    child: Some(core.name.clone()),
                    detail: "Shoal is not spawnable as a tree child".into(),
                })
            }
        };

        // The agent (claude/gemini) receives ONLY its task as a positional arg. Its papers
        // reach it via `.mcp.json` (`exomonad experimental node --papers …`), NOT a CLI flag —
        // neither `claude` nor `gemini` accepts `--papers`.
        let launch_cmd = format!("{} {} {}\n", env_prefix, agent_bin, esc(&core.task));

        exo_caps::Tmux::paste(self, &pane, &launch_cmd)
            .await
            .map_err(|e| SpawnError::Failed {
                op: "launch",
                child: Some(core.name.clone()),
                detail: e.to_string(),
            })?;

        Ok(core.name)
    }
}

#[async_trait]
impl Spawner for Runtime {
    async fn spawn_worker(&self, spec: WorkerSpec) -> Result<AgentName, SpawnError> {
        let name = self.resolve_child_name(spec.name, "worker").await?;
        let task = Self::render_spec_prompt(
            &spec.task,
            &spec.read_first,
            &spec.steps,
            &spec.verify,
            &spec.boundary,
            spec.context.as_ref(),
            &spec.done_criteria,
        );
        let core = BirthCore {
            kind: ChildKind::Inline,
            agent_type: AgentType::Gemini,
            role: NodeKind::Worker,
            branch: self.branch().clone(),
            name,
            task,
        };
        self.birth(core).await
    }

    async fn spawn_gemini(&self, spec: GeminiSpec) -> Result<AgentName, SpawnError> {
        let name = self.resolve_child_name(spec.name, "dev").await?;
        let task = Self::render_spec_prompt(
            &spec.task,
            &spec.read_first,
            &spec.steps,
            &spec.verify,
            &spec.boundary,
            spec.context.as_ref(),
            &spec.done_criteria,
        );
        let core = BirthCore {
            kind: ChildKind::Worktree,
            agent_type: AgentType::Gemini,
            role: NodeKind::Dev,
            branch: Branch::from_path(&self.node_path().child(&name)),
            name,
            task,
        };
        self.birth(core).await
    }

    async fn fork_wave(&self, specs: Vec<ForkSpec>) -> Vec<Result<AgentName, SpawnError>> {
        let mut results = Vec::with_capacity(specs.len());
        for spec in specs {
            let name = match self.resolve_child_name(spec.name, "tl").await {
                Ok(n) => n,
                Err(e) => {
                    results.push(Err(e));
                    continue;
                }
            };
            let task = Self::render_spec_prompt(
                &spec.task,
                &spec.read_first,
                &spec.steps,
                &spec.verify,
                &spec.boundary,
                spec.context.as_ref(),
                &spec.done_criteria,
            );
            let core = BirthCore {
                kind: ChildKind::Worktree,
                agent_type: AgentType::Claude,
                role: NodeKind::Tl,
                branch: Branch::from_path(&self.node_path().child(&name)),
                name,
                task,
            };
            results.push(self.birth(core).await);
        }
        results
    }

    async fn reclaim_worktree(&self, child: &AgentName) -> Result<(), SpawnError> {
        let records = self.read_child_records().await?;
        let current_set = exo_caps::fold_children(&records);
        let record = current_set.get(child).ok_or_else(|| SpawnError::Failed {
            op: "reclaim_worktree",
            child: Some(child.clone()),
            detail: "unknown child".into(),
        })?;

        match record.kind {
            ChildKind::Worktree => {
                let path = self.working_dir.join(".exo/worktrees").join(child.as_str());
                exo_caps::Git::worktree_remove(self, &path)
                    .await
                    .map_err(|e| SpawnError::Failed {
                        op: "reclaim_worktree",
                        child: Some(child.clone()),
                        detail: e.to_string(),
                    })
            }
            ChildKind::Inline => Ok(()),
        }
    }

    async fn kill_pane(&self, child: &AgentName) -> Result<(), SpawnError> {
        let records = self.read_child_records().await?;
        let current_set = exo_caps::fold_children(&records);
        let record = current_set.get(child).ok_or_else(|| SpawnError::Failed {
            op: "kill_pane",
            child: Some(child.clone()),
            detail: "unknown child".into(),
        })?;

        exo_caps::Tmux::kill_pane(self, &record.pane)
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
        );

        let pane = PaneId::new("%1".into()).unwrap();
        let record = ChildRecord::Spawned {
            child: an("worker-1"),
            kind: ChildKind::Inline,
            pane: pane.clone(),
            inbox: rt.child_inbox_path(&pane),
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
        );

        let path = rt.child_inbox_path(&PaneId::new("%317".into()).unwrap());
        let s = path.as_path().to_string_lossy();
        assert!(s.contains("run-42"));
        assert!(s.contains("pane-317.jsonl"));
    }
}

#[cfg(test)]
mod extra_tests {
    use super::*;

    #[test]
    fn test_render_spec_prompt() {
        let task = "do work";
        let read_first = vec!["README.md".to_string()];
        let steps = vec!["step 1".to_string(), "step 2".to_string()];
        let verify = vec!["cargo test".to_string()];
        let boundary = vec!["no delete".to_string()];
        let context = Some("some context".to_string());
        let done_criteria = vec!["all green".to_string()];

        let prompt = Runtime::render_spec_prompt(
            task,
            &read_first,
            &steps,
            &verify,
            &boundary,
            context.as_ref(),
            &done_criteria,
        );

        assert!(prompt.contains("do work"));
        assert!(prompt.contains("BOUNDARY (DO NOT):\n- no delete"));
        assert!(prompt.contains("READ FIRST:\n- README.md"));
        assert!(prompt.contains("STEPS:\n1. step 1\n2. step 2"));
        assert!(prompt.contains("VERIFY:\n- cargo test"));
        assert!(prompt.contains("CONTEXT:\nsome context"));
        assert!(prompt.contains("DONE CRITERIA:\n- all green"));

        // Bare task
        let bare = Runtime::render_spec_prompt("task", &[], &[], &[], &[], None, &[]);
        assert_eq!(bare, "task");
    }
}
