//! `merge` tool — fold a child agent's branch into this node's branch (local `git merge`).
//!
//! v2/node-mode convergence is on-disk: children are git worktrees in the *same* repo, so a TL
//! folds a finished child by merging its branch locally — no PR, no remote, no GitHub. Review,
//! when added, runs *before* this (gating the merge); this tool is just the fold. A merge
//! conflict surfaces as a tool error for the TL to resolve.

use crate::branching::child_name;
use exo_caps::{AgentName, Branch, CapError, CapResult, Git, Process, SpawnError, Spawner};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

use exo_framework::{Tool, ToolOutput};

/// Arguments for the `merge` tool.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MergeArgs {
    /// The child's branch to fold into this node's branch (e.g. `main.root.feature`).
    pub branch: String,
    /// The child's agent name (e.g. `feature`). Derived from branch if omitted.
    pub child: Option<String>,
    /// Optional verification command run after a successful merge and before child teardown
    /// (e.g. `"cargo check -p exo"`). Split on whitespace only — no shell, no pipes/quoting/env
    /// expansion; wrap anything more complex in a script. There is no timeout: a hung command
    /// hangs this tool call. On failure the merge stays committed (NOT rolled back) and teardown
    /// is skipped, so the failing child is left alive to fix its work.
    #[serde(default)]
    pub gate: Option<String>,
}

/// The `merge` tool: local fold of a child branch.
pub struct Merge;

/// Last ~1KiB of a gate command's combined stdout+stderr, lossy-decoded.
fn gate_output_tail(out: &std::process::Output) -> String {
    let mut combined = out.stdout.clone();
    combined.extend_from_slice(&out.stderr);
    let start = combined.len().saturating_sub(1024);
    String::from_utf8_lossy(&combined[start..]).into_owned()
}

#[async_trait::async_trait]
impl<R: Git + Spawner + Process + Send + Sync> Tool<R> for Merge {
    const NAME: &'static str = "merge";
    const DESCRIPTION: &'static str =
        "Fold a child's branch into yours with a local `git merge` AND reclaim the child (kill its \
         pane + remove its worktree) — one-step fold + cleanup. ALWAYS prefer this over a raw `git \
         merge`, which leaks the child's pane and worktree. The child names its branch in its \
         `submit_branch` [READY] message. Children are worktrees of the same repo, so this needs \
         no remote, no PR. A merge conflict surfaces as an error for you to resolve. Optional \
         `gate`: a whitespace-split verification command run after the merge and before teardown — \
         on failure the merge stays committed but teardown is skipped, leaving the child alive to \
         fix its work. `branch` accepts ANY local ref, not just a tracked child's — this is the \
         supported succession escape hatch for dead-TL recovery (folding an orphaned descendant's \
         branch back in); pane/worktree reclaim only works for your own ledger children and is \
         best-effort otherwise.";
    type Args = MergeArgs;

    async fn run(ctx: &R, args: MergeArgs) -> CapResult<ToolOutput> {
        let branch = Branch::new(args.branch.clone())?;

        let gate = match args.gate.as_deref().map(str::trim) {
            Some("") => {
                return Err(CapError::invalid(
                    "merge",
                    "gate must be a non-empty command",
                ))
            }
            Some(g) => Some(g.to_string()),
            None => None,
        };

        ctx.merge(&branch).await?;

        if let Some(gate) = gate {
            let mut parts = gate.split_whitespace();
            let program = parts.next().expect("non-empty gate validated above");
            let gate_args: Vec<String> = parts.map(str::to_string).collect();

            let (ok, exit, tail) = match ctx.run(program, &gate_args).await {
                Ok(out) => {
                    let exit = match out.status.code() {
                        Some(c) => c.to_string(),
                        None => "signal".to_string(),
                    };
                    (out.status.success(), exit, gate_output_tail(&out))
                }
                Err(e) => (false, format!("spawn error: {e}"), String::new()),
            };

            if !ok {
                let text = format!(
                    "MERGED branch {} — but gate `{}` FAILED (exit {}): {}\nThe merge is already \
                     committed (NOT rolled back). Teardown was NOT performed — the child is left \
                     alive: fix and re-run, or tear down manually.",
                    branch.as_str(),
                    gate,
                    exit,
                    tail
                );
                let data = json!({
                    "branch": branch.as_str(),
                    "gate": { "cmd": gate, "exit": exit, "output_tail": tail },
                });
                return Ok(ToolOutput::with_data(text, data));
            }
        }

        let mut teardown = String::new();
        let child_candidate = args.child.or_else(|| Some(child_name(&branch).to_string()));

        if let Some(name_str) = child_candidate {
            if let Ok(child) = AgentName::new(name_str) {
                // UFCS: `Spawner::kill_pane` (by child name) — `Tmux::kill_pane` (by pane id)
                // is also in scope via the supertrait, so the bare method call is ambiguous.
                let killed = Spawner::kill_pane(ctx, &child).await;
                let reclaimed = ctx.reclaim_worktree(&child).await;

                let both_unknown_child = matches!(&killed, Err(SpawnError::UnknownChild(_)))
                    && matches!(&reclaimed, Err(SpawnError::UnknownChild(_)));

                let k_msg = match &killed {
                    Ok(_) => "ok".to_string(),
                    Err(e) => e.to_string(),
                };
                let r_msg = match &reclaimed {
                    Ok(_) => "ok".to_string(),
                    Err(e) => e.to_string(),
                };

                // `merge` deliberately accepts any local ref so a live ancestor can fold an
                // orphaned descendant's branch after a TL dies. In that case there IS no ledger
                // child to tear down, so reporting a generic 'teardown best-effort' failure would
                // be actively misleading — it reads as 'cleanup broke' when the truth is 'there
                // was nothing of mine to clean up'.
                teardown = if k_msg == "ok" && r_msg == "ok" {
                    format!(" (reclaimed {})", child.as_str())
                } else if both_unknown_child {
                    " — merged non-child ref; no teardown performed (succession escape hatch: \
                     pane/worktree reclaim only works for your own ledger children)"
                        .to_string()
                } else {
                    format!(" (teardown best-effort: kill={} reclaim={})", k_msg, r_msg)
                };
            }
        }

        let gate_suffix = if args.gate.is_some() {
            " (gate ok)"
        } else {
            ""
        };

        Ok(ToolOutput::with_data(
            format!(
                "merged branch {}{}{}",
                branch.as_str(),
                teardown,
                gate_suffix
            ),
            json!({ "branch": branch.as_str() }),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_framework::Tool;

    /// What a teardown method on [`SuccessionMock`] reports for a given call. The "both ok"
    /// path is already covered by the `MockRuntime`-based tests below, so this mock only needs
    /// to model the two failure shapes `MockRuntime::fail()` can't produce together.
    #[derive(Clone, Copy)]
    enum Teardown {
        Unknown,
        Failed,
    }

    impl Teardown {
        fn resolve(self, child: &AgentName) -> Result<(), SpawnError> {
            match self {
                Teardown::Unknown => Err(SpawnError::UnknownChild(child.clone())),
                Teardown::Failed => Err(SpawnError::Failed {
                    op: "mock",
                    child: Some(child.clone()),
                    detail: "mock forced failure".into(),
                }),
            }
        }
    }

    /// A bare stand-in for `R: Git + Spawner + Process`, used only to make `kill_pane`/
    /// `reclaim_worktree` report [`SpawnError::UnknownChild`] — `MockRuntime`'s `fail()` can only
    /// produce `SpawnError::Failed`, so it can't exercise the succession-escape-hatch
    /// classification in `Merge::run`. Every method besides `merge`/`kill_pane`/
    /// `reclaim_worktree` is unreachable — `Merge::run` never calls them for a `gate: None` merge.
    struct SuccessionMock {
        kill: Teardown,
        reclaim: Teardown,
    }

    #[async_trait::async_trait]
    impl exo_caps::Git for SuccessionMock {
        async fn current_branch(&self) -> Result<Branch, exo_caps::GitError> {
            unreachable!()
        }
        async fn head_sha(&self) -> Result<String, exo_caps::GitError> {
            unreachable!()
        }
        async fn merge_base(&self, _refish: &str) -> Result<Option<String>, exo_caps::GitError> {
            unreachable!()
        }
        async fn fork_point(&self) -> Result<Option<String>, exo_caps::GitError> {
            unreachable!()
        }
        async fn is_clean(&self) -> Result<bool, exo_caps::GitError> {
            unreachable!()
        }
        async fn status_porcelain(&self) -> Result<Vec<String>, exo_caps::GitError> {
            unreachable!()
        }
        async fn commits_between(
            &self,
            _base: &str,
            _head: &str,
        ) -> Result<Vec<exo_caps::CommitFiles>, exo_caps::GitError> {
            unreachable!()
        }
        async fn is_ahead_of(&self, _base: &str) -> Result<bool, exo_caps::GitError> {
            unreachable!()
        }
        async fn is_behind(&self, _base: &str) -> Result<bool, exo_caps::GitError> {
            unreachable!()
        }
        async fn fetch(&self) -> Result<(), exo_caps::GitError> {
            unreachable!()
        }
        async fn merge(&self, _branch: &Branch) -> Result<(), exo_caps::GitError> {
            Ok(())
        }
        async fn worktree_add(
            &self,
            _branch: &Branch,
            _at: &std::path::Path,
        ) -> Result<(), exo_caps::GitError> {
            unreachable!()
        }
        async fn worktree_remove(&self, _at: &std::path::Path) -> Result<(), exo_caps::GitError> {
            unreachable!()
        }
    }

    #[async_trait::async_trait]
    impl exo_caps::Tmux for SuccessionMock {
        async fn new_pane(
            &self,
            _cwd: &std::path::Path,
            _cmd: &str,
        ) -> Result<exo_caps::PaneId, exo_caps::TmuxError> {
            unreachable!()
        }
        async fn new_window(
            &self,
            _name: &str,
            _cwd: &std::path::Path,
            _cmd: &str,
        ) -> Result<exo_caps::PaneId, exo_caps::TmuxError> {
            unreachable!()
        }
        async fn paste(
            &self,
            _pane: &exo_caps::PaneId,
            _text: &str,
        ) -> Result<(), exo_caps::TmuxError> {
            unreachable!()
        }
        async fn kill_pane(&self, _pane: &exo_caps::PaneId) -> Result<(), exo_caps::TmuxError> {
            unreachable!()
        }
        async fn list_panes(
            &self,
        ) -> Result<std::collections::HashSet<String>, exo_caps::TmuxError> {
            unreachable!()
        }
    }

    #[async_trait::async_trait]
    impl exo_caps::Fs for SuccessionMock {
        async fn read(&self, _path: &std::path::Path) -> Result<Vec<u8>, exo_caps::FsError> {
            unreachable!()
        }
        async fn write_atomic(
            &self,
            _path: &std::path::Path,
            _bytes: &[u8],
        ) -> Result<(), exo_caps::FsError> {
            unreachable!()
        }
        async fn read_dir(
            &self,
            _path: &std::path::Path,
        ) -> Result<Vec<std::path::PathBuf>, exo_caps::FsError> {
            unreachable!()
        }
    }

    #[async_trait::async_trait]
    impl Process for SuccessionMock {
        async fn run(
            &self,
            _program: &str,
            _args: &[String],
        ) -> Result<std::process::Output, exo_caps::ProcessError> {
            unreachable!()
        }
    }

    #[async_trait::async_trait]
    impl Spawner for SuccessionMock {
        async fn spawn<S: exo_caps::SpawnSpec>(&self, _spec: S) -> Result<AgentName, SpawnError> {
            unreachable!()
        }
        async fn reclaim_worktree(&self, child: &AgentName) -> Result<(), SpawnError> {
            self.reclaim.resolve(child)
        }
        async fn kill_pane(&self, child: &AgentName) -> Result<(), SpawnError> {
            self.kill.resolve(child)
        }
    }

    #[tokio::test]
    async fn test_merge_unknown_child_reports_succession_note() {
        let mock = SuccessionMock {
            kill: Teardown::Unknown,
            reclaim: Teardown::Unknown,
        };
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: None,
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("merged non-child ref"));
        assert!(out.text.contains("succession escape hatch"));
        assert!(!out.text.contains("teardown best-effort"));
    }

    #[tokio::test]
    async fn test_merge_mixed_unknown_and_other_error_keeps_generic_message() {
        let mock = SuccessionMock {
            kill: Teardown::Unknown,
            reclaim: Teardown::Failed,
        };
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: None,
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("teardown best-effort"));
        assert!(!out.text.contains("succession escape hatch"));
    }

    #[tokio::test]
    async fn test_merge_local_fold() {
        let mock = MockRuntime::default();
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: None,
            },
        )
        .await
        .unwrap();

        assert_eq!(
            out.text,
            "merged branch main.root.feature (reclaimed feature)"
        );
        assert_eq!(out.data, Some(json!({ "branch": "main.root.feature" })));
        let calls = mock.calls_made();
        assert!(calls.iter().any(
            |c| matches!(c, Call::Merge { branch } if branch.as_str() == "main.root.feature")
        ));
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::KillPane { child } if child.as_str() == "feature")));
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::ReclaimWorktree { child } if child.as_str() == "feature")));
    }

    #[tokio::test]
    async fn test_merge_explicit_child() {
        let mock = MockRuntime::default();
        // Agent name v1.2 becomes branch v1-2. Explicitly passing the real name
        // should override the branch-based heuristic.
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.v1-2".into(),
                child: Some("v1.2".into()),
                gate: None,
            },
        )
        .await
        .unwrap();

        assert_eq!(out.text, "merged branch main.root.v1-2 (reclaimed v1.2)");
        let calls = mock.calls_made();
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::KillPane { child } if child.as_str() == "v1.2")));
    }

    #[tokio::test]
    async fn test_merge_teardown_failure_formatting() {
        let mock = MockRuntime::failing("kill_pane");
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: None,
            },
        )
        .await
        .unwrap();

        // The exact error message depends on SpawnError's Display impl in MockRuntime
        assert!(out.text.contains("teardown best-effort: kill="));
        assert!(out.text.contains("reclaim=ok"));
    }

    #[tokio::test]
    async fn test_merge_error_path() {
        let mock = MockRuntime::failing("merge");
        let res = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: None,
            },
        )
        .await;
        assert!(res.is_err());
    }

    #[tokio::test]
    async fn test_merge_gate_success() {
        let mock = MockRuntime::default();
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: Some("cargo check -p exo".into()),
            },
        )
        .await
        .unwrap();

        assert_eq!(
            out.text,
            "merged branch main.root.feature (reclaimed feature) (gate ok)"
        );
        let calls = mock.calls_made();
        assert!(calls.iter().any(|c| matches!(
            c,
            Call::ProcessRun { program, args }
                if program == "cargo" && args == &vec!["check".to_string(), "-p".to_string(), "exo".to_string()]
        )));
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::KillPane { child } if child.as_str() == "feature")));
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::ReclaimWorktree { child } if child.as_str() == "feature")));
    }

    #[tokio::test]
    async fn test_merge_gate_failure_skips_teardown() {
        use std::os::unix::process::ExitStatusExt;

        let mut mock = MockRuntime::default();
        mock.process_output = std::process::Output {
            status: std::process::ExitStatus::from_raw(1 << 8), // exit code 1
            stdout: b"building...\n".to_vec(),
            stderr: b"error: something broke\n".to_vec(),
        };

        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: Some("cargo check -p exo".into()),
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("FAILED"));
        assert!(out.text.contains("exit 1"));
        assert!(out.text.contains("committed (NOT rolled back)"));
        assert!(out.text.contains("Teardown was NOT performed"));
        let data = out.data.unwrap();
        assert_eq!(data["gate"]["cmd"], "cargo check -p exo");
        assert_eq!(data["gate"]["exit"], "1");

        let calls = mock.calls_made();
        assert!(calls.iter().any(
            |c| matches!(c, Call::Merge { branch } if branch.as_str() == "main.root.feature")
        ));
        assert!(!calls.iter().any(|c| matches!(c, Call::KillPane { .. })));
        assert!(!calls
            .iter()
            .any(|c| matches!(c, Call::ReclaimWorktree { .. })));
    }

    #[tokio::test]
    async fn test_merge_empty_gate_rejected_before_merge() {
        let mock = MockRuntime::default();
        let res = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: Some("   ".into()),
            },
        )
        .await;

        assert!(res.is_err());
        let calls = mock.calls_made();
        assert!(!calls.iter().any(|c| matches!(c, Call::Merge { .. })));
    }

    #[tokio::test]
    async fn test_merge_gate_omitted_matches_no_gate_behavior() {
        let mock = MockRuntime::default();
        let out = Merge::run(
            &mock,
            MergeArgs {
                branch: "main.root.feature".into(),
                child: None,
                gate: None,
            },
        )
        .await
        .unwrap();

        assert_eq!(
            out.text,
            "merged branch main.root.feature (reclaimed feature)"
        );
        assert!(!out.text.contains("gate ok"));
    }
}
