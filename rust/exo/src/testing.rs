//! A shared mock runtime for unit-testing policy tools/hooks against **mock caps, zero IO**
//! — the seam's payoff of unit-testing domain logic in pure Rust. Every tool tests its `run`
//! against this one mock instead of writing its own, so the mocks don't diverge.
//!
//! [`MockRuntime`] impls every `exo-caps` trait (so it is `PolicyCaps`). It **records** the
//! calls it receives in interior-mutable logs (a tool asserts "I called `Bus::deliver` with
//! this message") and returns configurable canned values. Construct with `default()` for the
//! happy path, then tweak fields, or use the `with_*` builders.
//!
//! Only compiled under `cfg(test)` consumers via `pub` — it lives in the crate so every
//! tool module's `#[cfg(test)] mod tests` can `use crate::testing::MockRuntime`.

use async_trait::async_trait;
use exo_caps::{
    Addressee, AgentName, Branch, Bus, BusError, ChildKind, ChildLiveness, Fs, FsError, Git,
    GitError, Kv, KvError, Message, PaneId, Process, ProcessError, RoleKind, SpawnError, SpawnSpec,
    Spawner, Tmux, TmuxError, Topology, TopologyError, TopologyView, TreeNode,
};
use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

/// One recorded interaction, for `run`-level assertions. Add variants as tools need to
/// observe more calls — this enum is part of the shared test contract.
#[derive(Debug, Clone, PartialEq)]
pub enum Call {
    BusDeliver {
        to: Addressee,
        msg: Message,
    },
    Fetch,
    Merge {
        branch: Branch,
    },
    /// One collapsed `Spawner::spawn` — the role's `role_str` (the spec is generic over the domain
    /// role, so we record the stable string) + the rendered task.
    Spawn {
        role: String,
        task: String,
        fork_session: bool,
    },
    ForkWave {
        n: usize,
    },
    KvGet {
        key: String,
    },
    KvSet {
        key: String,
        value: String,
    },
    FsRead {
        path: String,
    },
    FsWrite {
        path: String,
    },
    KillPane {
        child: AgentName,
    },
    ReclaimWorktree {
        child: AgentName,
    },
}

/// Canned return values + a recording log. Interior-mutable so the cap methods take `&self`
/// (as the traits require) while still recording. Fields are `pub` so a test can set up the
/// exact scenario (e.g. `mock.is_clean = false` then assert `stop` blocks).
pub struct MockRuntime {
    pub calls: Mutex<Vec<Call>>,
    pub kv: Mutex<HashMap<String, String>>,
    pub files: Mutex<HashMap<String, Vec<u8>>>,

    // canned git state (the stop-gate clean check + merge tests read these)
    pub current_branch: Branch,
    pub head_sha: String,
    /// What `merge_base` returns for any refish (set `None` to exercise the submit fallback chain).
    pub merge_base: Option<String>,
    /// What `fork_point` returns (default None so submit tests exercise the merge_base fallback).
    pub fork_point: Option<String>,
    pub is_clean: bool,
    /// What [`ChildLiveness::any_child_busy`] returns. Default `true` (a working child → the
    /// subtree-idle gate treats the node as busy); set `false` to model a quiescent subtree.
    pub child_busy: bool,
    /// If set, the named cap method returns its `*Error` instead of the happy path. Keyed by
    /// a short op label (e.g. "merge") so a test can exercise error branches.
    pub fail: Mutex<Option<&'static str>>,
}

impl Default for MockRuntime {
    fn default() -> Self {
        MockRuntime {
            calls: Mutex::new(Vec::new()),
            kv: Mutex::new(HashMap::new()),
            files: Mutex::new(HashMap::new()),
            current_branch: Branch::new("dev.policy-claude".into()).unwrap(),
            head_sha: "0000000000000000000000000000000000000000".into(),
            merge_base: Some("basebasebasebasebasebasebasebasebasebase".into()),
            fork_point: None,
            is_clean: true,
            child_busy: true,
            fail: Mutex::new(None),
        }
    }
}

impl MockRuntime {
    /// Record a call (test helper).
    pub fn record(&self, c: Call) {
        self.calls.lock().unwrap().push(c);
    }
    /// The recorded calls, in order — for `assert_eq!(mock.calls_made(), vec![…])`.
    pub fn calls_made(&self) -> Vec<Call> {
        self.calls.lock().unwrap().clone()
    }
    /// Force the next matching cap op to fail, for error-branch tests.
    pub fn failing(op: &'static str) -> Self {
        let m = MockRuntime::default();
        *m.fail.lock().unwrap() = Some(op);
        m
    }
    fn should_fail(&self, op: &'static str) -> bool {
        *self.fail.lock().unwrap() == Some(op)
    }
}

#[async_trait]
impl Bus for MockRuntime {
    async fn deliver(&self, to: Addressee, msg: Message) -> Result<(), BusError> {
        if self.should_fail("deliver") {
            return Err(BusError::Append {
                detail: "mock forced failure".into(),
            });
        }
        self.record(Call::BusDeliver { to, msg });
        Ok(())
    }
}

#[async_trait]
impl Git for MockRuntime {
    async fn current_branch(&self) -> Result<Branch, GitError> {
        Ok(self.current_branch.clone())
    }
    async fn head_sha(&self) -> Result<String, GitError> {
        Ok(self.head_sha.clone())
    }
    async fn merge_base(&self, _refish: &str) -> Result<Option<String>, GitError> {
        Ok(self.merge_base.clone())
    }
    async fn fork_point(&self) -> Result<Option<String>, GitError> {
        Ok(self.fork_point.clone())
    }
    async fn is_clean(&self) -> Result<bool, GitError> {
        Ok(self.is_clean)
    }
    async fn fetch(&self) -> Result<(), GitError> {
        if self.should_fail("fetch") {
            return Err(GitError::Failed {
                op: "fetch",
                detail: "mock forced failure".into(),
            });
        }
        self.record(Call::Fetch);
        Ok(())
    }
    async fn merge(&self, branch: &Branch) -> Result<(), GitError> {
        if self.should_fail("merge") {
            return Err(GitError::Failed {
                op: "merge",
                detail: "mock forced failure".into(),
            });
        }
        self.record(Call::Merge {
            branch: branch.clone(),
        });
        Ok(())
    }
    async fn worktree_add(&self, _branch: &Branch, _at: &Path) -> Result<(), GitError> {
        Ok(())
    }
    async fn worktree_remove(&self, _at: &Path) -> Result<(), GitError> {
        Ok(())
    }
}

#[async_trait]
impl Spawner for MockRuntime {
    async fn spawn<S: SpawnSpec>(&self, spec: S) -> Result<AgentName, SpawnError> {
        if self.should_fail("spawn") {
            return Err(SpawnError::Failed {
                op: "spawn",
                child: None,
                detail: "mock forced failure".into(),
            });
        }
        let role = spec.role().role_str().to_string();
        let fork_session = spec.fork_session();
        let name = spec
            .name()
            .unwrap_or_else(|| AgentName::new(format!("{}-mock", spec.name_prefix())).unwrap());
        let task = spec.into_task();
        self.record(Call::Spawn {
            role,
            task,
            fork_session,
        });
        Ok(name)
    }
    // Override the default loop only to record the wave size as one call (the tools assert on it).
    async fn fork_wave<S: SpawnSpec>(&self, specs: Vec<S>) -> Vec<Result<AgentName, SpawnError>> {
        self.record(Call::ForkWave { n: specs.len() });
        specs
            .into_iter()
            .enumerate()
            .map(|(i, s)| {
                Ok(s.name()
                    .unwrap_or_else(|| AgentName::new(format!("fork-mock-{i}")).unwrap()))
            })
            .collect()
    }
    async fn reclaim_worktree(&self, child: &AgentName) -> Result<(), SpawnError> {
        if self.should_fail("reclaim_worktree") {
            return Err(SpawnError::Failed {
                op: "reclaim_worktree",
                child: Some(child.clone()),
                detail: "mock forced failure".into(),
            });
        }
        self.record(Call::ReclaimWorktree {
            child: child.clone(),
        });
        Ok(())
    }
    async fn kill_pane(&self, child: &AgentName) -> Result<(), SpawnError> {
        if self.should_fail("kill_pane") {
            return Err(SpawnError::Failed {
                op: "kill_pane",
                child: Some(child.clone()),
                detail: "mock forced failure".into(),
            });
        }
        self.record(Call::KillPane {
            child: child.clone(),
        });
        Ok(())
    }
}

#[async_trait]
impl Kv for MockRuntime {
    async fn get(&self, key: &str) -> Result<Option<String>, KvError> {
        self.record(Call::KvGet { key: key.into() });
        Ok(self.kv.lock().unwrap().get(key).cloned())
    }
    async fn set(&self, key: &str, value: &str) -> Result<(), KvError> {
        self.record(Call::KvSet {
            key: key.into(),
            value: value.into(),
        });
        self.kv.lock().unwrap().insert(key.into(), value.into());
        Ok(())
    }
}

#[async_trait]
impl Fs for MockRuntime {
    async fn read(&self, path: &Path) -> Result<Vec<u8>, FsError> {
        let key = path.display().to_string();
        self.record(Call::FsRead { path: key.clone() });
        self.files
            .lock()
            .unwrap()
            .get(&key)
            .cloned()
            .ok_or_else(|| FsError::At {
                op: "read",
                path: key,
                source: std::io::Error::new(std::io::ErrorKind::NotFound, "mock: no such file"),
            })
    }
    async fn write_atomic(&self, path: &Path, bytes: &[u8]) -> Result<(), FsError> {
        let key = path.display().to_string();
        self.record(Call::FsWrite { path: key.clone() });
        self.files.lock().unwrap().insert(key, bytes.to_vec());
        Ok(())
    }
}

#[async_trait]
impl Tmux for MockRuntime {
    async fn new_pane(&self, _cwd: &Path, _cmd: &str) -> Result<PaneId, TmuxError> {
        Ok(PaneId::new("%999".into()).unwrap())
    }
    async fn new_window(&self, _name: &str, _cwd: &Path, _cmd: &str) -> Result<PaneId, TmuxError> {
        Ok(PaneId::new("%999".into()).unwrap())
    }
    async fn paste(&self, _pane: &PaneId, _text: &str) -> Result<(), TmuxError> {
        Ok(())
    }
    async fn kill_pane(&self, _pane: &PaneId) -> Result<(), TmuxError> {
        Ok(())
    }
    async fn list_panes(&self) -> Result<std::collections::HashSet<String>, TmuxError> {
        // A successful probe of an empty world. Liveness behavior is tested via the canned
        // `Topology`/`ChildLiveness` impls below, not through this set.
        Ok(std::collections::HashSet::new())
    }
}

#[async_trait]
impl Process for MockRuntime {
    async fn run(
        &self,
        _program: &str,
        _args: &[String],
    ) -> Result<std::process::Output, ProcessError> {
        // A successful empty output. Tools needing a specific exit code construct their own.
        Ok(std::process::Output {
            status: Default::default(),
            stdout: Vec::new(),
            stderr: Vec::new(),
        })
    }
}

#[async_trait]
impl Topology for MockRuntime {
    async fn topology(&self) -> Result<TopologyView, TopologyError> {
        // A small canned tree: self `mock` (under `mock-parent`) with one worktree child.
        Ok(TopologyView {
            node: TreeNode {
                name: "mock".into(),
                kind: None,
                pane: "%0".into(),
                pane_alive: true,
                model_label: None,
                children: vec![TreeNode {
                    name: "child-a".into(),
                    kind: Some(ChildKind::Worktree),
                    pane: "%1".into(),
                    // Topology reports pane *existence* only (for the `tree` tool); idle is a
                    // separate axis, modelled by `child_busy` via the `ChildLiveness` impl below.
                    pane_alive: true,
                    model_label: Some("kimi".into()),
                    children: vec![],
                }],
            },
            parent: Some("mock-parent".into()),
            path: vec!["mock-parent".into(), "mock".into()],
        })
    }
}

#[async_trait]
impl ChildLiveness for MockRuntime {
    async fn any_child_busy(&self) -> bool {
        self.child_busy
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_framework::PolicyCaps;

    fn assert_policy_caps<T: PolicyCaps>() {}

    #[test]
    fn mock_is_policy_caps() {
        // If this compiles, MockRuntime satisfies the full cap union — the whole point.
        assert_policy_caps::<MockRuntime>();
    }

    #[tokio::test]
    async fn records_and_returns() {
        let m = MockRuntime::default();
        let msg = Message {
            text: exo_caps::MessageBody::new("hi".into()).unwrap(),
            summary: exo_caps::Summary::new("hi".into()).unwrap(),
            kind: exo_caps::MessageKind::Chat,
        };
        m.deliver(Addressee::Parent, msg.clone()).await.unwrap();
        assert_eq!(
            m.calls_made(),
            vec![Call::BusDeliver {
                to: Addressee::Parent,
                msg
            }]
        );
    }

    #[tokio::test]
    async fn forced_failure_surfaces() {
        let m = MockRuntime::failing("merge");
        let branch = Branch::new("dev.child".into()).unwrap();
        assert!(m.merge(&branch).await.is_err());
    }
}
