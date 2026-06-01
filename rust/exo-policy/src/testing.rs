//! A shared mock runtime for unit-testing policy tools/hooks against **mock caps, zero IO**
//! — the seam's payoff the WASM guest couldn't have. Every leaf (P1–P7) tests its `run`
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
    Addressee, AgentName, Branch, Bus, BusError, CiStatus, ForkSpec, Fs, FsError, GeminiSpec, Git,
    GitError, GitHub, GitHubError, Kv, KvError, Log, Message, PaneId, Process, ProcessError,
    ReviewState, SpawnError, Spawner, Tmux, TmuxError, WorkerSpec,
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
    FilePr {
        title: String,
        body: String,
        base: Branch,
    },
    MergePr {
        pr: u64,
    },
    SpawnWorker {
        spec_task: String,
        step_count: usize,
    },
    SpawnGemini {
        spec_task: String,
        step_count: usize,
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
    LogInfo {
        msg: String,
    },
    LogError {
        msg: String,
    },
}

/// Canned return values + a recording log. Interior-mutable so the cap methods take `&self`
/// (as the traits require) while still recording. Fields are `pub` so a test can set up the
/// exact scenario (e.g. `mock.pr_for_branch = Some(7)` then assert `stop` blocks).
pub struct MockRuntime {
    pub calls: Mutex<Vec<Call>>,
    pub kv: Mutex<HashMap<String, String>>,
    pub files: Mutex<HashMap<String, Vec<u8>>>,

    // canned GitHub state (the stop-gate + merge tests read these)
    pub current_branch: Branch,
    pub pr_for_branch: Option<u64>,
    pub has_unaddressed_changes: bool,
    pub review_state: Option<ReviewState>,
    pub ci_status: CiStatus,
    pub is_clean: bool,
    /// Next `file_pr` returns this PR number.
    pub next_pr: u64,
    /// If set, the named cap method returns its `*Error` instead of the happy path. Keyed by
    /// a short op label (e.g. "merge_pr") so a test can exercise error branches.
    pub fail: Mutex<Option<&'static str>>,
}

impl Default for MockRuntime {
    fn default() -> Self {
        MockRuntime {
            calls: Mutex::new(Vec::new()),
            kv: Mutex::new(HashMap::new()),
            files: Mutex::new(HashMap::new()),
            current_branch: Branch::new("dev.policy-claude".into()).unwrap(),
            pr_for_branch: None,
            has_unaddressed_changes: false,
            review_state: None,
            ci_status: CiStatus::Passing,
            is_clean: true,
            next_pr: 1,
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
    async fn is_clean(&self) -> Result<bool, GitError> {
        Ok(self.is_clean)
    }
    async fn worktree_add(&self, _branch: &Branch, _at: &Path) -> Result<(), GitError> {
        Ok(())
    }
    async fn worktree_remove(&self, _at: &Path) -> Result<(), GitError> {
        Ok(())
    }
}

#[async_trait]
impl GitHub for MockRuntime {
    async fn file_pr(&self, title: &str, body: &str, base: &Branch) -> Result<u64, GitHubError> {
        if self.should_fail("file_pr") {
            return Err(GitHubError::Failed {
                op: "file_pr",
                detail: "mock forced failure".into(),
            });
        }
        self.record(Call::FilePr {
            title: title.into(),
            body: body.into(),
            base: base.clone(),
        });
        Ok(self.next_pr)
    }
    async fn pr_for_branch(&self, _branch: &Branch) -> Result<Option<u64>, GitHubError> {
        Ok(self.pr_for_branch)
    }
    async fn merge_pr(&self, pr: u64) -> Result<(), GitHubError> {
        if self.should_fail("merge_pr") {
            return Err(GitHubError::Failed {
                op: "merge_pr",
                detail: "mock forced failure".into(),
            });
        }
        self.record(Call::MergePr { pr });
        Ok(())
    }
    async fn has_unaddressed_changes(&self, _pr: u64) -> Result<bool, GitHubError> {
        Ok(self.has_unaddressed_changes)
    }
    async fn review_state(&self, _pr: u64) -> Result<Option<ReviewState>, GitHubError> {
        Ok(self.review_state)
    }
    async fn ci_status(&self, _pr: u64) -> Result<CiStatus, GitHubError> {
        Ok(self.ci_status)
    }
}

#[async_trait]
impl Spawner for MockRuntime {
    async fn spawn_worker(&self, spec: WorkerSpec) -> Result<AgentName, SpawnError> {
        self.record(Call::SpawnWorker {
            spec_task: spec.task.clone(),
            step_count: spec.steps.len(),
        });
        Ok(spec
            .name
            .unwrap_or_else(|| AgentName::new("worker-mock".into()).unwrap()))
    }
    async fn spawn_gemini(&self, spec: GeminiSpec) -> Result<AgentName, SpawnError> {
        self.record(Call::SpawnGemini {
            spec_task: spec.task.clone(),
            step_count: spec.steps.len(),
        });
        Ok(spec
            .name
            .unwrap_or_else(|| AgentName::new("gemini-mock".into()).unwrap()))
    }
    async fn fork_wave(&self, specs: Vec<ForkSpec>) -> Vec<Result<AgentName, SpawnError>> {
        self.record(Call::ForkWave { n: specs.len() });
        specs
            .into_iter()
            .enumerate()
            .map(|(i, s)| {
                Ok(s.name
                    .unwrap_or_else(|| AgentName::new(format!("fork-mock-{i}")).unwrap()))
            })
            .collect()
    }
    async fn reclaim_worktree(&self, _child: &AgentName) -> Result<(), SpawnError> {
        Ok(())
    }
    async fn kill_pane(&self, _child: &AgentName) -> Result<(), SpawnError> {
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
    async fn paste(&self, _pane: &PaneId, _text: &str) -> Result<(), TmuxError> {
        Ok(())
    }
    async fn kill_pane(&self, _pane: &PaneId) -> Result<(), TmuxError> {
        Ok(())
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

impl Log for MockRuntime {
    fn info(&self, msg: &str) {
        self.record(Call::LogInfo { msg: msg.into() });
    }
    fn error(&self, msg: &str) {
        self.record(Call::LogError { msg: msg.into() });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::caps::PolicyCaps;

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
        let m = MockRuntime::failing("merge_pr");
        assert!(m.merge_pr(3).await.is_err());
    }
}
