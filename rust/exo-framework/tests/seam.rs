use async_trait::async_trait;
use exo_caps::{
    Addressee, AgentName, AgentType, Branch, Bus, BusError, CapResult, ChildKind, ChildLiveness,
    CommitFiles, Fs, FsError, Git, GitError, Kv, KvError, Message, PaneId, Persona, Process,
    ProcessError, RoleKind, SpawnError, SpawnSpec, Spawner, Tmux, TmuxError, ToolName, Topology,
    TopologyError, TopologyView,
};
use exo_framework::{
    ok_json, parse, schema_json, BoxFuture, ErasedTool, Exomonad, HookDecision, HookInput, RoleDef,
    SessionStartOutput, SystemCtx, SystemOutcome, ToolOutput,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::future::Future;
use std::path::Path;
use std::pin::Pin;
use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

// --- block_on helper ---
fn block_on<F: Future>(mut fut: F) -> F::Output {
    unsafe fn noop_clone(_: *const ()) -> RawWaker {
        noop_raw_waker()
    }
    unsafe fn noop(_: *const ()) {}
    fn noop_raw_waker() -> RawWaker {
        static VTABLE: RawWakerVTable = RawWakerVTable::new(noop_clone, noop, noop, noop);
        RawWaker::new(std::ptr::null(), &VTABLE)
    }

    let waker = unsafe { Waker::from_raw(noop_raw_waker()) };
    let mut cx = Context::from_waker(&waker);
    let mut fut = unsafe { Pin::new_unchecked(&mut fut) };

    loop {
        match fut.as_mut().poll(&mut cx) {
            Poll::Ready(v) => return v,
            Poll::Pending => {}
        }
    }
}

// --- TestCaps: Mock implementation of PolicyCaps ---
struct TestCaps;

#[async_trait]
impl Git for TestCaps {
    async fn current_branch(&self) -> Result<Branch, GitError> {
        unimplemented!()
    }
    async fn head_sha(&self) -> Result<String, GitError> {
        unimplemented!()
    }
    async fn merge_base(&self, _refish: &str) -> Result<Option<String>, GitError> {
        unimplemented!()
    }
    async fn fork_point(&self) -> Result<Option<String>, GitError> {
        unimplemented!()
    }
    async fn is_clean(&self) -> Result<bool, GitError> {
        unimplemented!()
    }
    async fn status_porcelain(&self) -> Result<Vec<String>, GitError> {
        unimplemented!()
    }
    async fn commits_between(
        &self,
        _base: &str,
        _head: &str,
    ) -> Result<Vec<CommitFiles>, GitError> {
        unimplemented!()
    }
    async fn fetch(&self) -> Result<(), GitError> {
        unimplemented!()
    }
    async fn merge(&self, _branch: &Branch) -> Result<(), GitError> {
        unimplemented!()
    }
    async fn worktree_add(&self, _branch: &Branch, _at: &Path) -> Result<(), GitError> {
        unimplemented!()
    }
    async fn worktree_remove(&self, _at: &Path) -> Result<(), GitError> {
        unimplemented!()
    }
    async fn is_ahead_of(&self, _base: &str) -> Result<bool, GitError> {
        unimplemented!()
    }
    async fn is_behind(&self, _base: &str) -> Result<bool, GitError> {
        unimplemented!()
    }
}

#[async_trait]
impl Bus for TestCaps {
    async fn deliver(&self, _to: Addressee, _msg: Message) -> Result<(), BusError> {
        unimplemented!()
    }
}

#[async_trait]
impl Spawner for TestCaps {
    async fn spawn<S: SpawnSpec>(&self, _spec: S) -> Result<AgentName, SpawnError> {
        unimplemented!()
    }
    async fn reclaim_worktree(&self, _child: &AgentName) -> Result<(), SpawnError> {
        unimplemented!()
    }
    async fn kill_pane(&self, _child: &AgentName) -> Result<(), SpawnError> {
        unimplemented!()
    }
}

#[async_trait]
impl Kv for TestCaps {
    async fn get(&self, _key: &str) -> Result<Option<String>, KvError> {
        unimplemented!()
    }
    async fn set(&self, _key: &str, _value: &str) -> Result<(), KvError> {
        unimplemented!()
    }
}

#[async_trait]
impl Fs for TestCaps {
    async fn read(&self, _path: &Path) -> Result<Vec<u8>, FsError> {
        unimplemented!()
    }
    async fn write_atomic(&self, _path: &Path, _bytes: &[u8]) -> Result<(), FsError> {
        unimplemented!()
    }
    async fn read_dir(&self, _path: &Path) -> Result<Vec<std::path::PathBuf>, FsError> {
        unimplemented!()
    }
}

#[async_trait]
impl Tmux for TestCaps {
    async fn new_pane(&self, _cwd: &Path, _cmd: &str) -> Result<PaneId, TmuxError> {
        unimplemented!()
    }
    async fn new_window(&self, _name: &str, _cwd: &Path, _cmd: &str) -> Result<PaneId, TmuxError> {
        unimplemented!()
    }
    async fn paste(&self, _pane: &PaneId, _text: &str) -> Result<(), TmuxError> {
        unimplemented!()
    }
    async fn kill_pane(&self, _pane: &PaneId) -> Result<(), TmuxError> {
        unimplemented!()
    }
    async fn list_panes(&self) -> Result<std::collections::HashSet<String>, TmuxError> {
        unimplemented!()
    }
}

#[async_trait]
impl Process for TestCaps {
    async fn run(
        &self,
        _program: &str,
        _args: &[String],
    ) -> Result<std::process::Output, ProcessError> {
        unimplemented!()
    }
}

#[async_trait]
impl Topology for TestCaps {
    async fn topology(&self) -> Result<TopologyView, TopologyError> {
        unimplemented!()
    }
}

#[async_trait]
impl ChildLiveness for TestCaps {
    async fn any_child_busy(&self) -> bool {
        unimplemented!()
    }
}

// --- Test Domain Types ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum TestRole {
    Lead,
    Reviewer,
}

impl RoleKind for TestRole {
    fn all() -> &'static [Self] {
        &[TestRole::Lead, TestRole::Reviewer]
    }
    fn agent_type(&self) -> AgentType {
        match self {
            TestRole::Lead => AgentType::Claude,
            TestRole::Reviewer => AgentType::Shoal,
        }
    }
    fn role_str(&self) -> &'static str {
        match self {
            TestRole::Lead => "lead",
            TestRole::Reviewer => "reviewer",
        }
    }
    fn protocol(&self) -> &'static str {
        match self {
            TestRole::Lead => "",
            TestRole::Reviewer => "review protocol",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum TestSystem {
    Action,
    Status,
}

struct TestSpawn;
impl SpawnSpec for TestSpawn {
    type Role = TestRole;
    fn role(&self) -> TestRole {
        TestRole::Lead
    }
    fn child_kind(&self) -> ChildKind {
        ChildKind::Worktree
    }
    fn name(&self) -> Option<AgentName> {
        None
    }
    fn name_prefix(&self) -> &str {
        "test"
    }
    fn fork_session(&self) -> bool {
        false
    }
    fn into_task(self) -> String {
        "task".into()
    }
}

struct EchoTool;
#[derive(Deserialize, JsonSchema)]
struct EchoArgs {
    msg: String,
}

#[async_trait]
impl<R: Send + Sync> ErasedTool<R> for EchoTool {
    fn name(&self) -> &str {
        "echo"
    }
    fn description(&self) -> &str {
        "echo description"
    }
    fn schema(&self) -> Value {
        schema_json::<EchoArgs>()
    }
    async fn call(&self, _ctx: &R, j: Value) -> CapResult<Value> {
        let args: EchoArgs = parse(j)?;
        ok_json(ToolOutput::text(args.msg))
    }
}

// Hook fns
fn pre_tool<'a>(_: &'a TestCaps, _: &'a HookInput) -> BoxFuture<'a, HookDecision> {
    Box::pin(async { HookDecision::Allow })
}

fn session_start<'a>(_: &'a TestCaps) -> BoxFuture<'a, SessionStartOutput> {
    Box::pin(async { SessionStartOutput::default() })
}

struct TestDomain;
impl Exomonad for TestDomain {
    type Caps = TestCaps;
    type Role = TestRole;
    type System = TestSystem;
    type Spawn = TestSpawn;

    fn role_def(role: TestRole) -> RoleDef<TestCaps> {
        match role {
            TestRole::Lead => RoleDef {
                tools: vec![Box::new(EchoTool)],
                pre_tool_use: pre_tool,
                session_start,
            },
            TestRole::Reviewer => RoleDef {
                tools: vec![],
                pre_tool_use: pre_tool,
                session_start,
            },
        }
    }

    fn handle_system<'a, C: SystemCtx>(
        _ctx: &'a C,
        _from: &'a Persona,
        system: &'a TestSystem,
    ) -> BoxFuture<'a, CapResult<SystemOutcome>> {
        Box::pin(async move {
            match system {
                TestSystem::Action => Ok(SystemOutcome::ReclaimSender),
                TestSystem::Status => Ok(SystemOutcome::Done),
            }
        })
    }
}

// --- Tests ---

#[test]
fn role_def_lists_expected_tools() {
    let rd = TestDomain::role_def(TestRole::Lead);
    let names: Vec<_> = rd.tools.iter().map(|t| t.name()).collect();
    assert_eq!(names, vec!["echo"]);

    let rd_rev = TestDomain::role_def(TestRole::Reviewer);
    assert!(rd_rev.tools.is_empty());
}

#[test]
fn role_def_pre_tool_use_and_session_start_hooks_fire() {
    let rd_lead = TestDomain::role_def(TestRole::Lead);
    let caps = TestCaps;
    let input = HookInput {
        tool_name: ToolName::new("echo".into()).unwrap(),
        tool_input: json!({}),
    };

    let decision = block_on((rd_lead.pre_tool_use)(&caps, &input));
    assert_eq!(decision, HookDecision::Allow);

    let session = block_on((rd_lead.session_start)(&caps));
    assert_eq!(session, SessionStartOutput::default());
}

#[test]
fn echo_tool_dispatches_through_json_edge() {
    let tool = EchoTool;
    let caps = TestCaps;
    let args = json!({ "msg": "hello" });
    let res = block_on(tool.call(&caps, args)).unwrap();
    assert_eq!(res, json!({ "text": "hello" }));
}

#[test]
fn role_protocol_default_is_empty_and_override_nonempty() {
    assert_eq!(TestRole::Lead.protocol(), "");
    assert_eq!(TestRole::Reviewer.protocol(), "review protocol");

    assert_eq!(TestRole::Lead.agent_type(), AgentType::Claude);
    assert_eq!(TestRole::Reviewer.agent_type(), AgentType::Shoal);
}

struct MockCtx {
    branch: Branch,
}

#[async_trait]
impl SystemCtx for MockCtx {
    fn own_branch(&self) -> &Branch {
        &self.branch
    }
    async fn head_sha(&self) -> CapResult<String> {
        Ok("deadbeef".into())
    }
    async fn deliver_parent(&self, _msg: Message) -> CapResult<()> {
        Ok(())
    }
    async fn deliver_to_self(&self, _from: &str, _summary: &str, _text: &str) -> CapResult<()> {
        Ok(())
    }
    async fn read_file(&self, _path: &std::path::Path) -> CapResult<Option<Vec<u8>>> {
        Ok(None)
    }
    async fn write_file(&self, _path: &std::path::Path, _bytes: &[u8]) -> CapResult<()> {
        Ok(())
    }
}

#[test]
fn handle_system_returns_reclaim_and_done() {
    let ctx = MockCtx {
        branch: Branch::new("main".into()).unwrap(),
    };
    let from = Persona::Agent(AgentName::new("test".into()).unwrap());

    let res_action = block_on(TestDomain::handle_system(&ctx, &from, &TestSystem::Action)).unwrap();
    assert_eq!(res_action, SystemOutcome::ReclaimSender);

    let res_status = block_on(TestDomain::handle_system(&ctx, &from, &TestSystem::Status)).unwrap();
    assert_eq!(res_status, SystemOutcome::Done);
}
