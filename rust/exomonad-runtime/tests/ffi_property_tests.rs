use exomonad_runtime::common::HostResult;
use extism::{Function, Manifest, Plugin, UserData, Val, ValType, Wasm};
use proptest::prelude::*;
use proptest::test_runner::{Config, TestRunner};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};

// Load WASM bytes statically to avoid disk I/O and enable caching
const WASM_BYTES: &[u8] = include_bytes!("fixtures/wasm-guest.wasm");

#[derive(Debug, Serialize, Deserialize)]
struct TestCall {
    function: String,
    args: Value,
}

#[derive(Debug, Serialize, Deserialize)]
struct TestResult<T> {
    success: bool,
    result: Option<T>,
    error: Option<String>,
}

fn call_guest<T: for<'de> Deserialize<'de>>(
    plugin: &mut Plugin,
    func_name: &str,
    args: Value,
) -> T {
    let call = TestCall {
        function: func_name.to_string(),
        args,
    };

    let input_bytes = serde_json::to_vec(&call).unwrap();
    let res_bytes = plugin
        .call::<Vec<u8>, Vec<u8>>("handle_test_call", input_bytes)
        .expect("WASM call failed");

    let res: TestResult<T> =
        serde_json::from_slice(&res_bytes).expect("Failed to deserialize TestResult");

    if !res.success {
        panic!("Guest returned error: {:?}", res.error);
    }

    res.result.expect("Success but no result")
}

// Helper to create a dummy function (returns void or empty)
fn dummy_fn(name: &str) -> Function {
    Function::new(
        name,
        [ValType::I64],
        [ValType::I64],
        UserData::new(()),
        |_plugin, _inputs, outputs, _user_data| {
            // Return null/empty pointer (0)
            outputs[0] = Val::I64(0);
            Ok(())
        },
    )
    .with_namespace("env")
}

fn dummy_void_fn(name: &str) -> Function {
    Function::new(name, [ValType::I64], [], UserData::new(()), |_, _, _, _| {
        Ok(())
    })
    .with_namespace("env")
}

// Helper to build the plugin with one active mock function and the rest dummies
fn build_test_plugin(active_fn: Function) -> Plugin {
    let active_name = active_fn.name().to_string();

    let all_names = vec![
        "git_get_branch",
        "git_get_worktree",
        "git_get_dirty_files",
        "git_get_recent_commits",
        "git_has_unpushed_commits",
        "git_get_remote_url",
        "git_get_repo_info",
        "github_list_issues",
        "github_get_issue",
        "github_create_pr",
        "github_list_prs",
        "github_get_pr_for_branch",
        "github_get_pr_review_comments",
        "agent_spawn",
        "agent_spawn_batch",
        "agent_cleanup",
        "agent_cleanup_batch",
        "agent_list",
        "fs_read_file",
        "fs_write_file",
        "file_pr",
        "wait_for_copilot_review",
    ];

    let void_names = vec!["log_info", "log_error", "emit_event"];

    let mut functions = Vec::new();

    // Add active function
    functions.push(active_fn);

    // Add dummies for others
    for name in all_names {
        if name != active_name {
            functions.push(dummy_fn(name));
        }
    }
    for name in void_names {
        if name != active_name {
            functions.push(dummy_void_fn(name));
        }
    }

    let manifest = Manifest::new([Wasm::data(WASM_BYTES)]);
    Plugin::new(&manifest, functions, true).expect("Failed to create plugin")
}

// Generic test runner for stateful FFI tests
// I: Input type (Host side)
// O: Output type (Host side)
// S: Strategy for Input
fn run_stateful_test<I, O, S>(
    strategy: S,
    func_name: &str,
    // Factory to create the host function, given access to shared state
    host_fn_factory: impl Fn(Arc<Mutex<Option<(I, O)>>>) -> Function,
    // Generator for expected output based on input
    output_gen: impl Fn(&I) -> O,
) where
    I: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned + Send + Sync + 'static,
    O: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned + Send + Sync + 'static,
    S: Strategy<Value = I>,
{
    let state: Arc<Mutex<Option<(I, O)>>> = Arc::new(Mutex::new(None));
    let host_fn = host_fn_factory(state.clone());
    let plugin = build_test_plugin(host_fn);

    let mut runner = TestRunner::new(Config::with_cases(10));

    // We need to use RefCell or similar if we want to mutate plugin inside the closure,
    // but TestRunner runs the closure repeatedly.
    // Actually, TestRunner::run takes `&mut self` and a closure.
    // The closure can capture `&mut plugin` if we wrap it?
    // No, FnMut allows mutating captured state.
    // But `runner.run` takes `impl Fn(&S::Value) -> ...`. This is `Fn`, not `FnMut`?
    // Checking docs: TestRunner::run takes `Fn`. So we can't mutate captured variables.
    // Solution: Wrap Plugin in RefCell (for interior mutability)

    let plugin_cell = std::cell::RefCell::new(plugin);

    runner
        .run(&strategy, |input| {
            let expected_output = output_gen(&input);

            // Update state
            {
                let mut guard = state.lock().unwrap();
                *guard = Some((input.clone(), expected_output.clone()));
            }

            let input_json = serde_json::to_value(&input).unwrap();

            // Borrow plugin mutably
            let mut plugin = plugin_cell.borrow_mut();
            let result: O = call_guest(&mut plugin, func_name, input_json);

            assert_eq!(result, expected_output);
            Ok(())
        })
        .unwrap();
}

// Generic host function implementation that checks input against state and returns output from state
fn stateful_host_fn<I, O>(name: &str, state: Arc<Mutex<Option<(I, O)>>>) -> Function
where
    I: Debug + PartialEq + DeserializeOwned + Send + Sync + 'static,
    O: Debug + Clone + Serialize + Send + Sync + 'static,
{
    Function::new(
        name,
        [ValType::I64],
        [ValType::I64],
        UserData::new(state),
        |plugin, inputs, outputs, user_data| {
            let offset = inputs[0].unwrap_i64() as u64;
            let mem = plugin.memory_from_val(&Val::I64(offset as i64)).unwrap();
            let bytes = plugin.memory_bytes(mem).unwrap();

            let received: I = serde_json::from_slice(bytes).expect("Host failed to parse input");

            // UserData wraps T in Arc<Mutex<T>>.
            // My T is Arc<Mutex<Option<(I, O)>>>.
            // So we have Arc<Mutex<Arc<Mutex<Option<(I, O)>>>>>.

            // 1. Lock the outer mutex (UserData)
            // Note: user_data might be the Arc itself or have a method to access it.
            // Based on previous error, user_data.get().unwrap() returns the Arc struct (by clone? or it is the Arc?).
            // If user_data is UserData<T>, get() returns &T? No, user_data seems to be the wrapper.

            // Let's trust the error: found `Arc<Mutex<Arc<Mutex...>>>`
            let outer_arc = user_data.get().unwrap();
            let inner_arc_guard = outer_arc.lock().unwrap();

            // inner_arc_guard is MutexGuard<Arc<Mutex<Option...>>>
            // Dereference to get Arc<Mutex<Option...>>
            let inner_arc = &*inner_arc_guard;

            // 2. Lock the inner mutex
            let guard = inner_arc.lock().unwrap();

            let (expected_input, output_val) =
                (*guard).as_ref().expect("State not set by test runner");

            assert_eq!(&received, expected_input, "Host received unexpected input");

            let res = HostResult::Success(output_val.clone());

            let out_bytes = serde_json::to_vec(&res).unwrap();
            let out_mem = plugin.memory_new(out_bytes).unwrap();
            outputs[0] = Val::I64(out_mem.offset() as i64);
            Ok(())
        },
    )
    .with_namespace("env")
}

// ============================================================================
// Git Branch Test
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
struct GitHostInput {
    #[serde(rename = "workingDir")]
    working_dir: String,
    #[serde(rename = "containerId")]
    container_id: String,
}

prop_compose! {
    fn arb_git_host_input()(
        working_dir in "[a-z0-9/_-]+",
        container_id in "[a-z0-9]+"
    ) -> GitHostInput {
        GitHostInput {
            working_dir,
            container_id,
        }
    }
}

#[test]
fn test_git_get_branch_roundtrip() {
    run_stateful_test(
        arb_git_host_input(),
        "git_get_branch",
        |state| stateful_host_fn("git_get_branch", state),
        |_| "main".to_string(),
    );
}

use exomonad_runtime::services::git::{Commit, RepoInfo, WorktreeInfo};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
struct GitLogInput {
    #[serde(rename = "workingDir")]
    working_dir: String,
    #[serde(rename = "containerId")]
    container_id: String,
    limit: u32,
}

prop_compose! {
    fn arb_git_log_input()(
        working_dir in "[a-z0-9/_-]+",
        container_id in "[a-z0-9]+",
        limit in 1u32..100
    ) -> GitLogInput {
        GitLogInput {
            working_dir,
            container_id,
            limit,
        }
    }
}

#[test]
fn test_git_get_worktree_roundtrip() {
    run_stateful_test(
        arb_git_host_input(),
        "git_get_worktree",
        |state| stateful_host_fn("git_get_worktree", state),
        |input| WorktreeInfo {
            path: format!("/path/to/{}", input.working_dir),
            branch: "feature-branch".to_string(),
        },
    );
}

#[test]
fn test_git_get_dirty_files_roundtrip() {
    run_stateful_test(
        arb_git_host_input(),
        "git_get_dirty_files",
        |state| stateful_host_fn("git_get_dirty_files", state),
        |_| vec!["file1.rs".to_string(), "file2.hs".to_string()],
    );
}

#[test]
fn test_git_get_recent_commits_roundtrip() {
    run_stateful_test(
        arb_git_log_input(),
        "git_get_recent_commits",
        |state| stateful_host_fn("git_get_recent_commits", state),
        |_| {
            vec![
                Commit {
                    hash: "123".to_string(),
                    message: "msg1".to_string(),
                    author: "me".to_string(),
                    date: "now".to_string(),
                },
                Commit {
                    hash: "456".to_string(),
                    message: "msg2".to_string(),
                    author: "you".to_string(),
                    date: "then".to_string(),
                },
            ]
        },
    );
}

#[test]
fn test_git_has_unpushed_commits_roundtrip() {
    // This requires a slightly different strategy because output is not just function of input
    // But for roundtrip testing, we can just say "if input has 'odd' length, return true" to vary it.
    run_stateful_test(
        arb_git_host_input(),
        "git_has_unpushed_commits",
        |state| stateful_host_fn("git_has_unpushed_commits", state),
        |input| input.working_dir.len() % 2 == 0,
    );
}

#[test]
fn test_git_get_remote_url_roundtrip() {
    run_stateful_test(
        arb_git_host_input(),
        "git_get_remote_url",
        |state| stateful_host_fn("git_get_remote_url", state),
        |_| "https://github.com/owner/repo.git".to_string(),
    );
}

#[test]
fn test_git_get_repo_info_roundtrip() {
    run_stateful_test(
        arb_git_host_input(),
        "git_get_repo_info",
        |state| stateful_host_fn("git_get_repo_info", state),
        |_| RepoInfo {
            branch: "main".to_string(),
            owner: Some("owner".to_string()),
            name: Some("repo".to_string()),
        },
    );
}

// ============================================================================
// Agent Control Tests
// ============================================================================

use exomonad_runtime::services::agent_control::{AgentType, SpawnAgentInput, SpawnResult};
use exomonad_shared::{GithubOwner, GithubRepo, IssueNumber};

prop_compose! {
    fn arb_issue_number()(n in 1u64..1000000) -> IssueNumber {
        IssueNumber::try_from(n).unwrap()
    }
}

prop_compose! {
    fn arb_github_owner()(s in "[a-zA-Z0-9_-]{1,39}") -> GithubOwner {
        GithubOwner::try_from(s).unwrap()
    }
}

prop_compose! {
    fn arb_github_repo()(s in "[a-zA-Z0-9_-]{1,100}") -> GithubRepo {
        GithubRepo::try_from(s).unwrap()
    }
}

fn arb_agent_type() -> BoxedStrategy<AgentType> {
    prop_oneof![Just(AgentType::Claude), Just(AgentType::Gemini),].boxed()
}

prop_compose! {
    fn arb_spawn_agent_input()(
        issue_id in (1u64..1000000).prop_map(|n| n.to_string()),
        owner in arb_github_owner(),
        repo in arb_github_repo(),
        worktree_dir in proptest::option::of("[a-z0-9/_-]{1,50}"),
        agent_type in arb_agent_type(),
    ) -> SpawnAgentInput {
        SpawnAgentInput {
            issue_id,
            owner,
            repo,
            worktree_dir,
            agent_type,
        }
    }
}

#[test]
fn test_agent_spawn_roundtrip() {
    run_stateful_test(
        arb_spawn_agent_input(),
        "agent_spawn",
        |state| stateful_host_fn("agent_spawn", state),
        |_| SpawnResult {
            worktree_path: "/tmp/wt".to_string(),
            branch_name: "gh-123/fix".to_string(),
            tab_name: "tab".to_string(),
            issue_title: "Title".to_string(),
            agent_type: "gemini".to_string(),
        },
    );
}

// ============================================================================
// Filesystem Tests
// ============================================================================

use exomonad_runtime::services::filesystem::{
    ReadFileInput, ReadFileOutput, WriteFileInput, WriteFileOutput,
};

prop_compose! {
    fn arb_read_file_input()(
        path in "[a-z0-9/._-]{1,50}",
        max_bytes in any::<usize>(),
    ) -> ReadFileInput {
        ReadFileInput {
            path,
            max_bytes,
        }
    }
}

#[test]
fn test_fs_read_file_roundtrip() {
    run_stateful_test(
        arb_read_file_input(),
        "fs_read_file",
        |state| stateful_host_fn("fs_read_file", state),
        |_| ReadFileOutput {
            content: "content".to_string(),
            bytes_read: 7,
            truncated: false,
        },
    );
}

prop_compose! {
    fn arb_write_file_input()(
        path in "[a-z0-9/._-]{1,50}",
        content in ".*",
        create_parents in any::<bool>(),
    ) -> WriteFileInput {
        WriteFileInput {
            path,
            content,
            create_parents,
        }
    }
}

#[test]
fn test_fs_write_file_roundtrip() {
    run_stateful_test(
        arb_write_file_input(),
        "fs_write_file",
        |state| stateful_host_fn("fs_write_file", state),
        |input| WriteFileOutput {
            bytes_written: input.content.len(),
            path: format!("/abs/{}", input.path),
        },
    );
}
