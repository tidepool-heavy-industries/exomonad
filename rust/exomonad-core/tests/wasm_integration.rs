//! End-to-end integration tests: Rust host ↔ Haskell WASM plugin via trampoline.
//!
//! Loads the actual compiled WASM binary, registers mock effect handlers,
//! and verifies the full protobuf encoding/decoding pipeline:
//!
//! ```text
//! WASM guest → yield_effect / suspend → EffectRegistry → mock handler → EffectResponse → WASM guest
//! ```
//!
//! All WASM exports return `WasmResult<O>` (Done | Suspend envelope).
//! `PluginManager::call` (trampoline) is the single dispatch path.
//!
//! Requires: `just wasm-all` to build the WASM binary first.

use async_trait::async_trait;
use exomonad_core::{EffectError, EffectHandler, EffectResult, RuntimeBuilder};
use prost::Message;
use serde_json::{json, Value};
use serial_test::serial;

// ============================================================================
// Test Infrastructure
// ============================================================================

fn wasm_binary_bytes() -> Vec<u8> {
    let manifest = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = manifest.join("../../.exo/wasm/wasm-guest-unified.wasm");
    assert!(
        path.exists(),
        "WASM binary not found at {path:?}. Build with `just wasm-all`."
    );
    std::fs::read(&path).expect("Failed to read WASM binary")
}

async fn build_test_runtime() -> exomonad_core::Runtime {
    let wasm_bytes = wasm_binary_bytes();
    RuntimeBuilder::new()
        .with_effect_handler(MockGitHandler)
        .with_effect_handler(MockLogHandler)
        .with_effect_handler(MockAgentHandler)
        .with_effect_handler(MockFsHandler)
        .with_effect_handler(MockFilePRHandler)
        .with_effect_handler(MockMergePRHandler)
        .with_effect_handler(MockPopupHandler)
        .with_effect_handler(MockEventsHandler)
        .with_effect_handler(MockSessionHandler)
        .with_effect_handler(MockKvHandler)
        .with_effect_handler(MockGitHubHandler)
        .with_effect_handler(MockCopilotHandler)
        .with_wasm_bytes(wasm_bytes)
        .build()
        .await
        .expect("Failed to build runtime with WASM plugin")
}

/// Helper: call a tool and return the MCPCallOutput JSON.
async fn call_tool(
    runtime: &exomonad_core::Runtime,
    role: &str,
    tool_name: &str,
    tool_args: Value,
) -> Value {
    let input = json!({
        "role": role,
        "toolName": tool_name,
        "toolArgs": tool_args
    });
    runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .unwrap_or_else(|e| panic!("handle_mcp_call failed for {tool_name}: {e}"))
}

/// Helper: assert a tool call succeeded.
fn assert_tool_success(output: &Value, tool_name: &str) {
    assert_eq!(
        output["success"], true,
        "{tool_name} should succeed: {output:#}"
    );
}

/// Helper: assert a tool call failed.
fn assert_tool_error(output: &Value, tool_name: &str) {
    assert_eq!(
        output["success"], false,
        "{tool_name} should fail: {output:#}"
    );
}

// ============================================================================
// Mock Effect Handlers
// ============================================================================

struct MockGitHandler;

#[async_trait]
impl EffectHandler for MockGitHandler {
    fn namespace(&self) -> &str {
        "git"
    }

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::git::*;

        match effect_type {
            "git.get_branch" => {
                let _req = GetBranchRequest::decode(payload)
                    .map_err(|e| EffectError::invalid_input(format!("decode: {e}")))?;
                Ok(GetBranchResponse {
                    branch: "mock-main".into(),
                    detached: false,
                }
                .encode_to_vec())
            }
            "git.get_status" => {
                let _req = GetStatusRequest::decode(payload)
                    .map_err(|e| EffectError::invalid_input(format!("decode: {e}")))?;
                Ok(GetStatusResponse {
                    dirty_files: vec![],
                    staged_files: vec![],
                    untracked_files: vec![],
                }
                .encode_to_vec())
            }
            "git.get_recent_commits" => {
                let _req = GetCommitsRequest::decode(payload)
                    .map_err(|e| EffectError::invalid_input(format!("decode: {e}")))?;
                Ok(GetCommitsResponse { commits: vec![] }.encode_to_vec())
            }
            "git.has_unpushed_commits" => {
                let _req = HasUnpushedCommitsRequest::decode(payload)
                    .map_err(|e| EffectError::invalid_input(format!("decode: {e}")))?;
                Ok(HasUnpushedCommitsResponse {
                    has_unpushed: false,
                    count: 0,
                }
                .encode_to_vec())
            }
            "git.get_remote_url" => {
                let _req = GetRemoteUrlRequest::decode(payload)
                    .map_err(|e| EffectError::invalid_input(format!("decode: {e}")))?;
                Ok(GetRemoteUrlResponse {
                    url: "https://github.com/test/test.git".into(),
                }
                .encode_to_vec())
            }
            "git.get_repo_info" => {
                let _req = GetRepoInfoRequest::decode(payload)
                    .map_err(|e| EffectError::invalid_input(format!("decode: {e}")))?;
                Ok(GetRepoInfoResponse {
                    branch: "main".into(),
                    owner: "test-owner".into(),
                    name: "test-repo".into(),
                }
                .encode_to_vec())
            }
            _ => Err(EffectError::not_found(format!("mock_git/{effect_type}"))),
        }
    }
}

struct MockLogHandler;

#[async_trait]
impl EffectHandler for MockLogHandler {
    fn namespace(&self) -> &str {
        "log"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::log::*;

        match effect_type {
            "log.log" | "log.debug" | "log.info" | "log.warn" | "log.error" => {
                Ok(LogResponse { success: true }.encode_to_vec())
            }
            "log.emit_event" => Ok(EmitEventResponse {
                event_id: "mock-evt-1".into(),
            }
            .encode_to_vec()),
            _ => Err(EffectError::not_found(format!("mock_log/{effect_type}"))),
        }
    }
}

struct MockAgentHandler;

#[async_trait]
impl EffectHandler for MockAgentHandler {
    fn namespace(&self) -> &str {
        "agent"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::agent::*;

        match effect_type {
            "agent.spawn_subtree" => {
                let agent = AgentInfo {
                    id: "test-subtree-claude".into(),
                    issue: String::new(),
                    worktree_path: "/tmp/test-worktree".into(),
                    branch_name: "main.test-subtree".into(),
                    agent_type: 1,
                    role: 1,
                    status: 1,
                    zellij_tab: "test-subtree".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 1,
                };
                Ok(SpawnSubtreeResponse { agent: Some(agent) }.encode_to_vec())
            }
            "agent.spawn_leaf_subtree" => {
                let agent = AgentInfo {
                    id: "test-leaf-gemini".into(),
                    issue: String::new(),
                    worktree_path: "/tmp/test-leaf-worktree".into(),
                    branch_name: "main.test-leaf".into(),
                    agent_type: 2,
                    role: 2,
                    status: 1,
                    zellij_tab: "test-leaf".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 1,
                };
                Ok(SpawnLeafSubtreeResponse { agent: Some(agent) }.encode_to_vec())
            }
            "agent.spawn_worker" => {
                let agent = AgentInfo {
                    id: "test-worker-gemini".into(),
                    issue: String::new(),
                    worktree_path: String::new(),
                    branch_name: String::new(),
                    agent_type: 2,
                    role: 0,
                    status: 1,
                    zellij_tab: "test-worker".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 2,
                };
                Ok(SpawnWorkerResponse { agent: Some(agent) }.encode_to_vec())
            }
            "agent.cleanup_merged" => Ok(CleanupMergedResponse {
                cleaned: vec![],
                skipped: vec![],
                errors: vec![],
            }
            .encode_to_vec()),
            _ => Err(EffectError::not_found(format!("mock_agent/{effect_type}"))),
        }
    }
}

struct MockFsHandler;

#[async_trait]
impl EffectHandler for MockFsHandler {
    fn namespace(&self) -> &str {
        "fs"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::fs::*;

        match effect_type {
            "fs.read_file" => Ok(ReadFileResponse {
                content: "mock file content".into(),
                bytes_read: 17,
                truncated: false,
                total_size: 17,
            }
            .encode_to_vec()),
            _ => Err(EffectError::not_found(format!("mock_fs/{effect_type}"))),
        }
    }
}

struct MockFilePRHandler;

#[async_trait]
impl EffectHandler for MockFilePRHandler {
    fn namespace(&self) -> &str {
        "file_pr"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::file_pr::*;

        match effect_type {
            "file_pr.file_pr" => Ok(FilePrResponse {
                pr_url: "https://github.com/test/test/pull/42".into(),
                pr_number: 42,
                head_branch: "mock-main".into(),
                base_branch: "main".into(),
                created: true,
            }
            .encode_to_vec()),
            _ => Err(EffectError::not_found(format!(
                "mock_file_pr/{effect_type}"
            ))),
        }
    }
}

struct MockMergePRHandler;

#[async_trait]
impl EffectHandler for MockMergePRHandler {
    fn namespace(&self) -> &str {
        "merge_pr"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::merge_pr::*;

        match effect_type {
            "merge_pr.merge_pr" => Ok(MergePrResponse {
                success: true,
                message: "Merged PR #42".into(),
                git_fetched: true,
            }
            .encode_to_vec()),
            _ => Err(EffectError::not_found(format!(
                "mock_merge_pr/{effect_type}"
            ))),
        }
    }
}

struct MockPopupHandler;

#[async_trait]
impl EffectHandler for MockPopupHandler {
    fn namespace(&self) -> &str {
        "popup"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::popup::*;

        match effect_type {
            "popup.show_popup" => Ok(ShowPopupResponse {
                button: "submit".into(),
                values: b"{}".to_vec(),
            }
            .encode_to_vec()),
            _ => Err(EffectError::not_found(format!("mock_popup/{effect_type}"))),
        }
    }
}

struct MockEventsHandler;

#[async_trait]
impl EffectHandler for MockEventsHandler {
    fn namespace(&self) -> &str {
        "events"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::events::*;

        match effect_type {
            "events.notify_parent" => Ok(NotifyParentResponse { ack: true }.encode_to_vec()),
            "events.notify_event" => Ok(NotifyEventResponse { success: true }.encode_to_vec()),
            _ => Err(EffectError::not_found(format!("mock_events/{effect_type}"))),
        }
    }
}

struct MockSessionHandler;

#[async_trait]
impl EffectHandler for MockSessionHandler {
    fn namespace(&self) -> &str {
        "session"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::session::*;

        match effect_type {
            "session.register_claude_id" => {
                Ok(RegisterClaudeSessionResponse { success: true }.encode_to_vec())
            }
            "session.register_team" => Ok(RegisterTeamResponse { success: true }.encode_to_vec()),
            _ => Err(EffectError::not_found(format!(
                "mock_session/{effect_type}"
            ))),
        }
    }
}

struct MockKvHandler;

#[async_trait]
impl EffectHandler for MockKvHandler {
    fn namespace(&self) -> &str {
        "kv"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::kv::*;

        match effect_type {
            "kv.get" => Ok(GetResponse {
                found: false,
                value: String::new(),
            }
            .encode_to_vec()),
            "kv.set" => Ok(SetResponse { success: true }.encode_to_vec()),
            _ => Err(EffectError::not_found(format!("mock_kv/{effect_type}"))),
        }
    }
}

struct MockGitHubHandler;

#[async_trait]
impl EffectHandler for MockGitHubHandler {
    fn namespace(&self) -> &str {
        "github"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::github::*;

        match effect_type {
            "github.list_issues" => Ok(ListIssuesResponse { issues: vec![] }.encode_to_vec()),
            "github.get_issue" => Ok(GetIssueResponse {
                issue: None,
                comments: vec![],
            }
            .encode_to_vec()),
            "github.list_pull_requests" => Ok(ListPullRequestsResponse {
                pull_requests: vec![],
            }
            .encode_to_vec()),
            "github.get_pull_request" => Ok(GetPullRequestResponse {
                pull_request: None,
                reviews: vec![],
            }
            .encode_to_vec()),
            "github.get_pull_request_for_branch" => Ok(GetPullRequestForBranchResponse {
                pull_request: None,
                found: false,
            }
            .encode_to_vec()),
            "github.create_pull_request" => Ok(CreatePullRequestResponse {
                pull_request: None,
                url: "https://github.com/test/test/pull/99".into(),
            }
            .encode_to_vec()),
            "github.get_pull_request_review_comments" => {
                Ok(GetPullRequestReviewCommentsResponse { comments: vec![] }.encode_to_vec())
            }
            _ => Err(EffectError::not_found(format!("mock_github/{effect_type}"))),
        }
    }
}

struct MockCopilotHandler;

#[async_trait]
impl EffectHandler for MockCopilotHandler {
    fn namespace(&self) -> &str {
        "copilot"
    }

    async fn handle(
        &self,
        effect_type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::copilot::*;

        match effect_type {
            "copilot.wait_for_copilot_review" => Ok(WaitForCopilotReviewResponse {
                status: "found".into(),
                comments: vec![],
            }
            .encode_to_vec()),
            _ => Err(EffectError::not_found(format!(
                "mock_copilot/{effect_type}"
            ))),
        }
    }
}

// ============================================================================
// Tool Listing Tests (per role)
// ============================================================================

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_tl_tools_include_spawn_and_merge() {
    let runtime = build_test_runtime().await;

    let tools: Vec<Value> = runtime
        .plugin_manager()
        .call("handle_list_tools", &json!({"role": "tl"}))
        .await
        .expect("handle_list_tools failed");

    let names: Vec<&str> = tools.iter().filter_map(|t| t["name"].as_str()).collect();

    for expected in [
        "spawn_subtree",
        "spawn_leaf_subtree",
        "spawn_workers",
        "merge_pr",
        "file_pr",
        "notify_parent",
    ] {
        assert!(
            names.contains(&expected),
            "TL role missing tool '{expected}'. Got: {names:?}"
        );
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_dev_tools_include_file_pr() {
    let runtime = build_test_runtime().await;

    let tools: Vec<Value> = runtime
        .plugin_manager()
        .call("handle_list_tools", &json!({"role": "dev"}))
        .await
        .expect("handle_list_tools failed for dev");

    let names: Vec<&str> = tools.iter().filter_map(|t| t["name"].as_str()).collect();

    assert!(
        names.contains(&"file_pr"),
        "Dev role missing file_pr. Got: {names:?}"
    );
    assert!(
        names.contains(&"notify_parent"),
        "Dev role missing notify_parent. Got: {names:?}"
    );

    // Dev should NOT have spawn or merge tools
    assert!(
        !names.contains(&"spawn_subtree"),
        "Dev role should not have spawn_subtree"
    );
    assert!(
        !names.contains(&"merge_pr"),
        "Dev role should not have merge_pr"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_worker_tools_include_notify_parent() {
    let runtime = build_test_runtime().await;

    let tools: Vec<Value> = runtime
        .plugin_manager()
        .call("handle_list_tools", &json!({"role": "worker"}))
        .await
        .expect("handle_list_tools failed for worker");

    let names: Vec<&str> = tools.iter().filter_map(|t| t["name"].as_str()).collect();

    assert!(
        names.contains(&"notify_parent"),
        "Worker role missing notify_parent. Got: {names:?}"
    );

    // Worker should have minimal tools
    assert!(
        !names.contains(&"spawn_subtree"),
        "Worker should not have_spawn_subtree"
    );
    assert!(
        !names.contains(&"file_pr"),
        "Worker should not have file_pr"
    );
}

// ============================================================================
// Tool Roundtrip Tests (multi-effect trampoline)
// ============================================================================

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_spawn_subtree_roundtrip() {
    let runtime = build_test_runtime().await;

    let output = call_tool(
        &runtime,
        "tl",
        "spawn_subtree",
        json!({
            "task": "Implement feature X",
            "branch_name": "feature-x"
        }),
    )
    .await;

    assert_tool_success(&output, "spawn_subtree");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_spawn_leaf_subtree_roundtrip() {
    let runtime = build_test_runtime().await;

    let output = call_tool(
        &runtime,
        "tl",
        "spawn_leaf_subtree",
        json!({
            "task": "Implement the Rust handler",
            "branch_name": "rust-handler"
        }),
    )
    .await;

    assert_tool_success(&output, "spawn_leaf_subtree");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_spawn_workers_roundtrip() {
    let runtime = build_test_runtime().await;

    let output = call_tool(
        &runtime,
        "tl",
        "spawn_workers",
        json!({
            "specs": [{
                "name": "rust-impl",
                "task": "Implement the Rust side"
            }]
        }),
    )
    .await;

    assert_tool_success(&output, "spawn_workers");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_file_pr_roundtrip() {
    let runtime = build_test_runtime().await;

    let output = call_tool(
        &runtime,
        "tl",
        "file_pr",
        json!({
            "title": "Add feature X",
            "body": "Implements feature X as specified"
        }),
    )
    .await;

    assert_tool_success(&output, "file_pr");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_merge_pr_roundtrip() {
    let runtime = build_test_runtime().await;

    let output = call_tool(
        &runtime,
        "tl",
        "merge_pr",
        json!({
            "pr_number": 42
        }),
    )
    .await;

    assert_tool_success(&output, "merge_pr");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_notify_parent_roundtrip() {
    let runtime = build_test_runtime().await;

    let output = call_tool(
        &runtime,
        "tl",
        "notify_parent",
        json!({
            "status": "success",
            "message": "All tasks completed"
        }),
    )
    .await;

    assert_tool_success(&output, "notify_parent");
}

// ============================================================================
// Argument Validation Tests
// ============================================================================

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_tool_missing_required_field() {
    let runtime = build_test_runtime().await;

    // spawn_subtree requires "task"
    let output = call_tool(&runtime, "tl", "spawn_subtree", json!({})).await;

    assert_tool_error(&output, "spawn_subtree (missing task)");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_tool_unknown_name_returns_error() {
    let runtime = build_test_runtime().await;

    let output = call_tool(&runtime, "tl", "nonexistent_tool_xyz", json!({})).await;

    assert_tool_error(&output, "nonexistent tool");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_tool_wrong_role_returns_error() {
    let runtime = build_test_runtime().await;

    // Dev role should not have spawn_subtree
    let output = call_tool(
        &runtime,
        "dev",
        "spawn_subtree",
        json!({"task": "test", "branch_name": "test"}),
    )
    .await;

    assert_tool_error(&output, "spawn_subtree as dev");
}

// ============================================================================
// Error Propagation Tests
// ============================================================================

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_unhandled_effect_returns_error() {
    // Build runtime with only log handler — agent effects will fail
    let wasm_bytes = wasm_binary_bytes();
    let runtime = RuntimeBuilder::new()
        .with_effect_handler(MockLogHandler)
        .with_wasm_bytes(wasm_bytes)
        .build()
        .await
        .expect("Failed to build runtime");

    let output = call_tool(
        &runtime,
        "tl",
        "spawn_subtree",
        json!({
            "task": "test",
            "branch_name": "test-branch"
        }),
    )
    .await;

    assert_tool_error(&output, "spawn_subtree without agent handler");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_effect_handler_error_propagates() {
    /// Handler that always returns an error.
    struct FailingAgentHandler;

    #[async_trait]
    impl EffectHandler for FailingAgentHandler {
        fn namespace(&self) -> &str {
            "agent"
        }

        async fn handle(
            &self,
            _effect_type: &str,
            _payload: &[u8],
            _ctx: &exomonad_core::effects::EffectContext,
        ) -> EffectResult<Vec<u8>> {
            Err(EffectError::custom(
                "spawn_failed",
                "Zellij session not found",
            ))
        }
    }

    let wasm_bytes = wasm_binary_bytes();
    let runtime = RuntimeBuilder::new()
        .with_effect_handler(MockGitHandler)
        .with_effect_handler(MockLogHandler)
        .with_effect_handler(FailingAgentHandler)
        .with_effect_handler(MockFsHandler)
        .with_wasm_bytes(wasm_bytes)
        .build()
        .await
        .expect("Failed to build runtime");

    let output = call_tool(
        &runtime,
        "tl",
        "spawn_subtree",
        json!({
            "task": "test",
            "branch_name": "test-branch"
        }),
    )
    .await;

    assert_tool_error(&output, "spawn_subtree with failing handler");

    // Verify the error message propagated
    let error_msg = output["error"].as_str().unwrap_or_default();
    assert!(
        error_msg.contains("spawn_failed") || error_msg.contains("Zellij session"),
        "Error message should contain handler error info, got: {error_msg}"
    );
}

// ============================================================================
// Hook Tests
// ============================================================================

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_hook_session_start() {
    let runtime = build_test_runtime().await;

    let hook_input = json!({
        "role": "tl",
        "session_id": "test-session-123",
        "hook_event_name": "SessionStart"
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_pre_tool_use", &hook_input)
        .await
        .expect("SessionStart hook failed");

    // SessionStart should return a valid hook response
    assert!(
        output.is_object(),
        "SessionStart should return an object: {output:#}"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_hook_pre_tool_use_allow() {
    let runtime = build_test_runtime().await;

    let hook_input = json!({
        "role": "tl",
        "session_id": "test-session",
        "hook_event_name": "PreToolUse",
        "tool_name": "Write"
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_pre_tool_use", &hook_input)
        .await
        .expect("PreToolUse hook failed");

    // Default behavior should allow tool use
    assert!(
        output.is_object(),
        "PreToolUse should return an object: {output:#}"
    );
}

// ============================================================================
// Multi-Suspend Verification
// ============================================================================

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_tool_multiple_suspends() {
    // spawn_subtree yields multiple effects:
    // 1. log.info (logging)
    // 2. git.get_branch or git.get_repo_info
    // 3. agent.spawn_subtree
    // 4. log.emit_event
    // Each suspend/resume cycle goes through the trampoline.
    let runtime = build_test_runtime().await;

    let output = call_tool(
        &runtime,
        "tl",
        "spawn_subtree",
        json!({
            "task": "Multi-suspend test",
            "branch_name": "multi-suspend"
        }),
    )
    .await;

    assert_tool_success(&output, "spawn_subtree (multi-suspend)");

    // The result should contain agent info from the mock handler
    let result = &output["result"];
    assert!(
        result.is_object() || result.is_string(),
        "spawn_subtree result should contain agent info: {output:#}"
    );
}

/// Test if sending a long task text to spawn_subtree causes a hang.
/// spawn_leaf_subtree hangs — this checks if the issue is text length vs P.render.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_spawn_subtree_long_text() {
    let runtime = build_test_runtime().await;

    // Simulate roughly the same amount of text as P.render + P.leafProfile produces
    let long_task = "A".repeat(600);

    let output = call_tool(
        &runtime,
        "tl",
        "spawn_subtree",
        json!({
            "task": long_task,
            "branch_name": "long-text-test"
        }),
    )
    .await;

    assert_tool_success(&output, "spawn_subtree (long text)");
}

/// Diagnostic: does spawn_subtree hang with multiline text containing newlines?
/// If this passes but spawn_leaf_subtree hangs, the issue is in the Haskell handler,
/// not in text encoding.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_spawn_subtree_multiline_text() {
    let runtime = build_test_runtime().await;

    // Simulate the text that P.render would produce for spawn_leaf_subtree
    let multiline_task = "## TASK\nImplement the Rust handler\n\n## Completion Protocol (Leaf Subtree)\nYou are a **leaf agent** in your own git worktree and branch.\n\nWhen you are done:\n\n1. **Commit your changes** with a descriptive message.\n2. **File a PR** using `file_pr` tool.\n3. **Wait for Copilot review** if it arrives.\n4. **Call `notify_parent`** with status `success`.\n\n**DO NOT:**\n- Merge your own PR\n- Push to main\n- Create additional branches";

    let result = tokio::time::timeout(
        std::time::Duration::from_secs(30),
        call_tool(
            &runtime,
            "tl",
            "spawn_subtree",
            json!({
                "task": multiline_task,
                "branch_name": "multiline-test"
            }),
        ),
    )
    .await;

    match result {
        Ok(output) => {
            eprintln!("=== spawn_subtree with multiline text completed: {output:#} ===");
            assert_tool_success(&output, "spawn_subtree (multiline)");
        }
        Err(_) => {
            panic!("spawn_subtree with multiline text hung after 30s — text encoding issue");
        }
    }
}

/// Diagnostic: spawn_leaf_subtree with timeout to observe trampoline logs.
/// Calls spawn_subtree first to warm up the WASM runtime, then tries spawn_leaf_subtree.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_spawn_leaf_subtree_timeout_diagnostic() {
    let runtime = build_test_runtime().await;

    // Warm up: call spawn_subtree first (this works)
    eprintln!("=== DIAGNOSTIC: Warming up with spawn_subtree ===");
    let warmup = call_tool(
        &runtime,
        "tl",
        "spawn_subtree",
        json!({"task": "warmup", "branch_name": "warmup"}),
    )
    .await;
    eprintln!("=== Warmup completed: success={} ===", warmup["success"]);

    eprintln!("=== DIAGNOSTIC: Starting spawn_leaf_subtree call ===");

    let result = tokio::time::timeout(
        std::time::Duration::from_secs(30),
        call_tool(
            &runtime,
            "tl",
            "spawn_leaf_subtree",
            json!({
                "task": "Test task",
                "branch_name": "test-leaf"
            }),
        ),
    )
    .await;

    match result {
        Ok(output) => {
            eprintln!("=== DIAGNOSTIC: spawn_leaf_subtree completed: {output:#} ===");
            assert_tool_success(&output, "spawn_leaf_subtree (diagnostic)");
        }
        Err(_) => {
            eprintln!("=== DIAGNOSTIC: spawn_leaf_subtree TIMED OUT after 30s ===");
            panic!("spawn_leaf_subtree hung — check trampoline logs above");
        }
    }
}

/// Diagnostic: spawn_workers with timeout.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
#[serial]
async fn wasm_spawn_workers_timeout_diagnostic() {
    let runtime = build_test_runtime().await;

    eprintln!("=== DIAGNOSTIC: Starting spawn_workers call ===");

    let result = tokio::time::timeout(
        std::time::Duration::from_secs(30),
        call_tool(
            &runtime,
            "tl",
            "spawn_workers",
            json!({
                "specs": [{
                    "name": "diag-worker",
                    "task": "Test task"
                }]
            }),
        ),
    )
    .await;

    match result {
        Ok(output) => {
            eprintln!("=== DIAGNOSTIC: spawn_workers completed: {output:#} ===");
            assert_tool_success(&output, "spawn_workers (diagnostic)");
        }
        Err(_) => {
            eprintln!("=== DIAGNOSTIC: spawn_workers TIMED OUT after 30s ===");
            panic!("spawn_workers hung — check trampoline logs above");
        }
    }
}
