//! End-to-end integration tests: Rust host ↔ Haskell WASM plugin via yield_effect.
//!
//! Loads the actual compiled WASM binary, registers mock effect handlers,
//! and verifies the full protobuf encoding/decoding pipeline:
//!
//! ```text
//! WASM guest → yield_effect → EffectRegistry → mock handler → EffectResponse → WASM guest
//! ```
//!
//! Requires: `just wasm-all` to build the WASM binary first.

use async_trait::async_trait;
use exomonad_core::{EffectError, EffectHandler, EffectResult, RuntimeBuilder};
use prost::Message;
use serde_json::{json, Value};

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
        .with_wasm_bytes(wasm_bytes)
        .build()
        .await
        .expect("Failed to build runtime with WASM plugin")
}

// ============================================================================
// Mock Effect Handlers
// ============================================================================

/// Returns known protobuf responses for git effects.
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

/// Accepts all log effects, returns success.
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

/// Returns mock agent spawn responses.
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
                    branch_name: "main/test-subtree".into(),
                    agent_type: 1,
                    role: 1,
                    status: 1,
                    zellij_tab: "test-subtree".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 1, // WORKTREE_PER_AGENT
                };
                Ok(SpawnSubtreeResponse { agent: Some(agent) }.encode_to_vec())
            }
            "agent.spawn_worker" => {
                let agent = AgentInfo {
                    id: "test-worker-gemini".into(),
                    issue: String::new(),
                    worktree_path: String::new(), // Shared dir
                    branch_name: String::new(),
                    agent_type: 2, // GEMINI
                    role: 0,
                    status: 1,
                    zellij_tab: "test-worker".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 2, // SHARED_DIR
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

/// Returns mock filesystem responses.
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

// ============================================================================
// Tests
// ============================================================================

/// Baseline: WASM loads and returns tool definitions (no effects triggered).
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_list_tools_returns_definitions() {
    let runtime = build_test_runtime().await;

    let tools: Vec<Value> = runtime
        .plugin_manager()
        .call("handle_list_tools", &json!({"role": "tl"}))
        .await
        .expect("handle_list_tools failed");

    assert!(!tools.is_empty(), "Expected at least one tool definition");

    let tool_names: Vec<&str> = tools.iter().filter_map(|t| t["name"].as_str()).collect();

    // The tl role exposes spawn + popup + messaging + coordination tools
    assert!(
        tool_names.contains(&"spawn_subtree"),
        "Missing spawn_subtree in {tool_names:?}"
    );
    assert!(
        tool_names.contains(&"spawn_workers"),
        "Missing spawn_workers in {tool_names:?}"
    );
}

/// spawn_subtree roundtrip: tool → agent.spawn_subtree effect → mock response.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_spawn_subtree_roundtrip() {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("exomonad_core=debug")
        .with_test_writer()
        .try_init();

    let runtime = build_test_runtime().await;

    let input = json!({
        "role": "tl",
        "toolName": "spawn_subtree",
        "toolArgs": {
            "task": "Implement feature X",
            "branch_name": "feature-x"
        }
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call failed for spawn_subtree");

    assert_eq!(
        output["result"]["success"], true,
        "spawn_subtree should succeed with mock handler: {output:#}"
    );
}

/// spawn_workers roundtrip: tool → agent.spawn_worker effect → mock response.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_spawn_workers_roundtrip() {
    let runtime = build_test_runtime().await;

    let input = json!({
        "role": "tl",
        "toolName": "spawn_workers",
        "toolArgs": {
            "specs": [
                {
                    "name": "rust-impl",
                    "task": "Implement the Rust side of feature X"
                }
            ]
        }
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call failed for spawn_workers");

    assert_eq!(
        output["result"]["success"], true,
        "spawn_workers should succeed with mock handler: {output:#}"
    );
}

/// Verify that effect handler errors propagate as tool errors.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_unhandled_effect_returns_error() {
    // Build runtime with only log handler — agent effects will fail
    let wasm_bytes = wasm_binary_bytes();
    let runtime = RuntimeBuilder::new()
        .with_effect_handler(MockLogHandler)
        .with_wasm_bytes(wasm_bytes)
        .build()
        .await
        .expect("Failed to build runtime");

    let input = json!({
        "role": "tl",
        "toolName": "spawn_subtree",
        "toolArgs": {
            "task": "test",
            "branch_name": "test-branch"
        }
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call should return output even on effect error");

    // The tool should report failure (agent handler not registered)
    assert_eq!(
        output["result"]["success"], false,
        "spawn_subtree should fail without agent handler: {output:#}"
    );
}
