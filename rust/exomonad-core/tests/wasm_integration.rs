//! End-to-end integration tests: Rust host ↔ Haskell WASM plugin via yield_effect.
//!
//! Loads the actual compiled WASM binary, registers mock effect handlers,
//! and verifies the full protobuf encoding/decoding pipeline:
//!
//! ```text
//! WASM guest → yield_effect → EffectRegistry → mock handler → EffectResponse → WASM guest
//! ```
//!
//! Requires: `just wasm-dev tl` to build the WASM binary first.

use async_trait::async_trait;
use exomonad_core::{EffectError, EffectHandler, EffectResult, RuntimeBuilder};
use prost::Message;
use serde_json::{json, Value};

// ============================================================================
// Test Infrastructure
// ============================================================================

fn wasm_binary_bytes() -> Vec<u8> {
    let manifest = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = manifest.join("../../.exomonad/wasm/wasm-guest-tl.wasm");
    assert!(
        path.exists(),
        "WASM binary not found at {path:?}. Build with `just wasm-dev tl`."
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

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
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

    async fn handle(&self, effect_type: &str, _payload: &[u8]) -> EffectResult<Vec<u8>> {
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

/// Returns empty/populated agent responses.
struct MockAgentHandler;

#[async_trait]
impl EffectHandler for MockAgentHandler {
    fn namespace(&self) -> &str {
        "agent"
    }

    async fn handle(&self, effect_type: &str, _payload: &[u8]) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::agent::*;

        match effect_type {
            "agent.list" => Ok(ListResponse { agents: vec![] }.encode_to_vec()),
            "agent.spawn_batch" => {
                let agent = AgentInfo {
                    id: "gh-999-test-claude".into(),
                    issue: "999".into(),
                    worktree_path: "/tmp/test-worktree".into(),
                    branch_name: "gh-999/test-feature".into(),
                    agent_type: 1,
                    role: 1,
                    status: 1,
                    zellij_tab: "999-test".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 0,
                };
                Ok(SpawnBatchResponse {
                    agents: vec![agent],
                    errors: vec![],
                }
                .encode_to_vec())
            }
            "agent.cleanup_batch" => Ok(CleanupBatchResponse {
                cleaned: vec![],
                failed: vec![],
                errors: vec![],
            }
            .encode_to_vec()),
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

    async fn handle(&self, effect_type: &str, _payload: &[u8]) -> EffectResult<Vec<u8>> {
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
        .call("handle_list_tools", &())
        .await
        .expect("handle_list_tools failed");

    assert!(!tools.is_empty(), "Expected at least one tool definition");

    let tool_names: Vec<&str> = tools.iter().filter_map(|t| t["name"].as_str()).collect();

    // The tl role exposes agent management + popup tools
    assert!(
        tool_names.contains(&"list_agents"),
        "Missing list_agents in {tool_names:?}"
    );
    assert!(
        tool_names.contains(&"spawn_agents"),
        "Missing spawn_agents in {tool_names:?}"
    );
    assert!(
        tool_names.contains(&"cleanup_agents"),
        "Missing cleanup_agents in {tool_names:?}"
    );
}

/// Spawn with multiple issues — exercises a larger SpawnBatchResponse.
///
/// A larger response means more protobuf bytes, testing the WASM memory
/// boundary more aggressively (the production bug is a truncation error).
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_spawn_multiple_agents_roundtrip() {
    let runtime = build_test_runtime().await;

    let input = json!({
        "toolName": "spawn_agents",
        "toolArgs": {
            "issues": ["100", "200", "300"],
            "owner": "test-owner",
            "repo": "test-repo"
        }
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call failed for spawn_agents with 3 issues");

    assert_eq!(
        output["success"], true,
        "spawn_agents with 3 issues should succeed: {output:#}"
    );
}

/// Agent list effect roundtrip: list_agents tool → agent.list effect → mock response.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_list_agents_roundtrip() {
    let runtime = build_test_runtime().await;

    let input = json!({
        "toolName": "list_agents",
        "toolArgs": {}
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call failed for list_agents");

    assert_eq!(
        output["success"], true,
        "list_agents should succeed: {output:#}"
    );
}

/// The critical path: spawn_agents tool → agent.spawn_batch effect → mock response.
///
/// This exercises the exact code path that fails in production with:
/// `BinaryError "failed to parse varint128: not enough bytes"`
///
/// If this test passes with mock handlers, the protobuf encoding is correct
/// and the bug is in the real AgentHandler's response or an environmental factor.
/// If it fails, we've reproduced the decode error and can inspect the diagnostic output.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_spawn_agents_roundtrip() {
    // Enable tracing to capture diagnostic output from yield_effect
    let _ = tracing_subscriber::fmt()
        .with_env_filter("exomonad_core=debug")
        .with_test_writer()
        .try_init();

    let runtime = build_test_runtime().await;

    let input = json!({
        "toolName": "spawn_agents",
        "toolArgs": {
            "issues": ["999"],
            "owner": "test-owner",
            "repo": "test-repo"
        }
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call failed for spawn_agents");

    // If this assertion fails, check the tracing output for diagnostic bytes
    assert_eq!(
        output["success"], true,
        "spawn_agents should succeed with mock handler: {output:#}"
    );

    // Verify the mock agent data flows through correctly
    let result_str = output["result"].to_string();
    assert!(
        result_str.contains("999") || result_str.contains("gh-999"),
        "Expected issue 999 in result: {result_str}"
    );
}

/// Large response test: exercises WASM memory boundary with realistic payload sizes.
///
/// Production spawn_agents with real GitHub data produces responses with long
/// worktree paths, branch names, and error messages. This tests the pipeline
/// with a response that's larger than typical test payloads.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wasm_large_response_roundtrip() {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("exomonad_core=debug")
        .with_test_writer()
        .try_init();

    // Use a custom handler that returns a large SpawnBatchResponse
    struct LargeResponseAgentHandler;

    #[async_trait]
    impl EffectHandler for LargeResponseAgentHandler {
        fn namespace(&self) -> &str {
            "agent"
        }

        async fn handle(&self, effect_type: &str, _payload: &[u8]) -> EffectResult<Vec<u8>> {
            use exomonad_proto::effects::agent::*;

            match effect_type {
                "agent.spawn_batch" => {
                    // Create 5 agents with long, realistic field values
                    let agents: Vec<AgentInfo> = (1..=5)
                        .map(|i| AgentInfo {
                            id: format!(
                                "gh-{i}-implement-comprehensive-ffi-wire-compatibility-test-coverage-claude"
                            ),
                            issue: format!("{i}"),
                            worktree_path: format!(
                                "/Users/developer/hangars/project/repo/.exomonad/worktrees/gh-{i}-implement-comprehensive-ffi-wire-compatibility-test-coverage-claude"
                            ),
                            branch_name: format!(
                                "gh-{i}/implement-comprehensive-ffi-wire-compatibility-test-coverage"
                            ),
                            agent_type: 1,
                            role: 1,
                            status: 1,
                            zellij_tab: format!(
                                "{i}-implement-comprehensive-ffi-wire-compatibility-test"
                            ),
                            error: String::new(),
                            pr_number: 0,
                            pr_url: String::new(),
                            topology: 0,
                        })
                        .collect();

                    let errors: Vec<String> = (6..=8)
                        .map(|i| {
                            format!(
                                "Issue {i}: spawn_agent timed out after 60s: \
                                 zellij new-tab failed with status: exit status: 1 \
                                 (stderr: Error: session 'main' not found, \
                                 layout file was: /tmp/exomonad-layout-{i}.kdl)"
                            )
                        })
                        .collect();

                    let resp = SpawnBatchResponse { agents, errors };
                    let bytes = prost::Message::encode_to_vec(&resp);
                    eprintln!(
                        "Large response: {} bytes, wrapped in EffectResponse",
                        bytes.len()
                    );
                    Ok(bytes)
                }
                _ => Err(EffectError::not_found(format!("large_agent/{effect_type}"))),
            }
        }
    }

    let wasm_bytes = wasm_binary_bytes();
    let runtime = RuntimeBuilder::new()
        .with_effect_handler(LargeResponseAgentHandler)
        .with_effect_handler(MockLogHandler)
        .with_wasm_bytes(wasm_bytes)
        .build()
        .await
        .expect("Failed to build runtime");

    let input = json!({
        "toolName": "spawn_agents",
        "toolArgs": {
            "issues": ["1", "2", "3", "4", "5", "6", "7", "8"],
            "owner": "test-owner",
            "repo": "test-repo"
        }
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call failed for large spawn_agents response");

    assert_eq!(
        output["success"], true,
        "spawn_agents with large response should succeed: {output:#}"
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
        "toolName": "list_agents",
        "toolArgs": {}
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call should return output even on effect error");

    // The tool should report failure (agent handler not registered)
    assert_eq!(
        output["success"], false,
        "list_agents should fail without agent handler: {output:#}"
    );
}
