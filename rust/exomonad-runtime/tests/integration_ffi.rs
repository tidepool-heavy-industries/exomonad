use exomonad_runtime::common::HostResult;
use exomonad_runtime::services::agent_control::{
    AgentType, BatchSpawnResult, SpawnAgentsInput, SpawnResult,
};
use extism::{Function, Manifest, Plugin, UserData, Val, ValType, Wasm};
use serde_json::json;

#[test]
fn test_agent_spawn_batch_roundtrip() {
    let wasm_path = "tests/fixtures/wasm-guest.wasm";
    if !std::path::Path::new(wasm_path).exists() {
        panic!(
            "WASM fixture not found at {}. Build it with 'just wasm'.",
            wasm_path
        );
    }

    let wasm = Wasm::file(wasm_path);
    let manifest = Manifest::new([wasm]);

    // Define mock host function agent_spawn_batch
    let spawn_batch_fn = Function::new(
        "agent_spawn_batch",
        [ValType::I64],
        [ValType::I64],
        UserData::new(()),
        |plugin, inputs, outputs, _user_data| {
            // Deserialize input
            let mem = plugin.memory_from_val(&inputs[0]).unwrap();
            let bytes = plugin.memory_bytes(mem).unwrap();

            let input: SpawnAgentsInput =
                serde_json::from_slice(bytes).expect("Failed to deserialize SpawnAgentsInput");

            // Verify input
            assert_eq!(input.issue_ids, vec!["123".to_string()]);
            assert_eq!(input.owner.as_str(), "owner");
            assert_eq!(input.repo.as_str(), "repo");
            assert_eq!(input.agent_type, AgentType::Gemini);

            // Serialize output
            let result = HostResult::Success(BatchSpawnResult {
                spawned: vec![SpawnResult {
                    worktree_path: "/tmp/wt".to_string(),
                    branch_name: "gh-123/fix".to_string(),
                    tab_name: "tab".to_string(),
                    issue_title: "Title".to_string(),
                    agent_type: "gemini".to_string(),
                }],
                failed: vec![],
            });

            let out_bytes = serde_json::to_vec(&result).unwrap();
            let out_mem = plugin.memory_new(out_bytes).unwrap();
            outputs[0] = Val::I64(out_mem.offset() as i64);
            Ok(())
        },
    )
    .with_namespace("env");

    let dummy_fn = |name: &str| {
        Function::new(
            name,
            [ValType::I64],
            [ValType::I64],
            UserData::new(()),
            |_plugin, _inputs, outputs, _user_data| {
                outputs[0] = Val::I64(0);
                Ok(())
            },
        )
        .with_namespace("env")
    };

    let dummy_void_fn = |name: &str| {
        Function::new(
            name,
            [ValType::I64],
            [],
            UserData::new(()),
            |_plugin, _inputs, _outputs, _user_data| Ok(()),
        )
        .with_namespace("env")
    };

    let functions = vec![
        spawn_batch_fn,
        dummy_fn("agent_spawn"),
        dummy_fn("agent_cleanup"),
        dummy_fn("agent_cleanup_batch"),
        dummy_fn("agent_list"),
        // Git
        dummy_fn("git_get_branch"),
        dummy_fn("git_get_worktree"),
        dummy_fn("git_get_dirty_files"),
        dummy_fn("git_get_recent_commits"),
        dummy_fn("git_has_unpushed_commits"),
        dummy_fn("git_get_remote_url"),
        dummy_fn("git_get_repo_info"),
        // GitHub
        dummy_fn("github_list_issues"),
        dummy_fn("github_get_issue"),
        dummy_fn("github_create_pr"),
        dummy_fn("github_list_prs"),
        dummy_fn("github_get_pr_for_branch"),
        dummy_fn("github_get_pr_review_comments"),
        // Log
        dummy_void_fn("log_info"),
        dummy_void_fn("log_error"),
        dummy_void_fn("emit_event"),
        // Filesystem
        dummy_fn("fs_read_file"),
        dummy_fn("fs_write_file"),
        // File PR
        dummy_fn("file_pr"),
        // Copilot
        dummy_fn("wait_for_copilot_review"),
    ];

    let mut plugin = Plugin::new(&manifest, functions, true).expect("Failed to create plugin");

    let tool_args = json!({
        "issues": ["123"],
        "owner": "owner",
        "repo": "repo",
        "agent_type": "gemini"
    });

    let mcp_input = json!({
        "toolName": "spawn_agents",
        "toolArgs": tool_args
    });

    let input_bytes = serde_json::to_vec(&mcp_input).unwrap();
    let res: Vec<u8> = plugin
        .call("handle_mcp_call", input_bytes)
        .expect("WASM call failed");

    let res_json: serde_json::Value = serde_json::from_slice(&res).unwrap();

    // Check success
    assert_eq!(res_json["success"], true, "MCP call failed: {:?}", res_json);

    // Check result
    let result = &res_json["result"];
    let batch_result: BatchSpawnResult = serde_json::from_value(result.clone()).unwrap();

    if batch_result.spawned.is_empty() {
        println!("Batch result failed: {:?}", batch_result.failed);
    }

    assert_eq!(
        batch_result.spawned.len(),
        1,
        "Spawned list is empty, failures: {:?}",
        batch_result.failed
    );
    assert_eq!(batch_result.spawned[0].branch_name, "gh-123/fix");
    assert_eq!(batch_result.spawned[0].agent_type, "gemini");
}
