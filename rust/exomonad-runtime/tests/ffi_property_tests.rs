use extism::{Plugin, Manifest, Wasm, Function, Val, ValType, UserData};
use exomonad_runtime::common::{HostResult};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::path::Path;
use proptest::prelude::*;

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

fn create_plugin(functions: Vec<Function>) -> Plugin {
    let wasm_path = "tests/fixtures/wasm-guest.wasm";
    if !Path::new(wasm_path).exists() {
        panic!("WASM fixture not found. Run 'just wasm-dev tl' and copy to fixtures.");
    }
    
    let wasm = Wasm::file(wasm_path);
    let manifest = Manifest::new([wasm]);
    Plugin::new(&manifest, functions, true).expect("Failed to create plugin")
}

fn call_guest<T: for<'de> Deserialize<'de>>(plugin: &mut Plugin, func_name: &str, args: Value) -> T {
    let call = TestCall {
        function: func_name.to_string(),
        args,
    };
    
    let input_bytes = serde_json::to_vec(&call).unwrap();
    // call returns Result<Vec<u8>>, need to handle that
    let res_bytes = plugin.call::<Vec<u8>, Vec<u8>>(
        "handle_test_call", 
        input_bytes
    ).expect("WASM call failed");
    
    let res: TestResult<T> = serde_json::from_slice(&res_bytes).expect("Failed to deserialize TestResult");
    
    if !res.success {
        panic!("Guest returned error: {:?}", res.error);
    }
    
    res.result.expect("Success but no result")
}

// Mock host function helper
fn mock_host_fn<F>(name: &str, handler: F) -> Function 
where
    F: Fn(&[u8]) -> Vec<u8> + Send + Sync + 'static,
{
    Function::new(
        name,
        [ValType::I64],
        [ValType::I64],
        UserData::new(handler),
        |plugin, inputs, outputs, user_data| {
            let offset = inputs[0].unwrap_i64() as u64;
            // memory_from_val takes a Val, construct one from offset
            let mem = plugin.memory_from_val(&Val::I64(offset as i64)).unwrap();
            let bytes = plugin.memory_bytes(mem).unwrap();
            
            let handler_guard = user_data.get().unwrap();
            let handler = handler_guard.lock().unwrap();
            let res_bytes = handler(bytes);
            
            let out_mem = plugin.memory_new(res_bytes).unwrap();
            outputs[0] = Val::I64(out_mem.offset() as i64);
            Ok(())
        }
    ).with_namespace("env")
}

// ============================================================================
// Git Branch Test
// ============================================================================

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct GitHostInput {
    workingDir: String,
    containerId: String,
}

proptest! {
    #[test]
    fn test_git_get_branch_roundtrip(
        working_dir in "[a-z0-9/_-]+",
        container_id in "[a-z0-9]+"
    ) {
        let input = GitHostInput {
            workingDir: working_dir.clone(),
            containerId: container_id.clone(),
        };
        let input_json = serde_json::to_value(&input).unwrap();
        
        let expected_result = "main".to_string();
        let expected_result_clone = expected_result.clone();
        
        let git_fn = mock_host_fn("git_get_branch", move |bytes| {
            let received: GitHostInput = serde_json::from_slice(bytes).expect("Host failed to parse input");
            assert_eq!(received.workingDir, working_dir);
            assert_eq!(received.containerId, container_id);
            
            let res = HostResult::Success(expected_result_clone.clone());
            serde_json::to_vec(&res).unwrap()
        });
        
        // Need dummy functions for all other imports to satisfy linker
        let dummy = |name| mock_host_fn(name, |_| vec![]);
        let dummy_void = |name| Function::new(name, [ValType::I64], [], UserData::new(()), |_,_,_,_| Ok(())).with_namespace("env");

        let functions = vec![
            git_fn,
            dummy("git_get_worktree"),
            dummy("git_get_dirty_files"),
            dummy("git_get_recent_commits"),
            dummy("git_has_unpushed_commits"),
            dummy("git_get_remote_url"),
            dummy("git_get_repo_info"),
            dummy("github_list_issues"),
            dummy("github_get_issue"),
            dummy("github_create_pr"),
            dummy("github_list_prs"),
            dummy("github_get_pr_for_branch"),
            dummy("github_get_pr_review_comments"),
            dummy_void("log_info"),
            dummy_void("log_error"),
            dummy_void("emit_event"),
            dummy("agent_spawn"),
            dummy("agent_spawn_batch"),
            dummy("agent_cleanup"),
            dummy("agent_cleanup_batch"),
            dummy("agent_list"),
            dummy("fs_read_file"),
            dummy("fs_write_file"),
            dummy("file_pr"),
            dummy("wait_for_copilot_review"),
        ];

        let mut plugin = create_plugin(functions);
        let result: String = call_guest(&mut plugin, "git_get_branch", input_json);
        
        assert_eq!(result, expected_result);
    }
}
