# Spec: Wave 1 Tidepool Tools (file_pr, merge_pr, notify_parent)

Single file change: `rust/exomonad-core/src/tidepool_backend.rs`

## ANTI-PATTERNS (READ FIRST)

- **DO NOT** modify any file other than `rust/exomonad-core/src/tidepool_backend.rs`
- **DO NOT** add any new crate dependencies
- **DO NOT** use `todo!()`, `unimplemented!()`, or `unwrap()` in non-test code
- **DO NOT** rename existing types or change existing tests (except `test_list_tools_returns_popup` per Step 8)
- **DO NOT** use EffectMachine at runtime for async service calls — only in mock tests
- **DO NOT** change the PopupEffect types or BridgeDispatcher (existing popup code stays untouched)
- **DO NOT** add comments explaining what you removed or changed — comments describe what IS

## READ FIRST

```
rust/exomonad-core/src/tidepool_backend.rs          # Current file (popup working end-to-end)
rust/exomonad-core/haskell/FilePREffect.hs           # Haskell GADT for file_pr
rust/exomonad-core/haskell/MergePREffect.hs          # Haskell GADT for merge_pr
rust/exomonad-core/haskell/NotifyEffect.hs           # Haskell GADT for notify_parent
rust/exomonad-core/src/services/file_pr.rs           # file_pr_async signature + FilePRInput/FilePROutput
rust/exomonad-core/src/services/merge_pr.rs          # merge_pr_async signature + MergePROutput
rust/exomonad-core/src/services/agent_control.rs     # resolve_agent_working_dir, resolve_parent_tab_name
rust/exomonad-core/src/services/zellij_events.rs     # inject_input
rust/exomonad-core/src/services/jj_workspace.rs      # JjWorkspaceService::new(PathBuf)
```

## STEP 1: Add FromCore/ToCore bridge types

After the existing `PopupResponse` struct (line ~66), add three new sections. Field order MUST match Haskell record declaration order exactly.

```rust
// =============================================================================
// file_pr bridge types
// =============================================================================

/// Mirrors Haskell: `FilePRInput { fprTitle, fprBody, fprBaseBranch }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "FilePRInput")]
struct FilePRToolInput {
    title: String,
    body: String,
    base_branch: String,
}

/// Mirrors Haskell: `FilePRResult { fprPrUrl, fprPrNumber, fprHeadBranch, fprResultBase, fprCreated }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "FilePRResult")]
struct FilePRToolResult {
    pr_url: String,
    pr_number: String,
    head_branch: String,
    result_base: String,
    created: String,
}

/// Mirrors Haskell GADT constructors for `FilePR`.
#[derive(Debug, FromCore)]
enum FilePRReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "CreateOrUpdatePR")]
    CreateOrUpdatePR(String, String, String),
}

// =============================================================================
// merge_pr bridge types
// =============================================================================

/// Mirrors Haskell: `MergePRInput { mprPrNumber, mprStrategy, mprWorkingDir }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MergePRInput")]
struct MergePRToolInput {
    pr_number: String,
    strategy: String,
    working_dir: String,
}

/// Mirrors Haskell: `MergePRResult { mprSuccess, mprMessage, mprJjFetched }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MergePRResult")]
struct MergePRToolResult {
    success: String,
    message: String,
    jj_fetched: String,
}

/// Mirrors Haskell GADT constructors for `MergePR`.
#[derive(Debug, FromCore)]
enum MergePRReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "MergePullRequest")]
    MergePullRequest(String, String, String),
}

// =============================================================================
// notify_parent bridge types
// =============================================================================

/// Mirrors Haskell: `NotifyInput { niStatus, niMessage }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NotifyInput")]
struct NotifyToolInput {
    status: String,
    message: String,
}

/// Mirrors Haskell: `NotifyResult { nrAck }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NotifyResult")]
struct NotifyToolResult {
    ack: String,
}

/// Mirrors Haskell GADT constructors for `Notify`.
#[derive(Debug, FromCore)]
enum NotifyReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "NotifyParent")]
    NotifyParent(String, String),
}
```

## STEP 2: Expand TidepoolBackend struct

Replace the existing `TidepoolBackend` struct with:

```rust
pub struct TidepoolBackend {
    /// Data constructor table from compiled PopupEffect.hs.
    table: Arc<DataConTable>,

    /// Compiled Haskell Core expression for `popupTool`.
    popup_expr: CoreExpr,

    /// Compiled file_pr expression + its DataConTable.
    file_pr_expr: CoreExpr,
    file_pr_table: Arc<DataConTable>,

    /// Compiled merge_pr expression + its DataConTable.
    merge_pr_expr: CoreExpr,
    merge_pr_table: Arc<DataConTable>,

    /// Compiled notify_parent expression + its DataConTable.
    notify_expr: CoreExpr,
    notify_table: Arc<DataConTable>,

    /// JJ workspace service for file_pr and merge_pr.
    jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,

    /// Zellij session name for popup and other UI services.
    zellij_session: Option<String>,

    /// Tag → effect name mapping for dispatch and logging.
    tag_names: Vec<String>,

    /// Agent identity context, threaded as user data to effect handlers.
    ctx: EffectContext,
}
```

## STEP 3: Expand TidepoolBackend::new()

Replace the existing `new()` method with:

```rust
pub fn new(zellij_session: Option<String>, ctx: EffectContext) -> Self {
    let (popup_expr, table) =
        tidepool_macro::haskell_expr!("haskell/PopupEffect.hs::popupTool");
    let (file_pr_expr, file_pr_table) =
        tidepool_macro::haskell_expr!("haskell/FilePREffect.hs::filePRTool");
    let (merge_pr_expr, merge_pr_table) =
        tidepool_macro::haskell_expr!("haskell/MergePREffect.hs::mergePRTool");
    let (notify_expr, notify_table) =
        tidepool_macro::haskell_expr!("haskell/NotifyEffect.hs::notifyTool");

    let working_dir = crate::services::agent_control::resolve_agent_working_dir(&ctx);
    let jj = Arc::new(crate::services::jj_workspace::JjWorkspaceService::new(working_dir));

    Self {
        table: Arc::new(table),
        popup_expr,
        file_pr_expr,
        file_pr_table: Arc::new(file_pr_table),
        merge_pr_expr,
        merge_pr_table: Arc::new(merge_pr_table),
        notify_expr,
        notify_table: Arc::new(notify_table),
        jj,
        zellij_session,
        tag_names: vec!["Popup".into()],
        ctx,
    }
}
```

## STEP 4: Add call methods

Add these three methods to the `impl TidepoolBackend` block, after the existing `call_popup` method:

```rust
/// Handle file_pr tool: parse MCP args, call file_pr service directly.
///
/// Bypasses EffectMachine at runtime (async service). The haskell_expr! compilation
/// validates the Haskell; mock tests verify the bridge; runtime calls services directly.
async fn call_file_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let title = args.get("title").and_then(|v| v.as_str()).unwrap_or("").to_string();
    let body = args.get("body").and_then(|v| v.as_str()).unwrap_or("").to_string();
    let base_branch = args.get("base_branch").and_then(|v| v.as_str()).map(|s| s.to_string());

    let working_dir = crate::services::agent_control::resolve_agent_working_dir(&self.ctx);

    let input = crate::services::file_pr::FilePRInput {
        title: title.clone(),
        body,
        base_branch,
        working_dir: Some(working_dir.to_string_lossy().to_string()),
    };

    debug!(title = %title, "Tidepool file_pr call");

    match crate::services::file_pr::file_pr_async(&input, self.jj.clone()).await {
        Ok(output) => Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({
                "pr_url": output.pr_url,
                "pr_number": output.pr_number.as_u64(),
                "head_branch": output.head_branch,
                "base_branch": output.base_branch,
                "created": output.created,
            })),
            error: None,
        }),
        Err(e) => {
            tracing::error!(error = %e, "Tidepool file_pr failed");
            Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(e.to_string()),
            })
        }
    }
}

/// Handle merge_pr tool: parse MCP args, call merge_pr service directly.
async fn call_merge_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let pr_number = args
        .get("pr_number")
        .and_then(|v| v.as_u64())
        .unwrap_or(0);
    let strategy = args
        .get("strategy")
        .and_then(|v| v.as_str())
        .unwrap_or("squash")
        .to_string();

    let working_dir = crate::services::agent_control::resolve_agent_working_dir(&self.ctx);
    let working_dir_str = working_dir.to_string_lossy().to_string();

    debug!(
        pr_number = pr_number,
        strategy = %strategy,
        "Tidepool merge_pr call"
    );

    let pr = crate::domain::PRNumber::new(pr_number);

    match crate::services::merge_pr::merge_pr_async(
        pr,
        &strategy,
        &working_dir_str,
        self.jj.clone(),
    )
    .await
    {
        Ok(output) => Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({
                "success": output.success,
                "message": output.message,
                "jj_fetched": output.jj_fetched,
            })),
            error: None,
        }),
        Err(e) => {
            tracing::error!(error = %e, "Tidepool merge_pr failed");
            Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(e.to_string()),
            })
        }
    }
}

/// Handle notify_parent tool: inject notification into parent's Zellij pane.
///
/// Simplified version: injects text directly via zellij_events::inject_input.
/// Skips event queue and remote forwarding (those are handled by the full
/// EventHandler when running through WasmBackend).
async fn call_notify_parent(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let status = args
        .get("status")
        .and_then(|v| v.as_str())
        .unwrap_or("success")
        .to_string();
    let message = args
        .get("message")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let agent_id = self.ctx.agent_name.to_string();
    let tab_name = crate::services::agent_control::resolve_parent_tab_name(&self.ctx);

    let notification = match status.as_str() {
        "success" => format!(
            "[CHILD COMPLETE: {}] {}",
            agent_id,
            if message.is_empty() {
                "Task completed successfully."
            } else {
                &message
            }
        ),
        "failure" => format!(
            "[CHILD FAILED: {}] {}",
            agent_id,
            if message.is_empty() {
                "Task failed."
            } else {
                &message
            }
        ),
        other => format!("[CHILD STATUS {}: {}] {}", agent_id, other, message),
    };

    debug!(
        agent_id = %agent_id,
        status = %status,
        tab = %tab_name,
        "Tidepool notify_parent call"
    );

    crate::services::zellij_events::inject_input(&tab_name, &notification);

    Ok(MCPCallOutput {
        success: true,
        result: Some(serde_json::json!({"ack": "delivered"})),
        error: None,
    })
}
```

## STEP 5: Add tool definitions

After the existing `popup_tool_definition()` function, add:

```rust
fn file_pr_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "file_pr".to_string(),
        description: "Create or update a pull request for the current branch. Auto-detects base branch from dot-separated naming.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "title": { "type": "string", "description": "PR title" },
                "body": { "type": "string", "description": "PR body/description" },
                "base_branch": { "type": "string", "description": "Target branch. Auto-detected from dot-separated naming if omitted." }
            },
            "required": ["title", "body"]
        }),
    }
}

fn merge_pr_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "merge_pr".to_string(),
        description: "Merge a GitHub pull request and fetch changes via jj".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "pr_number": { "type": "integer", "description": "PR number to merge" },
                "strategy": { "type": "string", "description": "Merge strategy: squash (default), merge, or rebase" }
            },
            "required": ["pr_number"]
        }),
    }
}

fn notify_parent_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "notify_parent".to_string(),
        description: "Signal to your parent that you are DONE. Injects notification into parent's Zellij pane.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "status": { "type": "string", "description": "'success' or 'failure'", "enum": ["success", "failure"] },
                "message": { "type": "string", "description": "One-line summary of what was accomplished or what went wrong" }
            },
            "required": ["status", "message"]
        }),
    }
}
```

## STEP 6: Update RuntimeBackend impl

Replace `list_tools`:

```rust
async fn list_tools(&self, role: &str) -> Result<Vec<ToolDefinition>> {
    debug!(role = %role, "Listing tools from tidepool backend");
    Ok(match role {
        "tl" => vec![
            popup_tool_definition(),
            file_pr_tool_definition(),
            merge_pr_tool_definition(),
            notify_parent_tool_definition(),
        ],
        "dev" => vec![
            file_pr_tool_definition(),
            notify_parent_tool_definition(),
        ],
        "worker" => vec![notify_parent_tool_definition()],
        _ => vec![],
    })
}
```

Replace `call_tool`:

```rust
async fn call_tool(
    &self,
    role: &str,
    tool_name: &str,
    args: JsonValue,
) -> Result<MCPCallOutput> {
    debug!(role = %role, tool = %tool_name, "Executing tool via tidepool");
    match tool_name {
        "popup" => self.call_popup(args).await,
        "file_pr" => self.call_file_pr(args).await,
        "merge_pr" => self.call_merge_pr(args).await,
        "notify_parent" => self.call_notify_parent(args).await,
        _ => Ok(MCPCallOutput {
            success: false,
            result: None,
            error: Some(format!("Unknown tool: {}", tool_name)),
        }),
    }
}
```

## STEP 7: Add tests

Add these tests inside the existing `mod tests` block, after the existing tests:

```rust
#[test]
fn test_file_pr_tool_definition_schema() {
    let def = file_pr_tool_definition();
    assert_eq!(def.name, "file_pr");
    assert_eq!(def.input_schema["type"], "object");
    assert!(def.input_schema["properties"]["title"].is_object());
    assert!(def.input_schema["properties"]["body"].is_object());
}

#[test]
fn test_merge_pr_tool_definition_schema() {
    let def = merge_pr_tool_definition();
    assert_eq!(def.name, "merge_pr");
    assert_eq!(def.input_schema["type"], "object");
    assert!(def.input_schema["properties"]["pr_number"].is_object());
}

#[test]
fn test_notify_parent_tool_definition_schema() {
    let def = notify_parent_tool_definition();
    assert_eq!(def.name, "notify_parent");
    assert_eq!(def.input_schema["type"], "object");
    assert!(def.input_schema["properties"]["status"].is_object());
}

#[tokio::test]
async fn test_list_tools_by_role() {
    let ctx = EffectContext {
        agent_name: crate::AgentName::from("test"),
        birth_branch: crate::BirthBranch::root(),
    };
    let backend = TidepoolBackend::new(None, ctx);

    let tl = backend.list_tools("tl").await.unwrap();
    assert_eq!(tl.len(), 4);
    assert!(tl.iter().any(|t| t.name == "popup"));
    assert!(tl.iter().any(|t| t.name == "file_pr"));
    assert!(tl.iter().any(|t| t.name == "merge_pr"));
    assert!(tl.iter().any(|t| t.name == "notify_parent"));

    let dev = backend.list_tools("dev").await.unwrap();
    assert_eq!(dev.len(), 2);
    assert!(dev.iter().any(|t| t.name == "file_pr"));
    assert!(dev.iter().any(|t| t.name == "notify_parent"));

    let worker = backend.list_tools("worker").await.unwrap();
    assert_eq!(worker.len(), 1);
    assert_eq!(worker[0].name, "notify_parent");

    let unknown = backend.list_tools("unknown").await.unwrap();
    assert!(unknown.is_empty());
}

#[test]
fn test_file_pr_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = FilePRToolInput {
        title: "feat: add tests".to_string(),
        body: "Adds unit tests".to_string(),
        base_branch: "main".to_string(),
    };
    let value = input.to_value(&backend.file_pr_table).unwrap();
    let back = FilePRToolInput::from_value(&value, &backend.file_pr_table).unwrap();
    assert_eq!(input.title, back.title);
    assert_eq!(input.body, back.body);
    assert_eq!(input.base_branch, back.base_branch);
}

#[test]
fn test_merge_pr_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = MergePRToolInput {
        pr_number: "42".to_string(),
        strategy: "squash".to_string(),
        working_dir: ".".to_string(),
    };
    let value = input.to_value(&backend.merge_pr_table).unwrap();
    let back = MergePRToolInput::from_value(&value, &backend.merge_pr_table).unwrap();
    assert_eq!(input.pr_number, back.pr_number);
    assert_eq!(input.strategy, back.strategy);
}

#[test]
fn test_notify_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = NotifyToolInput {
        status: "success".to_string(),
        message: "All done".to_string(),
    };
    let value = input.to_value(&backend.notify_table).unwrap();
    let back = NotifyToolInput::from_value(&value, &backend.notify_table).unwrap();
    assert_eq!(input.status, back.status);
    assert_eq!(input.message, back.message);
}

#[test]
fn test_file_pr_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = FilePRToolInput {
        title: "Test PR".to_string(),
        body: "Test body".to_string(),
        base_branch: "main".to_string(),
    };

    struct MockFilePR {
        tool_input: Option<FilePRToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockFilePR {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req =
                FilePRReq::from_value(request, cx.table()).map_err(TidepoolEffectError::Bridge)?;
            match req {
                FilePRReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input
                        .to_value(cx.table())
                        .map_err(TidepoolEffectError::Bridge)
                }
                FilePRReq::CreateOrUpdatePR(title, _body, base) => {
                    let result = FilePRToolResult {
                        pr_url: "https://github.com/test/test/pull/1".to_string(),
                        pr_number: "1".to_string(),
                        head_branch: "test-branch".to_string(),
                        result_base: base,
                        created: "true".to_string(),
                    };
                    result
                        .to_value(cx.table())
                        .map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockFilePR {
        tool_input: Some(tool_input),
    };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.file_pr_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.file_pr_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = FilePRToolResult::from_value(&result, &backend.file_pr_table)
        .expect("Should decode FilePRToolResult");
    assert_eq!(response.pr_number, "1");
    assert_eq!(response.result_base, "main");
    assert_eq!(response.created, "true");
}

#[test]
fn test_merge_pr_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = MergePRToolInput {
        pr_number: "42".to_string(),
        strategy: "squash".to_string(),
        working_dir: ".".to_string(),
    };

    struct MockMergePR {
        tool_input: Option<MergePRToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockMergePR {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req = MergePRReq::from_value(request, cx.table())
                .map_err(TidepoolEffectError::Bridge)?;
            match req {
                MergePRReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input
                        .to_value(cx.table())
                        .map_err(TidepoolEffectError::Bridge)
                }
                MergePRReq::MergePullRequest(pr_num, _strategy, _dir) => {
                    let result = MergePRToolResult {
                        success: "true".to_string(),
                        message: format!("Merged PR #{}", pr_num),
                        jj_fetched: "true".to_string(),
                    };
                    result
                        .to_value(cx.table())
                        .map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockMergePR {
        tool_input: Some(tool_input),
    };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.merge_pr_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.merge_pr_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = MergePRToolResult::from_value(&result, &backend.merge_pr_table)
        .expect("Should decode MergePRToolResult");
    assert_eq!(response.success, "true");
    assert!(response.message.contains("42"));
    assert_eq!(response.jj_fetched, "true");
}

#[test]
fn test_notify_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = NotifyToolInput {
        status: "success".to_string(),
        message: "All tests pass".to_string(),
    };

    struct MockNotify {
        tool_input: Option<NotifyToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockNotify {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req =
                NotifyReq::from_value(request, cx.table()).map_err(TidepoolEffectError::Bridge)?;
            match req {
                NotifyReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input
                        .to_value(cx.table())
                        .map_err(TidepoolEffectError::Bridge)
                }
                NotifyReq::NotifyParent(_status, _message) => {
                    let result = NotifyToolResult {
                        ack: "delivered".to_string(),
                    };
                    result
                        .to_value(cx.table())
                        .map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockNotify {
        tool_input: Some(tool_input),
    };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.notify_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.notify_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = NotifyToolResult::from_value(&result, &backend.notify_table)
        .expect("Should decode NotifyToolResult");
    assert_eq!(response.ack, "delivered");
}
```

## STEP 8: Update existing test

The existing `test_list_tools_returns_popup` test will fail because `list_tools("tl")` now returns 4 tools instead of 1. Update it:

```rust
#[tokio::test]
async fn test_list_tools_returns_popup() {
    let ctx = EffectContext {
        agent_name: crate::AgentName::from("test"),
        birth_branch: crate::BirthBranch::root(),
    };
    let backend = TidepoolBackend::new(None, ctx);
    let tools = backend.list_tools("tl").await.unwrap();
    assert!(tools.iter().any(|t| t.name == "popup"));
}
```

## VERIFY

```bash
PATH="/tmp/tidepool-extract-new/bin:$PATH" \
PKG_CONFIG_PATH=/nix/store/2ivy0r8ab3bnps5957vfrxcjfcgad661-openssl-3.6.0-dev/lib/pkgconfig \
  cargo test -p exomonad-core --features tidepool --lib tidepool_backend 2>&1
```

All existing tests must still pass. All new tests must pass. Zero warnings.

## DONE CRITERIA

- [ ] `cargo test -p exomonad-core --features tidepool --lib tidepool_backend` passes (all tests green)
- [ ] 3 new bridge type sections (FromCore/ToCore structs + FromCore enum per tool)
- [ ] 3 new `haskell_expr!` calls in `new()` validating Haskell compilation
- [ ] 3 new `call_*` methods calling real services
- [ ] 3 new tool definition functions
- [ ] `list_tools` returns role-appropriate tool sets (tl=4, dev=2, worker=1)
- [ ] `call_tool` dispatches all 4 tools
- [ ] 3 bridge roundtrip tests + 3 full EffectMachine pipeline tests + 3 schema tests + 1 role-based list test
- [ ] Existing popup tests unchanged and passing
- [ ] Zero new files created
