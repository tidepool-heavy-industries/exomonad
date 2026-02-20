# Spec: Wave 2 Tidepool Tools (spawn_subtree, spawn_leaf_subtree, spawn_workers)

Single file change: `rust/exomonad-core/src/tidepool_backend.rs`

**Prerequisite:** Wave 1 must be merged first. This spec assumes the Wave 1 changes are present (expanded TidepoolBackend struct, `jj` field, role-based `list_tools`, etc.).

## ANTI-PATTERNS (READ FIRST)

- **DO NOT** modify any file other than `rust/exomonad-core/src/tidepool_backend.rs`
- **DO NOT** add any new crate dependencies
- **DO NOT** use `todo!()`, `unimplemented!()`, or `unwrap()` in non-test code
- **DO NOT** rename existing types or change existing tests
- **DO NOT** use EffectMachine at runtime for async service calls — only in mock tests
- **DO NOT** change any existing bridge types (Popup*, FilePR*, MergePR*, Notify*)
- **DO NOT** add comments explaining what you removed or changed — comments describe what IS
- **DO NOT** overengineer the `call_spawn_workers` method — iterate the JSON array directly in Rust, call `spawn_worker` per item

## READ FIRST

```
rust/exomonad-core/src/tidepool_backend.rs               # Current file (after Wave 1)
rust/exomonad-core/haskell/SpawnEffect.hs                 # Haskell GADT for spawn_subtree
rust/exomonad-core/haskell/SpawnLeafEffect.hs             # Haskell GADT for spawn_leaf_subtree
rust/exomonad-core/haskell/SpawnWorkersEffect.hs          # Haskell GADT for spawn_workers
rust/exomonad-core/src/services/agent_control.rs          # AgentControlService, SpawnSubtreeOptions, SpawnWorkerOptions, SpawnResult
```

## STEP 1: Add FromCore/ToCore bridge types

Add after the existing notify_parent bridge types section:

```rust
// =============================================================================
// spawn_subtree bridge types
// =============================================================================

/// Mirrors Haskell: `SpawnSubtreeInput { ssiTask, ssiBranchName, ssiParentSessionId }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeInput")]
struct SpawnSubtreeToolInput {
    task: String,
    branch_name: String,
    parent_session_id: String,
}

/// Mirrors Haskell: `SpawnSubtreeResult { ssrTabName, ssrBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnSubtreeResult")]
struct SpawnSubtreeToolResult {
    tab_name: String,
    branch_name: String,
}

/// Mirrors Haskell GADT constructors for `SpawnSubtreeOp`.
#[derive(Debug, FromCore)]
enum SpawnSubtreeReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "SpawnSubtree")]
    SpawnSubtree(String, String, String),
}

// =============================================================================
// spawn_leaf_subtree bridge types
// =============================================================================

/// Mirrors Haskell: `SpawnLeafInput { sliTask, sliBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnLeafInput")]
struct SpawnLeafToolInput {
    task: String,
    branch_name: String,
}

/// Mirrors Haskell: `SpawnLeafResult { slrTabName, slrBranchName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnLeafResult")]
struct SpawnLeafToolResult {
    tab_name: String,
    branch_name: String,
}

/// Mirrors Haskell GADT constructors for `SpawnLeafOp`.
#[derive(Debug, FromCore)]
enum SpawnLeafReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "SpawnLeaf")]
    SpawnLeaf(String, String),
}

// =============================================================================
// spawn_workers bridge types
// =============================================================================

/// Mirrors Haskell: `WorkerSpec { wsName, wsPrompt }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "WorkerSpec")]
struct WorkerSpecBridge {
    name: String,
    prompt: String,
}

/// Mirrors Haskell: `SpawnWorkerResult { swrTabName }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "SpawnWorkerResult")]
struct SpawnWorkerToolResult {
    tab_name: String,
}

/// Mirrors Haskell GADT constructors for `SpawnWorkersOp`.
#[derive(Debug, FromCore)]
enum SpawnWorkersReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "SpawnWorker")]
    SpawnWorker(String, String),
}
```

## STEP 2: Expand TidepoolBackend struct

Add these fields to the existing struct (after `notify_table`):

```rust
    /// Compiled spawn_subtree expression + its DataConTable.
    spawn_subtree_expr: CoreExpr,
    spawn_subtree_table: Arc<DataConTable>,

    /// Compiled spawn_leaf expression + its DataConTable.
    spawn_leaf_expr: CoreExpr,
    spawn_leaf_table: Arc<DataConTable>,

    /// Compiled spawn_workers expression + its DataConTable.
    spawn_workers_expr: CoreExpr,
    spawn_workers_table: Arc<DataConTable>,

    /// Agent control service for spawn operations.
    agent_control: Arc<crate::services::agent_control::AgentControlService>,
```

## STEP 3: Expand TidepoolBackend::new()

Add these `haskell_expr!` calls after the existing ones:

```rust
    let (spawn_subtree_expr, spawn_subtree_table) =
        tidepool_macro::haskell_expr!("haskell/SpawnEffect.hs::spawnSubtreeTool");
    let (spawn_leaf_expr, spawn_leaf_table) =
        tidepool_macro::haskell_expr!("haskell/SpawnLeafEffect.hs::spawnLeafTool");
    let (spawn_workers_expr, spawn_workers_table) =
        tidepool_macro::haskell_expr!("haskell/SpawnWorkersEffect.hs::spawnWorkersTool");
```

Create the `AgentControlService` and add it:

```rust
    let agent_control = Arc::new(
        crate::services::agent_control::AgentControlService::new(
            working_dir.clone(),
            None, // No GitHubService needed for spawn operations
            jj.clone(),
        )
        .with_birth_branch(ctx.birth_branch.clone())
        .with_zellij_session(zellij_session.clone().unwrap_or_default()),
    );
```

Add the new fields to the `Self { ... }` construction:

```rust
        spawn_subtree_expr,
        spawn_subtree_table: Arc::new(spawn_subtree_table),
        spawn_leaf_expr,
        spawn_leaf_table: Arc::new(spawn_leaf_table),
        spawn_workers_expr,
        spawn_workers_table: Arc::new(spawn_workers_table),
        agent_control,
```

## STEP 4: Add call methods

Add after the existing `call_notify_parent`:

```rust
/// Handle spawn_subtree tool: parse MCP args, call AgentControlService.
async fn call_spawn_subtree(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let task = args.get("task").and_then(|v| v.as_str()).unwrap_or("").to_string();
    let branch_name = args
        .get("branch_name")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    debug!(branch = %branch_name, "Tidepool spawn_subtree call");

    let options = crate::services::agent_control::SpawnSubtreeOptions {
        task,
        branch_name: branch_name.clone(),
        parent_session_id: None,
    };

    match self
        .agent_control
        .spawn_subtree(&options, &self.ctx.birth_branch)
        .await
    {
        Ok(result) => Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({
                "tab_name": result.tab_name,
                "branch_name": branch_name,
                "agent_dir": result.agent_dir.to_string_lossy(),
            })),
            error: None,
        }),
        Err(e) => {
            tracing::error!(error = %e, "Tidepool spawn_subtree failed");
            Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(e.to_string()),
            })
        }
    }
}

/// Handle spawn_leaf_subtree tool: parse MCP args, call AgentControlService.
async fn call_spawn_leaf(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let task = args.get("task").and_then(|v| v.as_str()).unwrap_or("").to_string();
    let branch_name = args
        .get("branch_name")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    debug!(branch = %branch_name, "Tidepool spawn_leaf_subtree call");

    let options = crate::services::agent_control::SpawnSubtreeOptions {
        task,
        branch_name: branch_name.clone(),
        parent_session_id: None,
    };

    match self
        .agent_control
        .spawn_leaf_subtree(&options, &self.ctx.birth_branch)
        .await
    {
        Ok(result) => Ok(MCPCallOutput {
            success: true,
            result: Some(serde_json::json!({
                "tab_name": result.tab_name,
                "branch_name": branch_name,
                "agent_dir": result.agent_dir.to_string_lossy(),
            })),
            error: None,
        }),
        Err(e) => {
            tracing::error!(error = %e, "Tidepool spawn_leaf_subtree failed");
            Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(e.to_string()),
            })
        }
    }
}

/// Handle spawn_workers tool: iterate specs array, call spawn_worker per item.
async fn call_spawn_workers(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let specs = args
        .get("specs")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    debug!(count = specs.len(), "Tidepool spawn_workers call");

    let mut results = Vec::new();
    for spec in &specs {
        let name = spec.get("name").and_then(|v| v.as_str()).unwrap_or("").to_string();
        let task = spec.get("task").and_then(|v| v.as_str()).unwrap_or("").to_string();

        // Build prompt from spec fields (same as WASM handler)
        let prompt = if let Some(p) = spec.get("prompt").and_then(|v| v.as_str()) {
            p.to_string()
        } else {
            let mut parts = vec![format!("Task: {}", task)];
            if let Some(steps) = spec.get("steps").and_then(|v| v.as_array()) {
                parts.push("Steps:".to_string());
                for (i, s) in steps.iter().enumerate() {
                    if let Some(text) = s.as_str() {
                        parts.push(format!("{}. {}", i + 1, text));
                    }
                }
            }
            if let Some(context) = spec.get("context").and_then(|v| v.as_str()) {
                parts.push(format!("\nContext:\n{}", context));
            }
            if let Some(done) = spec.get("done_criteria").and_then(|v| v.as_array()) {
                parts.push("Done when:".to_string());
                for d in done {
                    if let Some(text) = d.as_str() {
                        parts.push(format!("- {}", text));
                    }
                }
            }
            if let Some(verify) = spec.get("verify").and_then(|v| v.as_array()) {
                parts.push("Verify:".to_string());
                for v in verify {
                    if let Some(text) = v.as_str() {
                        parts.push(format!("$ {}", text));
                    }
                }
            }
            if let Some(boundary) = spec.get("boundary").and_then(|v| v.as_array()) {
                parts.push("DO NOT:".to_string());
                for b in boundary {
                    if let Some(text) = b.as_str() {
                        parts.push(format!("- {}", text));
                    }
                }
            }
            if let Some(read) = spec.get("read_first").and_then(|v| v.as_array()) {
                parts.push("Read first:".to_string());
                for r in read {
                    if let Some(text) = r.as_str() {
                        parts.push(format!("- {}", text));
                    }
                }
            }
            parts.join("\n")
        };

        let options = crate::services::agent_control::SpawnWorkerOptions {
            name: name.clone(),
            prompt,
        };

        match self
            .agent_control
            .spawn_worker(&options, &self.ctx.birth_branch)
            .await
        {
            Ok(result) => {
                results.push(serde_json::json!({
                    "name": name,
                    "tab_name": result.tab_name,
                    "success": true,
                }));
            }
            Err(e) => {
                tracing::error!(name = %name, error = %e, "Tidepool spawn_worker failed");
                results.push(serde_json::json!({
                    "name": name,
                    "success": false,
                    "error": e.to_string(),
                }));
            }
        }
    }

    Ok(MCPCallOutput {
        success: true,
        result: Some(serde_json::json!({ "workers": results })),
        error: None,
    })
}
```

## STEP 5: Add tool definitions

After the existing `notify_parent_tool_definition()`:

```rust
fn spawn_subtree_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "spawn_subtree".to_string(),
        description: "Fork a worktree node off your current branch. The child gets full coordination tools (can spawn its own children).".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "task": { "type": "string", "description": "Description of the sub-problem to solve" },
                "branch_name": { "type": "string", "description": "Branch name suffix (will be prefixed with current branch)" }
            },
            "required": ["task", "branch_name"]
        }),
    }
}

fn spawn_leaf_subtree_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "spawn_leaf_subtree".to_string(),
        description: "Fork a worktree for a Gemini leaf agent. Gets own branch for PR filing but cannot spawn children.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "task": { "type": "string", "description": "Description of the sub-problem to solve" },
                "branch_name": { "type": "string", "description": "Branch name suffix (will be prefixed with current branch)" }
            },
            "required": ["task", "branch_name"]
        }),
    }
}

fn spawn_workers_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "spawn_workers".to_string(),
        description: "Spawn multiple worker agents in one call. Each gets a Zellij pane in the current worktree.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "specs": {
                    "type": "array",
                    "description": "Array of worker specifications",
                    "items": {
                        "type": "object",
                        "required": ["name", "task"],
                        "properties": {
                            "name": { "type": "string", "description": "Human-readable name for the leaf agent" },
                            "task": { "type": "string", "description": "Short description of the task" },
                            "prompt": { "type": "string", "description": "Raw prompt (escape hatch). If provided, all other fields except name are ignored." },
                            "steps": { "type": "array", "items": { "type": "string" }, "description": "Numbered implementation steps" },
                            "context": { "type": "string", "description": "Freeform context: code snippets, examples, detailed specs" },
                            "done_criteria": { "type": "array", "items": { "type": "string" }, "description": "Acceptance criteria for completion" },
                            "verify": { "type": "array", "items": { "type": "string" }, "description": "Commands to verify the work" },
                            "boundary": { "type": "array", "items": { "type": "string" }, "description": "Things the agent must NOT do" },
                            "read_first": { "type": "array", "items": { "type": "string" }, "description": "Files the agent should read before starting" }
                        }
                    }
                }
            },
            "required": ["specs"]
        }),
    }
}
```

## STEP 6: Update RuntimeBackend impl

Update `list_tools` — add spawn tools to TL role only:

```rust
async fn list_tools(&self, role: &str) -> Result<Vec<ToolDefinition>> {
    debug!(role = %role, "Listing tools from tidepool backend");
    Ok(match role {
        "tl" => vec![
            popup_tool_definition(),
            file_pr_tool_definition(),
            merge_pr_tool_definition(),
            notify_parent_tool_definition(),
            spawn_subtree_tool_definition(),
            spawn_leaf_subtree_tool_definition(),
            spawn_workers_tool_definition(),
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

Update `call_tool` — add 3 new match arms:

```rust
    "spawn_subtree" => self.call_spawn_subtree(args).await,
    "spawn_leaf_subtree" => self.call_spawn_leaf(args).await,
    "spawn_workers" => self.call_spawn_workers(args).await,
```

## STEP 7: Add tests

Add to the existing `mod tests` block:

```rust
#[test]
fn test_spawn_subtree_tool_definition_schema() {
    let def = spawn_subtree_tool_definition();
    assert_eq!(def.name, "spawn_subtree");
    assert!(def.input_schema["properties"]["task"].is_object());
    assert!(def.input_schema["properties"]["branch_name"].is_object());
}

#[test]
fn test_spawn_leaf_tool_definition_schema() {
    let def = spawn_leaf_subtree_tool_definition();
    assert_eq!(def.name, "spawn_leaf_subtree");
    assert!(def.input_schema["properties"]["task"].is_object());
}

#[test]
fn test_spawn_workers_tool_definition_schema() {
    let def = spawn_workers_tool_definition();
    assert_eq!(def.name, "spawn_workers");
    assert!(def.input_schema["properties"]["specs"].is_object());
}

#[test]
fn test_spawn_subtree_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = SpawnSubtreeToolInput {
        task: "implement feature".to_string(),
        branch_name: "feature-a".to_string(),
        parent_session_id: "".to_string(),
    };
    let value = input.to_value(&backend.spawn_subtree_table).unwrap();
    let back = SpawnSubtreeToolInput::from_value(&value, &backend.spawn_subtree_table).unwrap();
    assert_eq!(input.task, back.task);
    assert_eq!(input.branch_name, back.branch_name);
}

#[test]
fn test_spawn_leaf_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = SpawnLeafToolInput {
        task: "implement leaf".to_string(),
        branch_name: "leaf-1".to_string(),
    };
    let value = input.to_value(&backend.spawn_leaf_table).unwrap();
    let back = SpawnLeafToolInput::from_value(&value, &backend.spawn_leaf_table).unwrap();
    assert_eq!(input.task, back.task);
    assert_eq!(input.branch_name, back.branch_name);
}

#[test]
fn test_spawn_subtree_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = SpawnSubtreeToolInput {
        task: "build feature".to_string(),
        branch_name: "feat".to_string(),
        parent_session_id: "".to_string(),
    };

    struct MockSpawn {
        tool_input: Option<SpawnSubtreeToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockSpawn {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req = SpawnSubtreeReq::from_value(request, cx.table())
                .map_err(TidepoolEffectError::Bridge)?;
            match req {
                SpawnSubtreeReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
                SpawnSubtreeReq::SpawnSubtree(_task, branch, _session) => {
                    let result = SpawnSubtreeToolResult {
                        tab_name: format!("🧠 {}", branch),
                        branch_name: format!("main.{}", branch),
                    };
                    result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockSpawn { tool_input: Some(tool_input) };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.spawn_subtree_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.spawn_subtree_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = SpawnSubtreeToolResult::from_value(&result, &backend.spawn_subtree_table)
        .expect("Should decode SpawnSubtreeToolResult");
    assert!(response.tab_name.contains("feat"));
    assert_eq!(response.branch_name, "main.feat");
}

#[test]
fn test_spawn_leaf_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = SpawnLeafToolInput {
        task: "build leaf".to_string(),
        branch_name: "leaf-1".to_string(),
    };

    struct MockLeaf {
        tool_input: Option<SpawnLeafToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockLeaf {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req = SpawnLeafReq::from_value(request, cx.table())
                .map_err(TidepoolEffectError::Bridge)?;
            match req {
                SpawnLeafReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
                SpawnLeafReq::SpawnLeaf(_task, branch) => {
                    let result = SpawnLeafToolResult {
                        tab_name: format!("♊ {}", branch),
                        branch_name: format!("main.{}", branch),
                    };
                    result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockLeaf { tool_input: Some(tool_input) };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.spawn_leaf_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.spawn_leaf_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = SpawnLeafToolResult::from_value(&result, &backend.spawn_leaf_table)
        .expect("Should decode SpawnLeafToolResult");
    assert!(response.tab_name.contains("leaf-1"));
}
```

**Note on spawn_workers EffectMachine test:** `spawnWorkersTool` uses `mapM` to iterate a list, yielding `SpawnWorker` per item. This requires the EffectMachine to support Haskell list construction via `FromCore`/`ToCore`. If list support is not yet implemented in the tidepool bridge, skip the spawn_workers pipeline test and add only the schema + roundtrip tests. The runtime `call_spawn_workers` bypasses EffectMachine anyway.

## STEP 8: Update list_tools test

Update the `test_list_tools_by_role` test (added in Wave 1) to expect 7 TL tools:

```rust
let tl = backend.list_tools("tl").await.unwrap();
assert_eq!(tl.len(), 7); // popup, file_pr, merge_pr, notify_parent, spawn_subtree, spawn_leaf_subtree, spawn_workers
```

## VERIFY

```bash
PATH="/tmp/tidepool-extract-new/bin:$PATH" \
PKG_CONFIG_PATH=/nix/store/2ivy0r8ab3bnps5957vfrxcjfcgad661-openssl-3.6.0-dev/lib/pkgconfig \
  cargo test -p exomonad-core --features tidepool --lib tidepool_backend 2>&1
```

## DONE CRITERIA

- [ ] `cargo test -p exomonad-core --features tidepool --lib tidepool_backend` passes
- [ ] 3 new bridge type sections (spawn_subtree, spawn_leaf, spawn_workers)
- [ ] 3 new `haskell_expr!` calls in `new()`
- [ ] 3 new `call_*` methods calling AgentControlService
- [ ] 3 new tool definition functions
- [ ] `list_tools("tl")` returns 7 tools
- [ ] `call_tool` dispatches all 7 tools
- [ ] Bridge roundtrip + pipeline tests for spawn_subtree and spawn_leaf
- [ ] Schema tests for all 3 spawn tools
- [ ] All existing tests still passing
- [ ] Zero new files created
