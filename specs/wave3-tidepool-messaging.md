# Spec: Wave 3 Tidepool Tools (note, answer_question, get_agent_messages)

Single file change: `rust/exomonad-core/src/tidepool_backend.rs`

**Prerequisite:** Wave 2 must be merged first. This spec assumes Waves 1+2 are present (all spawn tools, JJ service, AgentControlService, etc.).

## ANTI-PATTERNS (READ FIRST)

- **DO NOT** modify any file other than `rust/exomonad-core/src/tidepool_backend.rs`
- **DO NOT** add any new crate dependencies
- **DO NOT** use `todo!()`, `unimplemented!()`, or `unwrap()` in non-test code
- **DO NOT** rename existing types or change existing tests
- **DO NOT** use EffectMachine at runtime for async service calls — only in mock tests
- **DO NOT** change any existing bridge types (Popup*, FilePR*, MergePR*, Notify*, Spawn*)
- **DO NOT** add comments explaining what you removed or changed — comments describe what IS

## READ FIRST

```
rust/exomonad-core/src/tidepool_backend.rs               # Current file (after Waves 1+2)
rust/exomonad-core/haskell/NoteEffect.hs                  # Haskell GADT for note
rust/exomonad-core/haskell/AnswerEffect.hs                # Haskell GADT for answer_question
rust/exomonad-core/haskell/MessagesEffect.hs              # Haskell GADT for get_agent_messages
rust/exomonad-core/src/handlers/messaging.rs              # MessagingHandler (reference for service logic)
rust/exomonad-core/src/services/inbox.rs                  # inbox_path, append_message, read_unread, create_message, poll_unread
rust/exomonad-core/src/services/questions.rs              # QuestionRegistry::new, register, resolve, cancel
rust/exomonad-core/src/services/zellij_events.rs          # inject_input
rust/exomonad-core/src/services/agent_control.rs          # resolve_parent_tab_name
```

## Service signatures (for reference)

```rust
// services/inbox.rs
pub fn inbox_path(project_dir: &Path, agent_name: &str) -> PathBuf;
pub fn create_message(from: String, text: String, summary: Option<String>) -> InboxMessage;
pub fn append_message(path: &Path, message: &InboxMessage) -> Result<()>;
pub fn read_unread(path: &Path) -> Result<Vec<InboxMessage>>;
pub fn poll_unread(path: &Path, timeout: Duration, interval: Duration) -> Result<Vec<InboxMessage>>;

// services/questions.rs
pub fn new() -> Self;
pub fn register(&self) -> (String, oneshot::Receiver<String>);
pub fn resolve(&self, question_id: &str, answer: String) -> bool;
```

## STEP 1: Add FromCore/ToCore bridge types

Add after the existing spawn_workers bridge types section:

```rust
// =============================================================================
// note bridge types
// =============================================================================

/// Mirrors Haskell: `NoteInput { niContent }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NoteInput")]
struct NoteToolInput {
    content: String,
}

/// Mirrors Haskell: `NoteResult { nrAck }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "NoteResult")]
struct NoteToolResult {
    ack: String,
}

/// Mirrors Haskell GADT constructors for `NoteOp`.
#[derive(Debug, FromCore)]
enum NoteReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "SendNote")]
    SendNote(String),
}

// =============================================================================
// answer_question bridge types
// =============================================================================

/// Mirrors Haskell: `AnswerInput { aiAgentId, aiQuestionId, aiAnswer }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "AnswerInput")]
struct AnswerToolInput {
    agent_id: String,
    question_id: String,
    answer: String,
}

/// Mirrors Haskell: `AnswerResult { arStatus, arAgentId, arQuestionId }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "AnswerResult")]
struct AnswerToolResult {
    status: String,
    agent_id: String,
    question_id: String,
}

/// Mirrors Haskell GADT constructors for `AnswerOp`.
#[derive(Debug, FromCore)]
enum AnswerReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "AnswerQuestion")]
    AnswerQuestion(String, String, String),
}

// =============================================================================
// get_agent_messages bridge types
// =============================================================================

/// Mirrors Haskell: `MessagesInput { miAgentId, miTimeoutSecs }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MessagesInput")]
struct MessagesToolInput {
    agent_id: String,
    timeout_secs: String,
}

/// Mirrors Haskell: `MessagesResult { mrMessagesJson, mrWarning }`
#[derive(Debug, FromCore, ToCore)]
#[core(name = "MessagesResult")]
struct MessagesToolResult {
    messages_json: String,
    warning: String,
}

/// Mirrors Haskell GADT constructors for `MessagesOp`.
#[derive(Debug, FromCore)]
enum MessagesReq {
    #[core(name = "GetToolInput")]
    GetToolInput,
    #[core(name = "GetMessages")]
    GetMessages(String, String),
}
```

## STEP 2: Expand TidepoolBackend struct

Add these fields (after `agent_control`):

```rust
    /// Compiled note expression + its DataConTable.
    note_expr: CoreExpr,
    note_table: Arc<DataConTable>,

    /// Compiled answer_question expression + its DataConTable.
    answer_expr: CoreExpr,
    answer_table: Arc<DataConTable>,

    /// Compiled get_agent_messages expression + its DataConTable.
    messages_expr: CoreExpr,
    messages_table: Arc<DataConTable>,

    /// Question registry for answer_question (bridges oneshot channels).
    question_registry: Arc<crate::services::questions::QuestionRegistry>,

    /// Project directory for inbox file paths.
    project_dir: std::path::PathBuf,
```

## STEP 3: Expand TidepoolBackend::new()

Add these `haskell_expr!` calls:

```rust
    let (note_expr, note_table) =
        tidepool_macro::haskell_expr!("haskell/NoteEffect.hs::noteTool");
    let (answer_expr, answer_table) =
        tidepool_macro::haskell_expr!("haskell/AnswerEffect.hs::answerTool");
    let (messages_expr, messages_table) =
        tidepool_macro::haskell_expr!("haskell/MessagesEffect.hs::messagesTool");
```

Create the question registry:

```rust
    let question_registry = Arc::new(crate::services::questions::QuestionRegistry::new());
```

Add the new fields to `Self { ... }`:

```rust
        note_expr,
        note_table: Arc::new(note_table),
        answer_expr,
        answer_table: Arc::new(answer_table),
        messages_expr,
        messages_table: Arc::new(messages_table),
        question_registry,
        project_dir: working_dir.clone(),
```

## STEP 4: Add call methods

Add after the existing `call_spawn_workers`:

```rust
/// Handle note tool: write a note to the TL's inbox and inject into parent pane.
async fn call_note(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let content = args
        .get("content")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let agent_id = self.ctx.agent_name.to_string();

    debug!(agent = %agent_id, "Tidepool note call");

    let tl_inbox = crate::services::inbox::inbox_path(&self.project_dir, "team-lead");
    let msg = crate::services::inbox::create_message(
        agent_id.clone(),
        content.clone(),
        Some(content.chars().take(50).collect()),
    );

    if let Err(e) = crate::services::inbox::append_message(&tl_inbox, &msg) {
        tracing::error!(error = %e, "Tidepool note: inbox write failed");
        return Ok(MCPCallOutput {
            success: false,
            result: None,
            error: Some(e.to_string()),
        });
    }

    // Best-effort pane injection
    let tab_name = crate::services::agent_control::resolve_parent_tab_name(&self.ctx);
    let formatted = format!("[note from {}] {}", agent_id, content);
    crate::services::zellij_events::inject_input(&tab_name, &formatted);

    Ok(MCPCallOutput {
        success: true,
        result: Some(serde_json::json!({"ack": true})),
        error: None,
    })
}

/// Handle answer_question tool: write answer to agent's inbox, resolve oneshot channel.
async fn call_answer_question(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let agent_id = args
        .get("agent_id")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let question_id = args
        .get("question_id")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let answer = args
        .get("answer")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    debug!(
        agent = %agent_id,
        question_id = %question_id,
        "Tidepool answer_question call"
    );

    // Write answer to agent's inbox
    let agent_inbox = crate::services::inbox::inbox_path(&self.project_dir, &agent_id);
    let msg = crate::services::inbox::create_message(
        "team-lead".to_string(),
        answer.clone(),
        Some(format!("Answer to {}", question_id)),
    );

    if let Err(e) = crate::services::inbox::append_message(&agent_inbox, &msg) {
        tracing::error!(error = %e, "Tidepool answer_question: inbox write failed");
        return Ok(MCPCallOutput {
            success: false,
            result: None,
            error: Some(e.to_string()),
        });
    }

    // Resolve the oneshot channel so send_question unblocks immediately
    let resolved = self.question_registry.resolve(&question_id, answer);
    tracing::info!(
        agent = %agent_id,
        question_id = %question_id,
        resolved,
        "Resolved question via QuestionRegistry"
    );

    Ok(MCPCallOutput {
        success: true,
        result: Some(serde_json::json!({
            "status": "answered",
            "agent_id": agent_id,
            "question_id": question_id,
        })),
        error: None,
    })
}

/// Handle get_agent_messages tool: read unread messages from inbox.
async fn call_get_messages(&self, args: JsonValue) -> Result<MCPCallOutput> {
    let agent_id = args
        .get("agent_id")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let timeout_secs: i64 = args
        .get("timeout_secs")
        .and_then(|v| v.as_i64())
        .unwrap_or(0);

    debug!(
        agent = %agent_id,
        timeout_secs,
        "Tidepool get_agent_messages call"
    );

    let tl_inbox = crate::services::inbox::inbox_path(&self.project_dir, "team-lead");

    let all_messages = if timeout_secs > 0 {
        let timeout = std::time::Duration::from_secs(timeout_secs as u64);
        let interval = std::time::Duration::from_secs(2);
        let tl_inbox_clone = tl_inbox.clone();
        match tokio::task::spawn_blocking(move || {
            crate::services::inbox::poll_unread(&tl_inbox_clone, timeout, interval)
        })
        .await
        {
            Ok(Ok(msgs)) => msgs,
            Ok(Err(e)) => {
                return Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                });
            }
            Err(e) => {
                return Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(format!("spawn_blocking failed: {}", e)),
                });
            }
        }
    } else {
        match crate::services::inbox::read_unread(&tl_inbox) {
            Ok(msgs) => msgs,
            Err(e) => {
                return Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                });
            }
        }
    };

    // Filter by agent_id if provided, otherwise return all
    let filtered: Vec<_> = if agent_id.is_empty() {
        all_messages
    } else {
        all_messages
            .into_iter()
            .filter(|m| m.from == agent_id)
            .collect()
    };

    let messages_json: Vec<serde_json::Value> = filtered
        .iter()
        .map(|m| {
            serde_json::json!({
                "from": m.from,
                "text": m.text,
                "timestamp": m.timestamp,
                "read": m.read,
            })
        })
        .collect();

    Ok(MCPCallOutput {
        success: true,
        result: Some(serde_json::json!({
            "messages": messages_json,
            "count": messages_json.len(),
        })),
        error: None,
    })
}
```

## STEP 5: Add tool definitions

After the existing `spawn_workers_tool_definition()`:

```rust
fn note_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "note".to_string(),
        description: "Send a note to the team lead's inbox. Fire-and-forget.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "content": { "type": "string", "description": "Note content" }
            },
            "required": ["content"]
        }),
    }
}

fn answer_question_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "answer_question".to_string(),
        description: "Answer a pending question from an agent. Unblocks the agent immediately.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "agent_id": { "type": "string", "description": "Agent that asked the question" },
                "question_id": { "type": "string", "description": "Question ID from the question message" },
                "answer": { "type": "string", "description": "Answer text" }
            },
            "required": ["agent_id", "question_id", "answer"]
        }),
    }
}

fn get_agent_messages_tool_definition() -> ToolDefinition {
    ToolDefinition {
        name: "get_agent_messages".to_string(),
        description: "Read unread messages from agents. Supports long-polling with timeout.".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "agent_id": { "type": "string", "description": "Filter to messages from this agent (empty = all agents)" },
                "timeout_secs": { "type": "integer", "description": "Long-poll timeout in seconds (0 = immediate return)", "default": 0 }
            },
            "required": []
        }),
    }
}
```

## STEP 6: Update RuntimeBackend impl

Update `list_tools` — add messaging tools to appropriate roles:

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
            note_tool_definition(),
            answer_question_tool_definition(),
            get_agent_messages_tool_definition(),
        ],
        "dev" => vec![
            file_pr_tool_definition(),
            notify_parent_tool_definition(),
            note_tool_definition(),
        ],
        "worker" => vec![
            notify_parent_tool_definition(),
            note_tool_definition(),
        ],
        _ => vec![],
    })
}
```

Update `call_tool` — add 3 new match arms:

```rust
    "note" => self.call_note(args).await,
    "answer_question" => self.call_answer_question(args).await,
    "get_agent_messages" => self.call_get_messages(args).await,
```

## STEP 7: Add tests

Add to the existing `mod tests` block:

```rust
#[test]
fn test_note_tool_definition_schema() {
    let def = note_tool_definition();
    assert_eq!(def.name, "note");
    assert!(def.input_schema["properties"]["content"].is_object());
}

#[test]
fn test_answer_question_tool_definition_schema() {
    let def = answer_question_tool_definition();
    assert_eq!(def.name, "answer_question");
    assert!(def.input_schema["properties"]["agent_id"].is_object());
    assert!(def.input_schema["properties"]["question_id"].is_object());
    assert!(def.input_schema["properties"]["answer"].is_object());
}

#[test]
fn test_get_agent_messages_tool_definition_schema() {
    let def = get_agent_messages_tool_definition();
    assert_eq!(def.name, "get_agent_messages");
    assert!(def.input_schema["properties"]["agent_id"].is_object());
    assert!(def.input_schema["properties"]["timeout_secs"].is_object());
}

#[test]
fn test_note_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = NoteToolInput {
        content: "hello TL".to_string(),
    };
    let value = input.to_value(&backend.note_table).unwrap();
    let back = NoteToolInput::from_value(&value, &backend.note_table).unwrap();
    assert_eq!(input.content, back.content);
}

#[test]
fn test_answer_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = AnswerToolInput {
        agent_id: "worker-1".to_string(),
        question_id: "q-123".to_string(),
        answer: "yes".to_string(),
    };
    let value = input.to_value(&backend.answer_table).unwrap();
    let back = AnswerToolInput::from_value(&value, &backend.answer_table).unwrap();
    assert_eq!(input.agent_id, back.agent_id);
    assert_eq!(input.question_id, back.question_id);
    assert_eq!(input.answer, back.answer);
}

#[test]
fn test_messages_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );
    let input = MessagesToolInput {
        agent_id: "worker-1".to_string(),
        timeout_secs: "30".to_string(),
    };
    let value = input.to_value(&backend.messages_table).unwrap();
    let back = MessagesToolInput::from_value(&value, &backend.messages_table).unwrap();
    assert_eq!(input.agent_id, back.agent_id);
    assert_eq!(input.timeout_secs, back.timeout_secs);
}

#[test]
fn test_note_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = NoteToolInput {
        content: "progress update".to_string(),
    };

    struct MockNote {
        tool_input: Option<NoteToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockNote {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req =
                NoteReq::from_value(request, cx.table()).map_err(TidepoolEffectError::Bridge)?;
            match req {
                NoteReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
                NoteReq::SendNote(_content) => {
                    let result = NoteToolResult {
                        ack: "sent".to_string(),
                    };
                    result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockNote { tool_input: Some(tool_input) };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.note_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.note_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = NoteToolResult::from_value(&result, &backend.note_table)
        .expect("Should decode NoteToolResult");
    assert_eq!(response.ack, "sent");
}

#[test]
fn test_answer_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = AnswerToolInput {
        agent_id: "worker-1".to_string(),
        question_id: "q-42".to_string(),
        answer: "use option B".to_string(),
    };

    struct MockAnswer {
        tool_input: Option<AnswerToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockAnswer {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req = AnswerReq::from_value(request, cx.table())
                .map_err(TidepoolEffectError::Bridge)?;
            match req {
                AnswerReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
                AnswerReq::AnswerQuestion(agent, qid, _answer) => {
                    let result = AnswerToolResult {
                        status: "answered".to_string(),
                        agent_id: agent,
                        question_id: qid,
                    };
                    result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockAnswer { tool_input: Some(tool_input) };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.answer_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.answer_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = AnswerToolResult::from_value(&result, &backend.answer_table)
        .expect("Should decode AnswerToolResult");
    assert_eq!(response.status, "answered");
    assert_eq!(response.agent_id, "worker-1");
    assert_eq!(response.question_id, "q-42");
}

#[test]
fn test_messages_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root(),
        },
    );

    let tool_input = MessagesToolInput {
        agent_id: "worker-1".to_string(),
        timeout_secs: "0".to_string(),
    };

    struct MockMessages {
        tool_input: Option<MessagesToolInput>,
    }
    impl DispatchEffect<EffectContext> for MockMessages {
        fn dispatch(
            &mut self,
            tag: u64,
            request: &Value,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            assert_eq!(tag, 0);
            let req = MessagesReq::from_value(request, cx.table())
                .map_err(TidepoolEffectError::Bridge)?;
            match req {
                MessagesReq::GetToolInput => {
                    let input = self.tool_input.take().ok_or_else(|| {
                        TidepoolEffectError::Handler("tool_input already consumed".into())
                    })?;
                    input.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
                MessagesReq::GetMessages(agent, _timeout) => {
                    let result = MessagesToolResult {
                        messages_json: format!("[{{\"from\":\"{}\",\"text\":\"hello\"}}]", agent),
                        warning: "".to_string(),
                    };
                    result.to_value(cx.table()).map_err(TidepoolEffectError::Bridge)
                }
            }
        }
    }

    let mut dispatcher = MockMessages { tool_input: Some(tool_input) };
    let mut heap = tidepool_core_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_core_effect::EffectMachine::new(&backend.messages_table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&backend.messages_expr, &mut dispatcher, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = MessagesToolResult::from_value(&result, &backend.messages_table)
        .expect("Should decode MessagesToolResult");
    assert!(response.messages_json.contains("worker-1"));
    assert!(response.warning.is_empty());
}
```

## STEP 8: Update list_tools test

Update the `test_list_tools_by_role` test to expect final tool counts:

```rust
let tl = backend.list_tools("tl").await.unwrap();
assert_eq!(tl.len(), 10); // popup, file_pr, merge_pr, notify_parent, spawn_subtree, spawn_leaf_subtree, spawn_workers, note, answer_question, get_agent_messages

let dev = backend.list_tools("dev").await.unwrap();
assert_eq!(dev.len(), 3); // file_pr, notify_parent, note

let worker = backend.list_tools("worker").await.unwrap();
assert_eq!(worker.len(), 2); // notify_parent, note
```

## VERIFY

```bash
PATH="/tmp/tidepool-extract-new/bin:$PATH" \
PKG_CONFIG_PATH=/nix/store/2ivy0r8ab3bnps5957vfrxcjfcgad661-openssl-3.6.0-dev/lib/pkgconfig \
  cargo test -p exomonad-core --features tidepool --lib tidepool_backend 2>&1
```

## DONE CRITERIA

- [ ] `cargo test -p exomonad-core --features tidepool --lib tidepool_backend` passes
- [ ] 3 new bridge type sections (note, answer_question, get_agent_messages)
- [ ] 3 new `haskell_expr!` calls in `new()`
- [ ] 3 new `call_*` methods calling inbox/question services
- [ ] 3 new tool definition functions
- [ ] `list_tools("tl")` returns 10 tools, `list_tools("dev")` returns 3, `list_tools("worker")` returns 2
- [ ] `call_tool` dispatches all 10 tools
- [ ] 3 bridge roundtrip tests + 3 full EffectMachine pipeline tests + 3 schema tests
- [ ] All existing tests still passing
- [ ] Zero new files created
