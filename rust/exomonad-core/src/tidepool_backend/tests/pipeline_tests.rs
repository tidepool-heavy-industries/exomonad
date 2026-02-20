use super::*;

#[tokio::test]
async fn test_list_tools_returns_popup() {
    let ctx = EffectContext {
        agent_name: crate::AgentName::from("test"),
        birth_branch: crate::BirthBranch::root()
    };
    let backend = TidepoolBackend::new(None, ctx);
    let tools = backend.list_tools("tl").await.unwrap();
    assert!(tools.iter().any(|t| t.name == "popup"));
}

#[tokio::test]
async fn test_list_tools_by_role() {
    let ctx = EffectContext {
        agent_name: crate::AgentName::from("test"),
        birth_branch: crate::BirthBranch::root()
    };
    let backend = TidepoolBackend::new(None, ctx);

    let tl = backend.list_tools("tl").await.unwrap();
    assert_eq!(tl.len(), 10);
    assert!(tl.iter().any(|t| t.name == "popup"));
    assert!(tl.iter().any(|t| t.name == "file_pr"));
    assert!(tl.iter().any(|t| t.name == "merge_pr"));
    assert!(tl.iter().any(|t| t.name == "notify_parent"));
    assert!(tl.iter().any(|t| t.name == "spawn_subtree"));
    assert!(tl.iter().any(|t| t.name == "spawn_leaf_subtree"));
    assert!(tl.iter().any(|t| t.name == "spawn_workers"));
    assert!(tl.iter().any(|t| t.name == "note"));
    assert!(tl.iter().any(|t| t.name == "answer_question"));
    assert!(tl.iter().any(|t| t.name == "get_agent_messages"));

    let dev = backend.list_tools("dev").await.unwrap();
    assert_eq!(dev.len(), 3);
    assert!(dev.iter().any(|t| t.name == "file_pr"));
    assert!(dev.iter().any(|t| t.name == "notify_parent"));
    assert!(dev.iter().any(|t| t.name == "note"));

    let worker = backend.list_tools("worker").await.unwrap();
    assert_eq!(worker.len(), 2);
    assert!(worker.iter().any(|t| t.name == "notify_parent"));
    assert!(worker.iter().any(|t| t.name == "note"));

    let unknown = backend.list_tools("unknown").await.unwrap();
    assert!(unknown.is_empty());
}

#[test]
fn test_note_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = NoteToolInput {
        content: "progress update".to_string(),
    };

    // Mock NoteOp handler (tag 4): accepts InjectNote, returns NoteResult
    struct MockNoteOp;
    impl EffectHandler<EffectContext> for MockNoteOp {
        type Request = NoteOpReq;
        fn handle(
            &mut self,
            req: NoteOpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                NoteOpReq::InjectNote(_, _) => {
                    cx.respond(NoteToolResult {
                        ack: "sent".to_string(),
                    })
                }
            }
        }
    }

    // Eff '[NoteInput', Identity, FormatOp, Inbox, NoteOp]
    let tool = &backend.tools["note"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockIdentity,
        FormatOpHandler,
        MockInbox,
        MockNoteOp,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = NoteToolResult::from_value(&result, &tool.table)
        .expect("Should decode NoteToolResult");
    assert_eq!(response.ack, "sent");
}

#[test]
fn test_answer_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = AnswerToolInput {
        agent_id: "worker-1".to_string(),
        question_id: "q-42".to_string(),
        answer: "use option B".to_string(),
    };

    // Eff '[AnswerInput', Inbox, Questions]
    let tool = &backend.tools["answer_question"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockInbox,
        MockQuestions,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = AnswerToolResult::from_value(&result, &tool.table)
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
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = MessagesToolInput {
        agent_id: "worker-1".to_string(),
        timeout_secs: "0".to_string(),
    };

    // Mock MessagesOp handler (tag 1)
    struct MockMessagesOp;
    impl EffectHandler<EffectContext> for MockMessagesOp {
        type Request = MessagesOpReq;
        fn handle(
            &mut self,
            req: MessagesOpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                                                        MessagesOpReq::FetchMessages(agent, _timeout) => {
                                                            cx.respond(MessagesToolResult {
                                                                messages_json: format!(
                                                                    r#"[{{ "from":"{}", "text":"hello" }}]"#,
                                                                    agent
                                                                ),
                                                                warning: "".to_string(),
                                                            })
                                                        }
                                                }
        }
    }

    // Eff '[MessagesInput', MessagesOp]
    let tool = &backend.tools["get_agent_messages"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockMessagesOp,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = MessagesToolResult::from_value(&result, &tool.table)
        .expect("Should decode MessagesToolResult");
    assert!(response.messages_json.contains("worker-1"));
    assert!(response.warning.is_empty());
}

#[test]
fn test_spawn_subtree_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = SpawnSubtreeToolInput {
        task: "build feature".to_string(),
        branch_name: "feat".to_string(),
    };

    // Mock SpawnSubtreeOp handler (tag 1)
    struct MockSpawnOp;
    impl EffectHandler<EffectContext> for MockSpawnOp {
        type Request = SpawnSubtreeOpReq;
        fn handle(
            &mut self,
            req: SpawnSubtreeOpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                SpawnSubtreeOpReq::SpawnSubtree(_task, branch) => {
                    let result = SpawnSubtreeToolResult {
                        tab_name: format!("🧠 {}", branch),
                        branch_name: format!("main.{}", branch),
                    };
                    cx.respond(result)
                }
            }
        }
    }

    // Eff '[SpawnSubtreeInput', SpawnSubtreeOp]
    let tool = &backend.tools["spawn_subtree"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockSpawnOp,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = SpawnSubtreeToolResult::from_value(&result, &tool.table)
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
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = SpawnLeafToolInput {
        task: "build leaf".to_string(),
        branch_name: "leaf-1".to_string(),
    };

    // Mock SpawnLeafOp handler (tag 1)
    struct MockLeafOp;
    impl EffectHandler<EffectContext> for MockLeafOp {
        type Request = SpawnLeafOpReq;
        fn handle(
            &mut self,
            req: SpawnLeafOpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                SpawnLeafOpReq::SpawnLeaf(_task, branch) => {
                    let result = SpawnLeafToolResult {
                        tab_name: format!("♊ {}", branch),
                        branch_name: format!("main.{}", branch),
                    };
                    cx.respond(result)
                }
            }
        }
    }

    // Eff '[SpawnLeafInput', SpawnLeafOp]
    let tool = &backend.tools["spawn_leaf_subtree"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockLeafOp,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = SpawnLeafToolResult::from_value(&result, &tool.table)
        .expect("Should decode SpawnLeafToolResult");
    assert!(response.tab_name.contains("leaf-1"));
}

#[test]
fn test_file_pr_effect_pipeline() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = FilePRToolInput {
        title: "Test PR".to_string(),
        body: "Test body".to_string(),
        base_branch: "main".to_string(),
    };

    // Mock FilePROp handler (tag 2)
    struct MockFilePROp;
    impl EffectHandler<EffectContext> for MockFilePROp {
        type Request = FilePROpReq;
        fn handle(
            &mut self,
            req: FilePROpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                FilePROpReq::CreateOrUpdatePR(_title, _body, base, _dir) => {
                    let result = FilePRToolResult {
                        pr_url: "https://github.com/test/test/pull/1".to_string(),
                        pr_number: "1".to_string(),
                        head_branch: "test-branch".to_string(),
                        result_base: base,
                        created: "true".to_string(),
                    };
                    cx.respond(result)
                }
            }
        }
    }

    // Eff '[FilePRInput', Identity, FilePROp]
    let tool = &backend.tools["file_pr"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockIdentity,
        MockFilePROp,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = FilePRToolResult::from_value(&result, &tool.table)
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
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = MergePRToolInput {
        pr_number: "42".to_string(),
        strategy: "squash".to_string(),
    };

    // Mock MergePROp handler (tag 2)
    struct MockMergePROp;
    impl EffectHandler<EffectContext> for MockMergePROp {
        type Request = MergePROpReq;
        fn handle(
            &mut self,
            req: MergePROpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                MergePROpReq::MergePullRequest(pr_num, _strategy, _dir) => {
                    let result = MergePRToolResult {
                        success: "true".to_string(),
                        message: format!("Merged PR #{}", pr_num),
                        jj_fetched: "true".to_string(),
                    };
                    cx.respond(result)
                }
            }
        }
    }

    // Eff '[MergePRInput', Identity, MergePROp]
    let tool = &backend.tools["merge_pr"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockIdentity,
        MockMergePROp,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = MergePRToolResult::from_value(&result, &tool.table)
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
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = NotifyToolInput {
        status: "success".to_string(),
        message: "All tests pass".to_string(),
    };

    // Eff '[NotifyInput', Identity, FormatOp, NotifyOp]
    // Mock NotifyOp handler (tag 3)
    struct MockNotifyOp;
    impl EffectHandler<EffectContext> for MockNotifyOp {
        type Request = NotifyOpReq;
        fn handle(
            &mut self,
            req: NotifyOpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                NotifyOpReq::DeliverNotification(_, _) => {
                    cx.respond(NotifyToolResult {
                        ack: "delivered".to_string(),
                    })
                }
            }
        }
    }

    let tool = &backend.tools["notify_parent"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockIdentity,
        FormatOpHandler,
        MockNotifyOp,
    ];
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete");

    let response = NotifyToolResult::from_value(&result, &tool.table)
        .expect("Should decode NotifyToolResult");
    assert_eq!(response.ack, "delivered");
}

#[tokio::test]
async fn test_call_unknown_tool() {
    let ctx = EffectContext {
        agent_name: crate::AgentName::from("test"),
        birth_branch: crate::BirthBranch::root()
    };
    let backend = TidepoolBackend::new(None, ctx);
    let result = backend
        .call_tool("tl", "nonexistent", serde_json::json!({}))
        .await
        .unwrap();
    assert!(!result.success);
    assert!(result.error.unwrap().contains("Unknown tool"));
}

#[test]
fn test_minimal_single_effect() {
    // Minimal test: Haskell `send GetToolInput` (returns Int).
    let (expr, table) =
        tidepool_macro::haskell_expr!("haskell/PopupMinimal.hs::minimal");

    struct Echo42;
    impl EffectHandler<EffectContext> for Echo42 {
        type Request = GetToolInputReq;
        fn handle(
            &mut self,
            _req: GetToolInputReq,
            _cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            Ok(Value::Lit(tidepool_repr::Literal::LitInt(42)))
        }
    }

    let ctx = EffectContext {
        agent_name: crate::AgentName::from("test"),
        birth_branch: crate::BirthBranch::root()
    };
    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&table, &mut heap).unwrap();
    let mut handlers = frunk::hlist![Echo42];

    let result = machine.run_with_user(&expr, &mut handlers, &ctx).unwrap();
    match result {
        Value::Lit(tidepool_repr::Literal::LitInt(n)) => assert_eq!(n, 42),
        other => panic!("Expected Lit(42), got {:?}", other),
    }
}

#[test]
fn test_effect_machine_full_pipeline() {
    // Full pipeline: popupTool yields GetToolInput (tag 0), GetOwnTab (tag 1),
    // then ShowPopup (tag 2). Eff '[PopupInput', Identity, PopupOp]
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );

    let tool_input = ToolInput {
        title: "Test Title".to_string(),
        components: "[]".to_string(),
    };

    // Mock PopupOp handler (tag 2)
    struct MockPopupOp;
    impl EffectHandler<EffectContext> for MockPopupOp {
        type Request = PopupOpReq;
        fn handle(
            &mut self,
            req: PopupOpReq,
            cx: &TidepoolEffectContext<'_, EffectContext>,
        ) -> Result<Value, TidepoolEffectError> {
            match req {
                                                        PopupOpReq::ShowPopup(title, _components, _tab) => {
                                                            let response = PopupResponse {
                                                                button: "submit".to_string(),
                                                                values: format!(r#"[{{ "from":"{}" }}]"#, title),
                                                            };
                                                            cx.respond(response)
                                                        }
                                                }
        }
    }

    let tool = &backend.tools["popup"];
    let mut handlers = frunk::hlist![
        ToolInputHandler { input: Some(tool_input) },
        MockIdentity,
        MockPopupOp,
    ];

    let mut heap = tidepool_eval::heap::VecHeap::new();
    let mut machine =
        tidepool_effect::EffectMachine::new(&tool.table, &mut heap).unwrap();

    let result = machine
        .run_with_user(&tool.expr, &mut handlers, &backend.ctx)
        .expect("EffectMachine should complete successfully");

    let response = PopupResponse::from_value(&result, &tool.table)
        .expect("Should decode PopupResponse from result");
    assert_eq!(response.button, "submit");
    assert!(response.values.contains("Test Title"));
}
