use super::*;

#[test]
fn test_tool_input_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["popup"].table;
    let input = ToolInput {
        title: "Pick one".to_string(),
        components: r#"[{"type":"choice"}]"#.to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = ToolInput::from_value(&value, table).unwrap();
    assert_eq!(input.title, back.title);
    assert_eq!(input.components, back.components);
}

#[test]
fn test_popup_response_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["popup"].table;
    let resp = PopupResponse {
        button: "submit".to_string(),
        values: r#"{"pick":"A"}"#.to_string(),
    };
    let value = resp.to_value(table).unwrap();
    let back = PopupResponse::from_value(&value, table).unwrap();
    assert_eq!(resp.button, back.button);
    assert_eq!(resp.values, back.values);
}

#[test]
fn test_note_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["note"].table;
    let input = NoteToolInput {
        content: "hello TL".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = NoteToolInput::from_value(&value, table).unwrap();
    assert_eq!(input.content, back.content);
}

#[test]
fn test_answer_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["answer_question"].table;
    let input = AnswerToolInput {
        agent_id: "worker-1".to_string(),
        question_id: "q-123".to_string(),
        answer: "yes".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = AnswerToolInput::from_value(&value, table).unwrap();
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
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["get_agent_messages"].table;
    let input = MessagesToolInput {
        agent_id: "worker-1".to_string(),
        timeout_secs: "30".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = MessagesToolInput::from_value(&value, table).unwrap();
    assert_eq!(input.agent_id, back.agent_id);
    assert_eq!(input.timeout_secs, back.timeout_secs);
}

#[test]
fn test_spawn_subtree_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["spawn_subtree"].table;
    let input = SpawnSubtreeToolInput {
        task: "implement feature".to_string(),
        branch_name: "feature-a".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = SpawnSubtreeToolInput::from_value(&value, table).unwrap();
    assert_eq!(input.task, back.task);
    assert_eq!(input.branch_name, back.branch_name);
}

#[test]
fn test_spawn_leaf_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["spawn_leaf_subtree"].table;
    let input = SpawnLeafToolInput {
        task: "implement leaf".to_string(),
        branch_name: "leaf-1".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = SpawnLeafToolInput::from_value(&value, table).unwrap();
    assert_eq!(input.task, back.task);
    assert_eq!(input.branch_name, back.branch_name);
}

#[test]
fn test_worker_spec_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["spawn_workers"].table;
    let input = WorkerSpecBridge {
        name: "worker-1".to_string(),
        prompt: "do something".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = WorkerSpecBridge::from_value(&value, table).unwrap();
    assert_eq!(input.name, back.name);
    assert_eq!(input.prompt, back.prompt);
}

#[test]
fn test_file_pr_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["file_pr"].table;
    let input = FilePRToolInput {
        title: "feat: add tests".to_string(),
        body: "Adds unit tests".to_string(),
        base_branch: "main".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = FilePRToolInput::from_value(&value, table).unwrap();
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
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["merge_pr"].table;
    let input = MergePRToolInput {
        pr_number: "42".to_string(),
        strategy: "squash".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = MergePRToolInput::from_value(&value, table).unwrap();
    assert_eq!(input.pr_number, back.pr_number);
    assert_eq!(input.strategy, back.strategy);
}

#[test]
fn test_notify_bridge_roundtrip() {
    let backend = TidepoolBackend::new(
        None,
        EffectContext {
            agent_name: crate::AgentName::from("test"),
            birth_branch: crate::BirthBranch::root()
        },
    );
    let table = &backend.tools["notify_parent"].table;
    let input = NotifyToolInput {
        status: "success".to_string(),
        message: "All done".to_string(),
    };
    let value = input.to_value(table).unwrap();
    let back = NotifyToolInput::from_value(&value, table).unwrap();
    assert_eq!(input.status, back.status);
    assert_eq!(input.message, back.message);
}
