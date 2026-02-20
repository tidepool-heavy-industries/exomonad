use super::*;

#[test]
fn test_popup_tool_definition_schema() {
    let def = popup_tool_definition();
    assert_eq!(def.name, "popup");
    assert_eq!(def.input_schema["type"], "object");
    assert!(def.input_schema["properties"]["elements"].is_object());
    assert!(def.input_schema["properties"]["panes"].is_object());
    assert!(def.input_schema["properties"]["title"].is_object());
    assert!(def.input_schema["properties"]["start"].is_object());
}

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
