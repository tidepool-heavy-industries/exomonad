use exomonad_shared::protocol::{ControlMessage, ControlResponse};
use serde_json::Value;
use std::fs;
use std::path::PathBuf;

fn get_fixture_path(name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/fixtures");
    path.push(name);
    path
}

fn assert_golden_response(fixture_name: &str) {
    let path = get_fixture_path(fixture_name);
    let json_str = fs::read_to_string(path).expect("Failed to read fixture");

    // Test deserialization
    let response: ControlResponse =
        serde_json::from_str(&json_str).expect(&format!("Failed to deserialize {}", fixture_name));

    // Test serialization
    let serialized = serde_json::to_value(&response).unwrap();
    let expected: Value = serde_json::from_str(&json_str).unwrap();

    assert_eq!(
        serialized, expected,
        "Serialization mismatch for {}",
        fixture_name
    );
}

fn assert_golden_message(fixture_name: &str) {
    let path = get_fixture_path(fixture_name);
    let json_str = fs::read_to_string(path).expect("Failed to read fixture");

    // Test deserialization
    let message: ControlMessage =
        serde_json::from_str(&json_str).expect(&format!("Failed to deserialize {}", fixture_name));

    // Test serialization
    let serialized = serde_json::to_value(&message).unwrap();
    let expected: Value = serde_json::from_str(&json_str).unwrap();

    assert_eq!(
        serialized, expected,
        "Serialization mismatch for {}",
        fixture_name
    );
}

#[test]
fn test_hook_response_allow_golden() {
    assert_golden_response("hook_response_allow.json");
}

#[test]
fn test_hook_response_deny_golden() {
    assert_golden_response("hook_response_deny.json");
}

#[test]
fn test_mcp_tool_call_golden() {
    assert_golden_message("mcp_tool_call.json");
}

#[test]
fn test_mcp_tool_response_success_golden() {
    assert_golden_response("mcp_tool_response_success.json");
}

#[test]
fn test_mcp_tool_response_error_golden() {
    assert_golden_response("mcp_tool_response_error.json");
}

#[test]
fn test_ping_golden() {
    assert_golden_message("ping.json");
}

#[test]
fn test_pong_golden() {
    assert_golden_response("pong.json");
}

#[test]
fn test_tools_list_request_golden() {
    assert_golden_message("tools_list_request.json");
}

#[test]
fn test_tools_list_response_golden() {
    assert_golden_response("tools_list_response.json");
}
