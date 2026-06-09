use exo_framework::*;
use serde_json::json;

fn assert_roundtrip<T>(val: &T)
where
    T: serde::Serialize + for<'de> serde::Deserialize<'de> + std::fmt::Debug + PartialEq,
{
    let json = serde_json::to_string(val).expect("failed to serialize");
    let back: T = serde_json::from_str(&json).expect("failed to deserialize");
    assert_eq!(val, &back, "Roundtrip failed for JSON: {}", json);
}

#[test]
fn test_hook_decision_roundtrip() {
    let variants = [
        HookDecision::Allow,
        HookDecision::Deny {
            reason: "blocked".into(),
        },
        HookDecision::Modify {
            input: json!({"arg": 1}),
        },
    ];
    for v in variants {
        assert_roundtrip(&v);
    }

    // Tag pinning
    assert_eq!(
        serde_json::to_value(&HookDecision::Allow).unwrap(),
        json!({"decision": "allow"})
    );
    assert_eq!(
        serde_json::to_value(&HookDecision::Deny { reason: "r".into() }).unwrap(),
        json!({"decision": "deny", "reason": "r"})
    );
}

#[test]
fn test_stop_decision_roundtrip() {
    let variants = [
        StopDecision::Allow,
        StopDecision::Block {
            reason: "dirty".into(),
        },
    ];
    for v in variants {
        assert_roundtrip(&v);
    }

    // Tag pinning
    assert_eq!(
        serde_json::to_value(&StopDecision::Allow).unwrap(),
        json!({"decision": "allow"})
    );
}

#[test]
fn test_session_start_output_roundtrip() {
    let s1 = SessionStartOutput {
        additional_context: None,
    };
    let s2 = SessionStartOutput {
        additional_context: Some("hello".into()),
    };
    assert_roundtrip(&s1);
    assert_roundtrip(&s2);
}

#[test]
fn test_hook_input_roundtrip() {
    let h = HookInput {
        tool_name: "test".into(),
        tool_input: json!({"a": 1}),
    };
    assert_roundtrip(&h);
}

#[test]
fn test_tool_output_roundtrip() {
    let o1 = ToolOutput::text("hi");
    let o2 = ToolOutput::with_data("hi", json!({"res": true}));
    assert_roundtrip(&o1);
    assert_roundtrip(&o2);
}
