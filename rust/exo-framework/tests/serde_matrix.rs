use exo_framework::{HookDecision, HookInput, SessionStartOutput, StopDecision, ToolOutput};
use serde_json::{json, Value};

#[test]
fn hook_decision_round_trip() {
    let cases = vec![
        (HookDecision::Allow, json!({ "decision": "allow" })),
        (
            HookDecision::Deny {
                reason: "blocked".into(),
            },
            json!({ "decision": "deny", "reason": "blocked" }),
        ),
        (
            HookDecision::Modify {
                input: json!({ "x": 1 }),
            },
            json!({ "decision": "modify", "input": { "x": 1 } }),
        ),
    ];

    for (val, expected) in cases {
        let j = serde_json::to_value(&val).unwrap();
        assert_eq!(j, expected);
        let back: HookDecision = serde_json::from_value(j).unwrap();
        assert_eq!(back, val);
    }
}

#[test]
fn stop_decision_round_trip() {
    let cases = vec![
        (StopDecision::Allow, json!({ "decision": "allow" })),
        (
            StopDecision::Block {
                reason: "wait".into(),
            },
            json!({ "decision": "block", "reason": "wait" }),
        ),
    ];

    for (val, expected) in cases {
        let j = serde_json::to_value(&val).unwrap();
        assert_eq!(j, expected);
        let back: StopDecision = serde_json::from_value(j).unwrap();
        assert_eq!(back, val);
    }
}

#[test]
fn session_start_output_serde() {
    let s1 = SessionStartOutput {
        additional_context: Some("ctx".into()),
    };
    let j1 = serde_json::to_value(&s1).unwrap();
    assert_eq!(j1, json!({ "additional_context": "ctx" }));

    let s2 = SessionStartOutput {
        additional_context: None,
    };
    let j2 = serde_json::to_value(&s2).unwrap();
    assert_eq!(j2, json!({}));

    let back1: SessionStartOutput = serde_json::from_value(j1).unwrap();
    assert_eq!(back1, s1);
    let back2: SessionStartOutput = serde_json::from_value(j2).unwrap();
    assert_eq!(back2, s2);
}

#[test]
fn hook_input_deserialization_defaults() {
    let j = json!({ "tool_name": "test" });
    let input: HookInput = serde_json::from_value(j).unwrap();
    assert_eq!(input.tool_name, "test");
    assert_eq!(input.tool_input, Value::Null);

    let j2 = json!({ "tool_name": "test", "tool_input": { "arg": 1 } });
    let input2: HookInput = serde_json::from_value(j2).unwrap();
    assert_eq!(input2.tool_input, json!({ "arg": 1 }));
}

#[test]
fn tool_output_serde() {
    let o1 = ToolOutput::with_data("ok", json!({ "res": 1 }));
    let j1 = serde_json::to_value(&o1).unwrap();
    assert_eq!(j1, json!({ "text": "ok", "data": { "res": 1 } }));

    let o2 = ToolOutput::text("just text");
    let j2 = serde_json::to_value(&o2).unwrap();
    assert_eq!(j2, json!({ "text": "just text" })); // data omitted when None

    let back1: ToolOutput = serde_json::from_value(j1).unwrap();
    assert_eq!(back1, o1);
    let back2: ToolOutput = serde_json::from_value(j2).unwrap();
    assert_eq!(back2, o2);
}
