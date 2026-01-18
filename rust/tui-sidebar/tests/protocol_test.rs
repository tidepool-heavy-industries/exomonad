use serde_json;
use tui_sidebar::protocol::*;

#[test]
fn test_ui_spec_roundtrip() {
    let spec = UISpec {
        id: "test".to_string(),
        layout: Layout::Vertical {
            elements: vec![
                Element::Text {
                    id: "t1".to_string(),
                    content: "Hello".to_string(),
                },
                Element::Button {
                    id: "b1".to_string(),
                    label: "OK".to_string(),
                },
            ],
        },
    };

    let json = serde_json::to_string(&spec).unwrap();
    let parsed: UISpec = serde_json::from_str(&json).unwrap();

    assert_eq!(spec.id, parsed.id);
}

#[test]
fn test_interaction_button_clicked() {
    let interaction = Interaction::ButtonClicked {
        ui_id: "form1".to_string(),
        button_id: "ok".to_string(),
    };

    let json = serde_json::to_string(&interaction).unwrap();

    // Verify JSON structure
    assert!(json.contains(r#""type":"ButtonClicked"#));
    assert!(json.contains(r#""ui_id":"form1"#));
    assert!(json.contains(r#""button_id":"ok"#));

    // Roundtrip
    let parsed: Interaction = serde_json::from_str(&json).unwrap();
    match parsed {
        Interaction::ButtonClicked { ui_id, button_id } => {
            assert_eq!(ui_id, "form1");
            assert_eq!(button_id, "ok");
        }
        _ => panic!("Wrong interaction type"),
    }
}

#[test]
fn test_interaction_input_submitted() {
    let interaction = Interaction::InputSubmitted {
        ui_id: "form1".to_string(),
        input_id: "name".to_string(),
        value: "Alice".to_string(),
    };

    let json = serde_json::to_string(&interaction).unwrap();

    // Verify JSON structure
    assert!(json.contains(r#""type":"InputSubmitted"#));
    assert!(json.contains(r#""ui_id":"form1"#));
    assert!(json.contains(r#""input_id":"name"#));
    assert!(json.contains(r#""value":"Alice"#));

    // Roundtrip
    let parsed: Interaction = serde_json::from_str(&json).unwrap();
    match parsed {
        Interaction::InputSubmitted {
            ui_id,
            input_id,
            value,
        } => {
            assert_eq!(ui_id, "form1");
            assert_eq!(input_id, "name");
            assert_eq!(value, "Alice");
        }
        _ => panic!("Wrong interaction type"),
    }
}

#[test]
fn test_all_element_types() {
    let elements = vec![
        Element::Text {
            id: "t1".to_string(),
            content: "Text".to_string(),
        },
        Element::Input {
            id: "i1".to_string(),
            label: "Name".to_string(),
            value: "".to_string(),
        },
        Element::Button {
            id: "b1".to_string(),
            label: "Submit".to_string(),
        },
        Element::Table {
            id: "table1".to_string(),
            columns: vec!["Col1".to_string(), "Col2".to_string()],
            rows: vec![vec!["A".to_string(), "B".to_string()]],
        },
        Element::Select {
            id: "s1".to_string(),
            label: "Choose".to_string(),
            options: vec!["Opt1".to_string(), "Opt2".to_string()],
            selected: Some(0),
        },
        Element::List {
            id: "l1".to_string(),
            items: vec!["Item1".to_string(), "Item2".to_string()],
        },
        Element::Progress {
            id: "p1".to_string(),
            label: "Loading".to_string(),
            value: 50,
            max: 100,
        },
    ];

    let layout = Layout::Vertical { elements };
    let spec = UISpec {
        id: "test".to_string(),
        layout,
    };

    // Roundtrip
    let json = serde_json::to_string(&spec).unwrap();
    let parsed: UISpec = serde_json::from_str(&json).unwrap();

    assert_eq!(spec.id, parsed.id);
}

#[test]
fn test_split_layout() {
    let layout = Layout::Split {
        left: Box::new(Layout::Vertical {
            elements: vec![Element::Text {
                id: "t1".to_string(),
                content: "Left".to_string(),
            }],
        }),
        right: Box::new(Layout::Vertical {
            elements: vec![Element::Text {
                id: "t2".to_string(),
                content: "Right".to_string(),
            }],
        }),
        ratio: 50,
    };

    let spec = UISpec {
        id: "split_test".to_string(),
        layout,
    };

    // Roundtrip
    let json = serde_json::to_string(&spec).unwrap();
    let parsed: UISpec = serde_json::from_str(&json).unwrap();

    assert_eq!(spec.id, parsed.id);
}

#[test]
fn test_element_update_types() {
    let updates = vec![
        ElementUpdate::SetText {
            text: "New text".to_string(),
        },
        ElementUpdate::SetValue {
            value: "New value".to_string(),
        },
        ElementUpdate::SetProgress { value: 75, max: 100 },
        ElementUpdate::SetOptions {
            options: vec!["A".to_string(), "B".to_string()],
            selected: Some(1),
        },
    ];

    for update in updates {
        // Roundtrip
        let json = serde_json::to_string(&update).unwrap();
        let _parsed: ElementUpdate = serde_json::from_str(&json).unwrap();
    }
}
