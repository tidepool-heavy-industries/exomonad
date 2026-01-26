use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupDefinition {
    pub title: String,
    pub components: Vec<Component>,
}

/// Component deserialized from Haskell ToJSON format.
/// Haskell uses flat structure with "type" field, not nested "spec" object.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type")]
pub enum Component {
    #[serde(rename = "text")]
    Text {
        id: String,
        content: String,
    },
    #[serde(rename = "slider")]
    Slider {
        id: String,
        label: String,
        min: f32,
        max: f32,
        default: f32,
    },
    #[serde(rename = "checkbox")]
    Checkbox {
        id: String,
        label: String,
        default: bool,
    },
    #[serde(rename = "textbox")]
    Textbox {
        id: String,
        label: String,
        placeholder: Option<String>,
        rows: Option<u32>,
    },
    #[serde(rename = "choice")]
    Choice {
        id: String,
        label: String,
        options: Vec<String>,
        default: Option<usize>,
    },
    #[serde(rename = "multiselect")]
    Multiselect {
        id: String,
        label: String,
        options: Vec<String>,
    },
    #[serde(rename = "group")]
    Group {
        id: String,
        label: String,
    },
}

impl Component {
    /// Get the component ID.
    pub fn id(&self) -> &str {
        match self {
            Component::Text { id, .. } => id,
            Component::Slider { id, .. } => id,
            Component::Checkbox { id, .. } => id,
            Component::Textbox { id, .. } => id,
            Component::Choice { id, .. } => id,
            Component::Multiselect { id, .. } => id,
            Component::Group { id, .. } => id,
        }
    }

    /// Get the component label (for display). Text components use content as label.
    pub fn label(&self) -> &str {
        match self {
            Component::Text { content, .. } => content,
            Component::Slider { label, .. } => label,
            Component::Checkbox { label, .. } => label,
            Component::Textbox { label, .. } => label,
            Component::Choice { label, .. } => label,
            Component::Multiselect { label, .. } => label,
            Component::Group { label, .. } => label,
        }
    }
}


/// Visibility rules from Haskell TUI.hs.
/// Uses serde untagged since Haskell encodes these as simple objects.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum VisibilityRule {
    /// Show if checkbox with ID is checked. {"visible_when": "checkbox-id"}
    Checked(String),
    /// Show if choice equals value. {"visible_when": {"choice-id": "value"}}
    Equals(HashMap<String, String>),
}

/// Internal state of the popup form
#[derive(Debug, Clone)]
pub struct PopupState {
    pub values: HashMap<String, ElementValue>,
    pub button_clicked: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElementValue {
    Number(f32),
    Boolean(bool),
    Text(String),
    Choice(usize),
    MultiChoice(Vec<bool>),
}

impl PopupState {
    pub fn new(definition: &PopupDefinition) -> Self {
        let mut values = HashMap::new();
        for component in &definition.components {
            match component {
                Component::Slider { id, default, .. } => {
                    values.insert(id.clone(), ElementValue::Number(*default));
                }
                Component::Checkbox { id, default, .. } => {
                    values.insert(id.clone(), ElementValue::Boolean(*default));
                }
                Component::Textbox { id, .. } => {
                    values.insert(id.clone(), ElementValue::Text(String::new()));
                }
                Component::Choice { id, default, .. } => {
                    values.insert(id.clone(), ElementValue::Choice(default.unwrap_or(0)));
                }
                Component::Multiselect { id, options, .. } => {
                    values.insert(
                        id.clone(),
                        ElementValue::MultiChoice(vec![false; options.len()]),
                    );
                }
                _ => {}
            }
        }
        Self {
            values,
            button_clicked: None,
        }
    }

    pub fn get_number(&self, id: &str) -> Option<f32> {
        match self.values.get(id) {
            Some(ElementValue::Number(n)) => Some(*n),
            _ => None,
        }
    }

    pub fn get_boolean(&self, id: &str) -> Option<bool> {
        match self.values.get(id) {
            Some(ElementValue::Boolean(b)) => Some(*b),
            _ => None,
        }
    }

    pub fn get_text(&self, id: &str) -> Option<&str> {
        match self.values.get(id) {
            Some(ElementValue::Text(t)) => Some(t),
            _ => None,
        }
    }

    pub fn get_choice(&self, id: &str) -> Option<usize> {
        match self.values.get(id) {
            Some(ElementValue::Choice(c)) => Some(*c),
            _ => None,
        }
    }

    pub fn get_multichoice(&self, id: &str) -> Option<&[bool]> {
        match self.values.get(id) {
            Some(ElementValue::MultiChoice(v)) => Some(v),
            _ => None,
        }
    }

    pub fn set_number(&mut self, id: &str, value: f32) {
        self.values.insert(id.to_string(), ElementValue::Number(value));
    }

    pub fn set_boolean(&mut self, id: &str, value: bool) {
        self.values.insert(id.to_string(), ElementValue::Boolean(value));
    }

    pub fn set_text(&mut self, id: &str, value: String) {
        self.values.insert(id.to_string(), ElementValue::Text(value));
    }

    pub fn set_choice(&mut self, id: &str, value: usize) {
        self.values.insert(id.to_string(), ElementValue::Choice(value));
    }

    pub fn set_multichoice(&mut self, id: &str, value: Vec<bool>) {
        self.values.insert(id.to_string(), ElementValue::MultiChoice(value));
    }
}

/// JSON result containing all form values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupResult {
    pub button: String, // "submit" or "decline"
    pub values: Value,
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_component_deserialization() {
        // Test the Haskell-format JSON
        let json = r#"{"id":"my-slider","type":"slider","label":"Volume","min":0.0,"max":100.0,"default":50.0}"#;
        let parsed: Component = serde_json::from_str(json).unwrap();

        match parsed {
            Component::Slider { id, label, min, max, default } => {
                assert_eq!(id, "my-slider");
                assert_eq!(label, "Volume");
                assert_eq!(min, 0.0);
                assert_eq!(max, 100.0);
                assert_eq!(default, 50.0);
            }
            _ => panic!("Expected Slider component"),
        }
    }

    #[test]
    fn test_text_component_deserialization() {
        let json = r#"{"id":"msg","type":"text","content":"Hello World"}"#;
        let parsed: Component = serde_json::from_str(json).unwrap();

        match parsed {
            Component::Text { id, content } => {
                assert_eq!(id, "msg");
                assert_eq!(content, "Hello World");
            }
            _ => panic!("Expected Text component"),
        }
    }

    #[test]
    fn test_popup_state_initialization() {
        let def = PopupDefinition {
            title: "Test Popup".to_string(),
            components: vec![
                Component::Checkbox {
                    id: "chk".to_string(),
                    label: "Check".to_string(),
                    default: true,
                },
                Component::Textbox {
                    id: "txt".to_string(),
                    label: "Text".to_string(),
                    placeholder: None,
                    rows: None,
                },
            ],
        };

        let state = PopupState::new(&def);
        assert_eq!(state.get_boolean("chk"), Some(true));
        assert_eq!(state.get_text("txt"), Some(""));
    }

    #[test]
    fn test_visibility_rule_deserialization() {
        // String format for Checked
        let json = r#""some-checkbox""#;
        let rule: VisibilityRule = serde_json::from_str(json).unwrap();
        assert_eq!(rule, VisibilityRule::Checked("some-checkbox".to_string()));

        // Object format for Equals
        let json_eq = r#"{"choice":"Value"}"#;
        let rule_eq: VisibilityRule = serde_json::from_str(json_eq).unwrap();
        assert_eq!(rule_eq, VisibilityRule::Equals(HashMap::from([("choice".to_string(), "Value".to_string())])));
    }

    // ═══════════════════════════════════════════════════════════════════
    // GOLDEN FILE TESTS - Wire format contract with Haskell
    // ═══════════════════════════════════════════════════════════════════
    // These tests parse canonical JSON that Haskell must produce.
    // NOTE: Disabled because golden files are not present in this worktree subset.
    /*
    #[test]
    fn golden_text_component() {
        let json = include_str!("../../../tests/golden/tui/text_component.json");
        let parsed: Component = serde_json::from_str(json).expect("Failed to parse golden text_component.json");
        match parsed {
            Component::Text { id, content } => {
                assert_eq!(id, "msg");
                assert_eq!(content, "Hello World");
            }
            _ => panic!("Expected Text component"),
        }
    }

    #[test]
    fn golden_slider_component() {
        let json = include_str!("../../../tests/golden/tui/slider_component.json");
        let parsed: Component = serde_json::from_str(json).expect("Failed to parse golden slider_component.json");
        match parsed {
            Component::Slider { id, label, min, max, default } => {
                assert_eq!(id, "volume");
                assert_eq!(label, "Volume");
                assert_eq!(min, 0.0);
                assert_eq!(max, 100.0);
                assert_eq!(default, 50.0);
            }
            _ => panic!("Expected Slider component"),
        }
    }

    #[test]
    fn golden_checkbox_component() {
        let json = include_str!("../../../tests/golden/tui/checkbox_component.json");
        let parsed: Component = serde_json::from_str(json).expect("Failed to parse golden checkbox_component.json");
        match parsed {
            Component::Checkbox { id, label, default } => {
                assert_eq!(id, "enabled");
                assert_eq!(label, "Enable feature");
                assert_eq!(default, false);
            }
            _ => panic!("Expected Checkbox component"),
        }
    }

    #[test]
    fn golden_popup_definition() {
        let json = include_str!("../../../tests/golden/tui/popup_definition.json");
        let parsed: PopupDefinition = serde_json::from_str(json).expect("Failed to parse golden popup_definition.json");
        assert_eq!(parsed.title, "Confirm Action");
        assert_eq!(parsed.components.len(), 2);
        match &parsed.components[0] {
            Component::Text { id, content } => {
                assert_eq!(id, "action");
                assert_eq!(content, "Action: Delete files");
            }
            _ => panic!("Expected Text component at index 0"),
        }
    }

    #[test]
    fn golden_popup_result() {
        let json = include_str!("../../../tests/golden/tui/popup_result.json");
        let parsed: PopupResult = serde_json::from_str(json).expect("Failed to parse golden popup_result.json");
        assert_eq!(parsed.button, "submit");
    }
    */
}
