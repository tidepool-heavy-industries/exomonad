use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupDefinition {
    pub title: String,
    pub components: Vec<Component>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Component {
    pub id: String,
    pub label: String,
    pub spec: ComponentSpec,
    pub visible_when: Option<VisibilityRule>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "tag", content = "contents")]
pub enum ComponentSpec {
    Text {
        content: String,
    },
    Slider {
        label: String,
        min: f32,
        max: f32,
        default: f32,
    },
    Checkbox {
        label: String,
        default: bool,
    },
    Textbox {
        label: String,
        placeholder: Option<String>,
        rows: Option<u32>,
    },
    Choice {
        label: String,
        options: Vec<String>,
        default: Option<usize>,
    },
    Multiselect {
        label: String,
        options: Vec<String>,
    },
    Group {
        label: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "tag", content = "contents")]
pub enum VisibilityRule {
    Checked(String),                   // Visible if checkbox with ID is checked
    Equals(HashMap<String, String>),   // Visible if choice with ID equals value
    GreaterThan { id: String, min_value: f32 },
    LessThan { id: String, max_value: f32 },
    CountEquals { id: String, exact_count: usize },
    CountGreaterThan { id: String, min_count: usize },
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
            match &component.spec {
                ComponentSpec::Slider { default, .. } => {
                    values.insert(component.id.clone(), ElementValue::Number(*default));
                }
                ComponentSpec::Checkbox { default, .. } => {
                    values.insert(component.id.clone(), ElementValue::Boolean(*default));
                }
                ComponentSpec::Textbox { .. } => {
                    values.insert(component.id.clone(), ElementValue::Text(String::new()));
                }
                ComponentSpec::Choice { default, .. } => {
                    values.insert(component.id.clone(), ElementValue::Choice(default.unwrap_or(0)));
                }
                ComponentSpec::Multiselect { options, .. } => {
                    values.insert(
                        component.id.clone(),
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

impl PopupResult {
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_component_serialization() {
        let component = Component {
            id: "my-slider".to_string(),
            label: "Volume".to_string(),
            spec: ComponentSpec::Slider {
                label: "Volume".to_string(),
                min: 0.0,
                max: 100.0,
                default: 50.0,
            },
            visible_when: None,
        };

        let json = serde_json::to_string(&component).unwrap();
        let expected = r#"{"id":"my-slider","type":"slider","label":"Volume","min":0.0,"max":100.0,"default":50.0}"#;
        assert_eq!(json, expected);

        let parsed: Component = serde_json::from_str(expected).unwrap();
        assert_eq!(parsed, component);
    }

    #[test]
    fn test_popup_state_initialization() {
        let def = PopupDefinition {
            title: "Test Popup".to_string(),
            components: vec![
                Component {
                    id: "chk".to_string(),
                    label: "Check".to_string(),
                    spec: ComponentSpec::Checkbox { label: "Check".to_string(), default: true },
                    visible_when: None,
                },
                Component {
                    id: "txt".to_string(),
                    label: "Text".to_string(),
                    spec: ComponentSpec::Textbox { label: "Text".to_string(), placeholder: None, rows: None },
                    visible_when: None,
                },
            ],
        };

        let state = PopupState::new(&def);
        assert_eq!(state.get_boolean("chk"), Some(true));
        assert_eq!(state.get_text("txt"), Some(""));
    }

    #[test]
    fn test_visibility_rule_serialization() {
        let rule = VisibilityRule::Checked("some-checkbox".to_string());
        let json = serde_json::to_string(&rule).unwrap();
        assert_eq!(json, "\"some-checkbox\"");

        let rule_eq = VisibilityRule::Equals(HashMap::from([("choice".to_string(), "Value".to_string())]));
        let json_eq = serde_json::to_string(&rule_eq).unwrap();
        assert_eq!(json_eq, "{\"choice\":\"Value\"}");
    }
}
