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
        default: Option<usize>, // Keeping for backward compat, though unused in logic
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
