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
        #[serde(default)]
        visible_when: Option<VisibilityRule>,
    },
    #[serde(rename = "slider")]
    Slider {
        id: String,
        label: String,
        min: f32,
        max: f32,
        default: f32,
        #[serde(default)]
        visible_when: Option<VisibilityRule>,
    },
    #[serde(rename = "checkbox")]
    Checkbox {
        id: String,
        label: String,
        default: bool,
        #[serde(default)]
        visible_when: Option<VisibilityRule>,
    },
    #[serde(rename = "textbox")]
    Textbox {
        id: String,
        label: String,
        placeholder: Option<String>,
        rows: Option<u32>,
        #[serde(default)]
        visible_when: Option<VisibilityRule>,
    },
    #[serde(rename = "choice")]
    Choice {
        id: String,
        label: String,
        options: Vec<String>,
        default: Option<usize>,
        #[serde(default)]
        visible_when: Option<VisibilityRule>,
    },
    #[serde(rename = "multiselect")]
    Multiselect {
        id: String,
        label: String,
        options: Vec<String>,
        default: Option<usize>, // Keeping for backward compat, though unused in logic
        #[serde(default)]
        visible_when: Option<VisibilityRule>,
    },
    #[serde(rename = "group")]
    Group {
        id: String,
        label: String,
        #[serde(default)]
        visible_when: Option<VisibilityRule>,
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

    /// Get the visibility rule.
    pub fn visible_when(&self) -> Option<&VisibilityRule> {
        match self {
            Component::Text { visible_when, .. } => visible_when.as_ref(),
            Component::Slider { visible_when, .. } => visible_when.as_ref(),
            Component::Checkbox { visible_when, .. } => visible_when.as_ref(),
            Component::Textbox { visible_when, .. } => visible_when.as_ref(),
            Component::Choice { visible_when, .. } => visible_when.as_ref(),
            Component::Multiselect { visible_when, .. } => visible_when.as_ref(),
            Component::Group { visible_when, .. } => visible_when.as_ref(),
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
    /// Show if a numeric component's value is greater than or equal to `min_value`.
    /// Haskell: GreaterThan { id, min_value }
    GreaterThan { id: String, min_value: f32 },
    /// Show if a numeric component's value is less than or equal to `max_value`.
    /// Haskell: LessThan { id, max_value }
    LessThan { id: String, max_value: f32 },
    /// Show if the count for a component (e.g., multiselect) equals `exact_count`.
    /// Haskell: CountEquals { id, exact_count }
    CountEquals { id: String, exact_count: u32 },
    /// Show if the count for a component is greater than or equal to `min_count`.
    /// Haskell: CountGreaterThan { id, min_count }
    CountGreaterThan { id: String, min_count: u32 },
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
    
    // Helper to extract plain JSON values for submission
    pub fn to_json_values(&self) -> Value {
        let mut map = serde_json::Map::new();
        for (k, v) in &self.values {
            let json_val = match v {
                ElementValue::Number(n) => serde_json::json!(n),
                ElementValue::Boolean(b) => serde_json::json!(b),
                ElementValue::Text(s) => serde_json::json!(s),
                ElementValue::Choice(i) => serde_json::json!(i),
                ElementValue::MultiChoice(vec) => serde_json::json!(vec),
            };
            map.insert(k.clone(), json_val);
        }
        Value::Object(map)
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
        self.values
            .insert(id.to_string(), ElementValue::Number(value));
    }

    pub fn set_boolean(&mut self, id: &str, value: bool) {
        self.values
            .insert(id.to_string(), ElementValue::Boolean(value));
    }

    pub fn set_text(&mut self, id: &str, value: String) {
        self.values
            .insert(id.to_string(), ElementValue::Text(value));
    }

    pub fn set_choice(&mut self, id: &str, value: usize) {
        self.values
            .insert(id.to_string(), ElementValue::Choice(value));
    }

    pub fn set_multichoice(&mut self, id: &str, value: Vec<bool>) {
        self.values
            .insert(id.to_string(), ElementValue::MultiChoice(value));
    }
}

/// JSON result containing all form values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupResult {
    pub button: String, // "submit" or "decline"
    pub values: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub time_spent_seconds: Option<f64>, // Time user spent interacting with popup
}
