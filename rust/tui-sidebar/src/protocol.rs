use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;

/// JSON popup definition with flat component structure
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PopupDefinition {
    pub title: String,
    pub components: Vec<Component>,
}

/// A single component with optional visibility conditions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Component {
    pub id: String,  // Required unique identifier
    #[serde(flatten)]
    pub spec: ComponentSpec,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visible_when: Option<VisibilityRule>,
}

/// The actual component specification
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum ComponentSpec {
    Text { content: String },
    Slider {
        label: String,
        min: f32,
        max: f32,
        #[serde(default = "default_slider_value")]
        default: f32,
    },
    Checkbox {
        label: String,
        #[serde(default)]
        default: bool,
    },
    Textbox {
        label: String,
        #[serde(default)]
        placeholder: Option<String>,
        #[serde(default)]
        rows: Option<u32>,
    },
    Choice {
        label: String,
        options: Vec<String>,
        #[serde(default)]
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

/// Simple visibility rules - single conditions only
///
/// Note: Uses untagged serialization, so each variant must have a unique structure.
/// Field names are deliberately different to avoid ambiguity during deserialization.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum VisibilityRule {
    // Simple: checkbox is checked
    Checked(String),  // Just the checkbox ID

    // Choice equals value
    Equals(HashMap<String, String>),  // {"choice_id": "Expected Value"}

    // Slider/number comparisons (different field names to disambiguate)
    GreaterThan { id: String, min_value: f32 },  // value > min_value
    LessThan { id: String, max_value: f32 },     // value < max_value

    // Multiselect has N items selected (different field names to disambiguate)
    CountEquals { id: String, exact_count: usize },      // count == exact_count
    CountGreaterThan { id: String, min_count: usize },   // count > min_count
}

// Helper function for default slider value
fn default_slider_value() -> f32 {
    50.0
}

/// Unified value type for all widget states
#[derive(Debug, Clone)]
pub enum ElementValue {
    Number(f32),
    Boolean(bool),
    Text(String),
    Choice(usize),
    MultiChoice(Vec<bool>),
}

/// Runtime state of all popup elements
#[derive(Debug, Clone)]
pub struct PopupState {
    pub values: HashMap<String, ElementValue>,  // ID -> value
    pub button_clicked: Option<String>,
}

impl PopupState {
    pub fn new(definition: &PopupDefinition) -> Self {
        let mut state = Self {
            values: HashMap::new(),
            button_clicked: None,
        };
        state.init_components(&definition.components);
        state
    }

    fn init_components(&mut self, components: &[Component]) {
        for component in components {
            match &component.spec {
                ComponentSpec::Slider { default, .. } => {
                    self.values.insert(component.id.clone(), ElementValue::Number(*default));
                }
                ComponentSpec::Checkbox { default, .. } => {
                    self.values.insert(component.id.clone(), ElementValue::Boolean(*default));
                }
                ComponentSpec::Textbox { .. } => {
                    self.values.insert(component.id.clone(), ElementValue::Text(String::new()));
                }
                ComponentSpec::Choice { default, .. } => {
                    self.values.insert(component.id.clone(), ElementValue::Choice(default.unwrap_or(0)));
                }
                ComponentSpec::Multiselect { options, .. } => {
                    self.values.insert(component.id.clone(), ElementValue::MultiChoice(vec![false; options.len()]));
                }
                _ => {}
            }
        }
    }

    // Helper methods for TUI component access
    pub fn get_number(&self, id: &str) -> Option<f32> {
        match self.values.get(id) {
            Some(ElementValue::Number(n)) => Some(*n),
            _ => None,
        }
    }

    pub fn set_number(&mut self, id: &str, value: f32) {
        self.values.insert(id.to_string(), ElementValue::Number(value));
    }

    pub fn get_boolean(&self, id: &str) -> Option<bool> {
        match self.values.get(id) {
            Some(ElementValue::Boolean(b)) => Some(*b),
            _ => None,
        }
    }

    pub fn set_boolean(&mut self, id: &str, value: bool) {
        self.values.insert(id.to_string(), ElementValue::Boolean(value));
    }

    pub fn get_text(&self, id: &str) -> Option<&String> {
        match self.values.get(id) {
            Some(ElementValue::Text(s)) => Some(s),
            _ => None,
        }
    }

    pub fn set_text(&mut self, id: &str, value: String) {
        self.values.insert(id.to_string(), ElementValue::Text(value));
    }

    pub fn get_choice(&self, id: &str) -> Option<usize> {
        match self.values.get(id) {
            Some(ElementValue::Choice(i)) => Some(*i),
            _ => None,
        }
    }

    pub fn set_choice(&mut self, id: &str, value: usize) {
        self.values.insert(id.to_string(), ElementValue::Choice(value));
    }

    pub fn get_multichoice(&self, id: &str) -> Option<&Vec<bool>> {
        match self.values.get(id) {
            Some(ElementValue::MultiChoice(choices)) => Some(choices),
            _ => None,
        }
    }

    pub fn set_multichoice(&mut self, id: &str, value: Vec<bool>) {
        self.values.insert(id.to_string(), ElementValue::MultiChoice(value));
    }
}

/// JSON result containing all form values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupResult {
    pub button: String,  // "submit" or "decline"
    pub values: Value,
}

impl PopupResult {
    pub fn from_state_with_definition(state: &PopupState, definition: &PopupDefinition) -> Self {
        let mut result_values = serde_json::Map::new();

        for component in &definition.components {
            if let Some(value) = state.values.get(&component.id) {
                let json_value = match (value, &component.spec) {
                    (ElementValue::Number(n), _) => json!(n),
                    (ElementValue::Boolean(b), _) => json!(b),
                    (ElementValue::Text(s), _) => json!(s),
                    (ElementValue::Choice(idx), ComponentSpec::Choice { options, .. }) => {
                        if let Some(selected) = options.get(*idx) {
                            json!(selected)
                        } else {
                            json!(null)
                        }
                    }
                    (ElementValue::MultiChoice(selections), ComponentSpec::Multiselect { options, .. }) => {
                        let selected: Vec<&String> = options.iter()
                            .enumerate()
                            .filter(|(i, _)| selections.get(*i).copied().unwrap_or(false))
                            .map(|(_, option)| option)
                            .collect();
                        json!(selected)
                    }
                    _ => json!(null),
                };
                result_values.insert(component.id.clone(), json_value);
            }
        }

        Self {
            button: state.button_clicked.clone().unwrap_or_else(|| "decline".to_string()),
            values: Value::Object(result_values),
        }
    }
}
