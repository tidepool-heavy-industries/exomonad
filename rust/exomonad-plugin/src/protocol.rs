pub use exomonad_ui_protocol::{
    Component, ElementValue, PopupDefinition, PopupState, VisibilityRule,
};
use serde::{Deserialize, Serialize};

// Minimal Status Bar Protocol
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum PluginState {
    Idle,
    Thinking,
    Error,
}

impl Default for PluginState {
    fn default() -> Self {
        Self::Idle
    }
}

impl std::fmt::Display for PluginState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PluginState::Idle => write!(f, "IDLE"),
            PluginState::Thinking => write!(f, "THINKING"),
            PluginState::Error => write!(f, "ERROR"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum PluginMessage {
    #[serde(rename = "status")]
    Status {
        state: PluginState,
        message: String,
    },
    #[serde(rename = "popup")]
    Popup {
        request_id: String,
        definition: PopupDefinition,
    },
    #[serde(rename = "close_popup")]
    ClosePopup,
}
