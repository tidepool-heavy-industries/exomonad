use serde::{Deserialize, Serialize};
pub use exomonad_ui_protocol::{
    Component, ElementValue, PopupDefinition, PopupResult, PopupState, VisibilityRule,
};

// Minimal Status Bar Protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum PluginMessage {
    #[serde(rename = "status")]
    Status {
        state: String, // "IDLE", "THINKING", "ERROR"
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