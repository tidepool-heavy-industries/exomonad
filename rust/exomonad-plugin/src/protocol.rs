use serde::{Deserialize, Serialize};

/// Plugin status state for the status bar display.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum PluginState {
    Idle,
    Thinking,
    Waiting,
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
            PluginState::Waiting => write!(f, "WAITING"),
            PluginState::Error => write!(f, "ERROR"),
        }
    }
}

/// Messages received via CustomMessage for status updates.
///
/// Note: Popups are now handled via CLI pipe mechanism, not CustomMessage.
/// Use `zellij pipe --plugin ... --name exomonad:popup --args ...` instead.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum PluginMessage {
    #[serde(rename = "status")]
    Status {
        state: PluginState,
        message: String,
    },
    #[serde(rename = "close_popup")]
    ClosePopup,
}
