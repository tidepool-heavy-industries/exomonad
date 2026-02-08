//! Popup effect handler for the `popup.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::popup`.

use crate::services::popup::{PopupInput, PopupService};
use async_trait::async_trait;
use exomonad_core::effects::{
    dispatch_popup_effect, EffectError, EffectHandler, EffectResult, PopupEffects,
};
use exomonad_proto::effects::popup::*;

/// Popup effect handler.
///
/// Handles all effects in the `popup.*` namespace by delegating to
/// the generated `dispatch_popup_effect` function.
pub struct PopupHandler {
    zellij_session: String,
}

impl PopupHandler {
    pub fn new(zellij_session: String) -> Self {
        Self { zellij_session }
    }
}

#[async_trait]
impl EffectHandler for PopupHandler {
    fn namespace(&self) -> &str {
        "popup"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_popup_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl PopupEffects for PopupHandler {
    async fn show_popup(&self, req: ShowPopupRequest) -> EffectResult<ShowPopupResponse> {
        let components: Vec<serde_json::Value> = if req.components.is_empty() {
            Vec::new()
        } else {
            serde_json::from_slice(&req.components).map_err(|e| {
                EffectError::invalid_input(format!("Invalid components JSON: {}", e))
            })?
        };

        let input = PopupInput {
            title: req.title,
            components,
        };

        let service = PopupService::new(self.zellij_session.clone());

        let output = service
            .show_popup(&input)
            .map_err(|e| EffectError::custom("popup_error", e.to_string()))?;

        let values_bytes = serde_json::to_vec(&output.values).map_err(|e| {
            EffectError::custom("popup_error", format!("Failed to serialize values: {}", e))
        })?;

        Ok(ShowPopupResponse {
            button: output.button,
            values: values_bytes,
        })
    }
}
