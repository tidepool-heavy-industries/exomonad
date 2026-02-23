//! Popup effect handler for the `popup.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::popup`.

use crate::effects::{dispatch_popup_effect, EffectError, EffectResult, PopupEffects, ResultExt};
use crate::services::popup::{PopupInput, PopupService};
use async_trait::async_trait;
use exomonad_proto::effects::popup::*;

/// Popup effect handler.
///
/// Handles all effects in the `popup.*` namespace by delegating to
/// the generated `dispatch_popup_effect` function.
pub struct PopupHandler {
    zellij_session: Option<String>,
}

impl PopupHandler {
    pub fn new(zellij_session: Option<String>) -> Self {
        Self { zellij_session }
    }
}

crate::impl_pass_through_handler!(PopupHandler, "popup", dispatch_popup_effect);

#[async_trait]
impl PopupEffects for PopupHandler {
    async fn show_popup(
        &self,
        req: ShowPopupRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ShowPopupResponse> {
        // Components bytes can be either:
        // - JSON array of component objects (form mode)
        // - JSON object with "panes" key (wizard mode)
        let raw_json: serde_json::Value = if req.components.is_empty() {
            serde_json::Value::Array(Vec::new())
        } else {
            serde_json::from_slice(&req.components).map_err(|e| {
                EffectError::invalid_input(format!("Invalid components JSON: {}", e))
            })?
        };

        let target_tab = crate::services::agent_control::resolve_own_tab_name(ctx);

        let input = PopupInput {
            title: req.title,
            raw_json,
            target_tab: Some(target_tab),
        };

        let service = PopupService::new(self.zellij_session.clone());

        let output = service.show_popup(&input).effect_err("popup")?;

        let values_bytes = serde_json::to_vec(&output.values).map_err(|e| {
            EffectError::custom("popup_error", format!("Failed to serialize values: {}", e))
        })?;

        Ok(ShowPopupResponse {
            button: output.button,
            values: values_bytes,
        })
    }
}
