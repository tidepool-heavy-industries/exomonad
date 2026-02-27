//! Popup effect handler for the `popup.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::popup`.

use crate::effects::{dispatch_popup_effect, EffectError, EffectResult, PopupEffects};
use crate::layout::resolve_plugin_path;
use crate::services::popup::{
    extract_choice_items, sanitize_payload_field, PopupInput, PopupOutput, PopupService,
};
use crate::services::zellij_ipc::ZellijIpc;
use anyhow::{Context, Result};
use async_trait::async_trait;
use exomonad_proto::effects::popup::*;

/// Popup effect handler.
///
/// Handles all effects in the `popup.*` namespace by delegating to
/// the generated `dispatch_popup_effect` function.
pub struct PopupHandler {
    zellij_ipc: Option<ZellijIpc>,
}

impl PopupHandler {
    pub fn new(zellij_ipc: Option<ZellijIpc>) -> Self {
        Self { zellij_ipc }
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
        let ipc = self.zellij_ipc.clone().ok_or_else(|| {
            EffectError::custom(
                "popup_error",
                "No Zellij session available — popup requires a Zellij session",
            )
        })?;

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

        let plugin_path = resolve_plugin_path().ok_or_else(|| {
            EffectError::custom(
                "popup_error",
                "Zellij plugin not found. Run 'just install-all' to install it.",
            )
        })?;

        // Payload building logic (moved from PopupService)
        let request_id = uuid::Uuid::new_v4().to_string();
        let payload = build_payload(&request_id, &input).map_err(|e| {
            EffectError::custom("popup_error", format!("Failed to build payload: {}", e))
        })?;

        let popup_service = PopupService::new(ipc, plugin_path);

        let response_str = tokio::task::spawn_blocking(move || {
            popup_service.show_popup(&payload)
        })
        .await
        .map_err(|e| EffectError::custom("popup_error", format!("Task join failed: {}", e)))?
        .map_err(|e| EffectError::custom("popup_error", format!("Popup failed: {}", e)))?;

        // Response parsing logic (moved from PopupService)
        let output = parse_response(&request_id, &response_str).map_err(|e| {
            EffectError::custom("popup_error", format!("Failed to parse response: {}", e))
        })?;

        let values_bytes = serde_json::to_vec(&output.values).map_err(|e| {
            EffectError::custom("popup_error", format!("Failed to serialize values: {}", e))
        })?;

        Ok(ShowPopupResponse {
            button: output.button,
            values: values_bytes,
        })
    }
}

fn build_payload(request_id: &str, input: &PopupInput) -> Result<String> {
    // Determine mode: wizard (object with "panes") or form (array of components)
    let payload = if input.raw_json.get("panes").is_some() {
        // Wizard mode: wrap in WizardRequest envelope
        let wizard_def: crate::ui_protocol::WizardDefinition =
            serde_json::from_value(input.raw_json.clone())
                .context("Failed to parse wizard definition")?;
        let request = crate::ui_protocol::WizardRequest {
            request_id: request_id.to_string(),
            wizard: wizard_def,
        };
        serde_json::to_string(&request).context("Failed to serialize WizardRequest")?
    } else if let Some(components_array) = input.raw_json.as_array() {
        // Form mode: try typed component parsing
        let components_res: Result<Vec<crate::ui_protocol::Component>, _> =
            serde_json::from_value(serde_json::Value::Array(components_array.clone()));

        if let Ok(components) = components_res {
            let request = crate::ui_protocol::PopupRequest {
                request_id: request_id.to_string(),
                definition: crate::ui_protocol::PopupDefinition {
                    title: input.title.clone(),
                    components,
                },
            };
            serde_json::to_string(&request).context("Failed to serialize PopupRequest")?
        } else {
            let items = extract_choice_items(components_array);
            if items.is_empty() {
                anyhow::bail!("Popup must have at least one choice item for Zellij display");
            }

            let safe_title = sanitize_payload_field(&input.title);
            let safe_items: Vec<String> =
                items.iter().map(|s| sanitize_payload_field(s)).collect();
            format!("{}|{}|{}", request_id, safe_title, safe_items.join(","))
        }
    } else {
        anyhow::bail!("Invalid popup input: expected array of components or wizard object");
    };

    // Inject target_tab routing into JSON payloads so only the correct plugin instance renders.
    let payload = if let Some(target_tab) = &input.target_tab {
        if payload.starts_with('{') {
            let mut json: serde_json::Value = serde_json::from_str(&payload)
                .context("Failed to re-parse payload for target_tab injection")?;
            json["target_tab"] = serde_json::Value::String(target_tab.clone());
            serde_json::to_string(&json).context("Failed to re-serialize payload")?
        } else {
            payload
        }
    } else {
        payload
    };

    Ok(payload)
}

fn parse_response(request_id: &str, response_str: &str) -> Result<PopupOutput> {
    let response_str = response_str.trim();

    if response_str.is_empty() {
        anyhow::bail!(
            "Empty response from zellij pipe - plugin may have failed to load or respond"
        );
    }

    // Parse response: try JSON first (new format), then legacy "request_id:selection"
    if response_str.starts_with('{') {
        let json: serde_json::Value = serde_json::from_str(response_str)
            .context("Failed to parse JSON popup response")?;

        let resp_request_id = json["request_id"].as_str().unwrap_or("");
        if resp_request_id != request_id {
            tracing::warn!(expected = %request_id, received = %resp_request_id, "Request ID mismatch");
        }

        let result = &json["result"];
        let button = result["button"].as_str().unwrap_or("submit").to_string();
        let values = result["values"].clone();

        // For wizard results, include panes_visited in the values
        if let Some(panes_visited) = result.get("panes_visited") {
            let mut combined = serde_json::Map::new();
            combined.insert("values".to_string(), values);
            combined.insert("panes_visited".to_string(), panes_visited.clone());
            return Ok(PopupOutput {
                button,
                values: serde_json::Value::Object(combined),
            });
        }

        return Ok(PopupOutput { button, values });
    }

    // Legacy format: "request_id:selection" or "request_id:CANCELLED"
    let (resp_request_id, selection) = response_str.split_once(':').context(format!(
        "Invalid popup response format: expected 'request_id:selection', got: {:?}",
        response_str
    ))?;

    if resp_request_id != request_id {
        tracing::warn!(expected = %request_id, received = %resp_request_id, "Request ID mismatch");
    }

    let (button, values) = if selection == "CANCELLED" {
        ("cancelled".to_string(), serde_json::json!({}))
    } else {
        (
            "submit".to_string(),
            serde_json::json!({"selected": selection}),
        )
    };

    Ok(PopupOutput { button, values })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effects::EffectHandler;

    #[test]
    fn test_namespace() {
        let handler = PopupHandler::new(None);
        assert_eq!(handler.namespace(), "popup");
    }

        #[test]

        fn test_construction_no_session() {

            let handler = PopupHandler::new(None);

            assert_eq!(handler.namespace(), "popup");

        }

    

        #[test]

        fn test_construction_with_session() {

            let ipc = ZellijIpc::new("my-session");

            let handler = PopupHandler::new(Some(ipc));

            assert_eq!(handler.namespace(), "popup");

        }

    

        #[test]

        fn test_build_payload_form() {

            let input = PopupInput {

                title: "Test".to_string(),

                raw_json: serde_json::json!([{

                    "type": "text",

                    "id": "msg",

                    "content": "Hello"

                }]),

                target_tab: None,

            };

            let res = build_payload("req-123", &input).unwrap();

            assert!(res.contains("req-123"));

            assert!(res.contains("Test"));

            assert!(res.contains("Hello"));

        }

    

        #[test]

        fn test_parse_response_json() {

            let response = serde_json::json!({

                "request_id": "req-123",

                "result": {

                    "button": "submit",

                    "values": {"name": "test"}

                }

            });

            let res = parse_response("req-123", &response.to_string()).unwrap();

            assert_eq!(res.button, "submit");

            assert_eq!(res.values["name"], "test");

        }

    

        #[test]

        fn test_parse_response_legacy() {

            let res = parse_response("req-123", "req-123:Option A").unwrap();

            assert_eq!(res.button, "submit");

            assert_eq!(res.values["selected"], "Option A");

        }

    }

    