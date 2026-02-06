//! Popup service for WASM host functions.
//!
//! Shows interactive popup forms via Zellij pipes and returns user responses.

use crate::common::{FFIBoundary, HostResult, IntoFFIResult};
use anyhow::{Context, Result};
use exomonad_ui_protocol::{PopupDefinition, PopupResult};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
use serde::{Deserialize, Serialize};
use std::process::{Command, Stdio};

// Re-export for host function registration
pub use exomonad_ui_protocol::PopupDefinition as PopupDefinitionType;

// ============================================================================
// Input/Output types for FFI
// ============================================================================

/// Input for show_popup host function (matches Haskell PopupRequest).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PopupInput {
    /// Title displayed at the top of the popup.
    pub title: String,
    /// List of UI components.
    pub components: Vec<serde_json::Value>,
}

impl FFIBoundary for PopupInput {}

/// Output from show_popup host function (matches Haskell PopupResponse).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PopupOutput {
    /// Button clicked: "submit" or "cancel"
    pub button: String,
    /// Component values as JSON object
    pub values: serde_json::Value,
}

impl FFIBoundary for PopupOutput {}

impl From<PopupResult> for PopupOutput {
    fn from(result: PopupResult) -> Self {
        Self {
            button: result.button,
            values: result.values,
        }
    }
}

// ============================================================================
// Popup Service
// ============================================================================

/// Popup service for showing interactive forms.
pub struct PopupService;

impl PopupService {
    /// Create a new popup service.
    pub fn new() -> Self {
        Self
    }

    /// Show a popup and wait for user response.
    ///
    /// Uses Zellij pipes for communication:
    /// 1. Send popup definition to `exomonad:popup:request`
    /// 2. Wait for response on `exomonad:popup:response`
    #[tracing::instrument(skip(self))]
    pub fn show_popup(&self, input: &PopupInput) -> Result<PopupOutput> {
        tracing::info!(title = %input.title, components = input.components.len(), "Showing popup");

        // Convert to PopupDefinition format expected by Zellij plugin
        let definition = PopupDefinition {
            title: input.title.clone(),
            components: input
                .components
                .iter()
                .filter_map(|c| serde_json::from_value(c.clone()).ok())
                .collect(),
        };

        let json =
            serde_json::to_string(&definition).context("Failed to serialize popup definition")?;

        tracing::debug!(json = %json, "Sending popup to Zellij");

        // Send to Zellij plugin via named pipe
        let mut send_cmd = Command::new("zellij")
            .args(["pipe", "--name", "exomonad:popup:request", "--"])
            .arg(&json)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn zellij pipe command")?;

        let send_status = send_cmd.wait().context("Failed to wait for zellij pipe")?;
        if !send_status.success() {
            anyhow::bail!("zellij pipe command failed with status: {}", send_status);
        }

        tracing::debug!("Waiting for popup response");

        // Wait for response from Zellij plugin
        // The response comes back on a named pipe
        let recv_cmd = Command::new("zellij")
            .args(["pipe", "--name", "exomonad:popup:response"])
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn zellij pipe receive command")?;

        let output = recv_cmd
            .wait_with_output()
            .context("Failed to wait for popup response")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Popup response pipe failed: {}", stderr);
        }

        let response_str =
            String::from_utf8(output.stdout).context("Invalid UTF-8 in popup response")?;

        tracing::debug!(response = %response_str, "Received popup response");

        let result: PopupResult =
            serde_json::from_str(&response_str).context("Failed to parse popup response")?;

        tracing::info!(
            button = %result.button,
            "Popup completed"
        );

        Ok(result.into())
    }
}

impl Default for PopupService {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Host Functions for WASM
// ============================================================================

fn get_input<T: serde::de::DeserializeOwned>(
    plugin: &mut CurrentPlugin,
    val: Val,
) -> Result<T, Error> {
    let handle = plugin
        .memory_from_val(&val)
        .ok_or_else(|| Error::msg("Invalid memory handle in input"))?;
    let bytes = plugin.memory_bytes(handle)?;
    Ok(serde_json::from_slice(bytes)?)
}

/// Create the show_popup host function.
pub fn show_popup_host_fn() -> Function {
    Function::new(
        "show_popup",
        [ValType::I64],
        [ValType::I64],
        UserData::new(PopupService::new()),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<PopupService>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "show_popup").entered();
            let input: PopupInput = get_input(plugin, inputs[0])?;
            tracing::info!(title = %input.title, "Popup requested");

            let service = user_data.get()?;
            let service = service.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = service.show_popup(&input);

            match &result {
                Ok(out) => tracing::info!(success = true, button = %out.button, "Completed"),
                Err(e) => tracing::warn!(error = %e, "Failed"),
            }

            let output: HostResult<PopupOutput> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

/// Register all popup host functions.
pub fn register_host_functions() -> Vec<Function> {
    vec![show_popup_host_fn()]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_popup_input_serialization() {
        let input = PopupInput {
            title: "Test".to_string(),
            components: vec![serde_json::json!({
                "type": "text",
                "id": "msg",
                "content": "Hello"
            })],
        };

        let json = serde_json::to_string(&input).unwrap();
        let parsed: PopupInput = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.title, "Test");
        assert_eq!(parsed.components.len(), 1);
    }

    #[test]
    fn test_popup_output_serialization() {
        let output = PopupOutput {
            button: "submit".to_string(),
            values: serde_json::json!({"choice": "option1"}),
        };

        let json = serde_json::to_string(&output).unwrap();
        let parsed: PopupOutput = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.button, "submit");
    }
}
