use crate::common::{HostResult, IntoFFIResult};
use anyhow::{Context, Result};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
use exomonad_ui_protocol::{PopupDefinition, PopupResult};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::sync::oneshot;
use tracing::debug;

// Map of request_id -> Sender channel for the response
type PendingRequests = Arc<Mutex<HashMap<String, oneshot::Sender<PopupResult>>>>;

#[derive(Clone)]
pub struct UIService {
    pending_requests: PendingRequests,
}

impl UIService {
    pub fn new() -> Self {
        Self {
            pending_requests: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub async fn show_popup(&self, definition: PopupDefinition) -> Result<PopupResult> {
        let request_id = uuid::Uuid::new_v4().to_string();
        let (tx, rx) = oneshot::channel();
        let title = definition.title.clone();

        // Register pending request
        {
            let mut pending = self.pending_requests.lock().map_err(|e| anyhow::anyhow!("Lock poisoned: {}", e))?;
            pending.insert(request_id.clone(), tx);
        }

        // Send message to Zellij plugin
        let message = serde_json::json!({
            "kind": "popup",
            "request_id": request_id,
            "definition": definition
        });
        
        let payload = serde_json::to_string(&message)?;
        
        debug!("Sending popup request {} to Zellij", request_id);
        
        // Use zellij CLI to send message
        // Note: This spawns a subprocess. In a high-throughput scenario, we might want to use
        // a persistent pipe connection or similar, but for UI interactions this is fine.
        let status = tokio::process::Command::new("zellij")
            .args(["action", "message", "--name", "exomonad", "--payload", &payload])
            .status()
            .await
            .context("Failed to execute zellij action message")?;
            
        if !status.success() {
            // Clean up if sending failed
            tracing::warn!(%status, %request_id, "Failed to send message to Zellij plugin; zellij might not be running or in PATH");
            let mut pending = self.pending_requests.lock().map_err(|e| anyhow::anyhow!("Lock poisoned: {}", e))?;
            pending.remove(&request_id);
            return Err(anyhow::anyhow!("zellij action message failed with status {}", status));
        }

        // Wait for response with timeout (10 minutes)
        match tokio::time::timeout(std::time::Duration::from_secs(600), rx).await {
            Ok(Ok(result)) => Ok(result),
            Ok(Err(_)) => {
                // Sender dropped (channel closed) without sending value
                Err(anyhow::anyhow!("Popup request cancelled or lost"))
            }
            Err(_) => {
                // Timeout occurred
                let mut pending = self.pending_requests.lock().map_err(|e| anyhow::anyhow!("Lock poisoned: {}", e))?;
                pending.remove(&request_id);
                Err(anyhow::anyhow!("Popup request '{}' ({}) timed out after 10 minutes", title, request_id))
            }
        }
    }

    /// Handles a reply from the UI (Zellij plugin).
    ///
    /// Note: A rare race condition exists where a timeout might occur just as a reply arrives.
    /// In this case, the timeout will be returned to the caller, and the late reply will be
    /// logged and ignored since the request_id has been removed from pending_requests.
    pub fn handle_reply(&self, request_id: &str, result: PopupResult) -> Result<()> {
        let mut pending = self.pending_requests.lock().map_err(|e| anyhow::anyhow!("Lock poisoned: {}", e))?;
        
        if let Some(tx) = pending.remove(request_id) {
            let _ = tx.send(result);
            Ok(())
        } else {
            debug!("Received reply for unknown/expired request {}", request_id);
            Ok(())
        }
    }
}

impl Default for UIService {
    fn default() -> Self {
        Self::new()
    }
}

fn block_on<F: std::future::Future>(future: F) -> Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => {
            // Extism host functions are expected to run inside an existing Tokio runtime.
            // Do not create a new runtime here; instead, surface an explicit error.
            Err(Error::msg(
                "block_on called without a current Tokio runtime; a runtime must be provided by the host",
            ))
        }
    }
}

pub fn ui_show_popup_host_fn(ui_service: Arc<UIService>) -> Function {
    Function::new(
        "ui_show_popup",
        [ValType::I64],
        [ValType::I64],
        UserData::new(ui_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<UIService>>|
         -> Result<(), Error> {
            let _span = tracing::info_span!("host_function", function = "ui_show_popup").entered();

            if inputs.is_empty() {
                return Err(Error::msg("ui_show_popup: expected input argument"));
            }
            
            let Json(definition): Json<PopupDefinition> = plugin.memory_get_val(&inputs[0])?;
            tracing::info!(title = %definition.title, "Showing popup");

            let ui_mutex = user_data.get()?;
            let ui_arc = ui_mutex.lock().map_err(|_| Error::msg("Poisoned lock"))?;
            
            let result = block_on(ui_arc.show_popup(definition))?;

            match &result {
                Ok(_) => tracing::info!("Popup completed successfully"),
                Err(e) => tracing::warn!(error = %e, "Popup failed"),
            }

            let output: HostResult<PopupResult> = result.into_ffi_result();

            plugin.memory_set_val(&mut outputs[0], Json(output))?;
            Ok(())
        },
    )
    .with_namespace("env")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handle_reply_unknown_request() {
        let service = UIService::new();
        // Should return Ok even if request is unknown
        let result = service.handle_reply(
            "unknown-id",
            PopupResult {
                button: "cancel".into(),
                values: serde_json::Value::Null,
                time_spent_seconds: None,
            },
        );
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_handle_reply_success() {
        let service = UIService::new();
        let request_id = "test-req-1";
        let (tx, rx) = oneshot::channel();

        // Manually insert a pending request
        {
            let mut pending = service.pending_requests.lock().unwrap();
            pending.insert(request_id.to_string(), tx);
        }

        // Handle reply
        let reply = PopupResult {
            button: "submit".into(),
            values: serde_json::json!({"foo": "bar"}),
            time_spent_seconds: Some(1.5),
        };
        
        let handle_result = service.handle_reply(request_id, reply.clone());
        assert!(handle_result.is_ok());

        // Verify receiver got the reply
        let received = rx.await.unwrap();
        assert_eq!(received, reply);
    }
}
