//! ACP Client implementation for ExoMonad.
//!
//! Implements the ACP `Client` trait to handle callbacks from Gemini agents:
//! - `session_notification`: forwards agent output to the TL via Teams inbox
//! - `request_permission`: auto-approves all tool calls

use agent_client_protocol::{
    RequestPermissionOutcome, RequestPermissionRequest, RequestPermissionResponse,
    SelectedPermissionOutcome, SessionNotification,
};

/// ACP client that auto-approves permissions and forwards notifications.
#[derive(Debug)]
pub struct ExoMonadAcpClient {
    /// Agent identifier for routing notifications.
    pub agent_id: String,
}

#[async_trait::async_trait]
impl agent_client_protocol::Client for ExoMonadAcpClient {
    async fn request_permission(
        &self,
        args: RequestPermissionRequest,
    ) -> agent_client_protocol::Result<RequestPermissionResponse> {
        tracing::info!(agent = %self.agent_id, request = ?args, "Auto-approving permission request");
        let outcome = if let Some(first_option) = args.options.first() {
            RequestPermissionOutcome::Selected(SelectedPermissionOutcome::new(
                first_option.option_id.clone(),
            ))
        } else {
            tracing::warn!(agent = %self.agent_id, "No permission options, cancelling");
            RequestPermissionOutcome::Cancelled
        };
        Ok(RequestPermissionResponse::new(outcome))
    }

    async fn session_notification(
        &self,
        args: SessionNotification,
    ) -> agent_client_protocol::Result<()> {
        tracing::debug!(agent = %self.agent_id, update = ?args.update, "ACP session notification");
        Ok(())
    }
}
