//! ACP Client implementation for ExoMonad.
//!
//! Implements the ACP `Client` trait to handle callbacks from Gemini agents:
//! - `session_notification`: forwards agent output to the TL via Teams inbox
//! - `request_permission`: auto-approves all tool calls

use agent_client_protocol::{
    RequestPermissionRequest, RequestPermissionResponse,
    SessionNotification,
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
        _args: RequestPermissionRequest,
    ) -> agent_client_protocol::Result<RequestPermissionResponse> {
        todo!("Auto-approve all permission requests")
    }

    async fn session_notification(
        &self,
        _args: SessionNotification,
    ) -> agent_client_protocol::Result<()> {
        todo!("Forward notification to TL via Teams inbox")
    }
}
