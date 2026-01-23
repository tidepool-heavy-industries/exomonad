use mantle_shared::error::Result;
use mantle_shared::protocol::{ControlMessage, ControlResponse};
use mantle_shared::socket::{control_socket_path, ControlSocket};
use tracing::info;

/// Run a health check against the control server.
///
/// Connects to the control socket, sends a Ping message, and waits for a Pong.
/// Returns Ok(()) if successful, or an error if the check fails.
pub fn run_health_check() -> Result<()> {
    let path = control_socket_path()?;
    info!(path = %path.display(), "Checking control server health");

    let mut socket = ControlSocket::connect(&path)?;

    let response = socket.send(&ControlMessage::Ping)?;

    match response {
        ControlResponse::Pong => {
            info!("Control server is healthy");
            Ok(())
        }
        other => Err(mantle_shared::error::MantleError::HealthCheck(format!(
            "Unexpected response to Ping: {:?}",
            other
        ))),
    }
}
