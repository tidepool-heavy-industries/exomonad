use exomonad_shared::error::Result;
use exomonad_shared::protocol::{ControlMessage, ControlResponse};
use exomonad_shared::socket::{control_socket_path, ControlSocket};
use tracing::info;

/// Run a health check against the control server.
///
/// Connects to the control socket, sends a Ping message, and waits for a Pong.
/// Returns Ok(()) if successful, or an error if the check fails.
pub async fn run_health_check() -> Result<()> {
    let path = control_socket_path()?;
    info!(path = %path.display(), "Checking control server health");

    let socket = ControlSocket::connect(&path)?;

    let response = socket.send(&ControlMessage::Ping).await?;

    match response {
        ControlResponse::Pong => {
            info!("Control server is healthy");
            Ok(())
        }
        other => Err(exomonad_shared::error::ExoMonadError::HealthCheck(format!(
            "Unexpected response to Ping: {:?}",
            other
        ))),
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::io::{Read, Write};
    use std::os::unix::net::UnixListener;
    use std::thread;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_health_check_success() {
        let dir = tempdir().unwrap();
        let socket_path = dir.path().join("control.sock");
        let socket_path_inner = socket_path.clone();

        // Start a simple HTTP-over-Unix Ping-Pong server
        let listener = UnixListener::bind(&socket_path).unwrap();

        let server_handle = thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();

            // Read request
            let mut buf = [0u8; 1024];
            let _ = stream.read(&mut buf).unwrap();

            // Construct response
            let response_obj = ControlResponse::Pong;
            let response_json = serde_json::to_string(&response_obj).unwrap();

            // Minimal HTTP response
            let http_response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}\r\n",
                response_json.len(),
                response_json
            );

            stream.write_all(http_response.as_bytes()).unwrap();
            stream.flush().unwrap();
        });

        // Run health check
        std::env::set_var("EXOMONAD_CONTROL_SOCKET", socket_path_inner);
        let result = run_health_check().await;
        assert!(result.is_ok());

        server_handle.join().unwrap();
    }

    #[tokio::test]
    async fn test_health_check_unexpected_response() {
        let dir = tempdir().unwrap();
        let socket_path = dir.path().join("control.sock");
        let socket_path_inner = socket_path.clone();

        let listener = UnixListener::bind(&socket_path).unwrap();

        let server_handle = thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            let mut buf = [0u8; 1024];
            let _ = stream.read(&mut buf).unwrap();

            // Return something else instead of Pong
            let response_obj = ControlResponse::ToolsListResponse { tools: vec![] };
            let response_json = serde_json::to_string(&response_obj).unwrap();

            let http_response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}\r\n",
                response_json.len(),
                response_json
            );

            stream.write_all(http_response.as_bytes()).unwrap();
            stream.flush().unwrap();
        });

        std::env::set_var("EXOMONAD_CONTROL_SOCKET", socket_path_inner);
        let result = run_health_check().await;
        assert!(result.is_err());

        let err = result.unwrap_err().to_string();
        assert!(err.contains("Unexpected response to Ping"));

        server_handle.join().unwrap();
    }
}
