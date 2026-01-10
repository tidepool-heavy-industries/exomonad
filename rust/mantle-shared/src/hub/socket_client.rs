//! Unix socket client for sending results to mantle-hub from containers.
//!
//! Containers have the hub socket mounted at /tmp/mantle.sock. This module
//! provides a simple way to write a SessionResult to that socket.

use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::path::Path;
use std::time::Duration;

use super::types::SessionResult;
use crate::error::{MantleError, Result};

/// Write a session result to the hub socket.
///
/// Connects to the hub socket, sends the result as JSON, and waits for
/// an acknowledgment response. Returns an error if the hub reports failure.
pub fn write_result_to_socket(socket_path: &Path, result: &SessionResult) -> Result<()> {
    // Connect to the socket
    let stream = UnixStream::connect(socket_path).map_err(|e| {
        MantleError::Hub(format!(
            "Failed to connect to hub socket at {}: {}",
            socket_path.display(),
            e
        ))
    })?;

    // Set read timeout for response
    stream
        .set_read_timeout(Some(Duration::from_secs(5)))
        .map_err(|e| MantleError::Hub(format!("Failed to set socket timeout: {}", e)))?;

    let mut writer = stream.try_clone().map_err(|e| {
        MantleError::Hub(format!("Failed to clone socket: {}", e))
    })?;

    // Serialize and send the result
    let json = serde_json::to_string(result)
        .map_err(|e| MantleError::Hub(format!("Failed to serialize result: {}", e)))?;

    writeln!(writer, "{}", json)
        .map_err(|e| MantleError::Hub(format!("Failed to write to hub socket: {}", e)))?;

    writer
        .flush()
        .map_err(|e| MantleError::Hub(format!("Failed to flush hub socket: {}", e)))?;

    // Read response
    let mut reader = BufReader::new(stream);
    let mut response = String::new();
    reader
        .read_line(&mut response)
        .map_err(|e| MantleError::Hub(format!("Failed to read hub response: {}", e)))?;

    // Parse response and check status
    if let Ok(resp) = serde_json::from_str::<serde_json::Value>(&response) {
        if resp.get("status").and_then(|s| s.as_str()) == Some("error") {
            let msg = resp
                .get("message")
                .and_then(|m| m.as_str())
                .unwrap_or("Unknown error");
            return Err(MantleError::Hub(format!("Hub rejected result: {}", msg)));
        }
    }

    Ok(())
}

/// Convert a RunResult to SessionResult for hub submission.
pub fn run_result_to_session_result(
    run_result: &crate::events::RunResult,
    session_id: &str,
) -> SessionResult {
    SessionResult {
        session_id: session_id.to_string(),
        exit_code: run_result.exit_code,
        is_error: run_result.is_error,
        result_text: run_result.result.clone(),
        structured_output: run_result.structured_output.clone(),
        total_cost_usd: run_result.total_cost_usd,
        num_turns: run_result.num_turns,
        cc_session_id: run_result.session_id.clone(),
        duration_secs: 0.0, // Will be calculated by hub or caller
        model_usage: run_result
            .model_usage
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    super::types::ModelUsage {
                        input_tokens: v.input_tokens,
                        output_tokens: v.output_tokens,
                        cache_read_input_tokens: v.cache_read_input_tokens,
                        cache_creation_input_tokens: v.cache_creation_input_tokens,
                        cost_usd: v.cost_usd,
                    },
                )
            })
            .collect(),
    }
}
