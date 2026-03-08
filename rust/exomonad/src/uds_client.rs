use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use tokio::net::UnixStream;
use tokio::io::{AsyncReadExt, AsyncWriteExt};

/// Find the server socket by walking up from the current directory.
pub fn find_server_socket() -> Result<PathBuf> {
    let mut curr = std::env::current_dir()?;
    loop {
        let candidate = curr.join(".exo/server.sock");
        if candidate.exists() {
            return Ok(candidate);
        }
        if !curr.pop() {
            break;
        }
    }
    anyhow::bail!("Server socket not found (.exo/server.sock)")
}

/// Perform an HTTP POST request over a Unix domain socket.
pub async fn uds_post(
    socket_path: &Path,
    path: &str,
    headers: Vec<(&str, &str)>,
    body: Vec<u8>,
) -> Result<(u16, Vec<u8>)> {
    let mut stream = UnixStream::connect(socket_path)
        .await
        .with_context(|| format!("Failed to connect to UDS socket at {}", socket_path.display()))?;

    let mut request = format!("POST {} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n", path);
    for (k, v) in headers {
        request.push_str(&format!("{}: {}\r\n", k, v));
    }
    request.push_str(&format!("Content-Length: {}\r\n\r\n", body.len()));

    stream.write_all(request.as_bytes()).await?;
    stream.write_all(&body).await?;
    stream.flush().await?;

    let mut response = Vec::new();
    stream.read_to_end(&mut response).await?;

    // Minimal HTTP response parsing
    let resp_str = String::from_utf8_lossy(&response);
    
    // Split into status line, headers, and body
    let mut parts = resp_str.splitn(2, "\r\n\r\n");
    let header_part = parts.next().context("Empty response")?;
    let _body_part = parts.next().unwrap_or("");

    let status_line = header_part.lines().next().context("Invalid HTTP response")?;
    let status = status_line
        .split_whitespace()
        .nth(1)
        .context("Invalid status line")?
        .parse::<u16>()?;

    // We need the raw body bytes, not the lossy string version
    let body_start = response.windows(4)
        .position(|w| w == b"\r\n\r\n")
        .map(|p| p + 4)
        .unwrap_or(response.len());
    let body_bytes = response[body_start..].to_vec();

    Ok((status, body_bytes))
}
