//! HTTP-over-Unix-Domain-Socket client for communicating with exomonad server.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// Walk up from CWD to find `.exo/server.sock`. Follows symlinks.
pub fn find_server_socket() -> Result<PathBuf> {
    let start = std::env::current_dir()?;
    let mut current = start.as_path();
    loop {
        let sock = current.join(".exo/server.sock");
        if sock.exists() {
            return Ok(sock);
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => {
                return Err(anyhow::anyhow!(
                    "No .exo/server.sock found (walked up from {}). Is exomonad serve running?",
                    start.display()
                ));
            }
        }
    }
}

/// HTTP POST over Unix Domain Socket.
///
/// Sends a POST request to the given path on the UDS server.
/// Returns (status_code, response_body_bytes).
pub async fn uds_post(
    socket: &Path,
    path: &str,
    headers: Vec<(&str, &str)>,
    body: Vec<u8>,
) -> Result<(u16, Vec<u8>)> {
    use http_body_util::{BodyExt, Full};
    use hyper::Request;
    use hyper_util::rt::TokioIo;
    use tokio::net::UnixStream;

    let stream = UnixStream::connect(socket)
        .await
        .with_context(|| format!("Failed to connect to socket: {}", socket.display()))?;
    let io = TokioIo::new(stream);

    let (mut sender, conn) = hyper::client::conn::http1::handshake(io)
        .await
        .context("HTTP/1.1 handshake failed")?;

    tokio::spawn(async move {
        if let Err(e) = conn.await {
            tracing::debug!("UDS connection closed: {}", e);
        }
    });

    let mut req_builder = Request::builder()
        .method("POST")
        .uri(path)
        .header("host", "localhost");

    for (key, value) in headers {
        req_builder = req_builder.header(key, value);
    }

    let req = req_builder
        .body(Full::new(hyper::body::Bytes::from(body)))
        .context("Failed to build request")?;

    let resp = sender.send_request(req).await.context("Request failed")?;
    let status = resp.status().as_u16();
    let body_bytes = resp
        .into_body()
        .collect()
        .await
        .context("Failed to read response body")?
        .to_bytes()
        .to_vec();

    Ok((status, body_bytes))
}

/// HTTP GET over Unix Domain Socket.
pub async fn uds_get(socket: &Path, path: &str) -> Result<(u16, Vec<u8>)> {
    use http_body_util::{BodyExt, Empty};
    use hyper::Request;
    use hyper_util::rt::TokioIo;
    use tokio::net::UnixStream;

    let stream = UnixStream::connect(socket)
        .await
        .with_context(|| format!("Failed to connect to socket: {}", socket.display()))?;
    let io = TokioIo::new(stream);

    let (mut sender, conn) = hyper::client::conn::http1::handshake(io)
        .await
        .context("HTTP/1.1 handshake failed")?;

    tokio::spawn(async move {
        if let Err(e) = conn.await {
            tracing::debug!("UDS connection closed: {}", e);
        }
    });

    let req = Request::builder()
        .method("GET")
        .uri(path)
        .header("host", "localhost")
        .body(Empty::<hyper::body::Bytes>::new())
        .context("Failed to build request")?;

    let resp = sender.send_request(req).await.context("Request failed")?;
    let status = resp.status().as_u16();
    let body_bytes = resp
        .into_body()
        .collect()
        .await
        .context("Failed to read response body")?
        .to_bytes()
        .to_vec();

    Ok((status, body_bytes))
}
