//! Hook-RPC client — the short-lived `exomonad experimental hook` process.

use std::path::{Path, PathBuf};
use std::time::Duration;

use exo_caps::{HookRequest, HookVerdict, NodePapers};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::UnixStream;
use tokio::time::timeout;

use crate::error::{NodeError, NodeResult};

/// Connect to a node's hook socket, send one [`HookRequest`], return the [`HookVerdict`]. The
/// caller prints `verdict.stdout` verbatim and exits 0. On any failure the caller must **fail
/// open** — the sidecar being down means there are no tools to gate anyway.
///
/// Framing: write the request JSON, half-close the write side (EOF signals end-of-request to the
/// server), then read the response JSON to EOF.
pub async fn client_request(sock: &Path, req: &HookRequest) -> NodeResult<HookVerdict> {
    let verdict = timeout(Duration::from_secs(5), async {
        let mut stream = UnixStream::connect(sock).await?;
        let bytes = serde_json::to_vec(req)
            .map_err(|e| std::io::Error::other(format!("encode HookRequest: {e}")))?;
        stream.write_all(&bytes).await?;
        stream.shutdown().await?;

        let mut resp = Vec::new();
        stream.read_to_end(&mut resp).await?;
        let verdict: HookVerdict = serde_json::from_slice(&resp).map_err(|e| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("decode HookVerdict: {e}"),
            )
        })?;
        Ok::<HookVerdict, NodeError>(verdict)
    })
    .await
    .map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::TimedOut,
            format!("hook socket RPC timed out for {}", sock.display()),
        )
    })??;

    Ok(verdict)
}

/// Resolve this node's hook socket path from its papers + ambient run env, identically to how the
/// sidecar's server binds it (both go through [`exo_caps::paths::hook_sock`], so they always
/// agree). The pane comes from papers; the run-id and home from the env the parent set at spawn.
pub fn resolve_hook_sock(papers_path: &Path) -> NodeResult<PathBuf> {
    let bytes = std::fs::read(papers_path)?;
    let papers: NodePapers = serde_json::from_slice(&bytes).map_err(|e| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("parse papers {}: {e}", papers_path.display()),
        )
    })?;
    let run_id = std::env::var("EXOMONAD_SWARM_RUN_ID")
        .map_err(|_| NodeError::MissingContext("EXOMONAD_SWARM_RUN_ID"))?;
    let home = std::env::var("HOME").map_err(|_| NodeError::MissingContext("HOME"))?;
    Ok(exo_caps::paths::hook_sock(
        Path::new(&home),
        &run_id,
        &papers.pane,
    ))
}
