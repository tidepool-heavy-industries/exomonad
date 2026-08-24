//! Listen-channel client — the long-lived `exo listen` process the agent arms under Claude
//! Code's `Monitor` tool. Its stdout IS the wake channel: each frame it prints becomes a
//! harness notification in the agent's session.

use std::path::{Path, PathBuf};
use std::time::Duration;

use exo_caps::NodePapers;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::UnixStream;

use super::{ListenAck, ListenFrame};
use crate::error::{NodeError, NodeResult};

/// Total budget for the initial connect: arming races the sidecar's socket bind at cold start
/// (the same race `exo hook session-start` tolerates), and the sidecar only exists once the
/// agent's harness has started it as the MCP server — 30s comfortably covers boot. Beyond that
/// the sidecar is genuinely absent and the agent should see the watch end loudly.
const CONNECT_BUDGET: Duration = Duration::from_secs(30);

/// A frame line longer than this is a protocol violation, not a message — bodies are ≤4 KiB by
/// construction (`MessageBody`), and even an `@`-ref render is a fraction of this.
const MAX_FRAME_LINE_BYTES: usize = 64 * 1024;

/// Resolve this node's listen socket path and name from its papers + ambient run env, identically
/// to how the sidecar's server binds it — both go through [`exo_caps::paths::listen_sock`], so
/// they always agree. Mirrors `hooksock::client::resolve_hook_sock`.
pub fn resolve_listen_sock(papers_path: &Path) -> NodeResult<(PathBuf, String)> {
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
    let sock = exo_caps::paths::listen_sock(Path::new(&home), &run_id, &papers.pane);
    let name = papers.path.name().as_str().to_string();
    Ok((sock, name))
}

/// The `exo listen` main loop: connect (bounded retry), then per frame — write its text to
/// stdout, flush, **then** ack — so an ack strictly implies bytes-on-stdout, which is what lets
/// the sidecar advance its inbound cursor. One buffered write per frame keeps a multi-line
/// message inside one Monitor notification batch.
///
/// Exit semantics (a Monitor watch ends when this process exits, and the harness reports that
/// to the agent):
/// - **Bad papers / env / unreachable socket** → diagnostic line on stdout (so the notification
///   shows it) and `Err` (non-zero exit). No endless retry: a wrong path retried forever would
///   silently convince the agent it is armed while messages queue.
/// - **EOF from the sidecar** → clean `Ok` exit: either the sidecar died (PDEATHSIG means the
///   agent is dying too) or this client was replaced latest-wins by a newer `exo listen`.
///   Reconnecting would thrash the slot against a successor, so the agent re-arms instead.
/// - **Protocol violation** (unparseable/oversized frame) → diagnostic + `Err`.
pub async fn run(papers_path: &Path) -> NodeResult<()> {
    let (sock, node) = resolve_listen_sock(papers_path).inspect_err(|e| {
        announce(&format!(
            "exo listen: cannot resolve listen socket ({e}) — NOT armed; messages queue until a listener attaches"
        ));
    })?;

    let stream = connect_with_retry(&sock).await.inspect_err(|e| {
        announce(&format!(
            "exo listen [{node}]: sidecar socket unreachable after {CONNECT_BUDGET:?} ({e}) — NOT armed; messages queue until a listener attaches"
        ));
    })?;

    let (read_half, mut write_half) = stream.into_split();
    let mut lines = BufReader::new(read_half).lines();
    let mut stdout = tokio::io::stdout();

    while let Some(line) = lines.next_line().await? {
        if line.len() > MAX_FRAME_LINE_BYTES {
            announce(&format!(
                "exo listen [{node}]: oversized frame ({} bytes) — protocol violation, exiting; re-arm to resume",
                line.len()
            ));
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "oversized listen frame",
            )
            .into());
        }
        let frame: ListenFrame = serde_json::from_str(&line).map_err(|e| {
            announce(&format!(
                "exo listen [{node}]: unparseable frame ({e}) — protocol violation, exiting; re-arm to resume"
            ));
            std::io::Error::new(std::io::ErrorKind::InvalidData, format!("bad frame: {e}"))
        })?;

        let mut out = frame.text.into_bytes();
        if out.last() != Some(&b'\n') {
            out.push(b'\n');
        }
        stdout.write_all(&out).await?;
        stdout.flush().await?;

        let mut ack = serde_json::to_vec(&ListenAck { seq: frame.seq })
            .map_err(|e| std::io::Error::other(format!("encode ack: {e}")))?;
        ack.push(b'\n');
        write_half.write_all(&ack).await?;
        write_half.flush().await?;
    }

    announce(&format!(
        "exo listen [{node}]: channel closed (sidecar gone or listener replaced) — re-arm to resume delivery; messages queue meanwhile"
    ));
    Ok(())
}

/// Print a client-status line to BOTH stdout (so the Monitor notification carries it to the
/// agent) and stderr (so it lands in any captured diagnostics).
fn announce(msg: &str) {
    println!("{msg}");
    eprintln!("{msg}");
}

async fn connect_with_retry(sock: &Path) -> NodeResult<UnixStream> {
    let deadline = tokio::time::Instant::now() + CONNECT_BUDGET;
    let mut delay = Duration::from_millis(200);
    loop {
        match UnixStream::connect(sock).await {
            Ok(s) => return Ok(s),
            Err(e) => {
                if tokio::time::Instant::now() + delay >= deadline {
                    return Err(e.into());
                }
                tokio::time::sleep(delay).await;
                delay = (delay * 2).min(Duration::from_secs(1));
            }
        }
    }
}
