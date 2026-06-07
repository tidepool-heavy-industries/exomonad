//! `exo hook <event> --papers <path>` — handle a CC/Gemini hook via the node's `exo-policy` gates
//! against its papers, with NO central server.
//!
//! SessionStart runs one-shot in-process (it must survive a cold-start race before the sidecar
//! socket is listening); every other event routes to the sidecar over its per-agent hook socket and
//! **fails open** on any error — an unreachable sidecar has no tools to gate, so never wedge the
//! agent.

use anyhow::{Context, Result};
use exomonad_shared::protocol::HookEventType;
use std::path::Path;

/// The allow-shaped hook stdout for a node, by its agent type. Used to fail open when the sidecar
/// socket is unreachable. Defaults to the Claude allow if papers can't be read (exit 0 is allow
/// for both harnesses anyway, so this only affects the printed JSON).
fn fail_open_shape(papers_path: &Path) -> &'static str {
    let agent_type = std::fs::read(papers_path)
        .ok()
        .and_then(|b| serde_json::from_slice::<exo_caps::NodePapers>(&b).ok())
        .map(|p| p.role.agent_type());
    match agent_type {
        Some(exo_caps::AgentType::Gemini) => "{}",
        _ => r#"{"continue":true}"#,
    }
}

pub async fn run(event: HookEventType, papers: std::path::PathBuf) -> Result<()> {
    use std::io::Read;
    let mut body = String::new();
    std::io::stdin().read_to_string(&mut body)?;

    // SessionStart stays one-shot in-process: it needs no live state and must survive
    // a cold-start race before the sidecar socket is listening.
    if event == HookEventType::SessionStart {
        let verdict = exo_node::handle_hook(
            exo_node::HookEvent::SessionStart,
            &papers,
            &body,
            exo::roster(),
        )
        .await
        .context("node session-start hook")?;
        println!("{verdict}");
        return Ok(());
    }

    // All other hooks route to the sidecar over its per-agent socket.
    let hook_event = match event {
        HookEventType::PreToolUse => exo_caps::HookEvent::PreToolUse,
        HookEventType::Stop => exo_caps::HookEvent::Stop,
        other => {
            eprintln!("[exo] node hook: unhandled event {other:?}, passing through");
            print!("{}", fail_open_shape(&papers));
            return Ok(());
        }
    };

    let req = exo_caps::HookRequest {
        event: hook_event,
        stdin_json: body,
    };

    // Fail-open on ANY error: if the sidecar is unreachable there are no tools to
    // gate, so never wedge the agent. Shape the allow per agent_type.
    match exo_node::hooksock::client::resolve_hook_sock(&papers) {
        Ok((sock, node)) => {
            match exo_node::hooksock::client::client_request(&node, &sock, &req).await {
                Ok(verdict) => print!("{}", verdict.stdout),
                Err(e) => {
                    eprintln!("[exo] node hook: socket RPC failed ({e}); failing open");
                    print!("{}", fail_open_shape(&papers));
                }
            }
        }
        Err(e) => {
            eprintln!("[exo] node hook: cannot resolve hook socket ({e}); failing open");
            print!("{}", fail_open_shape(&papers));
        }
    }
    Ok(())
}
