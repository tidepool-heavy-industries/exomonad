//! `exo hook <event> --papers <path>` — handle a CC hook via the node's `exo` gates
//! against its papers, with NO central server.
//!
//! SessionStart runs one-shot in-process (it must survive a cold-start race before the sidecar
//! socket is listening); every other event routes to the sidecar over its per-agent hook socket and
//! **fails open** on any error — an unreachable sidecar has no tools to gate, so never wedge the
//! agent.

use anyhow::{Context, Result};
use exomonad_shared::protocol::HookEventType;
use std::path::Path;

/// The allow-shaped hook stdout. Used to fail open when the sidecar socket is unreachable. Every
/// tree node is a Claude instance, so the Claude allow shape is universal.
fn fail_open_shape(_papers_path: &Path) -> &'static str {
    r#"{"continue":true}"#
}

pub async fn run(event: HookEventType, papers: std::path::PathBuf) -> Result<()> {
    use std::io::Read;
    let mut body = String::new();
    std::io::stdin().read_to_string(&mut body)?;

    // SessionStart stays one-shot in-process: it needs no live state and must survive
    // a cold-start race before the sidecar socket is listening.
    if event == HookEventType::SessionStart {
        let verdict = exo_node::handle_hook::<crate::domain::ExoDomain>(
            exo_node::HookEvent::SessionStart,
            &papers,
            &body,
        )
        .await
        .context("node session-start hook")?;
        println!("{verdict}");
        return Ok(());
    }

    // All other hooks route to the sidecar over its per-agent socket. `Stop` is deliberately
    // unhandled: a node's settings never register it anymore (see `exo-runtime::node_config`), so
    // this arm only exists for a stale settings file — fail open, same as any other unhandled event.
    let hook_event = match event {
        HookEventType::PreToolUse => exo_caps::HookEvent::PreToolUse,
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
