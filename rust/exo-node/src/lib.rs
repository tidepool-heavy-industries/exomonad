//! `exo-node` — the per-node sidecar (Wave 2).
//!
//! Assembles the real [`exo_runtime::Runtime`] (all caps) + a domain `D: Exomonad` (the domain's
//! tools / hooks / roles / system, monomorphized once by the binary via `run_node::<exo::ExoDomain>`)
//! into a running **two-loop sidecar**, one process per agent:
//!
//! ```text
//!   OUTBOUND (N1):  serve the role's Tools (from the injected roster) over a hand-written MCP/JSON-RPC
//!                   stdio server; send_message → Bus::deliver.
//!   INBOUND  (N2):  watch own ingestion inbox (cursor + notify-watch, N2b) → per entry,
//!                   last-hop dispatch (N2a) over the listen wake channel (N6) — no listener ⇒
//!                   the cursor pins and the entry queues.
//!   LISTEN   (N6):  serve the wake-channel socket; the agent's Monitor-armed `exo listen`
//!                   client attaches here (latest-wins) and its stdout wakes the agent.
//!   HOOK (N4):      `exo hook` → the role's pre_tool_use / session_start.
//! ```
//!
//! Convergence is on-disk (v2): a TL folds a finished child by merging its branch locally
//! (the `merge` tool). There is no GitHub poller / world-event layer.
//!
//! **Status: Wave-2 assembled.** [`bootstrap`] self-IDs from papers; the loop modules
//! (`outbound` N1, `dispatch` N2a, `inbound` N2b, `hook` N4) are implemented; [`run_node`]
//! wires the two stimuli as concurrent tokio tasks.

pub mod bootstrap;
pub mod dispatch;
pub mod error;
pub mod hook;
pub mod hooksock;
pub mod inbound;
pub mod listen;
pub mod outbound;
pub mod watchdog;

#[cfg(test)]
mod test_support;

pub use bootstrap::{bootstrap, NodeContext};
pub use error::{NodeError, NodeResult};
pub use hook::{handle as handle_hook, HookEvent};

use exo_caps::RoleKind;
use exo_framework::Exomonad;
use exo_runtime::Runtime;
use std::fs::{File, OpenOptions};
use std::os::fd::AsRawFd;
use std::path::Path;
use std::sync::Arc;

/// Run the node's concurrent stimuli in one process (outbound serve + inbound watch + hooksock):
/// - **outbound** ([`outbound::serve`]) — serve the role's MCP tools over stdio. This owns
///   stdin/stdout and returns when the stream closes (agent gone), so it is the node's
///   **lifetime anchor**: when it ends, the node ends.
/// - **inbound** ([`inbound::watch`]) — watch the ingestion inbox (cursor + notify) and route
///   each entry; ends on a `Control(Shutdown)`.
/// - **hooksock** ([`hooksock::serve`]) — background hook-RPC socket (N5); also aborted when serve returns.
/// - **watchdog** ([`watchdog::watch`]) — periodic wall-clock self-check (domain abandonment
///   timeouts via `Exomonad::handle_tick`, plus cooperative-shutdown reap retry); also aborted when
///   serve returns.
///
/// The background loops are aborted when `serve` returns. `Arc<NodeContext>` satisfies the
/// `R: Send + Sync + 'static` dispatch boundary. A background loop erroring is logged but does
/// not tear down the node — only the outbound anchor closing (or a shutdown) ends it.
pub async fn run_node<D: Exomonad<Caps = Runtime>>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
    // Codex creates its rollout concurrently with MCP startup. Discovery must therefore run
    // behind the stdio server: delaying or failing here prevents the initialize reply and causes
    // Codex to tear the MCP child down before its rollout can ever become discoverable.
    let binding_discovery = spawn_codex_binding_discovery(&ctx).await;
    let lock_path = exo_caps::paths::sidecar_owner_lock(&home(), &ctx.run_id, &ctx.own_pane);
    let owner_lock = try_acquire_sidecar_owner(&lock_path)?;
    let mut background = Vec::new();

    if owner_lock.is_some() {
        tracing::info!(path = %lock_path.display(), "this MCP connection owns node background loops");
        background.push(tokio::spawn({
            let ctx = ctx.clone();
            async move {
                if let Err(e) = inbound::watch(ctx).await {
                    tracing::error!("inbound loop exited with error: {e}");
                }
            }
        }));

        // N5 — per-agent hook-RPC socket. Background like inbound; an error is logged, not fatal.
        background.push(tokio::spawn({
            let ctx = ctx.clone();
            async move {
                if let Err(e) = hooksock::serve(ctx).await {
                    tracing::error!("hooksock loop exited with error: {e}");
                }
            }
        }));

        // N6 — the listen wake channel (the delivery last hop). Background like hooksock; an error
        // is logged, not fatal — with no server, dispatch errs and messages queue on the bus.
        background.push(tokio::spawn({
            let ctx = ctx.clone();
            async move {
                if let Err(e) = listen::serve(ctx).await {
                    tracing::error!("listen loop exited with error: {e}");
                }
            }
        }));

        // Watchdog — periodic wall-clock self-check (domain abandonment timeouts + cooperative-shutdown
        // reap retry). Replaces Stop-hook-triggered decisions, which can't tell "done" from "paused".
        background.push(tokio::spawn({
            let ctx = ctx.clone();
            async move {
                if let Err(e) = watchdog::watch(ctx).await {
                    tracing::error!("watchdog loop exited with error: {e}");
                }
            }
        }));

        // Periodic status publisher — writes the node's status snapshot to disk for visibility.
        background.push(tokio::spawn({
            let ctx = ctx.clone();
            async move {
                let status_path = exo_caps::paths::status_path(&home(), &ctx.run_id, &ctx.own_pane);
                // Ensure status directory exists
                if let Some(parent) = status_path.parent() {
                    let _ = std::fs::create_dir_all(parent);
                }

                let mut interval = tokio::time::interval(std::time::Duration::from_secs(5));
                loop {
                    interval.tick().await;
                    let shutdown_pending = ctx.shutdown_pending.lock().unwrap().is_some();
                    let mut snapshot = ctx
                        .runtime
                        .status_snapshot(ctx.kind.role_str(), shutdown_pending)
                        .await;
                    // Sidecar state the runtime can't see: is the wake-channel client attached?
                    snapshot.listener_connected = match ctx.runtime.agent_type() {
                        exo_caps::AgentType::Codex => {
                            if let Some(codex) = ctx.runtime.codex_node() {
                                exo_runtime::codex::read_binding(&codex.binding)
                                    .await
                                    .is_ok()
                            } else {
                                false
                            }
                        }
                        _ => ctx.listener.is_connected(),
                    };
                    if let Ok(bytes) = serde_json::to_vec(&snapshot) {
                        if let Err(e) =
                            exo_caps::Fs::write_atomic(&*ctx.runtime, &status_path, &bytes).await
                        {
                            tracing::error!("failed to write status snapshot: {e}");
                        }
                    }
                }
            }
        }));
    } else {
        tracing::info!(path = %lock_path.display(), "another MCP connection owns node background loops; serving outbound tools only");
    }

    // The outbound serve owns stdio and runs for the node's lifetime.
    let result = outbound::serve(ctx).await;

    // Agent stream closed (or serve errored) → reap the background loops.
    for task in &background {
        task.abort();
    }
    for task in background {
        let _ = task.await;
    }
    if let Some(task) = binding_discovery {
        task.abort();
        let _ = task.await;
    }

    // Keep the advisory lock alive until outbound closes and all owner tasks have been aborted.
    drop(owner_lock);

    result
}

async fn spawn_codex_binding_discovery<D: Exomonad<Caps = Runtime>>(
    ctx: &Arc<NodeContext<D>>,
) -> Option<tokio::task::JoinHandle<()>> {
    if ctx.runtime.agent_type() != exo_caps::AgentType::Codex {
        return None;
    }
    let codex = ctx.runtime.codex_node()?;
    if exo_runtime::codex::read_binding(&codex.binding)
        .await
        .is_ok_and(|binding| binding.v == exo_runtime::codex::CodexBinding::VERSION)
    {
        return None;
    }

    #[cfg(target_os = "linux")]
    {
        let parent_pid = unsafe { libc::getppid() } as u32;
        let cwd = ctx.runtime.working_dir().to_owned();
        let binding = codex.binding.clone();
        Some(tokio::spawn(async move {
            discover_and_bind_rollout(parent_pid, &cwd, &binding).await;
        }))
    }
    #[cfg(not(target_os = "linux"))]
    None
}

#[cfg(target_os = "linux")]
async fn discover_and_bind_rollout(parent_pid: u32, cwd: &Path, binding: &Path) {
    loop {
        match exo_runtime::codex::discover_parent_rollout(parent_pid, cwd) {
            Ok(thread_id) => match exo_runtime::codex::write_binding(binding, &thread_id).await {
                Ok(()) => {
                    tracing::info!(thread_id, "bound Exomonad node to local Codex rollout");
                    return;
                }
                Err(error) => {
                    tracing::warn!(%error, "failed to persist Codex rollout binding; retrying")
                }
            },
            Err(error) => tracing::debug!(%error, "Codex rollout not discoverable yet; retrying"),
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}

fn try_acquire_sidecar_owner(path: &Path) -> std::io::Result<Option<File>> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let file = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .open(path)?;
    // SAFETY: flock only inspects the valid file descriptor owned by `file`.
    let result = unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) };
    if result == 0 {
        return Ok(Some(file));
    }
    let error = std::io::Error::last_os_error();
    if error.kind() == std::io::ErrorKind::WouldBlock {
        Ok(None)
    } else {
        Err(error)
    }
}

fn home() -> std::path::PathBuf {
    std::env::var("HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| std::path::PathBuf::from("."))
}

#[cfg(test)]
mod owner_tests {
    use super::*;

    #[test]
    fn owner_lock_is_exclusive_and_reacquirable() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("node.owner.lock");
        let first = try_acquire_sidecar_owner(&path).unwrap();
        assert!(first.is_some());
        assert!(try_acquire_sidecar_owner(&path).unwrap().is_none());
        drop(first);
        assert!(try_acquire_sidecar_owner(&path).unwrap().is_some());
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn missing_rollout_remains_a_background_retry_not_a_startup_error() {
        let temp = tempfile::tempdir().unwrap();
        let binding = temp.path().join("binding.json");
        let task = tokio::spawn({
            let binding = binding.clone();
            async move {
                discover_and_bind_rollout(u32::MAX, Path::new("/not-yet-created"), &binding).await
            }
        });

        tokio::time::sleep(std::time::Duration::from_millis(25)).await;
        assert!(
            !task.is_finished(),
            "missing rollout must be retried, not returned as fatal"
        );
        assert!(!binding.exists());
        task.abort();
    }
}
