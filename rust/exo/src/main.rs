//! `exo` — the standalone v2 node-mode binary. The composition root: build the domain roster, hand
//! it to the engine, run the sidecar. Everything substantive lives in the framework (`exo-node` /
//! `exo-framework`) and the domain lib (`exo`); this `main` only wires them together and exposes the
//! node-mode CLI surface.
//!
//! ```text
//!   exo init [--session <s>] [--recreate]   # bootstrap a node-mode ROOT (own tmux session, no server)
//!   exo node --papers <path>                # run the node-mode sidecar for the node described by <path>
//!   exo hook <event> --papers <path>        # handle a CC hook via the node's exo gates
//! ```

mod config;
mod doctor;
mod domain;
mod hook;
mod init;

use domain::ExoDomain;

use std::sync::Arc;

use anyhow::Context;
use clap::{Parser, Subcommand};
use exomonad_shared::protocol::{HookEventType, Runtime as HookRuntime};
use tracing_subscriber::prelude::*;

#[derive(Parser)]
#[command(name = "exo")]
#[command(about = "ExoMonad v2 node-mode: per-agent sidecar swarm (no central server)")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Bootstrap a node-mode ROOT: own tmux session, root papers, NO central server.
    Init {
        /// tmux session name (default: "{config.tmux_session}-exp").
        #[arg(long)]
        session: Option<String>,
        /// Tear down an existing session of the same name first.
        #[arg(long)]
        recreate: bool,
    },

    /// Run the swarm-sidecar node mode: self-ID from papers, then the two-loop
    /// sidecar (outbound MCP serve + inbound ingestion-inbox watch).
    Node {
        /// Path to this node's birth papers (`node.json`), written by the parent at spawn.
        #[arg(long)]
        papers: std::path::PathBuf,
    },

    /// Handle a hook via `exo` against a node's papers, with NO central server.
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// The agent runtime. Reserved/unused in this path (every node-mode agent is Claude).
        #[arg(long, default_value = "claude")]
        runtime: HookRuntime,

        /// Path to this node's birth papers (`node.json`).
        #[arg(long)]
        papers: std::path::PathBuf,
    },

    /// Health-check + cleanup: audit .exo/worktrees and reclaim merged ones.
    Doctor {
        /// Actually reclaim merged worktrees (dry-run by default).
        #[arg(long)]
        fix: bool,

        /// Also reclaim UNMERGED worktrees (dangerous, use with caution).
        #[arg(long)]
        include_unmerged: bool,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { session, recreate } => {
            let cfg = config::discover().context("loading exo config")?;
            init::run(
                &cfg.tmux_session,
                cfg.model.as_deref(),
                cfg.yolo,
                cfg.wrap_nix,
                cfg.review_enabled,
                &cfg.profile_env,
                session,
                recreate,
            )
            .await
        }

        Commands::Node { papers } => {
            let cwd = std::env::current_dir().context("resolving node cwd")?;
            // Monomorphize the engine once at the `exo` domain — the engine never names a concrete
            // role/system/spawn; it resolves everything through `ExoDomain`'s `Exomonad` impl.
            let ctx = exo_node::bootstrap::<ExoDomain>(&papers, cwd)
                .map(Arc::new)
                .context("node self-ID / bootstrap")?;

            // Wire persistent file logging for the sidecar.
            let _guard = init_logging(&ctx.run_id, ctx.runtime.branch().as_str())
                .context("initializing persistent logging")?;

            exo_node::run_node(ctx).await.context("node run")
        }

        Commands::Hook {
            event,
            runtime: _,
            papers,
        } => {
            // Hooks also benefit from logging if papers are available.
            let cwd = std::env::current_dir().context("resolving node cwd")?;
            if let Ok(ctx) = exo_node::bootstrap::<ExoDomain>(&papers, cwd) {
                let _guard = init_logging(&ctx.run_id, ctx.runtime.branch().as_str()).ok();
                hook::run(event, papers).await
            } else {
                hook::run(event, papers).await
            }
        }

        Commands::Doctor {
            fix,
            include_unmerged,
        } => doctor::run(fix, include_unmerged).await,
    }
}

fn get_project_root() -> anyhow::Result<std::path::PathBuf> {
    let output = std::process::Command::new("git")
        .args(["rev-parse", "--git-common-dir"])
        .output()
        .context("running git rev-parse --git-common-dir")?;

    if !output.status.success() {
        anyhow::bail!(
            "git rev-parse failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let git_dir = std::path::PathBuf::from(String::from_utf8_lossy(&output.stdout).trim());
    git_dir
        .parent()
        .map(|p| p.to_path_buf())
        .context("resolving project root from git dir")
}

fn init_logging(
    run_id: &str,
    node_id: &str,
) -> anyhow::Result<tracing_appender::non_blocking::WorkerGuard> {
    let project_root = get_project_root()?;
    let log_dir = project_root.join(".exo/logs/sidecar").join(run_id);
    std::fs::create_dir_all(&log_dir).context("creating log directory")?;

    let file_appender = tracing_appender::rolling::never(log_dir, format!("{}.log", node_id));
    let (non_blocking, guard) = tracing_appender::non_blocking(file_appender);

    let filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info"));

    tracing_subscriber::registry()
        .with(filter)
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(non_blocking)
                .with_ansi(false),
        )
        .init();

    Ok(guard)
}
