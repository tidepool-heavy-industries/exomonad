//! `exo` — the standalone v2 node-mode binary. The composition root: build the domain roster, hand
//! it to the engine, run the sidecar. Everything substantive lives in the framework (`exo-node` /
//! `exo-framework`) and the domain lib (`exo`); this `main` only wires them together and exposes the
//! node-mode CLI surface.
//!
//! ```text
//!   exo init [--session <s>] [--recreate]   # bootstrap a node-mode ROOT (own tmux session, no server)
//!   exo node --papers <path>                # run the node-mode sidecar for the node described by <path>
//!   exo hook <event> --papers <path>        # handle a CC/Gemini hook via the node's exo gates
//! ```

mod config;
mod hook;
mod init;

use std::sync::Arc;

use anyhow::Context;
use clap::{Parser, Subcommand};
use exomonad_shared::protocol::{HookEventType, Runtime as HookRuntime};

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

        /// The runtime environment (Claude or Gemini). Reserved/unused in this path.
        #[arg(long, default_value = "claude")]
        runtime: HookRuntime,

        /// Path to this node's birth papers (`node.json`).
        #[arg(long)]
        papers: std::path::PathBuf,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { session, recreate } => {
            let cfg = config::discover();
            init::run(
                &cfg.tmux_session,
                cfg.model.as_deref(),
                cfg.yolo,
                cfg.wrap_nix,
                session,
                recreate,
            )
            .await
        }

        Commands::Node { papers } => {
            let cwd = std::env::current_dir().context("resolving node cwd")?;
            // Inject the domain roster into the engine — the engine never names a concrete role.
            let ctx = exo_node::bootstrap(&papers, cwd, exo::roster())
                .map(Arc::new)
                .context("node self-ID / bootstrap")?;
            exo_node::run_node(ctx).await.context("node run")
        }

        Commands::Hook {
            event,
            runtime: _,
            papers,
        } => hook::run(event, papers).await,
    }
}
