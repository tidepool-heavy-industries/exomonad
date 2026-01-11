//! mantle-hub: Session visualization hub for mantle.
//!
//! Long-running daemon that:
//! - Accepts session/node registrations and results via HTTP/Unix socket
//! - Persists session and node data to SQLite
//! - Serves a multi-page web UI for session browsing
//! - Broadcasts live updates via WebSocket
//!
//! ## Entity Model
//!
//! - **Session**: An orchestration run (tree of nodes)
//! - **Node**: An individual Claude Code execution
//!
//! ## Usage
//!
//! ```bash
//! # Start the hub server
//! mantle-hub serve
//!
//! # List all sessions
//! mantle-hub list
//!
//! # Create a new session (creates session + root node)
//! mantle-hub create --branch feature/test --prompt "Test"
//!
//! # Get session details
//! mantle-hub get <session-id>
//! ```

mod db;
mod error;
mod routes;
mod socket;
mod state;
mod types;

use clap::{Parser, Subcommand};
use std::net::SocketAddr;
use std::path::PathBuf;
use tracing::info;
use tracing_subscriber::EnvFilter;

use crate::state::AppState;
use crate::types::NodeResult;

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(name = "mantle-hub")]
#[command(about = "Session visualization hub for mantle")]
struct Cli {
    /// Database path (default: ~/.tidepool/hub.db)
    #[arg(long, global = true)]
    db: Option<PathBuf>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Start the hub server (default)
    Serve {
        /// HTTP server port
        #[arg(long, default_value = "7433")]
        port: u16,

        /// Unix socket path for container communication
        #[arg(long, default_value = "/tmp/mantle.sock")]
        socket: PathBuf,

        /// Static files directory (for development)
        #[arg(long)]
        static_dir: Option<PathBuf>,
    },

    /// List all sessions
    List {
        /// Output format (table, json)
        #[arg(long, default_value = "table")]
        format: String,
    },

    /// Create a new session with its root node
    Create {
        /// Branch name
        #[arg(long)]
        branch: String,

        /// Worktree path
        #[arg(long, default_value = ".")]
        worktree: PathBuf,

        /// Prompt text
        #[arg(long)]
        prompt: String,

        /// Model name
        #[arg(long, default_value = "sonnet")]
        model: String,
    },

    /// Submit a result for a node
    Submit {
        /// Node ID
        #[arg(long)]
        node_id: String,

        /// Exit code
        #[arg(long, default_value = "0")]
        exit_code: i32,

        /// Mark as error
        #[arg(long)]
        error: bool,

        /// Result text
        #[arg(long)]
        result_text: Option<String>,

        /// Total cost in USD
        #[arg(long, default_value = "0.0")]
        cost: f64,
    },

    /// Get a session by ID
    Get {
        /// Session ID
        session_id: String,

        /// Output format (table, json)
        #[arg(long, default_value = "table")]
        format: String,
    },

    /// Delete a session
    Delete {
        /// Session ID
        session_id: String,
    },
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| EnvFilter::new("mantle_hub=info,tower_http=debug")),
        )
        .init();

    let cli = Cli::parse();

    // Resolve database path
    let db_path = cli.db.unwrap_or_else(|| {
        let config_dir = directories::ProjectDirs::from("", "", "tidepool")
            .map(|d| d.data_dir().to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));
        std::fs::create_dir_all(&config_dir).ok();
        config_dir.join("hub.db")
    });

    match cli.command.unwrap_or(Commands::Serve {
        port: 7433,
        socket: PathBuf::from("/tmp/mantle.sock"),
        static_dir: None,
    }) {
        Commands::Serve {
            port,
            socket,
            static_dir,
        } => run_server(db_path, port, socket, static_dir).await,
        Commands::List { format } => run_list(db_path, &format).await,
        Commands::Create {
            branch,
            worktree,
            prompt,
            model,
        } => run_create(db_path, branch, worktree, prompt, model).await,
        Commands::Submit {
            node_id,
            exit_code,
            error,
            result_text,
            cost,
        } => run_submit(db_path, node_id, exit_code, error, result_text, cost).await,
        Commands::Get { session_id, format } => run_get(db_path, &session_id, &format).await,
        Commands::Delete { session_id } => run_delete(db_path, &session_id).await,
    }
}

// ============================================================================
// Command Implementations
// ============================================================================

async fn run_server(
    db_path: PathBuf,
    port: u16,
    socket: PathBuf,
    static_dir: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    let static_dir = static_dir.unwrap_or_else(|| {
        let exe_dir = std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|p| p.to_path_buf()))
            .unwrap_or_else(|| PathBuf::from("."));

        if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
            return PathBuf::from(manifest_dir).join("static");
        }

        exe_dir.join("static")
    });

    info!(db = %db_path.display(), static_dir = %static_dir.display(), "Initializing mantle-hub");

    let state = AppState::new(&db_path).await?;
    let app = routes::router(state.clone(), &static_dir);

    // Start Unix socket listener in background
    let socket_state = state.clone();
    let socket_path = socket.clone();
    tokio::spawn(async move {
        if let Err(e) = socket::listen(socket_path, socket_state).await {
            tracing::error!(error = %e, "Unix socket listener failed");
        }
    });

    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    info!(addr = %addr, socket = %socket.display(), "Starting mantle-hub");

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

async fn run_list(db_path: PathBuf, format: &str) -> Result<(), Box<dyn std::error::Error>> {
    let state = AppState::new(&db_path).await?;
    let sessions = db::list_sessions(&state.pool).await?;

    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&sessions)?);
    } else {
        if sessions.is_empty() {
            println!("No sessions found.");
            return Ok(());
        }

        println!(
            "{:<36}  {:<20}  {:<10}  {:<6}",
            "ID", "NAME", "STATE", "NODES"
        );
        println!("{}", "-".repeat(76));
        for s in sessions {
            let name: String = s.name.chars().take(20).collect();
            println!(
                "{:<36}  {:<20}  {:<10}  {:<6}",
                s.id, name, s.state, s.node_count
            );
        }
    }

    Ok(())
}

async fn run_create(
    db_path: PathBuf,
    branch: String,
    worktree: PathBuf,
    prompt: String,
    model: String,
) -> Result<(), Box<dyn std::error::Error>> {
    let state = AppState::new(&db_path).await?;

    let (session_id, node_id) = db::create_session(
        &state.pool,
        &branch,
        &worktree.display().to_string(),
        &prompt,
        &model,
    )
    .await?;

    println!("session_id={}", session_id);
    println!("node_id={}", node_id);
    Ok(())
}

async fn run_submit(
    db_path: PathBuf,
    node_id: String,
    exit_code: i32,
    is_error: bool,
    result_text: Option<String>,
    cost: f64,
) -> Result<(), Box<dyn std::error::Error>> {
    let state = AppState::new(&db_path).await?;

    let result = NodeResult {
        node_id: node_id.clone(),
        exit_code,
        is_error,
        result_text,
        structured_output: None,
        total_cost_usd: cost,
        num_turns: 1,
        cc_session_id: format!("cli-{}", uuid::Uuid::new_v4()),
        duration_secs: 0.0,
        model_usage: std::collections::HashMap::new(),
    };

    db::insert_result(&state.pool, &result).await?;

    println!("Submitted result for node: {}", node_id);
    Ok(())
}

async fn run_get(
    db_path: PathBuf,
    session_id: &str,
    format: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let state = AppState::new(&db_path).await?;
    let session_with_nodes = db::get_session_with_nodes(&state.pool, session_id).await?;

    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&session_with_nodes)?);
    } else {
        let session = &session_with_nodes.session;
        println!("Session: {}", session.id);
        println!("  Name:      {}", session.name);
        println!("  State:     {}", session.state);
        println!("  Nodes:     {}", session.node_count);
        println!("  Created:   {}", session.created_at);
        println!();

        if !session_with_nodes.nodes.is_empty() {
            println!("Nodes:");
            for node in &session_with_nodes.nodes {
                let parent = node
                    .parent_node_id
                    .as_ref()
                    .map(|p| format!(" (parent: {})", &p[..8]))
                    .unwrap_or_else(|| " (root)".to_string());
                println!("  {} - {}{}", &node.id[..8], node.state, parent);
                println!("    Branch:  {}", node.branch);
                println!(
                    "    Prompt:  {}",
                    node.prompt.chars().take(50).collect::<String>()
                );
                if let Some(ref result) = node.result {
                    println!("    Cost:    ${:.4}", result.total_cost_usd);
                    println!("    Duration: {:.1}s", result.duration_secs);
                }
                println!();
            }
        }
    }

    Ok(())
}

async fn run_delete(
    db_path: PathBuf,
    session_id: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let state = AppState::new(&db_path).await?;
    db::delete_session(&state.pool, session_id).await?;
    println!("Deleted session: {}", session_id);
    Ok(())
}
