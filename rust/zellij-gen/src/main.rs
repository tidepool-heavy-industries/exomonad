//! Generate Zellij layouts for ExoMonad agents.
//!
//! This binary generates KDL layout files with commands baked in as literals,
//! solving the problem where environment variables don't propagate to pane
//! processes spawned by Zellij layouts.
//!
//! ## Usage
//!
//! ```bash
//! # Generate main layout (TL/PM/Infrastructure tabs)
//! zellij-gen main
//!
//! # Generate subagent layout
//! zellij-gen subagent 346 exomonad-agent-346
//! ```

use askama::Template;
use clap::{Parser, Subcommand};
use std::env;
use std::fs;
use std::path::PathBuf;

const OUTPUT_DIR: &str = "/tmp/exomonad-layouts";
const DEFAULT_SOCKET: &str = "/sockets/control.sock";

#[derive(Parser)]
#[command(name = "zellij-gen", about = "Generate Zellij layouts for ExoMonad")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate main layout (TL/PM/Infrastructure tabs)
    Main,
    /// Generate subagent layout for a spawned agent
    Subagent {
        /// Issue ID (e.g., "346")
        issue_id: String,
        /// Container ID (e.g., "exomonad-agent-346")
        container_id: String,
    },
}

/// Template for an agent tab with interaction pane and status pane.
#[derive(Template)]
#[template(path = "agent_tab.kdl.j2", escape = "none")]
struct AgentTab {
    tab_name: String,
    pane_name: String,
    command: String,
    socket: String,
    focus: bool,
}

/// Template for the main layout with multiple agent tabs.
#[derive(Template)]
#[template(path = "main.kdl.j2", escape = "none")]
struct MainLayout {
    agent_tabs: Vec<String>,
}

/// Template for a single subagent layout.
#[derive(Template)]
#[template(path = "subagent.kdl.j2", escape = "none")]
struct SubagentLayout {
    agent_tab: String,
}

/// Get the control socket path from environment or use default.
fn get_socket() -> String {
    env::var("EXOMONAD_CONTROL_SOCKET").unwrap_or_else(|_| DEFAULT_SOCKET.to_string())
}

/// Generate the docker attach command with reconnect loop.
fn docker_attach_cmd(container: &str) -> String {
    // Extract a friendly name for the disconnect message
    let friendly_name = container
        .strip_prefix("exomonad-")
        .unwrap_or(container);

    format!(
        "while true; do docker attach --detach-keys 'ctrl-],]' {}; echo '[{} disconnected - reconnecting in 2s]'; sleep 2; done",
        container,
        friendly_name
    )
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    // Ensure output directory exists
    fs::create_dir_all(OUTPUT_DIR)?;

    let (content, filename) = match cli.command {
        Commands::Main => {
            let socket = get_socket();

            // Generate TL tab
            let tl = AgentTab {
                tab_name: "TL".into(),
                pane_name: "TL".into(),
                command: docker_attach_cmd("exomonad-tl"),
                socket: socket.clone(),
                focus: true,
            }
            .render()?;

            // Generate PM tab
            let pm = AgentTab {
                tab_name: "PM".into(),
                pane_name: "PM".into(),
                command: docker_attach_cmd("exomonad-pm"),
                socket: socket.clone(),
                focus: false,
            }
            .render()?;

            // Combine into main layout
            let layout = MainLayout {
                agent_tabs: vec![tl, pm],
            }
            .render()?;

            (layout, "main.kdl".to_string())
        }
        Commands::Subagent {
            issue_id,
            container_id,
        } => {
            let socket = get_socket();

            // Generate single agent tab
            let tab = AgentTab {
                tab_name: issue_id.clone(),
                pane_name: "Interaction".into(),
                command: docker_attach_cmd(&container_id),
                socket,
                focus: true,
            }
            .render()?;

            // Wrap in subagent layout
            let layout = SubagentLayout { agent_tab: tab }.render()?;

            (layout, format!("{}.kdl", issue_id))
        }
    };

    // Write to output file
    let path: PathBuf = [OUTPUT_DIR, &filename].iter().collect();
    fs::write(&path, content)?;

    // Print path to stdout for callers to capture
    println!("{}", path.display());

    Ok(())
}
