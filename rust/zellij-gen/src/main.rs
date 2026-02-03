//! CLI for generating Zellij layouts.
//!
//! ## Usage
//!
//! ```bash
//! # Generate subagent layout for a spawned agent
//! zellij-gen subagent --tab-name "🤖 123-test" --command "claude --prompt 'test'" --cwd /tmp/test
//! ```

use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;
use zellij_gen::{generate_agent_layout, AgentTabParams};

const OUTPUT_DIR: &str = "/tmp/exomonad-layouts";

#[derive(Parser)]
#[command(name = "zellij-gen", about = "Generate Zellij layouts for ExoMonad")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate subagent layout for a spawned agent
    Subagent {
        /// Tab name (e.g., "🤖 123-test")
        #[arg(long)]
        tab_name: String,

        /// Command to run (e.g., "claude --prompt 'test'")
        #[arg(long)]
        command: String,

        /// Working directory
        #[arg(long)]
        cwd: PathBuf,

        /// Shell to use (default: $SHELL or /bin/zsh)
        #[arg(long)]
        shell: Option<String>,

        /// Output file path (default: /tmp/exomonad-layouts/<tab_name>.kdl)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    // Ensure output directory exists
    fs::create_dir_all(OUTPUT_DIR)?;

    match cli.command {
        Commands::Subagent {
            tab_name,
            command,
            cwd,
            shell,
            output,
        } => {
            let shell = shell
                .or_else(|| std::env::var("SHELL").ok())
                .unwrap_or_else(|| "/bin/zsh".to_string());

            let params = AgentTabParams {
                tab_name: &tab_name,
                pane_name: "Agent",
                command: &command,
                cwd: &cwd,
                shell: &shell,
                focus: true,
            };

            let layout = generate_agent_layout(&params)?;

            // Determine output path
            let output_path = output.unwrap_or_else(|| {
                // Sanitize tab name for filename (ASCII-safe, consistent with agent_control.rs)
                let safe_name: String = tab_name
                    .chars()
                    .map(|c| if c.is_ascii_alphanumeric() || c == '-' || c == '_' { c } else { '-' })
                    .collect();
                PathBuf::from(OUTPUT_DIR).join(format!("{}.kdl", safe_name))
            });

            fs::write(&output_path, layout)?;
            println!("{}", output_path.display());
        }
    }

    Ok(())
}
