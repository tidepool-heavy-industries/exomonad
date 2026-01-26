use clap::{Parser, Subcommand};
use serde::Serialize;
use std::process;

mod commands;
mod domain;

#[derive(Parser)]
#[command(name = "docker-ctl")]
#[command(about = "Tidepool Docker Control CLI", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Execute a command in a running container
    Exec {
        /// Container ID or name
        container: String,
        
        /// Working directory inside the container
        #[arg(long)]
        workdir: Option<String>,
        
        /// User to run the command as
        #[arg(long)]
        user: Option<String>,
        
        /// Environment variables (KEY=VALUE)
        #[arg(short = 'e', long)]
        env: Vec<String>,
        
        /// Command and arguments to execute
        #[arg(last = true)]
        cmd: Vec<String>,
    },
    
    /// Spawn a new container
    Spawn {
        /// Issue ID for the container
        #[arg(long)]
        issue_id: String,

        /// Path to the worktree to mount
        #[arg(long)]
        worktree_path: String,

        /// Backend to use (claude or gemini)
        #[arg(long)]
        backend: String,

        /// User ID for the container
        #[arg(long)]
        uid: Option<u32>,

        /// Group ID for the container
        #[arg(long)]
        gid: Option<u32>,

        /// Expiration time (ISO 8601)
        #[arg(long)]
        expires_at: Option<String>,

        /// Environment variables (KEY=VALUE)
        #[arg(short = 'e', long = "env")]
        env: Vec<String>,
    },
    
    /// Stop and remove a container
    Stop {
        /// Container ID or name
        container: String,
        
        /// Timeout in seconds
        #[arg(long, default_value = "10")]
        timeout: u64,
    },
    
    /// Get container status
    Status {
        /// Container ID or name
        container: String,
    },
}

#[derive(Serialize)]
struct ErrorResponse {
    error: String,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    
    let result = match cli.command {
        Commands::Exec { container, workdir, user, env, cmd } => {
            commands::exec::run(container, workdir, user, env, cmd).await
        }
        Commands::Spawn { issue_id, worktree_path, backend, uid, gid, expires_at, env } => {
            commands::spawn::run(issue_id, worktree_path, backend, uid, gid, expires_at, env).await
        }
        Commands::Stop { container, timeout } => {
            commands::stop::run(container, timeout).await
        }
        Commands::Status { container } => {
            commands::status::run(container).await
        }
    };
    
    match result {
        Ok(output) => {
            println!("{}", output);
        }
        Err(e) => {
            let err_json = serde_json::to_string(&ErrorResponse {
                error: e.to_string(),
            }).unwrap();
            eprintln!("{}", err_json);
            process::exit(1);
        }
    }
}
