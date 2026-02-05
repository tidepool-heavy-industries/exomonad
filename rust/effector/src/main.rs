use anyhow::Result;
use clap::{Parser, Subcommand};

mod agent;
mod cabal;
mod git;
mod types;

#[derive(Parser)]
#[command(name = "effector")]
#[command(about = "Stateless IO executor for ExoMonad agents")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Cabal {
        #[command(subcommand)]
        action: CabalAction,
    },
    Git {
        #[command(subcommand)]
        action: GitAction,
    },
    Agent {
        #[command(subcommand)]
        action: AgentAction,
    },
}

#[derive(Subcommand)]
enum CabalAction {
    Build {
        #[arg(long, default_value = ".")]
        cwd: String,
        #[arg(long)]
        package: Option<String>,
    },
    Test {
        #[arg(long, default_value = ".")]
        cwd: String,
        #[arg(long)]
        package: Option<String>,
    },
}

#[derive(Subcommand)]
enum GitAction {
    Status {
        #[arg(long, default_value = ".")]
        cwd: String,
    },
    Diff {
        #[arg(long, default_value = ".")]
        cwd: String,
        #[arg(long)]
        staged: bool,
    },
    LsFiles {
        #[arg(long, default_value = ".")]
        cwd: String,
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
}

#[derive(Subcommand)]
enum AgentAction {
    /// All-in-one agent workspace setup (worktree + config + symlinks + hooks).
    Setup {
        #[arg(long)]
        project_dir: String,
        #[arg(long)]
        worktree_path: String,
        #[arg(long)]
        branch: String,
        #[arg(long, default_value = "origin/main")]
        start_point: String,
        #[arg(long, default_value = "dev")]
        role: String,
        #[arg(long, default_value = "claude")]
        agent_type: String,
        #[arg(long, default_value = "exomonad-sidecar")]
        sidecar_path: String,
    },
    /// Remove agent worktree and prune references.
    Teardown {
        #[arg(long)]
        project_dir: String,
        #[arg(long)]
        worktree_path: String,
        #[arg(long)]
        force: bool,
    },
    /// Pre-spawn git fetch origin main.
    FetchOrigin {
        #[arg(long)]
        project_dir: String,
    },
    /// List agent worktrees as structured JSON.
    List {
        #[arg(long)]
        project_dir: String,
    },
    /// Check if a branch has been merged into origin/main.
    IsMerged {
        #[arg(long)]
        project_dir: String,
        #[arg(long)]
        branch: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Cabal { action } => match action {
            CabalAction::Build { cwd, package } => cabal::build(&cwd, package),
            CabalAction::Test { cwd, package } => cabal::test(&cwd, package),
        },
        Commands::Git { action } => match action {
            GitAction::Status { cwd } => git::status(&cwd),
            GitAction::Diff { cwd, staged } => git::diff(&cwd, staged),
            GitAction::LsFiles { cwd, args } => git::ls_files(&cwd, args),
        },
        Commands::Agent { action } => match action {
            AgentAction::Setup {
                project_dir,
                worktree_path,
                branch,
                start_point,
                role,
                agent_type,
                sidecar_path,
            } => agent::setup(
                &project_dir,
                &worktree_path,
                &branch,
                &start_point,
                &role,
                &agent_type,
                &sidecar_path,
            ),
            AgentAction::Teardown {
                project_dir,
                worktree_path,
                force,
            } => agent::teardown(&project_dir, &worktree_path, force),
            AgentAction::FetchOrigin { project_dir } => agent::fetch_origin(&project_dir),
            AgentAction::List { project_dir } => agent::list(&project_dir),
            AgentAction::IsMerged {
                project_dir,
                branch,
            } => agent::is_merged(&project_dir, &branch),
        },
    }
}
