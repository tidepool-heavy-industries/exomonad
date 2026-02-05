use anyhow::Result;
use clap::{Parser, Subcommand};

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
    }
}
