use clap::{Parser, Subcommand};
use anyhow::Result;

mod types;
mod cabal;
mod git;
mod gh;

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
    Gh {
        #[command(subcommand)]
        action: GhAction,
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
enum GhAction {
    PrStatus {
        #[arg(long)]
        branch: Option<String>,
    },
    PrCreate {
        #[arg(long)]
        title: String,
        #[arg(long)]
        body: String,
        #[arg(long)]
        base: Option<String>,
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
        Commands::Gh { action } => match action {
            GhAction::PrStatus { branch } => gh::pr_status(branch),
            GhAction::PrCreate { title, body, base } => gh::pr_create(title, body, base),
        },
    }
}
