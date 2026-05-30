//! exo-scry CLI — thin clap shell over the library.

use clap::{Args, Parser, Subcommand};
#[allow(unused_imports)]
use exo_scry::ProbeTarget;

#[derive(Parser)]
#[command(
    name = "exo-scry",
    about = "Scry a Claude Code session's active team from live OS state (no registration)"
)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Resolve the active Claude Teams team for a target process/pane.
    ProbeTeam(ProbeArgs),
}

#[derive(Args)]
struct ProbeArgs {
    /// Probe this process — walk up to the owning Claude Code session (default).
    #[arg(long = "self", conflicts_with_all = ["pid", "pane"])]
    self_process: bool,
    /// Probe an explicit pid — walk up to its Claude Code session.
    #[arg(long, conflicts_with = "pane")]
    pid: Option<i32>,
    /// Probe a tmux pane id (e.g. "%306") — walk down to its Claude Code session.
    #[arg(long)]
    pane: Option<String>,
    /// Emit JSON instead of human-readable text.
    #[arg(long)]
    json: bool,
}

fn main() -> std::process::ExitCode {
    let cli = Cli::parse();
    match cli.cmd {
        Cmd::ProbeTeam(args) => probe_team(args),
    }
}

#[cfg(target_os = "linux")]
fn probe_team(args: ProbeArgs) -> std::process::ExitCode {
    let target = if let Some(pane) = args.pane {
        ProbeTarget::TmuxPane(pane)
    } else if let Some(pid) = args.pid {
        ProbeTarget::Pid(pid)
    } else {
        ProbeTarget::SelfProcess
    };

    match exo_scry::resolve_active_team(target) {
        Ok(Some(team)) => {
            if args.json {
                println!("{}", serde_json::to_string_pretty(&team).unwrap_or_default());
            } else {
                println!("active team: {}", team.team);
                println!("  claude pid:      {}", team.claude_pid);
                println!("  tasks dir:       {}", team.tasks_dir.display());
                if let Some(inbox) = &team.lead_inbox {
                    println!("  lead inbox:      {inbox}");
                }
                if let Some(sid) = &team.lead_session_id {
                    println!("  lead session id: {sid}");
                }
            }
            std::process::ExitCode::SUCCESS
        }
        Ok(None) => {
            if args.json {
                println!("null");
            } else {
                println!("active team: (none — this session is not in a team)");
            }
            std::process::ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("error: {e}");
            std::process::ExitCode::FAILURE
        }
    }
}

#[cfg(not(target_os = "linux"))]
fn probe_team(_args: ProbeArgs) -> std::process::ExitCode {
    eprintln!(
        "error: exo-scry's watch signal is currently Linux-only \
         (see the ActiveTeamSignal seam for the portable session-UUID path)"
    );
    std::process::ExitCode::FAILURE
}
