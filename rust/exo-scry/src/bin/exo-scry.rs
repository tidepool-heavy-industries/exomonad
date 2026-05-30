//! exo-scry CLI — thin clap shell over the library.

use clap::{Args, Parser, Subcommand};
use exo_scry::{ActiveTeam, Result};
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
    /// Resolve the active Claude Teams team for a target process/pane/session.
    ProbeTeam(ProbeArgs),
}

#[derive(Args)]
struct ProbeArgs {
    /// Probe this process — walk up to the owning Claude Code session (default).
    #[arg(long = "self", conflicts_with_all = ["pid", "pane", "session_id"])]
    self_process: bool,
    /// Probe an explicit pid — walk up to its Claude Code session.
    #[arg(long, conflicts_with_all = ["pane", "session_id"])]
    pid: Option<i32>,
    /// Probe a tmux pane id (e.g. "%306") — walk down to its Claude Code session.
    #[arg(long, conflicts_with = "session_id")]
    pane: Option<String>,
    /// Resolve by a known session UUID (portable; for self/sidecar contexts).
    #[arg(long = "session-id")]
    session_id: Option<String>,
    /// Use the portable cwd→transcript signal instead of inotify (assumes one
    /// live session per cwd; errors loudly if several share it).
    #[arg(long = "via-transcript")]
    via_transcript: bool,
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

fn probe_team(args: ProbeArgs) -> std::process::ExitCode {
    // The session-UUID path is portable; resolve it on any platform.
    if let Some(sid) = &args.session_id {
        return print_result(exo_scry::resolve_by_session(sid), args.json);
    }
    probe_by_target(&args)
}

#[cfg(target_os = "linux")]
fn probe_by_target(args: &ProbeArgs) -> std::process::ExitCode {
    // A pane id is a member's durable self-key: try matching it against
    // members' tmuxPaneId first; fall back to walking the pane to its session.
    if let Some(pane) = &args.pane {
        match exo_scry::resolve_by_pane(pane) {
            Ok(Some(team)) => return print_result(Ok(Some(team)), args.json),
            Ok(None) => {} // not a recorded member — fall through to the watch path
            Err(e) => return print_result(Err(e), args.json),
        }
    }
    let target = if let Some(pane) = &args.pane {
        ProbeTarget::TmuxPane(pane.clone())
    } else if let Some(pid) = args.pid {
        ProbeTarget::Pid(pid)
    } else {
        ProbeTarget::SelfProcess
    };
    let res = if args.via_transcript {
        exo_scry::resolve_via_transcript(target)
    } else {
        exo_scry::resolve_active_team(target)
    };
    print_result(res, args.json)
}

#[cfg(not(target_os = "linux"))]
fn probe_by_target(_args: &ProbeArgs) -> std::process::ExitCode {
    eprintln!(
        "error: process/pane probing is Linux-only (it reads the kernel's inotify \
         bookkeeping); use --session-id <uuid>, which is portable"
    );
    std::process::ExitCode::FAILURE
}

fn print_result(res: Result<Option<ActiveTeam>>, json: bool) -> std::process::ExitCode {
    match res {
        Ok(Some(team)) => {
            if json {
                println!("{}", serde_json::to_string_pretty(&team).unwrap_or_default());
            } else {
                println!("active team: {}", team.team);
                if let Some(me) = &team.me {
                    println!("  me:              {} ({})", me.name, me.agent_type);
                }
                if let Some(pid) = team.claude_pid {
                    println!("  claude pid:      {pid}");
                }
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
            if json {
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
