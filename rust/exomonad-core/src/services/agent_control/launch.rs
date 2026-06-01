//! Agent launch-command construction — the single source shared by classic mode
//! (`AgentControlService`) and experimental node mode (`exo-runtime`'s Spawner).
//!
//! These are free fns (not methods on the generic `AgentControlService<C>`) so both the
//! server world and the node sidecar can build identical launch commands without the
//! service context. The agent's prompt is always sourced from a **file** (`.exo/tmp/…`)
//! via `"$(cat <file>)"`, never interpolated inline — so multi-line / quote-bearing
//! prompts can't break shell parsing.

use super::{AgentType, ClaudeSpawnFlags};
use anyhow::{Context, Result};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tracing::info;

/// Shell-escape a value for safe interpolation: wrap in single quotes, escaping any `'`.
pub fn escape_for_shell_command(s: &str) -> String {
    // Replace ' with '\'' (end quote, escaped quote, start quote)
    let escaped = s.replace('\'', r"'\''");
    format!("'{}'", escaped)
}

/// Build the full shell command that launches `agent_type`, sourcing its prompt from
/// `prompt_file` (never inline). Prepends `env_vars`; wraps in `nix develop` if `cwd`
/// contains a `flake.nix`.
#[allow(clippy::too_many_arguments)]
pub fn build_agent_command(
    agent_type: AgentType,
    prompt_file: Option<&Path>,
    fork_session_id: Option<&str>,
    env_vars: &HashMap<String, String>,
    cwd: &Path,
    claude_flags: Option<&ClaudeSpawnFlags>,
    yolo: bool,
    // Wrap in `nix develop` when `cwd` has a flake (classic mode runs agents in the dev
    // shell). Experimental node mode launches plain, matching its root, so passes `false`.
    wrap_nix: bool,
) -> String {
    let cmd = agent_type.command();

    // Build permission flags for Claude agents
    let perms_flags = match agent_type {
        AgentType::Claude => {
            let mut flags = String::new();
            let mode = claude_flags.and_then(|f| f.permission_mode.as_ref());
            match mode {
                Some(m) => {
                    flags.push_str(" --permission-mode ");
                    flags.push_str(m.as_str());
                }
                None => flags.push_str(" --dangerously-skip-permissions"),
            }
            if let Some(f) = claude_flags {
                for tool in &f.allowed_tools {
                    flags.push_str(" --allowedTools ");
                    flags.push_str(&shell_escape::escape(tool.into()));
                }
                for tool in &f.disallowed_tools {
                    flags.push_str(" --disallowedTools ");
                    flags.push_str(&shell_escape::escape(tool.into()));
                }
            }
            flags
        }
        AgentType::Gemini => {
            if yolo {
                " --yolo".to_string()
            } else {
                String::new()
            }
        }
        AgentType::Shoal | AgentType::Process => String::new(),
    };

    let agent_command = match (prompt_file, fork_session_id) {
        (Some(pf), Some(session_id)) => {
            let escaped_session = escape_for_shell_command(session_id);
            let escaped_path = escape_for_shell_command(&pf.display().to_string());
            format!(
                "{}{} --resume {} --fork-session \"$(cat {})\"",
                cmd, perms_flags, escaped_session, escaped_path
            )
        }
        (Some(pf), None) => {
            let escaped_path = escape_for_shell_command(&pf.display().to_string());
            let flag = agent_type.prompt_flag();
            if flag.is_empty() {
                format!("{}{} \"$(cat {})\"", cmd, perms_flags, escaped_path)
            } else {
                format!("{}{} {} \"$(cat {})\"", cmd, perms_flags, flag, escaped_path)
            }
        }
        _ => format!("{}{}", cmd, perms_flags),
    };

    // Prepend env vars
    let env_prefix = env_vars
        .iter()
        .map(|(k, v)| format!("{}={}", k, shell_escape::escape(v.into())))
        .collect::<Vec<_>>()
        .join(" ");
    let full_command = if env_prefix.is_empty() {
        agent_command
    } else {
        format!("{} {}", env_prefix, agent_command)
    };

    // Wrap in nix develop shell if requested and flake.nix exists in cwd
    if wrap_nix && cwd.join("flake.nix").exists() {
        info!("Wrapping agent command in nix develop shell");
        let escaped = full_command.replace('\'', "'\\''");
        format!("nix develop -c sh -c '{}'", escaped)
    } else {
        full_command
    }
}

/// Write a prompt to a temp file and return the absolute path.
/// Files are written to `.exo/tmp/` in the project directory.
/// Uses timestamp+pid filenames to avoid races when multiple agents spawn concurrently.
pub async fn write_prompt_file(
    project_dir: &Path,
    agent_name: &str,
    prompt: &str,
) -> Result<PathBuf> {
    let tmp_dir = project_dir.join(".exo/tmp");
    tokio::fs::create_dir_all(&tmp_dir)
        .await
        .context("Failed to create .exo/tmp/")?;
    let ts = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let path = tmp_dir.join(format!("prompt-{}-{}.txt", ts, std::process::id()));
    tokio::fs::write(&path, prompt)
        .await
        .context("Failed to write prompt file")?;
    info!(path = %path.display(), agent = %agent_name, "Wrote prompt to temp file");
    Ok(path)
}
