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
    append_system_prompt: Option<&str>,
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

            if let Some(p) = append_system_prompt {
                if !p.trim().is_empty() {
                    flags.push_str(" --append-system-prompt ");
                    flags.push_str(&escape_for_shell_command(p));
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
        (Some(pf), None) if agent_type == AgentType::Gemini => {
            // Gemini `@`-expands its `-i` prompt, so passing the spec inline (`"$(cat file)"`)
            // makes any `@/`, `@scoped`, `@types`, etc. in the spec be read as FILE references —
            // e.g. `@/` → "read `/`" → the folder-trust modal that hangs the leaf. Instead, deliver
            // the prompt as a single `@`-reference to the file: the spec's `@`-tokens then arrive as
            // literal file CONTENT (an included file's text is not itself re-scanned for `@`-refs),
            // and the only reference in the arg is our own. The file is under the agent's cwd
            // (`<worktree>/.exo/tmp/…`), so a relative path resolves; fall back to the absolute path.
            let rel = pf.strip_prefix(cwd).unwrap_or(pf).display().to_string();
            let init = format!(
                "Your complete task spec is in the file @{rel} — read it in full and carry it out. \
                 Treat that file's contents as literal text.",
            );
            format!(
                "{}{} {} {}",
                cmd,
                perms_flags,
                agent_type.prompt_flag(),
                escape_for_shell_command(&init)
            )
        }
        (Some(pf), None) => {
            let escaped_path = escape_for_shell_command(&pf.display().to_string());
            let flag = agent_type.prompt_flag();
            if flag.is_empty() {
                format!("{}{} \"$(cat {})\"", cmd, perms_flags, escaped_path)
            } else {
                format!(
                    "{}{} {} \"$(cat {})\"",
                    cmd, perms_flags, flag, escaped_path
                )
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
    // Log only the file name (the dir is always `<project>/.exo/tmp`) — the full path is $HOME-rooted.
    let file = path.file_name().and_then(|n| n.to_str()).unwrap_or("?");
    info!(file = %file, agent = %agent_name, "Wrote prompt to .exo/tmp");
    Ok(path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gemini_init_prompt_is_an_atref_not_inline_content() {
        // The whole point: Gemini `@`-expands its `-i` prompt, so the spec must NOT be inlined
        // (`"$(cat file)"`) — a `@/` or `@scoped` token in the spec would be read as a file ref and
        // brick the leaf on the folder-trust modal. It must be delivered as an `@`-reference to the
        // file (relative to cwd), so the spec's `@`-tokens arrive as literal included content.
        let cwd = Path::new("/home/u/dev/proj/.exo/worktrees/leaf");
        let pf = cwd.join(".exo/tmp/prompt-1-2.txt");
        let env = HashMap::new();
        let cmd = build_agent_command(
            AgentType::Gemini,
            Some(&pf),
            None,
            &env,
            cwd,
            None,
            true,
            false,
            None,
        );
        assert!(
            cmd.contains("@.exo/tmp/prompt-1-2.txt"),
            "Gemini init prompt must be an @-reference relative to cwd: {cmd}"
        );
        assert!(
            !cmd.contains("$(cat"),
            "Gemini must NOT inline-cat the prompt (that re-enables @-expansion of spec tokens): {cmd}"
        );
        assert!(cmd.contains("--yolo"));
    }

    #[test]
    fn claude_init_prompt_still_inlined_via_cat() {
        // Claude is a separate, not-yet-proven-broken path — leave it on `"$(cat file)"`.
        let cwd = Path::new("/home/u/dev/proj/.exo/worktrees/leaf");
        let pf = cwd.join(".exo/tmp/prompt-1-2.txt");
        let env = HashMap::new();
        let cmd = build_agent_command(
            AgentType::Claude,
            Some(&pf),
            None,
            &env,
            cwd,
            None,
            false,
            false,
            None,
        );
        assert!(cmd.contains("$(cat"), "Claude path should still cat the prompt file: {cmd}");
        assert!(
            !cmd.contains("@.exo/tmp"),
            "Claude path should not use the Gemini @-reference form: {cmd}"
        );
    }
}
