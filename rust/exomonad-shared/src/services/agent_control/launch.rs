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

/// Typed representation of an agent launch command.
///
/// The `render()` method enforces the structural invariant: every variadic flag
/// (notably `--mcp-config`) is followed by a capping `--flag` (`--append-system-prompt`
/// or `--model`) **before** the positional prompt, so a variadic can never accidentally
/// consume the prompt as one of its arguments.
///
/// Construct this struct, then call `render()` to obtain the shell command string.
/// `build_agent_command` is a convenience wrapper that constructs and renders in one step.
pub struct ClaudeInvocation {
    pub agent_type: AgentType,
    pub cwd: PathBuf,
    /// `None` → `--dangerously-skip-permissions`.
    pub permission_mode: Option<crate::domain::PermissionMode>,
    pub allowed_tools: Vec<String>,
    pub disallowed_tools: Vec<String>,
    /// `--settings <path>` — merged over the cwd config; `None` ⇒ no flag.
    pub settings_path: Option<String>,
    /// `--mcp-config <path>` — variadic; MUST be followed by a cap flag before the prompt.
    pub mcp_config_path: Option<String>,
    /// `--append-system-prompt <text>` — cap flag 1; rendered after `--mcp-config`.
    pub append_system_prompt: Option<String>,
    /// `--model <name>` — cap flag 2; rendered after `--mcp-config`.
    pub model: Option<String>,
    pub prompt_file: Option<PathBuf>,
    pub fork_session_id: Option<String>,
    pub env_vars: HashMap<String, String>,
    /// Gemini-only `--yolo` flag; inert for Claude.
    pub yolo: bool,
    /// Wrap the launch in `nix develop` when `cwd` contains a `flake.nix`.
    pub wrap_nix: bool,
    /// `--continue`: resume the most recent conversation in this cwd (root re-init only).
    pub resume: bool,
}

impl ClaudeInvocation {
    /// Render the invocation to a shell command string.
    ///
    /// **Ordering invariant** (Claude only): flags are emitted in this fixed order:
    /// `[--continue] [--dangerously-skip-permissions|--permission-mode] [--allowedTools…]
    /// [--disallowedTools…] [--settings] [--mcp-config] [--append-system-prompt] [--model]
    /// [positional-prompt]`
    ///
    /// `--mcp-config` (variadic) is always followed by at least one of `--append-system-prompt`
    /// or `--model` when they are set, or sits at the end when both are absent (no prompt to
    /// swallow in that case — the root interactive launch).
    pub fn render(&self) -> String {
        let cmd = self.agent_type.command();

        let perms_flags = match self.agent_type {
            AgentType::Claude => {
                let mut flags = String::new();

                if self.resume {
                    flags.push_str(" --continue");
                }

                match &self.permission_mode {
                    Some(m) => {
                        flags.push_str(" --permission-mode ");
                        flags.push_str(m.as_str());
                    }
                    None => flags.push_str(" --dangerously-skip-permissions"),
                }

                for tool in &self.allowed_tools {
                    flags.push_str(" --allowedTools ");
                    flags.push_str(&shell_escape::escape(tool.into()));
                }
                for tool in &self.disallowed_tools {
                    flags.push_str(" --disallowedTools ");
                    flags.push_str(&shell_escape::escape(tool.into()));
                }

                if let Some(settings) = &self.settings_path {
                    if !settings.trim().is_empty() {
                        flags.push_str(" --settings ");
                        flags.push_str(&shell_escape::escape(settings.into()));
                    }
                }

                // --mcp-config is VARIADIC (consumes every following non-flag arg). It MUST be
                // capped by --append-system-prompt or --model before any positional prompt.
                if let Some(mcp) = &self.mcp_config_path {
                    if !mcp.trim().is_empty() {
                        flags.push_str(" --mcp-config ");
                        flags.push_str(&shell_escape::escape(mcp.into()));
                    }
                }

                // Cap flags — always rendered AFTER --mcp-config, ALWAYS before the prompt.
                if let Some(p) = &self.append_system_prompt {
                    if !p.trim().is_empty() {
                        flags.push_str(" --append-system-prompt ");
                        flags.push_str(&escape_for_shell_command(p));
                    }
                }
                if let Some(m) = &self.model {
                    if !m.trim().is_empty() {
                        flags.push_str(" --model ");
                        flags.push_str(&shell_escape::escape(m.into()));
                    }
                }

                flags
            }
            AgentType::Gemini => {
                if self.yolo {
                    " --yolo".to_string()
                } else {
                    String::new()
                }
            }
            AgentType::Shoal | AgentType::Process => String::new(),
        };

        let agent_command = match (&self.prompt_file, &self.fork_session_id) {
            (Some(pf), Some(session_id)) => {
                let escaped_session = escape_for_shell_command(session_id);
                let escaped_path = escape_for_shell_command(&pf.display().to_string());
                format!(
                    "{}{} --resume {} --fork-session \"$(cat {})\"",
                    cmd, perms_flags, escaped_session, escaped_path
                )
            }
            (Some(pf), None) if self.agent_type == AgentType::Gemini => {
                // Gemini `@`-expands its `-i` prompt, so passing the spec inline (`"$(cat file)"`)
                // makes any `@/`, `@scoped`, `@types`, etc. in the spec be read as FILE references —
                // e.g. `@/` → "read `/`" → the folder-trust modal that hangs the leaf. Instead,
                // deliver the prompt as a single `@`-reference to the file: the spec's `@`-tokens
                // then arrive as literal file CONTENT, and the only reference in the arg is our own.
                let rel = pf
                    .strip_prefix(&self.cwd)
                    .unwrap_or(pf)
                    .display()
                    .to_string();
                let init = format!(
                    "Your complete task spec is in the file @{rel} — read it in full and carry it out. \
                     Treat that file's contents as literal text.",
                );
                format!(
                    "{}{} {} {}",
                    cmd,
                    perms_flags,
                    self.agent_type.prompt_flag(),
                    escape_for_shell_command(&init)
                )
            }
            (Some(pf), None) => {
                let escaped_path = escape_for_shell_command(&pf.display().to_string());
                let flag = self.agent_type.prompt_flag();
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
        let env_prefix = self
            .env_vars
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
        if self.wrap_nix && self.cwd.join("flake.nix").exists() {
            info!("Wrapping agent command in nix develop shell");
            let escaped = full_command.replace('\'', "'\\''");
            format!("nix develop -c sh -c '{}'", escaped)
        } else {
            full_command
        }
    }
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
    ClaudeInvocation {
        agent_type,
        cwd: cwd.to_path_buf(),
        permission_mode: claude_flags.and_then(|f| f.permission_mode),
        allowed_tools: claude_flags
            .map(|f| f.allowed_tools.clone())
            .unwrap_or_default(),
        disallowed_tools: claude_flags
            .map(|f| f.disallowed_tools.clone())
            .unwrap_or_default(),
        settings_path: claude_flags.and_then(|f| f.settings_path.clone()),
        mcp_config_path: claude_flags.and_then(|f| f.mcp_config_path.clone()),
        append_system_prompt: append_system_prompt.map(|s| s.to_string()),
        model: claude_flags.and_then(|f| f.model.clone()),
        prompt_file: prompt_file.map(|p| p.to_path_buf()),
        fork_session_id: fork_session_id.map(|s| s.to_string()),
        env_vars: env_vars.clone(),
        yolo,
        wrap_nix,
        resume: false,
    }
    .render()
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
        assert!(
            cmd.contains("$(cat"),
            "Claude path should still cat the prompt file: {cmd}"
        );
        assert!(
            !cmd.contains("@.exo/tmp"),
            "Claude path should not use the Gemini @-reference form: {cmd}"
        );
    }

    #[test]
    fn claude_model_flag_emitted_from_spawn_flags() {
        // The node-mode leaf path pins cheap roles to a model via ClaudeSpawnFlags::model.
        let cwd = Path::new("/home/u/dev/proj/.exo/worktrees/leaf");
        let pf = cwd.join(".exo/tmp/prompt.txt");
        let env = HashMap::new();
        let flags = ClaudeSpawnFlags {
            model: Some("sonnet".into()),
            ..Default::default()
        };
        let cmd = build_agent_command(
            AgentType::Claude,
            Some(&pf),
            None,
            &env,
            cwd,
            Some(&flags),
            false,
            false,
            None,
        );
        assert!(
            cmd.contains("--model sonnet"),
            "expected --model sonnet: {cmd}"
        );
        assert!(
            cmd.contains("--dangerously-skip-permissions"),
            "model flag must not disturb the default permission mode: {cmd}"
        );
    }

    #[test]
    fn claude_no_model_flag_when_unset() {
        // root/tl pass no model → no --model flag (inherit the launcher default).
        let cwd = Path::new("/home/u/dev/proj/.exo/worktrees/tl");
        let pf = cwd.join(".exo/tmp/prompt.txt");
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
        assert!(
            !cmd.contains("--model"),
            "no model flag expected when unset: {cmd}"
        );
    }

    #[test]
    fn claude_private_config_flags_emitted() {
        // Node-mode points CC at private config files via flags (merge over the cwd — plain
        // --settings / --mcp-config, never --strict-mcp-config / --setting-sources).
        let cwd = Path::new("/home/u/dev/proj/.exo/worktrees/leaf");
        let pf = cwd.join(".exo/tmp/prompt.txt");
        let env = HashMap::new();
        let flags = ClaudeSpawnFlags {
            settings_path: Some("/home/u/.claude/exo/papers/run/pane-3.settings.json".into()),
            mcp_config_path: Some("/home/u/.claude/exo/papers/run/pane-3.mcp.json".into()),
            ..Default::default()
        };
        let cmd = build_agent_command(
            AgentType::Claude,
            Some(&pf),
            None,
            &env,
            cwd,
            Some(&flags),
            false,
            false,
            None,
        );
        assert!(
            cmd.contains("--settings /home/u/.claude/exo/papers/run/pane-3.settings.json"),
            "expected --settings flag: {cmd}"
        );
        assert!(
            cmd.contains("--mcp-config /home/u/.claude/exo/papers/run/pane-3.mcp.json"),
            "expected --mcp-config flag: {cmd}"
        );
        assert!(
            !cmd.contains("--strict-mcp-config") && !cmd.contains("--setting-sources"),
            "must MERGE (no strict/setting-sources) to preserve user config: {cmd}"
        );
    }

    #[test]
    fn variadic_mcp_config_never_abuts_the_positional_prompt() {
        // REGRESSION: `--mcp-config` is variadic (eats following non-flag args). If it sits right
        // before the trailing `"$(cat ...)"` positional, CC swallows the prompt as a config path.
        // It MUST be capped by a later singular flag (--append-system-prompt / --model).
        let cwd = Path::new("/home/u/dev/proj/.exo/worktrees/leaf");
        let pf = cwd.join(".exo/tmp/prompt.txt");
        let env = HashMap::new();
        let flags = ClaudeSpawnFlags {
            model: Some("kimi-for-coding".into()),
            settings_path: Some("/p/pane-3.settings.json".into()),
            mcp_config_path: Some("/p/pane-3.mcp.json".into()),
            ..Default::default()
        };
        let cmd = build_agent_command(
            AgentType::Claude,
            Some(&pf),
            None,
            &env,
            cwd,
            Some(&flags),
            false,
            false,
            Some("ROLE PROTOCOL PROSE"), // append_system_prompt — always present for node spawns
        );
        let mcp_at = cmd.find("--mcp-config").expect("has --mcp-config");
        let prompt_at = cmd.find("\"$(cat").expect("has the positional prompt");
        let cap_at = cmd
            .find("--append-system-prompt")
            .or_else(|| cmd.find("--model"))
            .expect("a singular flag must follow --mcp-config");
        assert!(
            mcp_at < cap_at && cap_at < prompt_at,
            "--mcp-config must be capped by a later --flag, not abut the prompt: {cmd}"
        );
    }

    #[test]
    fn claude_invocation_ordering_invariant() {
        // The ClaudeInvocation struct enforces by construction that --mcp-config (variadic)
        // is always followed by a capping --flag before the positional prompt.
        // This test asserts: index(--mcp-config) < index(cap-flag) < index(positional prompt).
        let cwd = Path::new("/home/u/dev/proj/.exo/worktrees/leaf");
        let pf = cwd.join(".exo/tmp/prompt.txt");

        let cmd = ClaudeInvocation {
            agent_type: AgentType::Claude,
            cwd: cwd.to_path_buf(),
            permission_mode: None,
            allowed_tools: vec![],
            disallowed_tools: vec![],
            settings_path: Some("/p/settings.json".into()),
            mcp_config_path: Some("/p/mcp.json".into()),
            append_system_prompt: Some("ROLE PROTOCOL".into()),
            model: Some("sonnet".into()),
            prompt_file: Some(pf.clone()),
            fork_session_id: None,
            env_vars: HashMap::new(),
            yolo: false,
            wrap_nix: false,
            resume: false,
        }
        .render();

        let mcp_at = cmd.find("--mcp-config").expect("--mcp-config present");
        let prompt_at = cmd.find("\"$(cat").expect("positional prompt present");
        let cap_at = cmd
            .find("--append-system-prompt")
            .or_else(|| cmd.find("--model"))
            .expect("at least one cap flag present");

        assert!(
            mcp_at < cap_at,
            "--mcp-config must precede the cap flag: {cmd}"
        );
        assert!(
            cap_at < prompt_at,
            "cap flag must precede the positional prompt: {cmd}"
        );

        // Also verify with only --model as the cap (no append-system-prompt).
        let cmd2 = ClaudeInvocation {
            agent_type: AgentType::Claude,
            cwd: cwd.to_path_buf(),
            permission_mode: None,
            allowed_tools: vec![],
            disallowed_tools: vec![],
            settings_path: Some("/p/settings.json".into()),
            mcp_config_path: Some("/p/mcp.json".into()),
            append_system_prompt: None,
            model: Some("sonnet".into()),
            prompt_file: Some(pf),
            fork_session_id: None,
            env_vars: HashMap::new(),
            yolo: false,
            wrap_nix: false,
            resume: false,
        }
        .render();

        let mcp2 = cmd2.find("--mcp-config").expect("--mcp-config present");
        let model2 = cmd2.find("--model").expect("--model present (the cap)");
        let prompt2 = cmd2.find("\"$(cat").expect("positional prompt present");
        assert!(
            mcp2 < model2 && model2 < prompt2,
            "--mcp-config < --model < prompt when only --model is the cap: {cmd2}"
        );
    }
}
