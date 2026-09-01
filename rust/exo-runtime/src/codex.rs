//! Small, process-local integration with the stock Codex CLI.
//!
//! Every Exomonad pane owns an ordinary embedded Codex TUI. Exomonad configures that TUI with
//! `-c` overrides, learns its real rollout UUID from the owning Codex process, and uses the public
//! `codex queue` command for the inbound last hop. No shared app-server or remote TUI is involved.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use tokio::process::Command;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LaunchMode<'a> {
    Fresh,
    Resume(&'a str),
    Fork(&'a str),
}

/// Codex-only routing guidance for Exomonad's deferred MCP inventory.
///
/// Codex also exposes a native `collaboration` namespace whose spawn vocabulary overlaps with
/// Exomonad's protocol. Keep this at the launch boundary so root and every spawned Codex role see
/// the distinction, while Claude's role prompts remain unchanged.
const EXOMONAD_TOOL_ROUTING: &str = r#"## Exomonad Tool Routing (Codex)

Exomonad orchestration uses the `mcp__exomonad__*` MCP tools, not Codex's native `collaboration.*` tools. Native `collaboration.spawn_agent` creates a Codex subagent but does not create an Exomonad node, worktree, branch, ledger entry, review, or fold; never use it for the Exomonad workflow described below.

The Exomonad MCP tools are lazy-loaded. Before an Exomonad operation, discover them in code mode through `functions.exec`: inspect/filter `ALL_TOOLS` for names beginning with `mcp__exomonad__`, then invoke the matching nested function on `tools` (for example `tools.mcp__exomonad__fork_wave(...)`). Use `mcp__exomonad__fork_wave`, `mcp__exomonad__spawn_dev`, and `mcp__exomonad__spawn_worker` to spawn; `mcp__exomonad__tree` to inspect the node tree; `mcp__exomonad__send_message` and `mcp__exomonad__notify_parent` for messages; `mcp__exomonad__submit_branch` for child submission; and `mcp__exomonad__merge` for the parent fold. If a tool is absent from your role's discovered inventory, that operation is not available to this node."#;

fn developer_instructions_with_tool_routing(instructions: &str) -> String {
    format!("{instructions}\n\n{EXOMONAD_TOOL_ROUTING}")
}

/// Build a shell command for a normal interactive Codex TUI.
///
/// The node identity is passed through `EXOMONAD_PAPERS`; the MCP server itself is supplied as
/// session-local CLI config, so neither the repository nor the user's global Codex config is
/// modified. `env_vars` names the launch environment Codex must copy into its stdio MCP child.
#[allow(clippy::too_many_arguments)]
pub fn tui_command(
    mode: LaunchMode<'_>,
    cwd: &Path,
    papers_path: &Path,
    model: Option<&str>,
    reasoning_effort: Option<&str>,
    developer_instructions: &str,
    prompt_path: Option<&Path>,
    launch_env: &HashMap<String, String>,
) -> String {
    let mut env: BTreeMap<String, String> = launch_env
        .iter()
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect();
    env.insert(
        "EXOMONAD_PAPERS".to_string(),
        papers_path.to_string_lossy().into_owned(),
    );

    let env_prefix = env
        .iter()
        .map(|(key, value)| format!("{key}={}", shell_escape::escape(value.as_str().into())))
        .collect::<Vec<_>>()
        .join(" ");

    let mut command = match mode {
        LaunchMode::Fresh => format!("{env_prefix} codex"),
        LaunchMode::Resume(_) => format!("{env_prefix} codex resume"),
        LaunchMode::Fork(_) => format!("{env_prefix} codex fork"),
    };

    command.push_str(" --ask-for-approval never --sandbox danger-full-access");
    if let Some(model) = model {
        push_arg(&mut command, "--model", model);
    }

    // Override every field that could retain a stale project/global definition of `exomonad`.
    push_config_string(&mut command, "mcp_servers.exomonad.command", "exo");
    push_config_value(
        &mut command,
        "mcp_servers.exomonad.args",
        serde_json::to_string(&["node"]).expect("static MCP args serialize"),
    );
    push_config_value(&mut command, "mcp_servers.exomonad.env", "{}".into());

    let mut forwarded: BTreeSet<String> = env.keys().cloned().collect();
    forwarded.insert("TMUX".into());
    forwarded.insert("TMUX_PANE".into());
    push_config_value(
        &mut command,
        "mcp_servers.exomonad.env_vars",
        serde_json::to_string(&forwarded).expect("MCP env names serialize"),
    );
    push_config_string(
        &mut command,
        "mcp_servers.exomonad.cwd",
        &cwd.to_string_lossy(),
    );
    push_config_value(&mut command, "mcp_servers.exomonad.enabled", "true".into());
    push_config_value(&mut command, "mcp_servers.exomonad.required", "true".into());
    let developer_instructions = developer_instructions_with_tool_routing(developer_instructions);
    push_config_string(
        &mut command,
        "developer_instructions",
        &developer_instructions,
    );
    if let Some(effort) = reasoning_effort {
        push_config_string(&mut command, "model_reasoning_effort", effort);
    }

    match mode {
        LaunchMode::Fresh => {}
        LaunchMode::Resume(thread_id) | LaunchMode::Fork(thread_id) => {
            command.push(' ');
            command.push_str(&shell_escape::escape(thread_id.into()));
        }
    }
    if let Some(prompt_path) = prompt_path {
        let prompt = shell_escape::escape(prompt_path.to_string_lossy());
        command.push_str(&format!(" \"$(cat -- {prompt})\""));
    }
    command
}

fn push_arg(command: &mut String, flag: &str, value: &str) {
    command.push(' ');
    command.push_str(flag);
    command.push(' ');
    command.push_str(&shell_escape::escape(value.into()));
}

fn push_config_string(command: &mut String, key: &str, value: &str) {
    push_config_value(
        command,
        key,
        serde_json::to_string(value).expect("string config value serializes"),
    );
}

fn push_config_value(command: &mut String, key: &str, value: String) {
    push_arg(command, "-c", &format!("{key}={value}"));
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CodexBinding {
    #[serde(default)]
    pub v: u32,
    pub thread_id: String,
}

fn binding_version() -> u32 {
    CodexBinding::VERSION
}

impl CodexBinding {
    /// Binding format written by the stock-TUI integration. Version zero denotes a legacy file
    /// that predates this field and must not be fed to `codex resume` automatically.
    // V1 trusted MCP `_meta.threadId`. In Codex 0.149 that identifies the surrounding hosted
    // conversation, not necessarily this local TUI rollout, and is not resume-safe.
    pub const VERSION: u32 = 2;
}

#[derive(Debug, thiserror::Error)]
pub enum CodexError {
    #[error("Codex IO: {0}")]
    Io(#[from] std::io::Error),
    #[error("Codex binding JSON: {0}")]
    Json(#[from] serde_json::Error),
    #[error("invalid Codex thread id {0:?}")]
    InvalidThreadId(String),
    #[error("codex {operation} failed ({status}): {stderr}")]
    Command {
        operation: &'static str,
        status: std::process::ExitStatus,
        stderr: String,
    },
}

pub async fn read_binding(path: &Path) -> Result<CodexBinding, CodexError> {
    let binding: CodexBinding = serde_json::from_slice(&tokio::fs::read(path).await?)?;
    validate_thread_id(&binding.thread_id)?;
    Ok(binding)
}

/// Atomically bind this Exomonad node to a resumable local Codex rollout UUID.
pub async fn write_binding(path: &Path, thread_id: impl Into<String>) -> Result<(), CodexError> {
    let thread_id = thread_id.into();
    validate_thread_id(&thread_id)?;
    if let Some(parent) = path.parent() {
        tokio::fs::create_dir_all(parent).await?;
    }
    let bytes = serde_json::to_vec_pretty(&CodexBinding {
        v: binding_version(),
        thread_id,
    })?;
    let tmp = temporary_binding_path(path);
    let mut file = tokio::fs::File::create(&tmp).await?;
    use tokio::io::AsyncWriteExt;
    file.write_all(&bytes).await?;
    file.sync_all().await?;
    drop(file);
    tokio::fs::rename(tmp, path).await?;
    Ok(())
}

fn temporary_binding_path(path: &Path) -> PathBuf {
    PathBuf::from(format!("{}.tmp", path.display()))
}

fn validate_thread_id(thread_id: &str) -> Result<(), CodexError> {
    uuid::Uuid::parse_str(thread_id)
        .map(|_| ())
        .map_err(|_| CodexError::InvalidThreadId(thread_id.to_string()))
}

/// Discover the rollout owned by a just-started Codex TUI from its open session file.
///
/// Codex does not expose its local resumable UUID to stdio MCP children. The MCP child is not
/// necessarily parented directly by the TUI (the app-server/supervisor layout has changed between
/// Codex releases), so walk the process ancestry and use the nearest ancestor which owns a
/// matching rollout descriptor. Internal Codex subagents can also be open in that descriptor
/// table; those are deliberately excluded because they are not the pane's resumable TUI thread.
#[cfg(target_os = "linux")]
pub fn discover_parent_rollout(parent_pid: u32, cwd: &Path) -> Result<String, CodexError> {
    let canonical_cwd = std::fs::canonicalize(cwd).unwrap_or_else(|_| cwd.to_owned());
    let mut pid = parent_pid;
    for _ in 0..16 {
        let candidates = rollout_candidates(pid, &canonical_cwd)?;
        if let Some((_, id)) = candidates.into_iter().max_by_key(|(modified, _)| *modified) {
            return Ok(id);
        }
        let next = process_parent_pid(pid)?;
        if next == 0 || next == pid {
            break;
        }
        pid = next;
    }
    Err(CodexError::InvalidThreadId(
        "no open Codex TUI rollout for this cwd in MCP process ancestry".into(),
    ))
}

#[cfg(target_os = "linux")]
fn rollout_candidates(
    pid: u32,
    canonical_cwd: &Path,
) -> Result<Vec<(std::time::SystemTime, String)>, CodexError> {
    let mut candidates = Vec::new();
    for entry in std::fs::read_dir(format!("/proc/{pid}/fd"))? {
        let target = match std::fs::read_link(entry?.path()) {
            Ok(target) => target,
            Err(_) => continue,
        };
        if target.extension().and_then(|value| value.to_str()) != Some("jsonl")
            || !target.to_string_lossy().contains("/.codex/sessions/")
        {
            continue;
        }
        let contents = match std::fs::read_to_string(&target) {
            Ok(contents) => contents,
            Err(_) => continue,
        };
        let value: serde_json::Value = match contents.lines().next().map(serde_json::from_str) {
            Some(Ok(value)) => value,
            _ => continue,
        };
        let payload = &value["payload"];
        // A TUI may have internal collaboration subagents open alongside its own rollout. Their
        // IDs are resumable, but resuming one here would restore the wrong conversation.
        if payload["thread_source"].as_str() == Some("subagent") {
            continue;
        }
        let Some(recorded_cwd) = payload["cwd"].as_str() else {
            continue;
        };
        let recorded_cwd =
            std::fs::canonicalize(recorded_cwd).unwrap_or_else(|_| PathBuf::from(recorded_cwd));
        if recorded_cwd != canonical_cwd {
            continue;
        }
        let Some(id) = payload["id"].as_str() else {
            continue;
        };
        if validate_thread_id(id).is_err() {
            continue;
        }
        candidates.push((std::fs::metadata(&target)?.modified()?, id.to_owned()));
    }
    Ok(candidates)
}

#[cfg(target_os = "linux")]
fn process_parent_pid(pid: u32) -> Result<u32, CodexError> {
    let status = std::fs::read_to_string(format!("/proc/{pid}/status"))?;
    status
        .lines()
        .find_map(|line| line.strip_prefix("PPid:\t"))
        .and_then(|value| value.trim().parse().ok())
        .ok_or_else(|| CodexError::InvalidThreadId(format!("cannot read parent pid for {pid}")))
}

pub async fn queue_message(cwd: &Path, thread_id: &str, message: &str) -> Result<(), CodexError> {
    validate_thread_id(thread_id)?;
    run_cli(
        cwd,
        "queue",
        ["queue", "--thread", thread_id, "--message", message],
    )
    .await
}

pub async fn archive_thread(cwd: &Path, thread_id: &str) -> Result<(), CodexError> {
    validate_thread_id(thread_id)?;
    run_cli(cwd, "archive", ["archive", thread_id]).await
}

async fn run_cli<'a>(
    cwd: &Path,
    operation: &'static str,
    args: impl IntoIterator<Item = &'a str>,
) -> Result<(), CodexError> {
    let output = Command::new("codex")
        .args(args)
        .current_dir(cwd)
        .output()
        .await?;
    if output.status.success() {
        Ok(())
    } else {
        Err(CodexError::Command {
            operation,
            status: output.status,
            stderr: String::from_utf8_lossy(&output.stderr).trim().to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const THREAD: &str = "01a05a16-97f5-7722-aa8d-467e01e2e5b4";

    #[test]
    fn stock_tui_command_is_node_scoped_and_has_no_remote_server() {
        let env = HashMap::from([
            ("EXOMONAD_SWARM_RUN_ID".into(), "run-1".into()),
            ("EXOMONAD_TMUX_SESSION".into(), "EXO".into()),
        ]);
        let command = tui_command(
            LaunchMode::Resume(THREAD),
            Path::new("/tmp/work"),
            Path::new("/tmp/node.json"),
            Some("gpt-test"),
            Some("high"),
            "node instructions",
            None,
            &env,
        );
        assert!(command.contains("codex resume"));
        assert!(command.contains(THREAD));
        assert!(command.contains("EXOMONAD_PAPERS=/tmp/node.json"));
        assert!(command.contains("mcp_servers.exomonad.command"));
        assert!(command.contains("developer_instructions"));
        assert!(command.contains("collaboration.spawn_agent"));
        assert!(command.contains("mcp__exomonad__fork_wave"));
        assert!(command.contains("mcp__exomonad__merge"));
        assert!(!command.contains("--remote"));
        assert!(!command.contains("app-server"));
    }

    #[test]
    fn fresh_child_command_reads_the_real_prompt_file() {
        let command = tui_command(
            LaunchMode::Fresh,
            Path::new("/tmp/work"),
            Path::new("/tmp/node.json"),
            None,
            None,
            "instructions",
            Some(Path::new("/tmp/prompt with spaces.md")),
            &HashMap::new(),
        );
        assert!(command.contains("codex --ask-for-approval"));
        assert!(command.contains("$(cat -- '/tmp/prompt with spaces.md')"));
    }

    #[test]
    fn codex_instructions_route_exomonad_operations_to_lazy_mcp_tools() {
        let instructions = developer_instructions_with_tool_routing("role protocol");

        assert!(instructions.starts_with("role protocol\n\n"));
        assert!(instructions.contains("not Codex's native `collaboration.*` tools"));
        assert!(instructions.contains("never use it for the Exomonad workflow"));
        assert!(instructions.contains("inspect/filter `ALL_TOOLS`"));
        assert!(instructions.contains("tools.mcp__exomonad__fork_wave"));
        for operation in [
            "mcp__exomonad__spawn_dev",
            "mcp__exomonad__spawn_worker",
            "mcp__exomonad__tree",
            "mcp__exomonad__send_message",
            "mcp__exomonad__notify_parent",
            "mcp__exomonad__submit_branch",
            "mcp__exomonad__merge",
        ] {
            assert!(instructions.contains(operation), "missing {operation}");
        }
    }

    #[tokio::test]
    async fn binding_round_trip_accepts_codex_uuid_and_old_extra_fields() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("binding.json");
        write_binding(&path, THREAD).await.unwrap();
        let current = read_binding(&path).await.unwrap();
        assert_eq!(current.thread_id, THREAD);
        assert_eq!(current.v, CodexBinding::VERSION);
        assert_eq!(
            current.v, 2,
            "MCP-derived v1 bindings must stay invalidated"
        );

        tokio::fs::write(
            &path,
            format!(r#"{{"thread_id":"{THREAD}","bootstrap_version":4}}"#),
        )
        .await
        .unwrap();
        let old = read_binding(&path).await.unwrap();
        assert_eq!(old.thread_id, THREAD);
        assert_eq!(old.v, 0);
    }
}
