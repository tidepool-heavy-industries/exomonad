use std::path::Path;
use tokio::io::AsyncWriteExt;

/// Write the Claude-specific configuration files (`.mcp.json` and `.claude/settings.local.json`)
/// to the given agent directory. This is identity-free and can be used for both the root agent
/// and child agents.
pub async fn write_node_agent_config(agent_dir: &Path, papers_path: &Path) -> std::io::Result<()> {
    let esc = |s: &str| shell_escape::escape(s.to_string().into()).into_owned();

    // 1. .mcp.json (MCP sidecar config)
    let mcp_config = serde_json::json!({
        "mcpServers": {
            "exomonad": {
                "type": "stdio",
                "command": exo_caps::invocation::BIN,
                "args": exo_caps::invocation::node_args(&papers_path.to_string_lossy())
            }
        }
    });

    let mcp_path = agent_dir.join(".mcp.json");
    let mcp_json = serde_json::to_vec_pretty(&mcp_config)
        .map_err(|e| std::io::Error::other(format!("mcp_config encode: {e}")))?;
    let mut f = tokio::fs::File::create(&mcp_path).await?;
    f.write_all(&mcp_json).await?;
    f.sync_all().await?;

    // 2. .claude/settings.local.json (CC node-mode hooks)
    let claude_dir = agent_dir.join(".claude");
    tokio::fs::create_dir_all(&claude_dir).await?;

    let p_str = esc(&papers_path.to_string_lossy());
    use exo_caps::invocation::{hook_command, PRE_TOOL_USE, SESSION_START, STOP};
    // The Stop hook is a local convergence gate (reads `git status`); no GitHub token needed.
    // PreToolUse + Stop route to the sidecar's hook socket via the thin client; SessionStart
    // stays one-shot in-process. (Gemini gets the equivalent BeforeTool/AfterAgent wiring in
    // `write_gemini_node_config` below.)
    let settings = serde_json::json!({
        "hooks": {
            "PreToolUse": [{
                "matcher": "*",
                "hooks": [{"type": "command", "command": hook_command(PRE_TOOL_USE, &p_str)}]
            }],
            "Stop": [{
                "hooks": [{"type": "command", "command": hook_command(STOP, &p_str)}]
            }],
            "SessionStart": [{
                "hooks": [{"type": "command", "command": hook_command(SESSION_START, &p_str)}]
            }]
        },
        "_exomonad_generated": true
    });

    let settings_path = claude_dir.join("settings.local.json");
    let settings_json = serde_json::to_vec_pretty(&settings)
        .map_err(|e| std::io::Error::other(format!("settings encode: {e}")))?;
    let mut f = tokio::fs::File::create(&settings_path).await?;
    f.write_all(&settings_json).await?;
    f.sync_all().await?;

    Ok(())
}

/// Write the Gemini-specific `settings.json` (MCP + hooks) to the given agent directory.
///
/// `protocol` is the resolved role-steering prose (override-or-const). When non-empty it is
/// written to a `protocol.md` beside the settings and referenced by absolute path in
/// `context.fileName` (mirrors classic's `generate_gemini_worker_settings`): Gemini has no
/// reliable session-start `additionalContext`, so the context file is its steering channel.
/// Skipped entirely when the protocol is empty.
pub async fn write_gemini_node_config(
    settings_path: &Path,
    papers_path: &Path,
    protocol: &str,
) -> std::io::Result<()> {
    let p_raw = papers_path.to_string_lossy();
    let p_esc = shell_escape::escape(p_raw.clone().into_owned().into()).into_owned();

    // Per-pane path (NOT the child's worktree): inline siblings share their parent's worktree,
    // so a worktree-local settings.json would clobber each other's papers pointer. Gemini reads
    // this via the absolute GEMINI_CLI_SYSTEM_SETTINGS_PATH env var, so location is free.
    if let Some(parent) = settings_path.parent() {
        tokio::fs::create_dir_all(parent).await?;
    }

    let context_path = if protocol.trim().is_empty() {
        None
    } else {
        let dir = settings_path.parent().unwrap_or_else(|| Path::new("."));
        let path = dir.join("protocol.md");
        let mut f = tokio::fs::File::create(&path).await?;
        f.write_all(protocol.as_bytes()).await?;
        f.sync_all().await?;
        Some(path)
    };

    let dir = settings_path.parent().unwrap_or_else(|| Path::new("."));
    let policy_path = dir.join("policy.toml");
    let mut f = tokio::fs::File::create(&policy_path).await?;
    f.write_all(gemini_policy_toml().as_bytes()).await?;
    f.sync_all().await?;

    let settings =
        gemini_settings_json(&p_raw, &p_esc, context_path.as_deref(), Some(&policy_path));
    let settings_json = serde_json::to_vec_pretty(&settings)
        .map_err(|e| std::io::Error::other(format!("gemini settings encode: {e}")))?;

    let mut f = tokio::fs::File::create(settings_path).await?;
    f.write_all(&settings_json).await?;
    f.sync_all().await?;

    Ok(())
}

fn gemini_policy_toml() -> &'static str {
    r#"[[rule]]
toolName = "run_shell_command"
decision = "allow"
priority = 100
allowRedirection = true

[[rule]]
toolName = "*"
decision = "allow"
priority = 90
"#
}

pub(crate) fn gemini_settings_json(
    papers_path: &str,
    p_str_escaped: &str,
    context_path: Option<&Path>,
    policy_path: Option<&Path>,
) -> serde_json::Value {
    use exo_caps::invocation::{
        hook_command, GEMINI_AFTER_AGENT, GEMINI_BEFORE_TOOL, GEMINI_SESSION_START, PRE_TOOL_USE,
        SESSION_START, STOP,
    };

    let mut hooks = serde_json::Map::new();
    hooks.insert(
        GEMINI_BEFORE_TOOL.to_string(),
        serde_json::json!([{
            "matcher": ".*",
            "hooks": [{"type": "command", "command": hook_command(PRE_TOOL_USE, p_str_escaped)}]
        }]),
    );
    hooks.insert(
        GEMINI_AFTER_AGENT.to_string(),
        serde_json::json!([{
            "matcher": "",
            "hooks": [{"type": "command", "command": hook_command(STOP, p_str_escaped)}]
        }]),
    );
    hooks.insert(
        GEMINI_SESSION_START.to_string(),
        serde_json::json!([{
            "matcher": "",
            "hooks": [{"type": "command", "command": hook_command(SESSION_START, p_str_escaped)}]
        }]),
    );

    let mut settings = serde_json::json!({
        "mcpServers": {
            "exomonad": {
                "type": "stdio",
                "command": exo_caps::invocation::BIN,
                "args": exo_caps::invocation::node_args(papers_path)
            }
        },
        "hooks": hooks
    });

    // Reference the role-steering context file (absolute path so a worktree child finds it),
    // alongside a project-local GEMINI.md if present — mirrors classic's `context.fileName`.
    if let Some(cp) = context_path {
        settings["context"] = serde_json::json!({
            "fileName": ["GEMINI.md", cp.to_string_lossy()]
        });
    }

    if let Some(pp) = policy_path {
        settings["adminPolicyPaths"] = serde_json::json!([pp.to_string_lossy()]);
    }

    settings
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::invocation::{GEMINI_AFTER_AGENT, GEMINI_BEFORE_TOOL, GEMINI_SESSION_START};

    #[test]
    fn test_gemini_settings_shape() {
        let papers = "/tmp/node.json";
        let escaped = "'/tmp/node.json'";
        let json = gemini_settings_json(papers, escaped, None, None);

        // 1. MCP server args (`exo node --papers <papers>` — papers is the last element)
        let args = &json["mcpServers"]["exomonad"]["args"];
        assert_eq!(args[2], papers);

        // 2. Hook keys and matchers (BeforeTool = regex (.*), lifecycle events = exact-string (""))
        let hooks = &json["hooks"];
        assert_eq!(hooks[GEMINI_BEFORE_TOOL][0]["matcher"], ".*");
        assert_eq!(hooks[GEMINI_AFTER_AGENT][0]["matcher"], "");
        assert_eq!(hooks[GEMINI_SESSION_START][0]["matcher"], "");

        // 3. Command wiring
        let cmd = hooks[GEMINI_BEFORE_TOOL][0]["hooks"][0]["command"]
            .as_str()
            .unwrap();
        assert!(cmd.contains("pre-tool-use"));
        assert!(cmd.contains(escaped));

        // 4. No context block when no protocol path is supplied.
        assert!(json.get("context").is_none());
    }

    #[test]
    fn test_gemini_settings_context_file() {
        let path = Path::new("/tmp/agent/protocol.md");
        let json = gemini_settings_json("/tmp/node.json", "'/tmp/node.json'", Some(path), None);
        let files = &json["context"]["fileName"];
        assert_eq!(files[0], "GEMINI.md");
        assert_eq!(files[1], "/tmp/agent/protocol.md");
    }

    #[test]
    fn test_gemini_settings_policy_path() {
        let path = Path::new("/tmp/agent/policy.toml");
        let json = gemini_settings_json("/tmp/node.json", "'/tmp/node.json'", None, Some(path));
        assert_eq!(json["adminPolicyPaths"][0], "/tmp/agent/policy.toml");
    }

    #[tokio::test]
    async fn test_write_gemini_node_config_writes_policy_file() {
        let tmp = tempfile::tempdir().unwrap();
        let settings_path = tmp.path().join("agent/settings.json");
        let papers_path = tmp.path().join("node.json");

        write_gemini_node_config(&settings_path, &papers_path, "")
            .await
            .unwrap();

        let policy_file = tmp.path().join("agent/policy.toml");
        let body = tokio::fs::read_to_string(&policy_file).await.unwrap();
        assert!(body.contains("run_shell_command"));
        assert!(body.contains("allowRedirection = true"));

        let settings: serde_json::Value =
            serde_json::from_slice(&tokio::fs::read(&settings_path).await.unwrap()).unwrap();
        assert_eq!(
            settings["adminPolicyPaths"][0],
            *policy_file.to_string_lossy()
        );
    }

    #[tokio::test]
    async fn test_write_gemini_node_config_writes_protocol_file() {
        let tmp = tempfile::tempdir().unwrap();
        let settings_path = tmp.path().join("agent/settings.json");
        let papers_path = tmp.path().join("node.json");

        write_gemini_node_config(&settings_path, &papers_path, "ROLE PROTOCOL BODY")
            .await
            .unwrap();

        let protocol_file = tmp.path().join("agent/protocol.md");
        let body = tokio::fs::read_to_string(&protocol_file).await.unwrap();
        assert_eq!(body, "ROLE PROTOCOL BODY");

        let settings: serde_json::Value =
            serde_json::from_slice(&tokio::fs::read(&settings_path).await.unwrap()).unwrap();
        let files = &settings["context"]["fileName"];
        assert_eq!(files[1], *protocol_file.to_string_lossy());
    }

    #[tokio::test]
    async fn test_write_gemini_node_config_skips_empty_protocol() {
        let tmp = tempfile::tempdir().unwrap();
        let settings_path = tmp.path().join("agent/settings.json");
        let papers_path = tmp.path().join("node.json");

        write_gemini_node_config(&settings_path, &papers_path, "  ")
            .await
            .unwrap();

        assert!(!tmp.path().join("agent/protocol.md").exists());
        let settings: serde_json::Value =
            serde_json::from_slice(&tokio::fs::read(&settings_path).await.unwrap()).unwrap();
        assert!(settings.get("context").is_none());
    }
}
