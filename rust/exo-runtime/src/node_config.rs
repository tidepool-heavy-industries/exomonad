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
    // stays one-shot in-process.
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

