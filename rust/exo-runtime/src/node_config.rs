use std::path::Path;
use tokio::io::AsyncWriteExt;

/// Write a node's Claude config (MCP server + hooks) to its **private** paths — the `claude`
/// process is pointed at them via `--mcp-config <mcp_path>` / `--settings <settings_path>`, so the
/// node NEVER writes the shared cwd's `.mcp.json` / `.claude/settings.local.json`. This is what keeps
/// an inline worker (which shares the parent's cwd) from clobbering the parent's config, and keeps
/// `.mcp.json` out of the repo entirely. Identity-free: used for both root and child agents.
///
/// `mcp_path` and `settings_path` are siblings of the node's papers
/// (see `exo_caps::paths::node_config_paths`); both parents are created. `papers_path` is the node's
/// papers, baked into the MCP server args + the hook commands.
pub async fn write_node_agent_config(
    settings_path: &Path,
    mcp_path: &Path,
    papers_path: &Path,
) -> std::io::Result<()> {
    let esc = |s: &str| shell_escape::escape(s.to_string().into()).into_owned();

    // 1. MCP sidecar config (loaded via `claude --mcp-config <mcp_path>`, merged over the cwd's).
    let mcp_config = serde_json::json!({
        "mcpServers": {
            "exomonad": {
                "type": "stdio",
                "command": exo_caps::invocation::BIN,
                "args": exo_caps::invocation::node_args(&papers_path.to_string_lossy())
            }
        }
    });

    if let Some(parent) = mcp_path.parent() {
        tokio::fs::create_dir_all(parent).await?;
    }
    let mcp_json = serde_json::to_vec_pretty(&mcp_config)
        .map_err(|e| std::io::Error::other(format!("mcp_config encode: {e}")))?;
    let mut f = tokio::fs::File::create(mcp_path).await?;
    f.write_all(&mcp_json).await?;
    f.sync_all().await?;

    // 2. CC node-mode hooks (loaded via `claude --settings <settings_path>`, merged over the cwd's).
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

    if let Some(parent) = settings_path.parent() {
        tokio::fs::create_dir_all(parent).await?;
    }
    let settings_json = serde_json::to_vec_pretty(&settings)
        .map_err(|e| std::io::Error::other(format!("settings encode: {e}")))?;
    let mut f = tokio::fs::File::create(settings_path).await?;
    f.write_all(&settings_json).await?;
    f.sync_all().await?;

    Ok(())
}
