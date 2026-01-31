//! Context file generation for spawned agents.
//!
//! Creates .exomonad/config.toml and INITIAL_CONTEXT.md in worktrees.

use anyhow::{Context, Result};
use std::path::Path;
use tokio::fs;
use tracing::info;

/// Write .exomonad/config.toml for the worktree.
pub async fn write_exomonad_config(worktree_path: &Path, role: &str) -> Result<()> {
    let exomonad_dir = worktree_path.join(".exomonad");
    fs::create_dir_all(&exomonad_dir)
        .await
        .context("Failed to create .exomonad directory")?;

    let config_path = exomonad_dir.join("config.toml");
    let content = format!(
        r#"# ExoMonad sidecar configuration
# Auto-generated for spawned agent

role = "{}"
"#,
        role
    );

    fs::write(&config_path, content)
        .await
        .context("Failed to write .exomonad/config.toml")?;

    info!(path = %config_path.display(), "Wrote .exomonad/config.toml");

    Ok(())
}

/// Write INITIAL_CONTEXT.md with issue details.
pub async fn write_initial_context(
    worktree_path: &Path,
    issue_id: &str,
    issue_title: &str,
    issue_body: &str,
    branch_name: &str,
    issue_url: &str,
) -> Result<()> {
    let context_path = worktree_path.join("INITIAL_CONTEXT.md");

    let content = format!(
        r#"# Issue #{issue_id}: {issue_title}

## Task

{issue_body}

## Details

- **Issue**: [{issue_id}]({issue_url})
- **Branch**: `{branch_name}`

## Instructions

1. Read the issue description above
2. Understand the requirements
3. Implement the necessary changes
4. Run tests to verify: `cargo test` or `just test`
5. Commit with message referencing the issue:
   ```
   git commit -m "[#{issue_id}] <brief summary>"
   ```
6. When complete, push your branch:
   ```
   git push -u origin {branch_name}
   ```

## Notes

- This worktree is isolated from the main repo
- Your changes won't affect other branches
- Use `/mcp` to verify MCP tools are connected
- Ask for help if you get stuck
"#,
        issue_id = issue_id,
        issue_title = issue_title,
        issue_body = issue_body,
        branch_name = branch_name,
        issue_url = issue_url,
    );

    fs::write(&context_path, content)
        .await
        .context("Failed to write INITIAL_CONTEXT.md")?;

    info!(path = %context_path.display(), "Wrote INITIAL_CONTEXT.md");

    Ok(())
}

/// Write .mcp.json for the worktree to connect to exomonad-sidecar.
pub async fn write_mcp_config(worktree_path: &Path, sidecar_path: &str) -> Result<()> {
    let mcp_path = worktree_path.join(".mcp.json");

    let content = format!(
        r#"{{
  "mcpServers": {{
    "exomonad": {{
      "type": "stdio",
      "command": "{}",
      "args": ["mcp-stdio"]
    }}
  }}
}}
"#,
        sidecar_path
    );

    fs::write(&mcp_path, content)
        .await
        .context("Failed to write .mcp.json")?;

    info!(path = %mcp_path.display(), "Wrote .mcp.json");

    Ok(())
}

