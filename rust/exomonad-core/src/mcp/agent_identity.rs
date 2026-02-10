//! Agent identity propagation via task-local storage.
//!
//! In HTTP serve mode, multiple Gemini agents hit the same MCP server process.
//! Each agent gets a unique URL: `/agents/{name}/mcp`. The route handler extracts
//! the agent name from the path and sets it as a task-local via `with_agent_id`,
//! making it available to effect handlers without threading through the call stack.
//!
//! Fallback chain: task-local → EXOMONAD_AGENT_ID env var → directory name.

use std::future::Future;

tokio::task_local! {
    /// Task-local agent identity, set by the per-agent route handler.
    static CURRENT_AGENT_ID: String;
}

/// Run a future with a specific agent identity set in task-local storage.
/// Use this from route handlers to set identity before dispatching to MCP.
pub async fn with_agent_id<F, T>(agent_id: String, f: F) -> T
where
    F: Future<Output = T>,
{
    CURRENT_AGENT_ID.scope(agent_id, f).await
}

/// Get the current agent identity.
///
/// Resolution order:
/// 1. Task-local (set by per-agent URL route handler)
/// 2. EXOMONAD_AGENT_ID environment variable (set in Zellij tab)
/// 3. Current directory name (last resort)
pub fn get_agent_id() -> String {
    // Try task-local first (HTTP serve mode, per-agent route)
    if let Ok(id) = CURRENT_AGENT_ID.try_with(|id| id.clone()) {
        return id;
    }

    // Fall back to env var (stdio mode or Zellij tab)
    if let Ok(id) = std::env::var("EXOMONAD_AGENT_ID") {
        return id;
    }

    // Last resort: directory name
    std::env::current_dir()
        .ok()
        .and_then(|path| path.file_name().map(|n| n.to_string_lossy().to_string()))
        .unwrap_or_else(|| "unknown-agent".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_agent_id_fallback() {
        let id = get_agent_id();
        assert!(!id.is_empty());
    }

    #[tokio::test]
    async fn test_task_local_scope() {
        let result = with_agent_id("test-agent".to_string(), async {
            get_agent_id()
        })
        .await;
        assert_eq!(result, "test-agent");
    }

    #[tokio::test]
    async fn test_task_local_not_set() {
        let id = get_agent_id();
        assert!(!id.is_empty());
    }
}
