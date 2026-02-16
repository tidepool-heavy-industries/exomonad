//! Agent identity propagation via task-local storage.
//!
//! In HTTP serve mode, multiple Gemini agents hit the same MCP server process.
//! Each agent gets a unique URL: `/agents/{name}/mcp`. The route handler extracts
//! the agent name from the path and sets it as a task-local via `with_agent_id`,
//! making it available to effect handlers without threading through the call stack.
//!
//! Fallback chain: task-local → EXOMONAD_AGENT_ID env var → directory name.

use crate::domain::{AgentName, BirthBranch};
use std::future::Future;

tokio::task_local! {
    /// Task-local agent identity, set by the per-agent route handler.
    pub static CURRENT_AGENT_ID: AgentName;
    /// Task-local birth-branch identity, set by the hook handler.
    pub static CURRENT_BIRTH_BRANCH: BirthBranch;
}

/// Run a future with a specific agent identity set in task-local storage.
/// Use this from route handlers to set identity before dispatching to MCP.
pub async fn with_agent_id<F, T>(agent_id: AgentName, f: F) -> T
where
    F: Future<Output = T>,
{
    CURRENT_AGENT_ID.scope(agent_id, f).await
}

/// Run a future with a specific birth-branch identity set in task-local storage.
pub async fn with_birth_branch<F, R>(branch: BirthBranch, f: F) -> R
where
    F: std::future::Future<Output = R>,
{
    CURRENT_BIRTH_BRANCH.scope(branch, f).await
}

/// Get the current birth-branch from task-local storage.
pub fn get_birth_branch() -> Option<BirthBranch> {
    CURRENT_BIRTH_BRANCH.try_with(|s| s.clone()).ok()
}

/// Get the current agent identity.
///
/// Resolution order:
/// 1. Task-local (set by per-agent URL route handler)
/// 2. EXOMONAD_AGENT_ID environment variable (set in Zellij tab)
/// 3. Current directory name (last resort)
pub fn get_agent_id() -> AgentName {
    // Try task-local first (HTTP serve mode, per-agent route)
    if let Ok(id) = CURRENT_AGENT_ID.try_with(|id| id.clone()) {
        return id;
    }

    // Fall back to env var (stdio mode or Zellij tab)
    if let Ok(id) = std::env::var("EXOMONAD_AGENT_ID") {
        return AgentName::from(id.as_str());
    }

    // Last resort: directory name
    let name = std::env::current_dir()
        .ok()
        .and_then(|path| path.file_name().map(|n| n.to_string_lossy().to_string()))
        .unwrap_or_else(|| "unknown-agent".to_string());
    AgentName::from(name.as_str())
}

/// Get the current agent identity as a raw String (for proto/logging boundaries).
pub fn get_agent_id_string() -> String {
    get_agent_id().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_agent_id_fallback() {
        let id = get_agent_id();
        assert!(!id.as_str().is_empty());
    }

    #[tokio::test]
    async fn test_task_local_scope() {
        let result = with_agent_id(AgentName::from("test-agent"), async { get_agent_id() }).await;
        assert_eq!(result.as_str(), "test-agent");
    }

    #[tokio::test]
    async fn test_task_local_not_set() {
        let id = get_agent_id();
        assert!(!id.as_str().is_empty());
    }

    #[tokio::test]
    async fn test_birth_branch_scope() {
        let result = with_birth_branch(BirthBranch::from("main.feature"), async {
            get_birth_branch()
        })
        .await;
        assert_eq!(result.unwrap().as_str(), "main.feature");
    }
}
