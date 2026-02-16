//! Agent effect handler for the `agent.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::agent`.

use crate::effects::{
    dispatch_agent_effect, AgentEffects, EffectError, EffectHandler, EffectResult,
};
use crate::services::agent_control::{
    AgentControlService, AgentInfo, AgentType as ServiceAgentType, SpawnGeminiTeammateOptions,
    SpawnOptions, SpawnSubtreeOptions, SpawnWorkerOptions,
};
use crate::services::claude_session_registry::ClaudeSessionRegistry;
use crate::{GithubOwner, GithubRepo, IssueNumber};
use async_trait::async_trait;
use exomonad_proto::effects::agent::*;
use std::sync::Arc;
use tracing::{info, warn};

/// Agent effect handler.
///
/// Handles all effects in the `agent.*` namespace by delegating to
/// the generated `dispatch_agent_effect` function.
pub struct AgentHandler {
    service: Arc<AgentControlService>,
    claude_session_registry: Option<Arc<ClaudeSessionRegistry>>,
}

impl AgentHandler {
    pub fn new(service: Arc<AgentControlService>) -> Self {
        Self {
            service,
            claude_session_registry: None,
        }
    }

    pub fn with_claude_session_registry(mut self, reg: Arc<ClaudeSessionRegistry>) -> Self {
        self.claude_session_registry = Some(reg);
        self
    }
}

#[async_trait]
impl EffectHandler for AgentHandler {
    fn namespace(&self) -> &str {
        "agent"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_agent_effect(self, effect_type, payload).await
    }
}

fn convert_agent_type(t: AgentType) -> ServiceAgentType {
    match t {
        AgentType::Claude => ServiceAgentType::Claude,
        AgentType::Gemini | AgentType::Unspecified => ServiceAgentType::Gemini,
    }
}

fn parse_issue_number(issue: &str) -> EffectResult<IssueNumber> {
    let n: u64 = issue
        .parse()
        .map_err(|_| EffectError::invalid_input(format!("Invalid issue number: {}", issue)))?;
    IssueNumber::try_from(n).map_err(|e| EffectError::invalid_input(e.to_string()))
}

fn parse_owner(owner: &str) -> EffectResult<GithubOwner> {
    GithubOwner::try_from(owner.to_string()).map_err(|e| EffectError::invalid_input(e.to_string()))
}

fn parse_repo(repo: &str) -> EffectResult<GithubRepo> {
    GithubRepo::try_from(repo.to_string()).map_err(|e| EffectError::invalid_input(e.to_string()))
}

#[async_trait]
impl AgentEffects for AgentHandler {
    async fn spawn(&self, req: SpawnRequest) -> EffectResult<SpawnResponse> {
        let issue_number = parse_issue_number(&req.issue)?;
        let options = SpawnOptions {
            owner: parse_owner(&req.owner)?,
            repo: parse_repo(&req.repo)?,
            agent_type: convert_agent_type(req.agent_type()),
            subrepo: if req.subrepo.is_empty() {
                None
            } else {
                Some(req.subrepo.clone())
            },
            base_branch: if req.base_branch.is_empty() {
                None
            } else {
                Some(req.base_branch.clone())
            },
        };

        let result = self
            .service
            .spawn_agent(issue_number, &options)
            .await
            .map_err(|e| EffectError::custom("agent_error", e.to_string()))?;

        Ok(SpawnResponse {
            agent: Some(spawn_result_to_proto(&req.issue, &result)),
        })
    }

    async fn spawn_batch(&self, req: SpawnBatchRequest) -> EffectResult<SpawnBatchResponse> {
        let agent_type = convert_agent_type(req.agent_type());
        let mut agents = Vec::new();
        let mut errors = Vec::new();

        for issue in &req.issues {
            let issue_number = match parse_issue_number(issue) {
                Ok(n) => n,
                Err(e) => {
                    errors.push(format!("Issue {}: {}", issue, e));
                    continue;
                }
            };
            let options = SpawnOptions {
                owner: parse_owner(&req.owner)?,
                repo: parse_repo(&req.repo)?,
                agent_type,
                subrepo: if req.subrepo.is_empty() {
                    None
                } else {
                    Some(req.subrepo.clone())
                },
                base_branch: None,
            };

            match self.service.spawn_agent(issue_number, &options).await {
                Ok(result) => agents.push(spawn_result_to_proto(issue, &result)),
                Err(e) => errors.push(format!("Issue {}: {}", issue, e)),
            }
        }

        Ok(SpawnBatchResponse { agents, errors })
    }

    async fn spawn_gemini_teammate(
        &self,
        req: SpawnGeminiTeammateRequest,
    ) -> EffectResult<SpawnGeminiTeammateResponse> {
        let options = SpawnGeminiTeammateOptions {
            name: req.name.clone(),
            prompt: req.prompt.clone(),
            agent_type: convert_agent_type(req.agent_type()),
            subrepo: if req.subrepo.is_empty() {
                None
            } else {
                Some(req.subrepo.clone())
            },
            base_branch: if req.base_branch.is_empty() {
                None
            } else {
                Some(req.base_branch.clone())
            },
        };

        let result = self
            .service
            .spawn_gemini_teammate(&options)
            .await
            .map_err(|e| EffectError::custom("agent_error", e.to_string()))?;

        Ok(SpawnGeminiTeammateResponse {
            agent: Some(teammate_result_to_proto(&req.name, &result)),
        })
    }

    async fn spawn_worker(&self, req: SpawnWorkerRequest) -> EffectResult<SpawnWorkerResponse> {
        let options = SpawnWorkerOptions {
            name: req.name.clone(),
            prompt: req.prompt.clone(),
        };

        let result = self
            .service
            .spawn_worker(&options)
            .await
            .map_err(|e| EffectError::custom("agent_error", e.to_string()))?;

        Ok(SpawnWorkerResponse {
            agent: Some(worker_result_to_proto(&req.name, &result)),
        })
    }

    async fn spawn_subtree(&self, req: SpawnSubtreeRequest) -> EffectResult<SpawnSubtreeResponse> {
        // Look up Claude session UUID from registry (overrides whatever WASM sent)
        let parent_session_id = if let Some(ref registry) = self.claude_session_registry {
            let registry_key = crate::mcp::agent_identity::get_agent_id();
            let key = if registry_key.is_empty() {
                "root".to_string()
            } else {
                registry_key
            };
            let claude_uuid = registry.get(&key).await;
            info!(
                key = %key,
                claude_uuid = ?claude_uuid,
                "Looked up Claude session UUID for spawn_subtree"
            );
            match claude_uuid {
                Some(uuid) => uuid,
                None => {
                    warn!(
                        key = %key,
                        "No Claude session UUID registered — child will start without --fork-session context. Ensure SessionStart hook is configured."
                    );
                    String::new()
                }
            }
        } else {
            req.parent_session_id.clone()
        };

        let options = SpawnSubtreeOptions {
            task: req.task.clone(),
            branch_name: req.branch_name.clone(),
            parent_session_id,
        };

        let result = self
            .service
            .spawn_subtree(&options)
            .await
            .map_err(|e| EffectError::custom("agent_error", e.to_string()))?;

        Ok(SpawnSubtreeResponse {
            agent: Some(subtree_result_to_proto(&req.branch_name, &result)),
        })
    }

    async fn spawn_leaf_subtree(
        &self,
        req: SpawnLeafSubtreeRequest,
    ) -> EffectResult<SpawnLeafSubtreeResponse> {
        let options = SpawnSubtreeOptions {
            task: req.task.clone(),
            branch_name: req.branch_name.clone(),
            parent_session_id: String::new(),
        };

        let result = self
            .service
            .spawn_leaf_subtree(&options)
            .await
            .map_err(|e| EffectError::custom("agent_error", e.to_string()))?;

        Ok(SpawnLeafSubtreeResponse {
            agent: Some(leaf_subtree_result_to_proto(&req.branch_name, &result)),
        })
    }

    async fn cleanup(&self, req: CleanupRequest) -> EffectResult<CleanupResponse> {
        match self.service.cleanup_agent(&req.issue, req.force).await {
            Ok(_) => Ok(CleanupResponse {
                success: true,
                error: String::new(),
            }),
            Err(e) => Ok(CleanupResponse {
                success: false,
                error: e.to_string(),
            }),
        }
    }

    async fn cleanup_batch(&self, req: CleanupBatchRequest) -> EffectResult<CleanupBatchResponse> {
        let subrepo = if req.subrepo.is_empty() {
            None
        } else {
            Some(req.subrepo.as_str())
        };

        let result = self
            .service
            .cleanup_agents(&req.issues, req.force, subrepo)
            .await;

        let failed_ids: Vec<String> = result.failed.iter().map(|(id, _)| id.clone()).collect();
        let errors: Vec<String> = result.failed.iter().map(|(_, err)| err.clone()).collect();

        Ok(CleanupBatchResponse {
            cleaned: result.cleaned,
            failed: failed_ids,
            errors,
        })
    }

    async fn cleanup_merged(
        &self,
        req: CleanupMergedRequest,
    ) -> EffectResult<CleanupMergedResponse> {
        let subrepo = if req.subrepo.is_empty() {
            None
        } else {
            Some(req.subrepo.as_str())
        };

        let result = self
            .service
            .cleanup_merged_agents(&req.issues, subrepo)
            .await
            .map_err(|e| EffectError::custom("agent_error", e.to_string()))?;

        let skipped: Vec<String> = result.failed.iter().map(|(id, _)| id.clone()).collect();
        let errors: Vec<String> = result.failed.iter().map(|(_, err)| err.clone()).collect();

        Ok(CleanupMergedResponse {
            cleaned: result.cleaned,
            skipped,
            errors,
        })
    }

    async fn list(&self, _req: ListRequest) -> EffectResult<ListResponse> {
        let infos = self
            .service
            .list_agents()
            .await
            .map_err(|e| EffectError::custom("agent_error", e.to_string()))?;

        let agents = infos.iter().map(service_info_to_proto).collect();
        Ok(ListResponse { agents })
    }
}

fn spawn_result_to_proto(
    issue: &str,
    result: &crate::services::agent_control::SpawnResult,
) -> exomonad_proto::effects::agent::AgentInfo {
    use crate::services::agent_control::Topology;

    exomonad_proto::effects::agent::AgentInfo {
        id: format!("{}-{}", issue, result.agent_type),
        issue: issue.to_string(),
        worktree_path: result.agent_dir.clone(),
        branch_name: String::new(),
        agent_type: if result.agent_type == "claude" {
            AgentType::Claude as i32
        } else {
            AgentType::Gemini as i32
        },
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: result.tab_name.clone(),
        error: String::new(),
        pr_number: 0,
        pr_url: String::new(),
        topology: Topology::WorktreePerAgent.to_proto(),
    }
}

fn teammate_result_to_proto(
    name: &str,
    result: &crate::services::agent_control::SpawnResult,
) -> exomonad_proto::effects::agent::AgentInfo {
    use crate::services::agent_control::Topology;

    exomonad_proto::effects::agent::AgentInfo {
        id: result.tab_name.clone(),
        issue: String::new(),
        worktree_path: String::new(),
        branch_name: String::new(),
        agent_type: if result.agent_type == "claude" {
            AgentType::Claude as i32
        } else {
            AgentType::Gemini as i32
        },
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: format!(
            "{} {}",
            if result.agent_type == "claude" {
                "\u{1F916}"
            } else {
                "\u{1F48E}"
            },
            name
        ),
        error: String::new(),
        pr_number: 0,
        pr_url: String::new(),
        topology: Topology::WorktreePerAgent.to_proto(),
    }
}

fn worker_result_to_proto(
    name: &str,
    result: &crate::services::agent_control::SpawnResult,
) -> exomonad_proto::effects::agent::AgentInfo {
    use crate::services::agent_control::Topology;

    exomonad_proto::effects::agent::AgentInfo {
        id: result.tab_name.clone(),
        issue: String::new(),
        worktree_path: String::new(),
        branch_name: String::new(),
        agent_type: AgentType::Gemini as i32,
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: format!("\u{1F48E} {}", name),
        error: String::new(),
        pr_number: 0,
        pr_url: String::new(),
        topology: Topology::SharedDir.to_proto(),
    }
}

fn subtree_result_to_proto(
    branch_name: &str,
    result: &crate::services::agent_control::SpawnResult,
) -> exomonad_proto::effects::agent::AgentInfo {
    use crate::services::agent_control::Topology;

    exomonad_proto::effects::agent::AgentInfo {
        id: result.tab_name.clone(),
        issue: String::new(),
        worktree_path: result.agent_dir.clone(),
        branch_name: branch_name.to_string(),
        agent_type: AgentType::Claude as i32,
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: format!("\u{1F916} {}", branch_name),
        error: String::new(),
        pr_number: 0,
        pr_url: String::new(),
        topology: Topology::WorktreePerAgent.to_proto(),
    }
}

fn leaf_subtree_result_to_proto(
    branch_name: &str,
    result: &crate::services::agent_control::SpawnResult,
) -> exomonad_proto::effects::agent::AgentInfo {
    use crate::services::agent_control::Topology;

    let agent_type = if result.agent_type == "claude" {
        AgentType::Claude
    } else {
        AgentType::Gemini
    };

    let emoji = if agent_type == AgentType::Claude {
        "\u{1F916}"
    } else {
        "\u{1F48E}"
    };

    exomonad_proto::effects::agent::AgentInfo {
        id: result.tab_name.clone(),
        issue: String::new(),
        worktree_path: result.agent_dir.clone(),
        branch_name: branch_name.to_string(),
        agent_type: agent_type as i32,
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: format!("{} {}", emoji, branch_name),
        error: String::new(),
        pr_number: 0,
        pr_url: String::new(),
        topology: Topology::WorktreePerAgent.to_proto(),
    }
}

fn service_info_to_proto(info: &AgentInfo) -> exomonad_proto::effects::agent::AgentInfo {
    let agent_type = match info.agent_type.as_deref() {
        Some("claude") => AgentType::Claude as i32,
        Some("gemini") => AgentType::Gemini as i32,
        _ => AgentType::Unspecified as i32,
    };

    let status = match info.status {
        crate::services::agent_control::AgentStatus::Running => AgentStatus::Running as i32,
        crate::services::agent_control::AgentStatus::Stopped => AgentStatus::Stopped as i32,
    };

    exomonad_proto::effects::agent::AgentInfo {
        id: info.issue_id.clone(),
        issue: info.issue_id.clone(),
        worktree_path: info.agent_dir.clone().unwrap_or_default(),
        branch_name: String::new(),
        agent_type,
        role: 0,
        status,
        zellij_tab: String::new(),
        error: String::new(),
        pr_number: info.pr.as_ref().map(|p| p.number as i32).unwrap_or(0),
        pr_url: info.pr.as_ref().map(|p| p.url.clone()).unwrap_or_default(),
        topology: info.topology.to_proto(),
    }
}
