//! Agent effect handler for the `agent.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::agent`.

use crate::services::agent_control::{
    AgentControlService, AgentInfo, AgentType as ServiceAgentType, SpawnOptions,
};
use async_trait::async_trait;
use exomonad_core::effects::{
    dispatch_agent_effect, AgentEffects, EffectError, EffectHandler, EffectResult,
};
use exomonad_proto::effects::agent::*;
use exomonad_shared::{GithubOwner, GithubRepo, IssueNumber};
use std::sync::Arc;

/// Agent effect handler.
///
/// Handles all effects in the `agent.*` namespace by delegating to
/// the generated `dispatch_agent_effect` function.
pub struct AgentHandler {
    service: Arc<AgentControlService>,
}

impl AgentHandler {
    pub fn new(service: Arc<AgentControlService>) -> Self {
        Self { service }
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
    IssueNumber::try_from(n)
        .map_err(|e| EffectError::invalid_input(e.to_string()))
}

fn parse_owner(owner: &str) -> EffectResult<GithubOwner> {
    GithubOwner::try_from(owner.to_string())
        .map_err(|e| EffectError::invalid_input(e.to_string()))
}

fn parse_repo(repo: &str) -> EffectResult<GithubRepo> {
    GithubRepo::try_from(repo.to_string())
        .map_err(|e| EffectError::invalid_input(e.to_string()))
}

#[async_trait]
impl AgentEffects for AgentHandler {
    async fn spawn(&self, req: SpawnRequest) -> EffectResult<SpawnResponse> {
        let issue_number = parse_issue_number(&req.issue)?;
        let options = SpawnOptions {
            owner: parse_owner(&req.owner)?,
            repo: parse_repo(&req.repo)?,
            agent_type: convert_agent_type(req.agent_type()),
            worktree_dir: if req.worktree_dir.is_empty() {
                None
            } else {
                Some(req.worktree_dir.clone())
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
                worktree_dir: if req.worktree_dir.is_empty() {
                    None
                } else {
                    Some(req.worktree_dir.clone())
                },
            };

            match self.service.spawn_agent(issue_number, &options).await {
                Ok(result) => agents.push(spawn_result_to_proto(issue, &result)),
                Err(e) => errors.push(format!("Issue {}: {}", issue, e)),
            }
        }

        Ok(SpawnBatchResponse { agents, errors })
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
        let result = self.service.cleanup_agents(&req.issues, req.force).await;

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
        _req: CleanupMergedRequest,
    ) -> EffectResult<CleanupMergedResponse> {
        let result = self
            .service
            .cleanup_merged_agents()
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
    exomonad_proto::effects::agent::AgentInfo {
        id: format!("{}-{}", issue, result.agent_type),
        issue: issue.to_string(),
        worktree_path: result.worktree_path.clone(),
        branch_name: result.branch_name.clone(),
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
        crate::services::agent_control::AgentStatus::OrphanWorktree => {
            AgentStatus::Stopped as i32
        }
        crate::services::agent_control::AgentStatus::OrphanTab => AgentStatus::Stopped as i32,
    };

    exomonad_proto::effects::agent::AgentInfo {
        id: info.issue_id.clone(),
        issue: info.issue_id.clone(),
        worktree_path: info.worktree_path.clone().unwrap_or_default(),
        branch_name: info.branch_name.clone().unwrap_or_default(),
        agent_type,
        role: 0,
        status,
        zellij_tab: String::new(),
        error: String::new(),
        pr_number: info.pr.as_ref().map(|p| p.number as i32).unwrap_or(0),
        pr_url: info.pr.as_ref().map(|p| p.url.clone()).unwrap_or_default(),
    }
}
