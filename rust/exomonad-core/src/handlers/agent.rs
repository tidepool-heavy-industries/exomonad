//! Agent effect handler for the `agent.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::agent`.

use crate::domain::{AgentName, BirthBranch, ClaudeSessionUuid};
use crate::effects::{
    dispatch_agent_effect, AgentEffects, EffectError, EffectHandler, EffectResult, ResultExt,
};

use super::non_empty;
use crate::services::agent_control::{
    AgentControlService, AgentInfo, AgentType as ServiceAgentType, SpawnGeminiTeammateOptions,
    SpawnOptions, SpawnSubtreeOptions, SpawnWorkerOptions,
};
use crate::services::claude_session_registry::ClaudeSessionRegistry;
use crate::{GithubOwner, GithubRepo, IssueNumber};
use async_trait::async_trait;
use exomonad_proto::effects::agent::*;
use std::sync::Arc;
use tracing::info;

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

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_agent_effect(self, effect_type, payload, ctx).await
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
    async fn spawn(
        &self,
        req: SpawnRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnResponse> {
        let issue_number = parse_issue_number(&req.issue)?;
        let options = SpawnOptions {
            owner: parse_owner(&req.owner)?,
            repo: parse_repo(&req.repo)?,
            agent_type: convert_agent_type(req.agent_type()),
            subrepo: non_empty(req.subrepo),
            base_branch: non_empty(req.base_branch).map(|s| BirthBranch::from(s.as_str())),
        };

        let result = self
            .service
            .spawn_agent(issue_number, &options, &ctx.birth_branch)
            .await
            .effect_err("agent")?;

        Ok(SpawnResponse {
            agent: Some(spawn_result_to_proto(&req.issue, &result)),
        })
    }

    async fn spawn_batch(
        &self,
        req: SpawnBatchRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnBatchResponse> {
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
                subrepo: non_empty(req.subrepo.clone()),
                base_branch: None,
            };

            match self
                .service
                .spawn_agent(issue_number, &options, &ctx.birth_branch)
                .await
            {
                Ok(result) => agents.push(spawn_result_to_proto(issue, &result)),
                Err(e) => errors.push(format!("Issue {}: {}", issue, e)),
            }
        }

        Ok(SpawnBatchResponse { agents, errors })
    }

    async fn spawn_gemini_teammate(
        &self,
        req: SpawnGeminiTeammateRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnGeminiTeammateResponse> {
        let options = SpawnGeminiTeammateOptions {
            name: AgentName::from(req.name.as_str()),
            prompt: req.prompt.clone(),
            agent_type: convert_agent_type(req.agent_type()),
            subrepo: non_empty(req.subrepo),
            base_branch: non_empty(req.base_branch).map(|s| BirthBranch::from(s.as_str())),
        };

        let result = self
            .service
            .spawn_gemini_teammate(&options, &ctx.birth_branch)
            .await
            .effect_err("agent")?;

        Ok(SpawnGeminiTeammateResponse {
            agent: Some(teammate_result_to_proto(&req.name, &result)),
        })
    }

    async fn spawn_worker(
        &self,
        req: SpawnWorkerRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnWorkerResponse> {
        let options = SpawnWorkerOptions {
            name: req.name.clone(),
            prompt: req.prompt.clone(),
        };

        let result = self
            .service
            .spawn_worker(&options, ctx)
            .await
            .effect_err("agent")?;

        Ok(SpawnWorkerResponse {
            agent: Some(worker_result_to_proto(&req.name, &result)),
        })
    }

    async fn spawn_subtree(
        &self,
        req: SpawnSubtreeRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnSubtreeResponse> {
        // fork-session disabled by default — stale/compacted session IDs cause
        // "No conversation found" errors in the child. Opt-in via parent_session_id field.
        let parent_session_id = if !req.parent_session_id.is_empty() {
            info!(
                session_id = %req.parent_session_id,
                "Using explicit parent_session_id for fork-session"
            );
            Some(ClaudeSessionUuid::from(req.parent_session_id.as_str()))
        } else {
            None
        };

        let options = SpawnSubtreeOptions {
            task: req.task.clone(),
            branch_name: req.branch_name.clone(),
            parent_session_id,
        };

        let result = self
            .service
            .spawn_subtree(&options, &ctx.birth_branch)
            .await
            .effect_err("agent")?;

        Ok(SpawnSubtreeResponse {
            agent: Some(subtree_result_to_proto(&req.branch_name, &result)),
        })
    }

    async fn spawn_leaf_subtree(
        &self,
        req: SpawnLeafSubtreeRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnLeafSubtreeResponse> {
        let options = SpawnSubtreeOptions {
            task: req.task.clone(),
            branch_name: req.branch_name.clone(),
            parent_session_id: None,
        };

        let result = self
            .service
            .spawn_leaf_subtree(&options, &ctx.birth_branch)
            .await
            .effect_err("agent")?;

        Ok(SpawnLeafSubtreeResponse {
            agent: Some(leaf_subtree_result_to_proto(&req.branch_name, &result)),
        })
    }

    async fn cleanup(
        &self,
        req: CleanupRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<CleanupResponse> {
        match self.service.cleanup_agent(&req.issue).await {
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

    async fn cleanup_batch(
        &self,
        req: CleanupBatchRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<CleanupBatchResponse> {
        let subrepo = non_empty(req.subrepo);
        let result = self
            .service
            .cleanup_agents(&req.issues, subrepo.as_deref())
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
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<CleanupMergedResponse> {
        let subrepo = non_empty(req.subrepo);
        let result = self
            .service
            .cleanup_merged_agents(&req.issues, subrepo.as_deref())
            .await
            .effect_err("agent")?;

        let skipped: Vec<String> = result.failed.iter().map(|(id, _)| id.clone()).collect();
        let errors: Vec<String> = result.failed.iter().map(|(_, err)| err.clone()).collect();

        Ok(CleanupMergedResponse {
            cleaned: result.cleaned,
            skipped,
            errors,
        })
    }

    async fn list(
        &self,
        _req: ListRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ListResponse> {
        let infos = self.service.list_agents().await.effect_err("agent")?;

        let agents = infos.iter().map(service_info_to_proto).collect();
        Ok(ListResponse { agents })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::EffectContext;
    use crate::services::git_worktree::GitWorktreeService;
    use std::path::PathBuf;

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from("main"),
        }
    }

    fn test_handler() -> AgentHandler {
        let dir = PathBuf::from(".");
        let git_wt = Arc::new(GitWorktreeService::new(dir.clone()));
        let service = Arc::new(AgentControlService::new(dir, None, git_wt));
        AgentHandler::new(service)
    }

    #[test]
    fn test_namespace() {
        let _ctx = test_ctx();
        let handler = test_handler();
        assert_eq!(handler.namespace(), "agent");
    }

    #[test]
    fn test_convert_agent_type() {
        assert_eq!(
            convert_agent_type(AgentType::Claude),
            ServiceAgentType::Claude
        );
        assert_eq!(
            convert_agent_type(AgentType::Gemini),
            ServiceAgentType::Gemini
        );
        assert_eq!(
            convert_agent_type(AgentType::Unspecified),
            ServiceAgentType::Gemini
        );
    }
}

fn spawn_result_to_proto(
    issue: &str,
    result: &crate::services::agent_control::SpawnResult,
) -> exomonad_proto::effects::agent::AgentInfo {
    use crate::services::agent_control::Topology;

    exomonad_proto::effects::agent::AgentInfo {
        id: format!("{}-{}", issue, result.agent_type.suffix()),
        issue: issue.to_string(),
        worktree_path: result.agent_dir.display().to_string(),
        branch_name: String::new(),
        agent_type: service_agent_type_to_proto(result.agent_type),
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
        agent_type: service_agent_type_to_proto(result.agent_type),
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: result.agent_type.tab_display_name(name),
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
        zellij_tab: ServiceAgentType::Gemini.tab_display_name(name),
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
        worktree_path: result.agent_dir.display().to_string(),
        branch_name: branch_name.to_string(),
        agent_type: AgentType::Claude as i32,
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: ServiceAgentType::Claude.tab_display_name(branch_name),
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

    exomonad_proto::effects::agent::AgentInfo {
        id: result.tab_name.clone(),
        issue: String::new(),
        worktree_path: result.agent_dir.display().to_string(),
        branch_name: branch_name.to_string(),
        agent_type: service_agent_type_to_proto(result.agent_type),
        role: 0,
        status: AgentStatus::Running as i32,
        zellij_tab: result.agent_type.tab_display_name(branch_name),
        error: String::new(),
        pr_number: 0,
        pr_url: String::new(),
        topology: Topology::WorktreePerAgent.to_proto(),
    }
}

fn service_agent_type_to_proto(at: ServiceAgentType) -> i32 {
    match at {
        ServiceAgentType::Claude => AgentType::Claude as i32,
        ServiceAgentType::Gemini => AgentType::Gemini as i32,
    }
}

fn service_info_to_proto(info: &AgentInfo) -> exomonad_proto::effects::agent::AgentInfo {
    let agent_type = match info.agent_type {
        Some(ServiceAgentType::Claude) => AgentType::Claude as i32,
        Some(ServiceAgentType::Gemini) => AgentType::Gemini as i32,
        None => AgentType::Unspecified as i32,
    };

    let status = match info.status {
        crate::services::agent_control::AgentStatus::Running => AgentStatus::Running as i32,
        crate::services::agent_control::AgentStatus::Stopped => AgentStatus::Stopped as i32,
    };

    exomonad_proto::effects::agent::AgentInfo {
        id: info.issue_id.clone(),
        issue: info.issue_id.clone(),
        worktree_path: info
            .agent_dir
            .as_ref()
            .map(|p| p.display().to_string())
            .unwrap_or_default(),
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
