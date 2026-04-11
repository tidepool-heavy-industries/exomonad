//! Agent effect handler for the `agent.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::agent`.

use crate::domain::{AgentName, AgentPermissions, BirthBranch, ClaudeSessionUuid, TeamName};
use crate::effects::{
    dispatch_agent_effect, AgentEffects, EffectError, EffectHandler, EffectResult, ResultExt,
    ResultExtPreserve,
};

use super::non_empty;
use crate::services::agent_control::{
    AgentControlService, AgentInfo, AgentType as ServiceAgentType, ClaudeSpawnFlags,
    SpawnGeminiTeammateOptions, SpawnLeafOptions, SpawnOptions, SpawnSubtreeOptions,
    SpawnWorkerOptions,
};
use crate::services::supervisor_registry::SupervisorInfo;
use crate::{GithubOwner, GithubRepo, IssueNumber};
use async_trait::async_trait;
use exomonad_proto::effects::agent::*;
use std::path::PathBuf;
use std::sync::Arc;
use tracing::{info, warn};

use crate::services::{
    HasAcpRegistry, HasAgentResolver, HasClaudeSessionRegistry, HasEventLog, HasGitHubClient,
    HasGitWorktreeService, HasProjectDir, HasSupervisorRegistry, HasTeamRegistry, HasTmuxIpc,
};

/// Agent effect handler.
///
/// Handles all effects in the `agent.*` namespace by delegating to
/// the generated `dispatch_agent_effect` function.
pub struct AgentHandler<C> {
    service: Arc<AgentControlService<C>>,
    ctx: Arc<C>,
}

impl<
        C: HasTeamRegistry
            + HasAcpRegistry
            + HasAgentResolver
            + HasGitHubClient
            + HasProjectDir
            + HasGitWorktreeService
            + HasSupervisorRegistry
            + HasClaudeSessionRegistry
            + HasEventLog
            + HasTmuxIpc
            + 'static,
    > AgentHandler<C>
{
    pub fn new(service: Arc<AgentControlService<C>>, ctx: Arc<C>) -> Self {
        Self { service, ctx }
    }

    /// Auto-register a spawned child in the SupervisorRegistry.
    /// Resolves the caller's team from TeamRegistry, then maps the child key
    /// to the caller as supervisor.
    async fn register_child_supervisor(
        &self,
        child_key: &str,
        ctx: &crate::effects::EffectContext,
    ) {
        let sup_reg = self.ctx.supervisor_registry();
        let team_reg = self.ctx.team_registry();
        let agent_key = ctx.agent_name.to_string();
        let team_name = if let Some(info) = team_reg.get(&agent_key).await {
            TeamName::from(info.team_name.as_str())
        } else if let Some(info) = team_reg.get(ctx.birth_branch.as_ref()).await {
            TeamName::from(info.team_name.as_str())
        } else {
            warn!(
                agent = %agent_key,
                child = %child_key,
                "No team found for agent — skipping supervisor registration"
            );
            return;
        };

        sup_reg
            .register(
                &[child_key.to_string()],
                SupervisorInfo {
                    supervisor: ctx.agent_name.clone(),
                    team: team_name,
                },
            )
            .await;
    }

    /// Register a spawned child as a synthetic member in the TL's actual team.
    ///
    /// Resolves the team from TeamRegistry (same pattern as `register_child_supervisor`)
    /// so the child is registered in the user-created team (e.g., "gh-issues"), not
    /// a hardcoded "exo-{branch}" team that CC doesn't recognize.
    async fn register_synthetic_member(
        &self,
        member_name: &AgentName,
        agent_type: &str,
        ctx: &crate::effects::EffectContext,
    ) {
        let team_reg = self.ctx.team_registry();
        let agent_key = ctx.agent_name.to_string();
        let team_name = if let Some(info) = team_reg.get(&agent_key).await {
            TeamName::from(info.team_name.as_str())
        } else if let Some(info) = team_reg.get(ctx.birth_branch.as_ref()).await {
            TeamName::from(info.team_name.as_str())
        } else {
            warn!(
                member = %member_name,
                "No team found — skipping synthetic member registration"
            );
            return;
        };
        if let Err(e) = crate::services::synthetic_members::register_synthetic_member(
            &team_name,
            member_name,
            agent_type,
        ) {
            warn!(
                member = %member_name,
                team = %team_name,
                error = %e,
                "Failed to register synthetic team member (non-fatal)"
            );
        }
    }

    /// Propagate the parent's team registration to a spawned sub-TL's identity keys.
    ///
    /// Sub-TLs don't call TeamCreate — they're part of the parent's team. But when
    /// a sub-TL spawns workers, `register_synthetic_member` looks up the sub-TL's
    /// keys in TeamRegistry and finds nothing. This method bridges that gap by
    /// registering the sub-TL's keys (agent_name, birth_branch) pointing to the
    /// parent's team.
    async fn propagate_team_to_child(
        &self,
        child_branch: &str,
        ctx: &crate::effects::EffectContext,
    ) {
        let team_reg = self.ctx.team_registry();
        let agent_key = ctx.agent_name.to_string();
        let parent_team = if let Some(info) = team_reg.get(&agent_key).await {
            info
        } else if let Some(info) = team_reg.get(ctx.birth_branch.as_ref()).await {
            info
        } else {
            warn!(
                child = %child_branch,
                "No team found for parent — skipping team propagation to sub-TL"
            );
            return;
        };

        // Derive the sub-TL's identity keys from the branch name.
        // Compute agent_name first, then use it for birth_branch (unified namespace).
        let child_identity = crate::services::agent_control::AgentIdentity::new(
            crate::services::agent_control::slugify(child_branch),
            crate::services::agent_control::AgentType::Claude,
        );
        let child_agent_name = child_identity.internal_name();
        let child_birth_branch = format!("{}.{}", ctx.birth_branch, child_agent_name);

        info!(
            child_agent = %child_agent_name,
            child_branch = %child_birth_branch,
            team = %parent_team.team_name,
            "Propagating parent team to sub-TL"
        );

        let team_info = claude_teams_bridge::TeamInfo {
            team_name: parent_team.team_name.clone(),
            inbox_name: parent_team.inbox_name.clone(),
        };

        // Register under agent_name and slug — NOT birth_branch.
        // Sub-TLs don't have CC Teams inboxes (they never call TeamCreate),
        // so registering their birth_branch would cause deliver_to_agent to
        // write to the parent's inbox (wrong recipient) instead of falling
        // back to tmux (correct recipient).
        team_reg
            .register(child_agent_name.as_str(), team_info.clone())
            .await;

        let slug = child_identity.slug();
        if slug != child_agent_name.as_str() {
            team_reg.register(slug, team_info).await;
        }
    }
}

#[async_trait]
impl<
        C: HasTeamRegistry
            + HasAcpRegistry
            + HasAgentResolver
            + HasGitHubClient
            + HasProjectDir
            + HasGitWorktreeService
            + HasSupervisorRegistry
            + HasClaudeSessionRegistry
            + HasEventLog
            + HasTmuxIpc
            + 'static,
    > EffectHandler for AgentHandler<C>
{
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

fn claude_spawn_flags(
    permission_mode: String,
    allowed_tools: Vec<String>,
    disallowed_tools: Vec<String>,
) -> ClaudeSpawnFlags {
    use crate::domain::PermissionMode;
    let mode = if permission_mode.is_empty() {
        None
    } else {
        Some(
            serde_json::from_value::<PermissionMode>(serde_json::Value::String(permission_mode))
                .unwrap_or_default(),
        )
    };
    ClaudeSpawnFlags {
        permission_mode: mode,
        allowed_tools,
        disallowed_tools,
    }
}

fn convert_agent_type(t: AgentType) -> EffectResult<ServiceAgentType> {
    match t {
        AgentType::Claude => Ok(ServiceAgentType::Claude),
        AgentType::Gemini => Ok(ServiceAgentType::Gemini),
        AgentType::Shoal => Ok(ServiceAgentType::Shoal),
        AgentType::Unspecified => Err(EffectError::invalid_input(
            "agent_type is required (must be 'claude', 'gemini', or 'shoal', got UNSPECIFIED)",
        )),
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
impl<
        C: HasTeamRegistry
            + HasAcpRegistry
            + HasAgentResolver
            + HasGitHubClient
            + HasProjectDir
            + HasGitWorktreeService
            + HasSupervisorRegistry
            + HasClaudeSessionRegistry
            + HasEventLog
            + HasTmuxIpc
            + 'static,
    > AgentEffects for AgentHandler<C>
{
    async fn spawn(
        &self,
        req: SpawnRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnResponse> {
        let issue_number = parse_issue_number(&req.issue)?;
        let options = SpawnOptions {
            owner: parse_owner(&req.owner)?,
            repo: parse_repo(&req.repo)?,
            agent_type: convert_agent_type(req.agent_type())?,
            subrepo: non_empty(req.subrepo).map(PathBuf::from),
            base_branch: non_empty(req.base_branch).map(|s| BirthBranch::from(s.as_str())),
        };

        let result = self
            .service
            .spawn_agent(issue_number, &options, &ctx.birth_branch)
            .await
            .effect_err_preserve("agent")?;

        Ok(SpawnResponse {
            agent: Some(spawn_result_to_proto(&req.issue, &result)),
        })
    }

    async fn spawn_batch(
        &self,
        req: SpawnBatchRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnBatchResponse> {
        let agent_type = convert_agent_type(req.agent_type())?;
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
                subrepo: non_empty(req.subrepo.clone()).map(PathBuf::from),
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
            agent_type: convert_agent_type(req.agent_type())?,
            subrepo: non_empty(req.subrepo).map(PathBuf::from),
            base_branch: non_empty(req.base_branch).map(|s| BirthBranch::from(s.as_str())),
        };

        let result = self
            .service
            .spawn_gemini_teammate(&options, &ctx.birth_branch)
            .await
            .effect_err_preserve("agent")?;

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
            name: AgentName::from(req.name.as_str()),
            prompt: req.prompt.clone(),
            claude_flags: claude_spawn_flags(
                req.permission_mode.clone(),
                req.allowed_tools.clone(),
                req.disallowed_tools.clone(),
            ),
        };

        let result = self
            .service
            .spawn_worker(&options, ctx)
            .await
            .effect_err_preserve("agent")?;

        let agent_info = worker_result_to_proto(&req.name, &result);

        tracing::info!(
            otel.name = "agent.spawned",
            child_agent = %agent_info.id,
            agent_type = %AgentType::try_from(agent_info.agent_type).map(|t| format!("{:?}", t)).unwrap_or_else(|_| "unknown".to_string()),
            branch = %agent_info.branch_name,
            spawn_type = "worker",
            "[event] agent.spawned"
        );
        if let Some(log) = self.ctx.event_log() {
            let _ = log.append(
                "agent.spawned",
                ctx.agent_name.as_ref(),
                &serde_json::json!({
                    "child_agent": agent_info.id, "agent_type": "gemini", "spawn_type": "worker",
                    "branch": agent_info.branch_name,
                }),
            );
        }

        // Register as synthetic team member using internal_name (slug-gemini),
        // matching what notify_parent sends as `from`.
        let identity = crate::services::agent_control::AgentIdentity::new(
            crate::services::agent_control::slugify(&req.name),
            crate::services::agent_control::AgentType::Gemini,
        );
        self.register_synthetic_member(&identity.internal_name(), "gemini-worker", ctx)
            .await;
        self.register_child_supervisor(&req.name, ctx).await;

        Ok(SpawnWorkerResponse {
            agent: Some(agent_info),
        })
    }

    async fn spawn_subtree(
        &self,
        req: SpawnSubtreeRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnSubtreeResponse> {
        // Only look up session for --fork-session when explicitly requested.
        // Default (fork_session=false) starts the child fresh — avoids stale/compacted
        // session IDs causing "No conversation found" errors.
        let parent_session_id = if req.fork_session {
            let key = if ctx.agent_name.as_str().is_empty() {
                crate::domain::AgentName::from("root")
            } else {
                ctx.agent_name.clone()
            };
            let claude_uuid = self.ctx.claude_session_registry().get(key.as_str()).await;
            info!(
                key = %key,
                claude_uuid = ?claude_uuid,
                "Looked up Claude session UUID for spawn_subtree (fork_session=true)"
            );
            if claude_uuid.is_none() {
                warn!(
                    key = %key,
                    "No Claude session UUID registered — child will start without --fork-session context. Ensure SessionStart hook is configured."
                );
            }
            claude_uuid.map(|s| ClaudeSessionUuid::from(s.as_str()))
        } else {
            info!("fork_session=false, child starts fresh");
            None
        };

        let options = SpawnSubtreeOptions {
            task: req.task.clone(),
            branch_name: req.branch_name.clone(),
            parent_session_id,
            role: non_empty(req.role.clone()).map(crate::domain::Role::new),
            agent_type: convert_agent_type(req.agent_type())?,
            claude_flags: claude_spawn_flags(
                req.permission_mode.clone(),
                req.allowed_tools.clone(),
                req.disallowed_tools.clone(),
            ),
            working_dir: non_empty(req.working_dir).map(PathBuf::from),
            permissions: req.permissions.map(|p| AgentPermissions {
                allow: p.allow,
                deny: p.deny,
                default_mode: None,
            }),
            standalone_repo: req.standalone_repo,
            allowed_dirs: req.allowed_dirs,
        };

        let result = self
            .service
            .spawn_subtree(&options, &ctx.birth_branch)
            .await
            .effect_err_preserve("agent")?;

        let agent_info = subtree_result_to_proto(&req.branch_name, &result);

        tracing::info!(
            otel.name = "agent.spawned",
            child_agent = %agent_info.id,
            agent_type = %AgentType::try_from(agent_info.agent_type).map(|t| format!("{:?}", t)).unwrap_or_else(|_| "unknown".to_string()),
            branch = %agent_info.branch_name,
            spawn_type = "subtree",
            "[event] agent.spawned"
        );
        if let Some(log) = self.ctx.event_log() {
            let _ = log.append(
                "agent.spawned",
                ctx.agent_name.as_ref(),
                &serde_json::json!({
                    "child_agent": agent_info.id, "agent_type": "claude", "spawn_type": "subtree",
                    "branch": agent_info.branch_name,
                }),
            );
        }

        self.register_child_supervisor(&req.branch_name, ctx).await;

        // Register sub-TL as synthetic member so it can receive Teams inbox messages
        let child_identity = crate::services::agent_control::AgentIdentity::new(
            crate::services::agent_control::slugify(&req.branch_name),
            crate::services::agent_control::AgentType::Claude,
        );
        self.register_synthetic_member(&child_identity.internal_name(), "claude-subtree", ctx)
            .await;

        // Propagate parent's team to sub-TL's identity keys so the sub-TL can
        // register its own workers as synthetic members when it spawns them
        self.propagate_team_to_child(&req.branch_name, ctx).await;

        Ok(SpawnSubtreeResponse {
            agent: Some(agent_info),
        })
    }

    async fn spawn_leaf_subtree(
        &self,
        req: SpawnLeafSubtreeRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnLeafSubtreeResponse> {
        let options = SpawnLeafOptions {
            task: req.task.clone(),
            branch_name: req.branch_name.clone(),
            role: non_empty(req.role.clone()).map(crate::domain::Role::new),
            agent_type: convert_agent_type(req.agent_type())?,
            claude_flags: claude_spawn_flags(
                req.permission_mode.clone(),
                req.allowed_tools.clone(),
                req.disallowed_tools.clone(),
            ),
            standalone_repo: req.standalone_repo,
            allowed_dirs: req.allowed_dirs,
        };

        let result = self
            .service
            .spawn_leaf_subtree(&options, &ctx.birth_branch)
            .await
            .effect_err_preserve("agent")?;

        let agent_info = leaf_subtree_result_to_proto(&req.branch_name, &result);

        tracing::info!(
            otel.name = "agent.spawned",
            child_agent = %agent_info.id,
            agent_type = %AgentType::try_from(agent_info.agent_type).map(|t| format!("{:?}", t)).unwrap_or_else(|_| "unknown".to_string()),
            branch = %agent_info.branch_name,
            spawn_type = "leaf_subtree",
            "[event] agent.spawned"
        );
        if let Some(log) = self.ctx.event_log() {
            let _ = log.append("agent.spawned", ctx.agent_name.as_ref(), &serde_json::json!({
                "child_agent": agent_info.id, "agent_type": "gemini", "spawn_type": "leaf_subtree",
                "branch": agent_info.branch_name,
            }));
        }

        // Register as synthetic team member using internal_name (slug-gemini),
        // matching what notify_parent sends as `from`.
        let leaf_identity = crate::services::agent_control::AgentIdentity::new(
            crate::services::agent_control::slugify(&req.branch_name),
            crate::services::agent_control::AgentType::Gemini,
        );
        self.register_synthetic_member(&leaf_identity.internal_name(), "gemini-leaf", ctx)
            .await;
        self.register_child_supervisor(&req.branch_name, ctx).await;

        Ok(SpawnLeafSubtreeResponse {
            agent: Some(agent_info),
        })
    }

    async fn spawn_acp(
        &self,
        req: SpawnAcpRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SpawnAcpResponse> {
        let registry = self.ctx.acp_registry();

        // Resolve working directory from context
        let working_dir = ctx.working_dir.clone();

        // Generate MCP settings for the agent using stdio transport
        let agent_name = AgentName::try_from(req.name.clone()).effect_err("agent")?;
        let context_path = self
            .service
            .resolve_role_context(&crate::domain::Role::worker());
        let settings_json = AgentControlService::<C>::generate_gemini_worker_settings(
            agent_name.as_str(),
            context_path.as_deref(),
            &self.service.extra_mcp_servers,
        );

        // Write settings to agent config dir
        let agent_dir = working_dir.join(format!(".exo/agents/{}", agent_name));
        tokio::fs::create_dir_all(&agent_dir)
            .await
            .effect_err("agent")?;
        let settings_path = agent_dir.join("settings.json");
        tokio::fs::write(
            &settings_path,
            serde_json::to_string_pretty(&settings_json).effect_err("agent")?,
        )
        .await
        .effect_err("agent")?;

        info!(
            agent = %agent_name,
            settings = %settings_path.display(),
            "Wrote ACP agent settings"
        );

        let env_vars = vec![
            (
                "GEMINI_CLI_SYSTEM_SETTINGS_PATH".into(),
                settings_path.to_string_lossy().into_owned(),
            ),
            ("EXOMONAD_AGENT_ID".into(), agent_name.to_string()),
        ];

        let conn = crate::services::acp_registry::connect_and_prompt(
            agent_name.clone(),
            "gemini",
            &working_dir,
            &req.prompt,
            env_vars,
        )
        .await
        .effect_err("agent")?;

        registry.register(conn).await;

        // Register as synthetic team member (uses TL's actual team, not hardcoded exo-{branch})
        self.register_synthetic_member(&agent_name, "gemini-acp", ctx)
            .await;

        info!(agent = %agent_name, "ACP agent spawned and registered");

        let agent_info = exomonad_proto::effects::agent::AgentInfo {
            id: agent_name.to_string(),
            issue: String::new(),
            worktree_path: String::new(),
            branch_name: String::new(),
            agent_type: AgentType::Gemini as i32,
            role: 0,
            alive: true,
            mux_window: String::new(),
            error: String::new(),
            pr_number: 0,
            pr_url: String::new(),
            topology: exomonad_proto::effects::agent::WorkspaceTopology::SharedDir as i32,
        };

        tracing::info!(
            otel.name = "agent.spawned",
            child_agent = %agent_info.id,
            agent_type = %AgentType::try_from(agent_info.agent_type).map(|t| format!("{:?}", t)).unwrap_or_else(|_| "unknown".to_string()),
            branch = %agent_info.branch_name,
            spawn_type = "acp",
            "[event] agent.spawned"
        );
        if let Some(log) = self.ctx.event_log() {
            let _ = log.append(
                "agent.spawned",
                ctx.agent_name.as_ref(),
                &serde_json::json!({
                    "child_agent": agent_info.id, "agent_type": "gemini", "spawn_type": "acp",
                    "branch": agent_info.branch_name,
                }),
            );
        }

        self.register_child_supervisor(&req.name, ctx).await;

        Ok(SpawnAcpResponse {
            agent: Some(agent_info),
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

    async fn close_self(
        &self,
        _req: CloseSelfRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<CloseSelfResponse> {
        let agent_key = ctx.agent_name.to_string();
        let agents_dir = std::path::Path::new(".exo/agents");

        // FIXME: Routing is written under internal_name (slug-suffix, e.g. "beta-gemini")
        // but MCP config passes bare slug as --name (e.g. "beta"). This suffix probing
        // is a band-aid — the real fix is making agent_name consistent between MCP config
        // and routing.json (either always include the suffix, or never).
        let candidates = std::iter::once(agent_key.clone()).chain(
            ["gemini", "claude", "shoal"]
                .iter()
                .map(|suffix| format!("{}-{}", agent_key, suffix)),
        );

        let mut routing = None;
        let mut resolved_internal_name = agent_key.clone();
        for candidate in candidates {
            let path = agents_dir.join(&candidate).join("routing.json");
            if let Ok(content) = std::fs::read_to_string(&path) {
                if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&content) {
                    info!(agent = %ctx.agent_name, path = %path.display(), "Found routing.json");
                    resolved_internal_name = candidate;
                    routing = Some(parsed);
                    break;
                }
            }
        }

        let mut closed = false;

        if let Some(ref r) = routing {
            // Try pane_id first (ephemeral workers)
            if let Some(pane_id) = r["pane_id"].as_str() {
                info!(agent = %ctx.agent_name, pane_id = %pane_id, "Closing worker pane");
                if let Err(e) =
                    crate::services::tmux_events::close_worker_pane(self.ctx.tmux_ipc(), pane_id)
                        .await
                {
                    warn!(agent = %ctx.agent_name, pane_id = %pane_id, error = %e, "Failed to close worker pane");
                } else {
                    closed = true;
                }
            }
            // Try window_id (worktree-based agents)
            else if let Some(window_id) = r["window_id"].as_str() {
                info!(agent = %ctx.agent_name, window_id = %window_id, "Closing agent window");
                let ipc = self.ctx.tmux_ipc();
                match crate::services::tmux_ipc::WindowId::parse(window_id) {
                    Ok(wid) => {
                        if let Err(e) = ipc.kill_window(&wid).await {
                            warn!(agent = %ctx.agent_name, window_id = %window_id, error = %e, "Failed to close agent window");
                        } else {
                            closed = true;
                        }
                    }
                    Err(e) => {
                        warn!(agent = %ctx.agent_name, window_id = %window_id, error = %e, "Invalid window_id in routing.json");
                    }
                }
            } else {
                warn!(agent = %ctx.agent_name, "No pane_id or window_id in routing.json");
            }
        } else {
            warn!(agent = %ctx.agent_name, "Could not read routing.json (tried {agent_key} and suffixed variants)");
        }

        // Remove synthetic team member registration after closing.
        // AgentResolver is the canonical source for agent identity.
        if closed {
            {
                let team_reg = self.ctx.team_registry();
                let member_name = {
                    let resolver = self.ctx.agent_resolver();
                    let name = crate::domain::AgentName::from(resolved_internal_name.as_str());
                    if let Ok(records) = resolver.records_ref().try_read() {
                        records.get(&name).map(|r| r.agent_name.clone())
                    } else {
                        None
                    }
                };
                if let Some(member_name) = member_name {
                    let birth_branch_str = ctx.birth_branch.as_str();
                    let team_info = if let Some(info) = team_reg.get(&agent_key).await {
                        Some(info)
                    } else if let Some(info) = team_reg.get(birth_branch_str).await {
                        Some(info)
                    } else if let Some(parent) = ctx.birth_branch.parent() {
                        team_reg.get(parent.as_str()).await
                    } else {
                        None
                    };
                    if let Some(info) = team_info {
                        let team_name = TeamName::from(info.team_name.as_str());
                        if let Err(e) = crate::services::synthetic_members::remove_synthetic_member(
                            &team_name,
                            &member_name,
                        ) {
                            warn!(team = %team_name, member = %member_name, error = %e, "Failed to remove synthetic member on close_self (non-fatal)");
                        }
                    }
                } else {
                    warn!(agent = %ctx.agent_name, "No resolver record for agent, skipping synthetic member cleanup");
                }
            }
        }

        info!(agent = %ctx.agent_name, closed, "Agent requested self-closure");

        Ok(CloseSelfResponse {
            success: closed,
            error: String::new(),
        })
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
        alive: true,
        mux_window: result.agent_name.to_string(),
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
        id: result.agent_name.to_string(),
        issue: String::new(),
        worktree_path: String::new(),
        branch_name: String::new(),
        agent_type: service_agent_type_to_proto(result.agent_type),
        role: 0,
        alive: true,
        mux_window: result.agent_type.tab_display_name(name),
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
        id: result.agent_name.to_string(),
        issue: String::new(),
        worktree_path: String::new(),
        branch_name: String::new(),
        agent_type: AgentType::Gemini as i32,
        role: 0,
        alive: true,
        mux_window: ServiceAgentType::Gemini.tab_display_name(name),
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
        id: result.agent_name.to_string(),
        issue: String::new(),
        worktree_path: result.agent_dir.display().to_string(),
        branch_name: branch_name.to_string(),
        agent_type: service_agent_type_to_proto(result.agent_type),
        role: 0,
        alive: true,
        mux_window: result.agent_type.tab_display_name(branch_name),
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
        id: result.agent_name.to_string(),
        issue: String::new(),
        worktree_path: result.agent_dir.display().to_string(),
        branch_name: branch_name.to_string(),
        agent_type: service_agent_type_to_proto(result.agent_type),
        role: 0,
        alive: true,
        mux_window: result.agent_type.tab_display_name(branch_name),
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
        ServiceAgentType::Shoal => AgentType::Shoal as i32,
        ServiceAgentType::Process => AgentType::Unspecified as i32,
    }
}

fn service_info_to_proto(info: &AgentInfo) -> exomonad_proto::effects::agent::AgentInfo {
    let agent_type = match info.agent_type {
        Some(ServiceAgentType::Claude) => AgentType::Claude as i32,
        Some(ServiceAgentType::Gemini) => AgentType::Gemini as i32,
        Some(ServiceAgentType::Shoal) => AgentType::Shoal as i32,
        Some(ServiceAgentType::Process) => AgentType::Unspecified as i32,
        None => AgentType::Unspecified as i32,
    };

    exomonad_proto::effects::agent::AgentInfo {
        id: info.internal_name.to_string(),
        issue: info.internal_name.to_string(),
        worktree_path: info
            .agent_dir
            .as_ref()
            .map(|p| p.display().to_string())
            .unwrap_or_default(),
        branch_name: String::new(),
        agent_type,
        role: 0,
        alive: info.has_tab,
        mux_window: String::new(),
        error: String::new(),
        pr_number: info.pr.as_ref().map(|p| p.number as i32).unwrap_or(0),
        pr_url: info.pr.as_ref().map(|p| p.url.clone()).unwrap_or_default(),
        topology: info.topology.to_proto(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_handler() -> AgentHandler<crate::services::Services> {
        let services = Arc::new(crate::services::Services::test());
        let service = Arc::new(AgentControlService::new(services.clone()));
        AgentHandler::new(service, services)
    }

    #[test]
    fn test_namespace() {
        let handler = test_handler();
        assert_eq!(handler.namespace(), "agent");
    }

    #[test]
    fn test_convert_agent_type() {
        assert_eq!(
            convert_agent_type(AgentType::Claude).unwrap(),
            ServiceAgentType::Claude
        );
        assert_eq!(
            convert_agent_type(AgentType::Gemini).unwrap(),
            ServiceAgentType::Gemini
        );
        assert!(convert_agent_type(AgentType::Unspecified).is_err());
    }
}
