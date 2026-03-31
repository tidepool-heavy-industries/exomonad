use crate::domain::{BranchName, CommitSha, GithubOwner, GithubRepo, PRNumber};
use crate::plugin_manager::PluginManager;
use crate::services::acp_registry::AcpRegistry;
use crate::services::agent_control::AgentType;
use crate::services::agent_resolver::AgentResolver;
use crate::services::event_queue::EventQueue;
use crate::services::github::{map_octo_err, GitHubClient};
use crate::services::repo;
use anyhow::Result;
use claude_teams_bridge::TeamRegistry;
use exomonad_proto::effects::events::{event::EventType, AgentMessage, Event};
use octocrab::params;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::process::Command;
use tokio::sync::{Mutex, RwLock};
use tracing::{debug, info, instrument, warn};

type PluginMap = Arc<RwLock<HashMap<crate::AgentName, Arc<PluginManager>>>>;

pub struct GitHubPoller {
    event_queue: Arc<EventQueue>,
    project_dir: PathBuf,
    poll_interval: Duration,
    state: Arc<Mutex<HashMap<PRNumber, PRState>>>,
    repo_info: Arc<Mutex<Option<(GithubOwner, GithubRepo)>>>, // (owner, name)
    team_registry: Option<Arc<TeamRegistry>>,
    acp_registry: Option<Arc<AcpRegistry>>,
    agent_resolver: Option<Arc<AgentResolver>>,
    plugins: Option<PluginMap>,
    event_log: Option<Arc<super::event_log::EventLog>>,
    github: Option<Arc<GitHubClient>>,
}

/// A Copilot review comment with optional file context.
#[derive(Debug, Clone, serde::Serialize)]
struct CopilotComment {
    body: String,
    path: Option<String>,
    diff_hunk: Option<String>,
}

/// A Copilot review with typed state.
#[derive(Debug, Clone, serde::Serialize)]
struct CopilotReview {
    body: String,
    state: ReviewState,
}

/// Branch info discovered from a worktree directory.
struct WorktreeBranch {
    branch: BranchName,
    agent_type: AgentType,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
enum ReviewState {
    None,
    ChangesRequested,
    Approved,
}

#[derive(Debug, Clone, serde::Serialize)]
enum PendingAction {
    WasmEvent {
        event_type: &'static str,
        payload: serde_json::Value,
    },
    EmitEvent {
        status: String,
        message: String,
        comments: Option<Vec<CopilotComment>>,
        reviews: Option<Vec<CopilotReview>>,
    },
}

#[derive(Debug, Clone)]
struct PRState {
    last_copilot_comment_count: usize,
    last_ci_status: String,
    branch_name: BranchName,
    agent_type: AgentType,
    first_seen: Instant,
    notified_parent_timeout: bool,
    last_review_state: ReviewState,
    last_sha: CommitSha,
    notified_parent_approved: bool,
    addressed_changes: bool,
}

impl PRState {
    fn new(
        branch: &BranchName,
        agent_type: AgentType,
        sha: &CommitSha,
        ci_status: &str,
        copilot_count: usize,
    ) -> Self {
        Self {
            last_copilot_comment_count: copilot_count,
            last_ci_status: ci_status.to_string(),
            branch_name: branch.clone(),
            agent_type,
            first_seen: Instant::now(),
            notified_parent_timeout: false,
            last_review_state: ReviewState::None,
            last_sha: sha.clone(),
            notified_parent_approved: false,
            addressed_changes: false,
        }
    }
}

/// Pure state machine: given old state + new observations, compute pending actions.
/// Testable without GitHub API calls.
#[allow(clippy::too_many_arguments)]
fn compute_pr_actions(
    old_state: &mut PRState,
    pr_number: PRNumber,
    pr_sha: &str,
    copilot_comments: &[CopilotComment],
    copilot_reviews: &[CopilotReview],
    ci_status: &str,
    branch: &str,
    format_message: &dyn Fn(&[CopilotComment], &[CopilotReview]) -> String,
) -> Vec<PendingAction> {
    let mut pending_actions = Vec::new();
    let copilot_count = copilot_comments.len() + copilot_reviews.len();

    // Reset review tracking if new commits pushed
    if pr_sha != old_state.last_sha.as_str() {
        old_state.last_sha = CommitSha::from(pr_sha);
        if old_state.last_review_state == ReviewState::ChangesRequested {
            old_state.last_review_state = ReviewState::None;
            old_state.notified_parent_timeout = false;
            old_state.first_seen = Instant::now();
            old_state.addressed_changes = true;

            // Fire FixesPushed event — Copilot does NOT re-review,
            // so this is the actionable signal for the TL.
            pending_actions.push(PendingAction::WasmEvent {
                event_type: "pr_review",
                payload: serde_json::json!({
                    "kind": "fixes_pushed",
                    "pr_number": pr_number.as_u64(),
                    "ci_status": ci_status,
                }),
            });
        } else {
            // New commits pushed outside of a review-response cycle.
            // Notify parent so TL knows the agent is active.
            pending_actions.push(PendingAction::WasmEvent {
                event_type: "pr_review",
                payload: serde_json::json!({
                    "kind": "commits_pushed",
                    "pr_number": pr_number.as_u64(),
                    "ci_status": ci_status,
                }),
            });
        }
    }

    // Check Copilot changes
    if copilot_count != old_state.last_copilot_comment_count {
        if copilot_count > old_state.last_copilot_comment_count {
            // New activity!
            let message = format_message(copilot_comments, copilot_reviews);
            pending_actions.push(PendingAction::EmitEvent {
                status: "copilot_review".to_string(),
                message: message.clone(),
                comments: Some(copilot_comments.to_vec()),
                reviews: Some(copilot_reviews.to_vec()),
            });

            // Fire WASM event handler
            pending_actions.push(PendingAction::WasmEvent {
                event_type: "pr_review",
                payload: serde_json::json!({
                    "kind": "review_received",
                    "pr_number": pr_number.as_u64(),
                    "comments": message,
                }),
            });
        }
        // Update state even if count decreased (to sync with reality)
        old_state.last_copilot_comment_count = copilot_count;
    }

    // Check for Copilot approval
    let approved = copilot_reviews
        .iter()
        .any(|r| r.state == ReviewState::Approved || r.body.to_lowercase().contains("approved"));
    if approved && old_state.last_review_state != ReviewState::Approved {
        old_state.last_review_state = ReviewState::Approved;
        old_state.notified_parent_approved = true;
        pending_actions.push(PendingAction::WasmEvent {
            event_type: "pr_review",
            payload: serde_json::json!({
                "kind": "approved",
                "pr_number": pr_number.as_u64(),
            }),
        });
    }

    // Check for changes_requested
    let changes_requested = copilot_reviews
        .iter()
        .any(|r| r.state == ReviewState::ChangesRequested);
    if changes_requested && old_state.last_review_state != ReviewState::ChangesRequested {
        old_state.last_review_state = ReviewState::ChangesRequested;
        pending_actions.push(PendingAction::WasmEvent {
            event_type: "pr_review",
            payload: serde_json::json!({
                "kind": "review_received",
                "pr_number": pr_number.as_u64(),
                "comments": format_message(copilot_comments, copilot_reviews),
            }),
        });
    }

    // Check CI changes
    if ci_status != old_state.last_ci_status {
        pending_actions.push(PendingAction::WasmEvent {
            event_type: "ci_status",
            payload: serde_json::json!({
                "pr_number": pr_number.as_u64(),
                "status": ci_status,
                "branch": branch,
            }),
        });
        pending_actions.push(PendingAction::EmitEvent {
            status: ci_status.to_string(),
            message: format!("[CI STATUS: {}] {}", branch, ci_status),
            comments: None,
            reviews: None,
        });
        old_state.last_ci_status = ci_status.to_string();
    }

    // Shorter timeout after addressing changes (Copilot won't re-review)
    let timeout_minutes: u64 = if old_state.addressed_changes { 5 } else { 15 };
    if !old_state.notified_parent_timeout
        && old_state.last_review_state == ReviewState::None
        && !old_state.notified_parent_approved
        && old_state.first_seen.elapsed() > Duration::from_secs(timeout_minutes * 60)
    {
        old_state.notified_parent_timeout = true;
        pending_actions.push(PendingAction::WasmEvent {
            event_type: "pr_review",
            payload: serde_json::json!({
                "kind": "timeout",
                "pr_number": pr_number.as_u64(),
                "minutes_elapsed": timeout_minutes,
            }),
        });
    }

    pending_actions
}

#[derive(Debug, Deserialize)]
#[serde(tag = "action")]
enum EventActionResponse {
    #[serde(rename = "inject_message")]
    InjectMessage { message: String },
    #[serde(rename = "notify_parent")]
    NotifyParent { message: String, pr_number: i64 },
    #[serde(rename = "no_action")]
    NoAction,
}

impl GitHubPoller {
    pub fn new(event_queue: Arc<EventQueue>, project_dir: PathBuf) -> Self {
        Self {
            event_queue,
            project_dir,
            poll_interval: Duration::from_secs(60),
            state: Arc::new(Mutex::new(HashMap::new())),
            repo_info: Arc::new(Mutex::new(None)),
            team_registry: None,
            acp_registry: None,
            agent_resolver: None,
            plugins: None,
            event_log: None,
            github: None,
        }
    }

    /// Set shared registries from Services in one call.
    pub fn with_services(mut self, services: &super::Services) -> Self {
        self.team_registry = Some(services.team_registry.clone());
        self.acp_registry = Some(services.acp_registry.clone());
        self.agent_resolver = Some(services.agent_resolver.clone());
        self.event_log = services.event_log.clone();
        self.github = services.github_client.clone();
        self
    }

    pub fn with_poll_interval(mut self, interval: Duration) -> Self {
        self.poll_interval = interval;
        self
    }

    pub fn with_plugins(mut self, plugins: PluginMap) -> Self {
        self.plugins = Some(plugins);
        self
    }

    pub async fn run(&self) {
        tracing::info!(
            poll_interval_secs = self.poll_interval.as_secs(),
            "GitHub poller started"
        );

        let base_interval = self.poll_interval;
        let max_backoff = Duration::from_secs(600); // 10 minutes max
        let mut consecutive_failures: u32 = 0;

        loop {
            let sleep_duration = if consecutive_failures == 0 {
                base_interval
            } else {
                let backoff = base_interval * 2u32.saturating_pow(consecutive_failures.min(6));
                backoff.min(max_backoff)
            };

            tokio::time::sleep(sleep_duration).await;

            match self.poll_cycle().await {
                Ok(()) => {
                    if consecutive_failures > 0 {
                        info!(
                            previous_failures = consecutive_failures,
                            "GitHub poller recovered"
                        );
                    }
                    consecutive_failures = 0;
                }
                Err(e) => {
                    consecutive_failures += 1;
                    let next_retry_secs = {
                        let backoff =
                            base_interval * 2u32.saturating_pow(consecutive_failures.min(6));
                        backoff.min(max_backoff).as_secs()
                    };
                    if consecutive_failures <= 3 {
                        warn!(
                            consecutive_failures,
                            next_retry_secs, "GitHub poller cycle failed: {}", e
                        );
                    } else {
                        debug!(
                            consecutive_failures,
                            next_retry_secs, "GitHub poller cycle failed: {}", e
                        );
                    }

                    // Report failure to GitHubClient for health tracking and rebuild.
                    if let Some(ref client) = self.github {
                        client.report_failure().await;
                    }
                }
            }
        }
    }

    /// Call handle_event on the agent's WASM plugin.
    /// The event JSON matches ExoMonad.Guest.Events.EventInput format:
    /// { "role": "dev", "event_type": "pr_review", "payload": { "kind": "approved", "pr_number": 123 } }
    #[instrument(skip_all, fields(branch = %branch, event_type = %event_type, role = tracing::field::Empty))]
    async fn call_handle_event(
        &self,
        branch: &str,
        agent_type: AgentType,
        event_type: &str,
        payload: serde_json::Value,
    ) -> Result<Option<EventActionResponse>> {
        let plugins = match &self.plugins {
            Some(p) => p,
            None => return Ok(None),
        };

        // Resolve the agent name from branch (slug after last dot)
        let agent_name = branch.rsplit_once('.').map(|(_, s)| s).unwrap_or(branch);
        let role = match agent_type {
            AgentType::Claude => "tl",
            AgentType::Gemini => "dev",
            AgentType::Shoal => "dev",
            AgentType::Process => return Ok(None), // Process companions don't have WASM handlers
        };

        tracing::Span::current().record("role", role);

        let event_input = serde_json::json!({
            "role": role,
            "event_type": event_type,
            "payload": payload,
        });

        let plugins_guard = plugins.read().await;
        let plugin = match plugins_guard.get(&crate::AgentName::from(agent_name)) {
            Some(p) => p.clone(),
            None => {
                info!(
                    "No plugin found for agent '{}', skipping event dispatch",
                    agent_name
                );
                return Ok(None);
            }
        };
        drop(plugins_guard);

        info!(
            "[EventDispatch] Calling handle_event for agent '{}': event_type={}, pr_payload={}",
            agent_name, event_type, payload
        );

        match plugin
            .call::<serde_json::Value, EventActionResponse>("handle_event", &event_input)
            .await
        {
            Ok(action) => {
                info!("[EventDispatch] handle_event returned: {:?}", action);

                let action_str = match action {
                    EventActionResponse::InjectMessage { .. } => "inject_message",
                    EventActionResponse::NotifyParent { .. } => "notify_parent",
                    EventActionResponse::NoAction => "no_action",
                };

                tracing::info!(
                    otel.name = "event.dispatched",
                    agent_id = %agent_name,
                    event_type = %event_type,
                    action = %action_str,
                    "[event] event.dispatched"
                );
                if let Some(ref log) = self.event_log {
                    let _ = log.append(
                        "event.dispatched",
                        agent_name,
                        &serde_json::json!({
                            "event_type": event_type,
                            "action": action_str,
                        }),
                    );
                }

                Ok(Some(action))
            }
            Err(e) => {
                warn!(
                    "[EventDispatch] handle_event failed for {}: {}",
                    agent_name, e
                );

                tracing::info!(
                    otel.name = "event.dispatch_failed",
                    agent_id = %agent_name,
                    event_type = %event_type,
                    error = %e,
                    "[event] event.dispatch_failed"
                );
                if let Some(ref log) = self.event_log {
                    let _ = log.append(
                        "event.dispatch_failed",
                        agent_name,
                        &serde_json::json!({
                            "event_type": event_type,
                            "error": e.to_string(),
                        }),
                    );
                }

                Ok(None)
            }
        }
    }

    async fn handle_event_action(
        &self,
        action: EventActionResponse,
        branch: &str,
        agent_type: AgentType,
        pr_number: PRNumber,
    ) {
        match action {
            EventActionResponse::InjectMessage { message } => {
                let resolver_ref = self.agent_resolver.as_deref();
                let agent_name = crate::domain::AgentName::from(branch);
                let tab_name = if let Some(resolver) = resolver_ref {
                    if let Ok(records) = resolver.records_ref().try_read() {
                        records.get(&agent_name).map(|r| r.display_name.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }.unwrap_or_else(|| {
                    let slug = branch.rsplit_once('.').map(|(_, s)| s).unwrap_or(branch);
                    agent_type.tab_display_name(slug)
                });
                crate::services::delivery::deliver_to_agent(
                    self.team_registry.as_deref(),
                    self.acp_registry.as_deref(),
                    &self.project_dir,
                    branch,
                    &tab_name,
                    "event-handler",
                    &message,
                    &format!("Event handler action for PR #{}", pr_number),
                )
                .await;
            }
            EventActionResponse::NotifyParent {
                message,
                pr_number: pr_num,
            } => {
                let agent_slug = branch.rsplit_once('.').map(|(_, s)| s).unwrap_or(branch);
                let parent_session_id = branch
                    .rsplit_once('.')
                    .map(|(parent, _)| parent.to_string())
                    .unwrap_or_else(|| "root".to_string());
                let parent_name = crate::domain::AgentName::from(parent_session_id.as_str());
                let parent_tab = crate::services::delivery::resolve_tab_name_for_agent(
                    &parent_name,
                    self.agent_resolver.as_deref(),
                );

                let summary = format!("Auto-notify: PR #{}", pr_num);
                let agent_name = crate::domain::AgentName::from(agent_slug);
                crate::services::delivery::notify_parent_delivery(
                    self.team_registry.as_deref(),
                    self.acp_registry.as_deref(),
                    self.event_log.as_deref(),
                    &self.event_queue,
                    &self.project_dir,
                    &agent_name,
                    &parent_session_id,
                    &parent_tab,
                    crate::services::delivery::NotifyStatus::Success,
                    &message,
                    Some(&summary),
                    "event_handler",
                )
                .await;
            }
            EventActionResponse::NoAction => {}
        }
    }

    #[tracing::instrument(skip_all, name = "github_poller.poll_cycle")]
    async fn poll_cycle(&self) -> Result<()> {
        // Ensure we have repo info
        let (owner, repo) = match self.get_repo_info().await? {
            Some(info) => info,
            None => return Ok(()),
        };

        // 1. Scan worktrees for branches
        let branches = self.scan_worktrees().await?;
        tracing::debug!(
            worktree_count = branches.len(),
            "Polling GitHub for PR updates"
        );

        if branches.is_empty() {
            return Ok(());
        }

        // 2. Fetch all open PRs
        let octo = match &self.github {
            Some(client) => client.get().await.map_err(|e| {
                tracing::warn!("GitHubPoller: no octocrab client: {}", e);
                e
            })?,
            None => {
                tracing::warn!("GitHubPoller: no GitHub client configured, skipping cycle");
                return Ok(());
            }
        };

        let prs_page = octo
            .pulls(owner.as_str(), repo.as_str())
            .list()
            .state(params::State::Open)
            .per_page(100)
            .send()
            .await;

        let prs = match prs_page {
            Ok(page) => page.into_iter().collect::<Vec<_>>(),
            Err(e) => {
                return Err(anyhow::anyhow!("Failed to fetch PRs: {}", map_octo_err(e)));
            }
        };

        let mut pr_map = HashMap::new();
        for pr in prs {
            // Validate external data from GitHub before constructing domain types.
            if pr.head.ref_field.is_empty() {
                tracing::warn!(
                    "GitHubPoller: skipping PR {} due to empty head.ref_field",
                    pr.number
                );
                continue;
            }
            if pr.head.sha.is_empty() {
                tracing::warn!(
                    "GitHubPoller: skipping PR {} due to empty head.sha",
                    pr.number
                );
                continue;
            }
            if pr.number == 0 {
                tracing::warn!("GitHubPoller: skipping PR with non-positive number: 0");
                continue;
            }

            let ref_name = BranchName::from(pr.head.ref_field.as_str());
            let sha = CommitSha::from(pr.head.sha.as_str());
            let pr_number = PRNumber::new(pr.number);
            pr_map.insert(ref_name, (pr_number, sha));
        }

        // 3. Match branches to PRs
        for wb in branches {
            if let Some((pr_number, pr_sha)) = pr_map.get(&wb.branch) {
                if let Err(e) = self
                    .process_pr(&owner, &repo, &wb.branch, *pr_number, pr_sha, wb.agent_type)
                    .await
                {
                    warn!("Failed to process branch {}: {}", wb.branch, e);
                }
            }
        }

        // 4. Detect merged/closed PRs and notify siblings
        let mut sibling_events: Vec<(String, AgentType, PRNumber, serde_json::Value)> = Vec::new();
        let mut removed_prs = Vec::new();
        {
            let mut state_guard = self.state.lock().await;
            let tracked: Vec<(PRNumber, BranchName)> = state_guard
                .iter()
                .map(|(n, s)| (*n, s.branch_name.clone()))
                .collect();

            for (pr_num, branch) in &tracked {
                if !pr_map.contains_key(branch.as_str()) {
                    let parent_branch = branch
                        .as_str()
                        .rsplit_once('.')
                        .map(|(parent, _)| parent)
                        .unwrap_or("main");

                    for (sib_num, sib_state) in state_guard.iter() {
                        if sib_num == pr_num {
                            continue;
                        }
                        let sib_parent = sib_state
                            .branch_name
                            .as_str()
                            .rsplit_once('.')
                            .map(|(p, _)| p)
                            .unwrap_or("main");
                        if sib_parent == parent_branch
                            && pr_map.contains_key(sib_state.branch_name.as_str())
                        {
                            sibling_events.push((
                                sib_state.branch_name.as_str().to_string(),
                                sib_state.agent_type,
                                *sib_num,
                                serde_json::json!({
                                    "merged_branch": branch.as_str(),
                                    "parent_branch": parent_branch,
                                    "sibling_pr_number": sib_num.as_u64(),
                                }),
                            ));
                        }
                    }

                    tracing::info!(
                        otel.name = "agent.sibling_merged",
                        agent_id = %branch,
                        pr_number = pr_num.as_u64(),
                        branch = %branch,
                        parent = %parent_branch,
                        "[event] agent.sibling_merged"
                    );
                    if let Some(ref log) = self.event_log {
                        let _ = log.append(
                            "agent.sibling_merged",
                            branch.as_str(),
                            &serde_json::json!({
                                "pr_number": pr_num.as_u64(),
                                "branch": branch.as_str(),
                                "parent": parent_branch,
                            }),
                        );
                    }

                    removed_prs.push(*pr_num);
                }
            }

            for pr_num in &removed_prs {
                state_guard.remove(pr_num);
            }
        }

        for (sib_branch, sib_agent_type, sib_pr_num, payload) in sibling_events {
            if let Ok(Some(action)) = self
                .call_handle_event(&sib_branch, sib_agent_type, "sibling_merged", payload)
                .await
            {
                self.handle_event_action(action, &sib_branch, sib_agent_type, sib_pr_num)
                    .await;
            }
        }

        Ok(())
    }

    async fn get_repo_info(&self) -> Result<Option<(GithubOwner, GithubRepo)>> {
        let mut info_guard = self.repo_info.lock().await;
        if let Some(info) = &*info_guard {
            return Ok(Some(info.clone()));
        }

        match repo::get_repo_info(&self.project_dir).await {
            Ok(info) => {
                let result = (
                    GithubOwner::from(info.owner.as_str()),
                    GithubRepo::from(info.repo.as_str()),
                );
                *info_guard = Some(result.clone());
                Ok(Some(result))
            }
            Err(e) => {
                warn!("Failed to get repo info: {}", e);
                Ok(None)
            }
        }
    }

    async fn scan_worktrees(&self) -> Result<Vec<WorktreeBranch>> {
        let worktrees_dir = self.project_dir.join(".exo/worktrees");
        if !worktrees_dir.exists() {
            return Ok(vec![]);
        }

        let mut branches = Vec::new();
        let mut entries = tokio::fs::read_dir(worktrees_dir).await?;

        while let Some(entry) = entries.next_entry().await? {
            if !entry.file_type().await?.is_dir() {
                continue;
            }

            let dir_name = entry.file_name().to_string_lossy().to_string();
            let agent_type = AgentType::from_dir_name(&dir_name);

            // Execute git branch --show-current in the worktree
            let output = Command::new("git")
                .arg("-C")
                .arg(entry.path())
                .args(["branch", "--show-current"])
                .output()
                .await?;

            if output.status.success() {
                let branch = String::from_utf8_lossy(&output.stdout).trim().to_string();
                if !branch.is_empty() {
                    branches.push(WorktreeBranch {
                        branch: BranchName::from(branch.as_str()),
                        agent_type,
                    });
                }
            }
        }

        Ok(branches)
    }

    async fn process_pr(
        &self,
        owner: &GithubOwner,
        repo: &GithubRepo,
        branch: &BranchName,
        pr_number: PRNumber,
        pr_sha: &CommitSha,
        agent_type: AgentType,
    ) -> Result<()> {
        // Poll details
        let (copilot_comments, copilot_reviews) =
            self.fetch_copilot_activity(owner, repo, pr_number).await?;
        let ci_status = self.fetch_ci_status(owner, repo, pr_sha).await?;

        let pending_actions = {
            let mut state_guard = self.state.lock().await;

            if let Some(old_state) = state_guard.get_mut(&pr_number) {
                if ci_status != old_state.last_ci_status {
                    info!(
                        "[Poller] CI status changed for {}: {} -> {}",
                        branch, old_state.last_ci_status, ci_status
                    );
                }

                compute_pr_actions(
                    old_state,
                    pr_number,
                    pr_sha.as_str(),
                    &copilot_comments,
                    &copilot_reviews,
                    &ci_status,
                    branch.as_str(),
                    &|c, r| self.format_copilot_message(c, r),
                )
            } else {
                // New PR tracked - do not emit, just store
                state_guard.insert(
                    pr_number,
                    PRState::new(
                        branch,
                        agent_type,
                        pr_sha,
                        &ci_status,
                        copilot_comments.len() + copilot_reviews.len(),
                    ),
                );
                vec![]
            }
        };

        // Execute pending actions outside of lock
        for action in pending_actions {
            match action {
                PendingAction::WasmEvent {
                    event_type,
                    payload,
                } => {
                    if let Ok(Some(action)) = self
                        .call_handle_event(branch.as_str(), agent_type, event_type, payload)
                        .await
                    {
                        self.handle_event_action(action, branch.as_str(), agent_type, pr_number)
                            .await;
                    }
                }
                PendingAction::EmitEvent {
                    status,
                    message,
                    comments,
                    reviews,
                } => {
                    self.emit_event_with_reviews(
                        branch.as_str(),
                        &status,
                        &message,
                        agent_type,
                        Some(pr_number),
                        comments,
                        reviews,
                    )
                    .await;
                }
            }
        }

        Ok(())
    }

    async fn fetch_copilot_activity(
        &self,
        owner: &GithubOwner,
        repo: &GithubRepo,
        pr_number: PRNumber,
    ) -> Result<(Vec<CopilotComment>, Vec<CopilotReview>)> {
        let octo = match &self.github {
            Some(client) => match client.get().await {
                Ok(c) => c,
                Err(_) => return Ok((Vec::new(), Vec::new())),
            },
            None => return Ok((Vec::new(), Vec::new())),
        };

        // Inline comments (with file context)
        let mut inline_comments = Vec::new();
        let comments_page = octo
            .pulls(owner.as_str(), repo.as_str())
            .list_comments(Some(pr_number.as_u64()))
            .send()
            .await;

        if let Ok(page) = comments_page {
            for c in page {
                let user_login = c.user.as_ref().map(|u| u.login.clone()).unwrap_or_default();
                if user_login.to_lowercase().contains("copilot") {
                    inline_comments.push(CopilotComment {
                        body: c.body,
                        path: Some(c.path),
                        diff_hunk: Some(c.diff_hunk),
                    });
                }
            }
        }

        // Reviews (with body text and state)
        let mut copilot_reviews = Vec::new();
        let reviews_page = octo
            .pulls(owner.as_str(), repo.as_str())
            .list_reviews(pr_number.as_u64())
            .send()
            .await;

        if let Ok(page) = reviews_page {
            for r in page {
                let user_login = r.user.as_ref().map(|u| u.login.clone()).unwrap_or_default();
                if user_login.to_lowercase().contains("copilot") {
                    let state = match r.state {
                        Some(octocrab::models::pulls::ReviewState::Approved) => {
                            ReviewState::Approved
                        }
                        Some(octocrab::models::pulls::ReviewState::ChangesRequested) => {
                            ReviewState::ChangesRequested
                        }
                        _ => ReviewState::None,
                    };
                    copilot_reviews.push(CopilotReview {
                        body: r.body.unwrap_or_default(),
                        state,
                    });
                }
            }
        }

        Ok((inline_comments, copilot_reviews))
    }

    async fn fetch_ci_status(
        &self,
        owner: &GithubOwner,
        repo: &GithubRepo,
        sha: &CommitSha,
    ) -> Result<String> {
        let octo = match &self.github {
            Some(client) => match client.get().await {
                Ok(c) => c,
                Err(_) => return Ok("unknown".to_string()),
            },
            None => return Ok("unknown".to_string()),
        };

        let runs = match octo
            .checks(owner.as_str(), repo.as_str())
            .list_check_runs_for_git_ref(octocrab::params::repos::Commitish(sha.to_string()))
            .send()
            .await
        {
            Ok(runs) => runs,
            Err(_) => return Ok("unknown".to_string()),
        };

        // Simplified logic: if any failure -> failure. If all success -> success. Else pending.
        let mut any_failure = false;
        let mut all_success = true;

        // If no check runs, consider it pending (or none)
        if runs.check_runs.is_empty() {
            return Ok("pending".to_string());
        }

        for run in runs.check_runs {
            match run.conclusion.as_deref() {
                Some("failure") | Some("timed_out") | Some("cancelled") => any_failure = true,
                Some("success") => {}
                _ => {
                    // Pending, skipped, neutral, etc.
                    // If it's not success or failure, it means we are not 'all_success'.
                    if run.completed_at.is_none() {
                        all_success = false;
                    }
                    // For now, if skipped, we treat as neutral (don't break all_success?)
                    // Actually, let's keep it simple: if not failure and not success, it's pending/other.
                    if run.conclusion.is_none() {
                        all_success = false;
                    }
                }
            }
        }

        if any_failure {
            Ok("failure".to_string())
        } else if all_success {
            Ok("success".to_string())
        } else {
            Ok("pending".to_string())
        }
    }

    fn format_copilot_message(
        &self,
        inline_comments: &[CopilotComment],
        copilot_reviews: &[CopilotReview],
    ) -> String {
        let mut msg = String::new();

        if !copilot_reviews.is_empty() {
            let review_bodies: Vec<String> = copilot_reviews
                .iter()
                .filter(|r| !r.body.is_empty())
                .map(|r| r.body.clone())
                .collect();

            if !review_bodies.is_empty() {
                msg.push_str("Review summary:\n");
                for body in review_bodies {
                    msg.push_str(&body);
                    msg.push('\n');
                }
            }
        }

        if !inline_comments.is_empty() {
            if !msg.is_empty() {
                msg.push('\n');
            }
            msg.push_str("Inline comments:\n");
            for (i, c) in inline_comments.iter().enumerate() {
                let file_label = c.path.as_deref().unwrap_or("unknown file");
                msg.push_str(&format!("{}. [{}] {}\n", i + 1, file_label, c.body));
                if let Some(ref hunk) = c.diff_hunk {
                    msg.push_str(&format!("   ```diff\n   {}\n   ```\n", hunk));
                }
            }
        }

        if msg.is_empty() {
            msg.push_str("Copilot review activity detected (no body text)");
        }

        msg
    }

    #[allow(dead_code)]
    async fn emit_event(
        &self,
        branch: &str,
        status: &str,
        message: &str,
        agent_type: AgentType,
        pr_number: Option<PRNumber>,
    ) {
        self.emit_event_with_reviews(branch, status, message, agent_type, pr_number, None, None)
            .await;
    }

    #[allow(clippy::too_many_arguments)]
    async fn emit_event_with_reviews(
        &self,
        branch: &str,
        status: &str,
        message: &str,
        _agent_type: AgentType,
        _pr_number: Option<PRNumber>,
        comments: Option<Vec<CopilotComment>>,
        reviews: Option<Vec<CopilotReview>>,
    ) {
        info!(
            "Emitting event for branch {}: {} - {}",
            branch, status, message
        );

        let event_name = match status {
            "copilot_review" => "copilot.review",
            "success" => "ci.status_changed",
            "failure" => "ci.status_changed",
            "pending" => "ci.status_changed",
            other => other,
        };

        let comments_json = comments
            .as_ref()
            .and_then(|c| serde_json::to_string(c).ok())
            .unwrap_or_default();
        let reviews_json = reviews
            .as_ref()
            .and_then(|r| serde_json::to_string(r).ok())
            .unwrap_or_default();

        tracing::info!(
            otel.name = event_name,
            agent_id = %branch,
            branch = %branch,
            status = %status,
            message = %message,
            comments = %comments_json,
            reviews = %reviews_json,
            "[event] {}",
            event_name
        );
        if let Some(ref log) = self.event_log {
            let _ = log.append(
                event_name,
                branch,
                &serde_json::json!({
                    "branch": branch,
                    "status": status,
                    "message": message,
                    "comments": comments,
                    "reviews": reviews,
                }),
            );
        }

        let event = Event {
            event_id: 0,
            event_type: Some(EventType::AgentMessage(AgentMessage {
                agent_id: branch.to_string(),
                status: status.to_string(),
                message: message.to_string(),
                changes: vec![],
            })),
        };
        self.event_queue.notify_event(branch, event).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_copilot_message_empty() {
        let poller = GitHubPoller::new(Arc::new(EventQueue::new()), PathBuf::from("."));
        let msg = poller.format_copilot_message(&[], &[]);
        assert_eq!(msg, "Copilot review activity detected (no body text)");
    }

    #[test]
    fn test_format_copilot_message_with_reviews() {
        let poller = GitHubPoller::new(Arc::new(EventQueue::new()), PathBuf::from("."));
        let reviews = vec![
            CopilotReview {
                body: "LGTM!".to_string(),
                state: ReviewState::Approved,
            },
            CopilotReview {
                body: "Great job.".to_string(),
                state: ReviewState::None,
            },
        ];
        let msg = poller.format_copilot_message(&[], &reviews);
        assert!(msg.contains("Review summary:"));
        assert!(msg.contains("LGTM!"));
        assert!(msg.contains("Great job."));
    }

    #[test]
    fn test_format_copilot_message_with_inline_comments() {
        let poller = GitHubPoller::new(Arc::new(EventQueue::new()), PathBuf::from("."));
        let inline = vec![
            CopilotComment {
                body: "Fix this typo".to_string(),
                path: Some("src/main.rs".to_string()),
                diff_hunk: Some("@@ -1,3 +1,3 @@".to_string()),
            },
            CopilotComment {
                body: "Add a comment here".to_string(),
                path: None,
                diff_hunk: None,
            },
        ];
        let msg = poller.format_copilot_message(&inline, &[]);
        assert!(msg.contains("Inline comments:"));
        assert!(msg.contains("1. [src/main.rs] Fix this typo"));
        assert!(msg.contains("```diff\n   @@ -1,3 +1,3 @@\n   ```"));
        assert!(msg.contains("2. [unknown file] Add a comment here"));
    }

    fn make_pr_state(branch: &str, sha: &str) -> PRState {
        PRState::new(
            &BranchName::from(branch),
            AgentType::Gemini,
            &CommitSha::from(sha),
            "pending",
            0,
        )
    }

    #[test]
    fn test_new_commits_after_changes_requested_fires_fixes_pushed() {
        let mut state = make_pr_state("main.feature", "abc123");
        state.last_review_state = ReviewState::ChangesRequested;

        let actions = compute_pr_actions(
            &mut state,
            PRNumber::new(123),
            "def456", // new SHA
            &[],
            &[],
            "success",
            "main.feature",
            &|_, _| String::new(),
        );

        assert!(actions.iter().any(
            |a| matches!(a, PendingAction::WasmEvent { event_type: "pr_review", payload } if payload.get("kind").and_then(|v| v.as_str()) == Some("fixes_pushed"))
        ));
        assert_eq!(state.addressed_changes, true);
        assert_eq!(state.last_review_state, ReviewState::None);
    }

    #[test]
    fn test_timeout_fires_after_threshold() {
        let mut state = make_pr_state("main.feature", "abc123");
        state.first_seen = Instant::now() - Duration::from_secs(20 * 60); // 20 minutes ago

        let actions = compute_pr_actions(
            &mut state,
            PRNumber::new(123),
            "abc123",
            &[],
            &[],
            "success",
            "main.feature",
            &|_, _| String::new(),
        );

        assert!(actions.iter().any(|a| matches!(a, PendingAction::WasmEvent { event_type: "pr_review", payload } if payload.get("kind").and_then(|v| v.as_str()) == Some("timeout"))));
    }

    #[test]
    fn test_approval_detected() {
        let mut state = make_pr_state("main.feature", "abc123");
        let reviews = vec![CopilotReview {
            body: "LGTM".to_string(),
            state: ReviewState::Approved,
        }];

        let actions = compute_pr_actions(
            &mut state,
            PRNumber::new(123),
            "abc123",
            &[],
            &reviews,
            "success",
            "main.feature",
            &|_, _| String::new(),
        );

        assert_eq!(state.last_review_state, ReviewState::Approved);
        assert!(actions.iter().any(|a| matches!(a, PendingAction::WasmEvent { event_type: "pr_review", payload } if payload.get("kind").and_then(|v| v.as_str()) == Some("approved"))));
    }

    #[test]
    fn test_same_sha_no_op() {
        let mut state = make_pr_state("main.feature", "abc123");
        state.last_ci_status = "success".to_string();

        let actions = compute_pr_actions(
            &mut state,
            PRNumber::new(123),
            "abc123",
            &[],
            &[],
            "success",
            "main.feature",
            &|_, _| String::new(),
        );

        assert!(actions.is_empty());
    }

    #[test]
    fn test_ci_status_change() {
        let mut state = make_pr_state("main.feature", "abc123");
        state.last_ci_status = "pending".to_string();

        let actions = compute_pr_actions(
            &mut state,
            PRNumber::new(123),
            "abc123",
            &[],
            &[],
            "failure",
            "main.feature",
            &|_, _| String::new(),
        );

        assert!(actions.iter().any(|a| matches!(
            a,
            PendingAction::WasmEvent {
                event_type: "ci_status",
                ..
            }
        )));
        assert!(actions
            .iter()
            .any(|a| matches!(a, PendingAction::EmitEvent { status, .. } if status == "failure")));
        assert_eq!(state.last_ci_status, "failure");
    }

    #[test]
    fn test_addressed_changes_shorter_timeout() {
        let mut state = make_pr_state("main.feature", "abc123");
        state.addressed_changes = true;
        state.first_seen = Instant::now() - Duration::from_secs(6 * 60); // 6 minutes ago

        let actions = compute_pr_actions(
            &mut state,
            PRNumber::new(123),
            "abc123",
            &[],
            &[],
            "success",
            "main.feature",
            &|_, _| String::new(),
        );

        assert!(actions.iter().any(|a| matches!(a, PendingAction::WasmEvent { event_type: "pr_review", payload } if payload.get("kind").and_then(|v| v.as_str()) == Some("timeout"))));
    }
}
