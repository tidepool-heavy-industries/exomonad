use crate::domain::PRNumber;
use crate::services::acp_registry::AcpRegistry;
use crate::services::agent_control::AgentType;
use crate::services::event_log::EventLog;
use crate::services::event_queue::EventQueue;
use crate::services::repo;
use crate::services::team_registry::TeamRegistry;
use crate::plugin_manager::PluginManager;
use anyhow::{Context, Result};
use exomonad_proto::effects::events::{event::EventType, Event, WorkerComplete};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::process::Command;
use tokio::sync::{Mutex, RwLock};
use tracing::{error, info, warn};

type PluginMap = Arc<RwLock<HashMap<crate::AgentName, Arc<PluginManager>>>>;

pub struct GitHubPoller {
    event_queue: Arc<EventQueue>,
    event_log: Option<Arc<EventLog>>,
    project_dir: PathBuf,
    poll_interval: Duration,
    state: Arc<Mutex<HashMap<PRNumber, PRState>>>,
    repo_info: Arc<Mutex<Option<(String, String)>>>, // (owner, name)
    team_registry: Option<Arc<TeamRegistry>>,
    acp_registry: Option<Arc<AcpRegistry>>,
    plugins: Option<PluginMap>,
}

/// A Copilot review comment with optional file context.
struct CopilotComment {
    body: String,
    path: Option<String>,
    diff_hunk: Option<String>,
}

/// A Copilot review with state.
struct CopilotReview {
    body: String,
    state: String, // "APPROVED", "CHANGES_REQUESTED", "COMMENTED"
}

/// Branch info discovered from a worktree directory.
struct WorktreeBranch {
    branch: String,
    agent_type: AgentType,
}

#[derive(Debug, Clone)]
struct PRState {
    last_copilot_comment_count: usize,
    last_ci_status: String,
    branch_name: String,
    agent_type: AgentType,
    first_seen: Instant,
    notified_parent_timeout: bool,
    last_review_state: String, // "none", "changes_requested", "approved"
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
            event_log: None,
            project_dir,
            poll_interval: Duration::from_secs(60),
            state: Arc::new(Mutex::new(HashMap::new())),
            repo_info: Arc::new(Mutex::new(None)),
            team_registry: None,
            acp_registry: None,
            plugins: None,
        }
    }

    pub fn with_team_registry(mut self, registry: Arc<TeamRegistry>) -> Self {
        self.team_registry = Some(registry);
        self
    }

    pub fn with_acp_registry(mut self, registry: Arc<AcpRegistry>) -> Self {
        self.acp_registry = Some(registry);
        self
    }

    pub fn with_plugins(mut self, plugins: PluginMap) -> Self {
        self.plugins = Some(plugins);
        self
    }

    pub fn with_event_log(mut self, event_log: Arc<EventLog>) -> Self {
        self.event_log = Some(event_log);
        self
    }

    pub async fn run(self) {
        tracing::info!(
            poll_interval_secs = self.poll_interval.as_secs(),
            "GitHub poller started"
        );

        let mut interval = tokio::time::interval(self.poll_interval);
        // First tick completes immediately
        interval.tick().await;

        loop {
            interval.tick().await;
            if let Err(e) = self.poll_cycle().await {
                error!("GitHub poller cycle failed: {}", e);
            }
        }
    }

    /// Call handle_event on the agent's WASM plugin.
    /// The event JSON matches ExoMonad.Guest.Events.EventInput format:
    /// { "role": "dev", "event_type": "pr_review", "payload": { "kind": "approved", "pr_number": 123 } }
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
        };

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
                Ok(Some(action))
            }
            Err(e) => {
                warn!(
                    "[EventDispatch] handle_event failed for {}: {}",
                    agent_name, e
                );
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
                let slug = branch.rsplit_once('.').map(|(_, s)| s).unwrap_or(branch);
                let tab_name = agent_type.tab_display_name(slug);
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
                let parent_tab = if parent_session_id == "root" || !parent_session_id.contains('.') {
                    "TL".to_string()
                } else {
                    let parent_slug = parent_session_id
                        .rsplit_once('.')
                        .map(|(_, s)| s)
                        .unwrap_or(&parent_session_id);
                    AgentType::Claude.tab_display_name(parent_slug)
                };

                let summary = format!("Auto-notify: PR #{}", pr_num);
                crate::services::delivery::notify_parent_delivery(
                    self.team_registry.as_deref(),
                    self.acp_registry.as_deref(),
                    self.event_log.as_deref(),
                    &self.event_queue,
                    &self.project_dir,
                    agent_slug,
                    &parent_session_id,
                    &parent_tab,
                    "success",
                    &message,
                    Some(&summary),
                )
                .await;
            }
            EventActionResponse::NoAction => {}
        }
    }

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
        let endpoint = format!("/repos/{}/{}/pulls?state=open&per_page=100", owner, repo);
        let output = Command::new("gh").args(["api", &endpoint]).output().await?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            warn!("Failed to fetch PRs: {}", stderr.trim());
            return Ok(());
        }

        #[derive(Deserialize)]
        struct PR {
            number: u64,
            head: Head,
        }
        #[derive(Deserialize)]
        struct Head {
            sha: String,
            #[serde(rename = "ref")]
            ref_name: String,
        }

        let prs: Vec<PR> =
            serde_json::from_slice(&output.stdout).context("Failed to parse PRs JSON")?;

        let mut pr_map = HashMap::new();
        for pr in prs {
            pr_map.insert(pr.head.ref_name.clone(), pr);
        }

        // 3. Match branches to PRs
        for wb in branches {
            if let Some(pr) = pr_map.get(&wb.branch) {
                if let Err(e) = self
                    .process_pr(
                        &owner,
                        &repo,
                        &wb.branch,
                        PRNumber::new(pr.number),
                        &pr.head.sha,
                        wb.agent_type,
                    )
                    .await
                {
                    warn!("Failed to process branch {}: {}", wb.branch, e);
                }
            }
        }

        // 4. Detect merged/closed PRs and notify siblings
        let mut notifications: Vec<(String, String, String, String)> = Vec::new();
        let mut removed_prs = Vec::new();
        {
            let mut state_guard = self.state.lock().await;
            let tracked: Vec<(PRNumber, String)> = state_guard
                .iter()
                .map(|(n, s)| (*n, s.branch_name.clone()))
                .collect();

            for (pr_num, branch) in &tracked {
                if !pr_map.contains_key(branch.as_str()) {
                    let parent_branch = branch
                        .rsplit_once('.')
                        .map(|(parent, _)| parent)
                        .unwrap_or("main");

                    for (sib_num, sib_state) in state_guard.iter() {
                        if sib_num == pr_num {
                            continue;
                        }
                        let sib_parent = sib_state
                            .branch_name
                            .rsplit_once('.')
                            .map(|(p, _)| p)
                            .unwrap_or("main");
                        if sib_parent == parent_branch
                            && pr_map.contains_key(sib_state.branch_name.as_str())
                        {
                            let slug = sib_state
                                .branch_name
                                .rsplit_once('.')
                                .map(|(_, s)| s)
                                .unwrap_or(&sib_state.branch_name);
                            let tab_name = sib_state.agent_type.tab_display_name(slug);
                            let msg = format!(
                                "[Sibling Merged] PR on branch {} was merged into {}. Rebase your branch to pick up the changes: git fetch origin && git rebase origin/{}",
                                branch, parent_branch, parent_branch
                            );
                            notifications.push((
                                sib_state.branch_name.clone(),
                                tab_name,
                                msg,
                                format!("Sibling merged: {}", branch),
                            ));
                        }
                    }

                    if let Some(ref log) = self.event_log {
                        let _ = log.append(
                            "agent.sibling_merged",
                            branch,
                            &serde_json::json!({
                                "pr_number": pr_num.as_u64(),
                                "branch": branch,
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

        for (agent_key, tab_name, msg, summary) in notifications {
            crate::services::delivery::deliver_to_agent(
                self.team_registry.as_deref(),
                self.acp_registry.as_deref(),
                &self.project_dir,
                &agent_key,
                &tab_name,
                "github-poller",
                &msg,
                &summary,
            )
            .await;
        }

        Ok(())
    }

    async fn get_repo_info(&self) -> Result<Option<(String, String)>> {
        let mut info_guard = self.repo_info.lock().await;
        if let Some(info) = &*info_guard {
            return Ok(Some(info.clone()));
        }

        match repo::get_repo_info(&self.project_dir).await {
            Ok(info) => {
                let result = (info.owner, info.repo);
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
                    branches.push(WorktreeBranch { branch, agent_type });
                }
            }
        }

        Ok(branches)
    }

    async fn process_pr(
        &self,
        owner: &str,
        repo: &str,
        branch: &str,
        pr_number: PRNumber,
        pr_sha: &str,
        agent_type: AgentType,
    ) -> Result<()> {
        // Poll details
        let (copilot_comments, copilot_reviews) =
            self.fetch_copilot_activity(owner, repo, pr_number).await?;
        let ci_status = self.fetch_ci_status(owner, repo, pr_sha).await?;

        let copilot_count = copilot_comments.len() + copilot_reviews.len();

        // Check for state changes
        let mut state_guard = self.state.lock().await;

        if let Some(old_state) = state_guard.get_mut(&pr_number) {
            // Check Copilot changes
            if copilot_count != old_state.last_copilot_comment_count {
                if copilot_count > old_state.last_copilot_comment_count {
                    // New activity!
                    let message = self.format_copilot_message(&copilot_comments, &copilot_reviews);
                    self.emit_event(
                        branch,
                        "copilot_review",
                        &message,
                        agent_type,
                        Some(pr_number),
                    )
                    .await;

                    // Fire WASM event handler
                    if let Ok(Some(action)) = self
                        .call_handle_event(
                            branch,
                            agent_type,
                            "pr_review",
                            serde_json::json!({
                                "kind": "review_received",
                                "pr_number": pr_number.as_u64(),
                                "comments": message,
                            }),
                        )
                        .await
                    {
                        self.handle_event_action(action, branch, agent_type, pr_number)
                            .await;
                    }
                }
                // Update state even if count decreased (to sync with reality)
                old_state.last_copilot_comment_count = copilot_count;
            }

            // Check for Copilot approval
            let approved = copilot_reviews
                .iter()
                .any(|r| r.state == "APPROVED" || r.body.to_lowercase().contains("approved"));
            if approved && old_state.last_review_state != "approved" {
                old_state.last_review_state = "approved".to_string();
                if let Ok(Some(action)) = self
                    .call_handle_event(
                        branch,
                        agent_type,
                        "pr_review",
                        serde_json::json!({
                            "kind": "approved",
                            "pr_number": pr_number.as_u64(),
                        }),
                    )
                    .await
                {
                    self.handle_event_action(action, branch, agent_type, pr_number)
                        .await;
                }
            }

            // Check CI changes
            if ci_status != old_state.last_ci_status {
                // Status changed!
                let message = format!("[CI STATUS: {}] {}", branch, ci_status);
                self.emit_event(branch, &ci_status, &message, agent_type, Some(pr_number))
                    .await;
                old_state.last_ci_status = ci_status;
            }

            // Check 15-minute timeout (no Copilot review)
            let timeout_minutes = 15;
            if !old_state.notified_parent_timeout
                && old_state.last_review_state == "none"
                && old_state.first_seen.elapsed() > Duration::from_secs(timeout_minutes * 60)
            {
                old_state.notified_parent_timeout = true;
                if let Ok(Some(action)) = self
                    .call_handle_event(
                        branch,
                        agent_type,
                        "pr_review",
                        serde_json::json!({
                            "kind": "timeout",
                            "pr_number": pr_number.as_u64(),
                            "minutes_elapsed": timeout_minutes,
                        }),
                    )
                    .await
                {
                    self.handle_event_action(action, branch, agent_type, pr_number)
                        .await;
                }
            }
        } else {
            // New PR tracked - do not emit, just store
            state_guard.insert(
                pr_number,
                PRState {
                    last_copilot_comment_count: copilot_count,
                    last_ci_status: ci_status,
                    branch_name: branch.to_string(),
                    agent_type,
                    first_seen: Instant::now(),
                    notified_parent_timeout: false,
                    last_review_state: "none".to_string(),
                },
            );
        }

        Ok(())
    }

    /// Fetch Copilot review activity on a PR.
    ///
    /// Returns (inline_comments, review_bodies) where inline comments include
    /// file path and diff hunk context for actionable feedback.
    async fn fetch_copilot_activity(
        &self,
        owner: &str,
        repo: &str,
        pr_number: PRNumber,
    ) -> Result<(Vec<CopilotComment>, Vec<CopilotReview>)> {
        // Inline comments (with file context)
        let endpoint = format!("/repos/{}/{}/pulls/{}/comments", owner, repo, pr_number);
        let output = Command::new("gh").args(["api", &endpoint]).output().await?;
        let mut inline_comments = Vec::new();

        if output.status.success() {
            #[derive(Deserialize)]
            struct Comment {
                body: String,
                user: User,
                path: Option<String>,
                diff_hunk: Option<String>,
            }
            #[derive(Deserialize)]
            struct User {
                login: String,
            }

            if let Ok(comments) = serde_json::from_slice::<Vec<Comment>>(&output.stdout) {
                for c in comments {
                    if c.user.login.to_lowercase().contains("copilot") {
                        inline_comments.push(CopilotComment {
                            body: c.body,
                            path: c.path,
                            diff_hunk: c.diff_hunk,
                        });
                    }
                }
            }
        }

        // Reviews (with body text and state)
        let endpoint_reviews = format!("/repos/{}/{}/pulls/{}/reviews", owner, repo, pr_number);
        let output_reviews = Command::new("gh")
            .args(["api", &endpoint_reviews])
            .output()
            .await?;
        let mut copilot_reviews = Vec::new();

        if output_reviews.status.success() {
            #[derive(Deserialize)]
            struct Review {
                body: Option<String>,
                state: String,
                user: User,
            }
            #[derive(Deserialize)]
            struct User {
                login: String,
            }
            if let Ok(reviews) = serde_json::from_slice::<Vec<Review>>(&output_reviews.stdout) {
                for r in reviews {
                    if r.user.login.to_lowercase().contains("copilot") {
                        copilot_reviews.push(CopilotReview {
                            body: r.body.unwrap_or_default(),
                            state: r.state,
                        });
                    }
                }
            }
        }

        Ok((inline_comments, copilot_reviews))
    }

    async fn fetch_ci_status(&self, owner: &str, repo: &str, sha: &str) -> Result<String> {
        // gh api repos/{owner}/{repo}/commits/{sha}/check-runs
        let endpoint = format!("/repos/{}/{}/commits/{}/check-runs", owner, repo, sha);
        let output = Command::new("gh").args(["api", &endpoint]).output().await?;

        if !output.status.success() {
            return Ok("unknown".to_string());
        }

        #[derive(Deserialize)]
        struct CheckRuns {
            check_runs: Vec<CheckRun>,
        }
        #[derive(Deserialize)]
        struct CheckRun {
            conclusion: Option<String>,
            status: String,
        }

        let runs: CheckRuns = serde_json::from_slice(&output.stdout)?;

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
                    if run.status == "in_progress" || run.status == "queued" {
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

    async fn emit_event(
        &self,
        branch: &str,
        status: &str,
        message: &str,
        agent_type: AgentType,
        pr_number: Option<PRNumber>,
    ) {
        info!(
            "Emitting event for branch {}: {} - {}",
            branch, status, message
        );

        // Write to JSONL event log
        if let Some(ref log) = self.event_log {
            let event_type = match status {
                "copilot_review" => "copilot.review",
                "success" => "ci.status_changed",
                "failure" => "ci.status_changed",
                "pending" => "ci.status_changed",
                other => other,
            };
            let data = serde_json::json!({
                "branch": branch,
                "status": status,
                "message": message,
            });
            if let Err(e) = log.append(event_type, branch, &data) {
                warn!("Failed to write poller event to JSONL log: {}", e);
            }
        }

        // 1. Queue event to owning agent (branch name IS the agent's session_id)
        let event = Event {
            event_id: 0,
            event_type: Some(EventType::WorkerComplete(WorkerComplete {
                worker_id: branch.to_string(),
                status: status.to_string(),
                message: message.to_string(),
                changes: vec![],
            })),
        };
        self.event_queue.notify_event(branch, event).await;

        // 2. Inject actionable events to agent's Zellij pane
        let agent_message = match status {
            "copilot_review" => {
                let pr_arg = pr_number.map(|n| n.to_string()).unwrap_or_default();
                Some(format!(
                    "[Copilot Review on {branch}] New review comments. \
                     View with: gh pr view {pr_arg} --json reviews,comments"
                ))
            }
            "failure" => Some(format!(
                "[CI Failed]\n\
                 CI checks failed on your branch ({branch}). \
                 Investigate the failures and push a fix."
            )),
            // success, pending, etc. — non-actionable, don't interrupt the agent
            _ => None,
        };

        if let Some(agent_message) = agent_message {
            let slug = branch.rsplit_once('.').map(|(_, s)| s).unwrap_or(branch);
            let tab_name = agent_type.tab_display_name(slug);

            crate::services::delivery::deliver_to_agent(
                self.team_registry.as_deref(),
                self.acp_registry.as_deref(),
                &self.project_dir,
                branch,
                &tab_name,
                "github-poller",
                &agent_message,
                &format!("GitHub event: {} on {}", status, branch),
            )
            .await;
        }
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
                state: "APPROVED".to_string(),
            },
            CopilotReview {
                body: "Great job.".to_string(),
                state: "COMMENTED".to_string(),
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
}
