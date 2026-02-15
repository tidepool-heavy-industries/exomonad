use crate::services::event_queue::EventQueue;
use crate::services::zellij_events;
use anyhow::{Context, Result};
use exomonad_proto::effects::events::{event::EventType, Event, WorkerComplete};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::process::Command;
use tokio::sync::Mutex;
use tracing::{error, info, warn};

pub struct GitHubPoller {
    event_queue: Arc<EventQueue>,
    project_dir: PathBuf,
    poll_interval: Duration,
    state: Arc<Mutex<HashMap<u64, PRState>>>, // Map PR number -> State
    repo_info: Arc<Mutex<Option<(String, String)>>>, // (owner, name)
}

#[derive(Debug, Clone, PartialEq)]
struct PRState {
    last_copilot_comment_count: usize,
    last_ci_status: String,
    #[allow(dead_code)]
    branch_name: String,
}

impl GitHubPoller {
    pub fn new(event_queue: Arc<EventQueue>, project_dir: PathBuf) -> Self {
        Self {
            event_queue,
            project_dir,
            poll_interval: Duration::from_secs(60),
            state: Arc::new(Mutex::new(HashMap::new())),
            repo_info: Arc::new(Mutex::new(None)),
        }
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
        for branch in branches {
            if let Some(pr) = pr_map.get(&branch) {
                if let Err(e) = self
                    .process_pr(&owner, &repo, &branch, pr.number, &pr.head.sha)
                    .await
                {
                    warn!("Failed to process branch {}: {}", branch, e);
                }
            }
        }

        Ok(())
    }

    async fn get_repo_info(&self) -> Result<Option<(String, String)>> {
        let mut info_guard = self.repo_info.lock().await;
        if let Some(info) = &*info_guard {
            return Ok(Some(info.clone()));
        }

        let output = Command::new("gh")
            .args(["repo", "view", "--json", "owner,name"])
            .output()
            .await
            .context("Failed to execute gh repo view")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            warn!("Failed to get repo info: {}", stderr.trim());
            return Ok(None);
        }

        #[derive(Deserialize)]
        struct RepoInfo {
            owner: RepoOwner,
            name: String,
        }
        #[derive(Deserialize)]
        struct RepoOwner {
            login: String,
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let info: RepoInfo =
            serde_json::from_str(&stdout).context("Failed to parse repo info JSON")?;

        let result = (info.owner.login, info.name);
        *info_guard = Some(result.clone());
        Ok(Some(result))
    }

    async fn scan_worktrees(&self) -> Result<Vec<String>> {
        let worktrees_dir = self.project_dir.join(".exomonad/worktrees");
        if !worktrees_dir.exists() {
            return Ok(vec![]);
        }

        let mut branches = Vec::new();
        let mut entries = tokio::fs::read_dir(worktrees_dir).await?;

        while let Some(entry) = entries.next_entry().await? {
            if !entry.file_type().await?.is_dir() {
                continue;
            }

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
                    branches.push(branch);
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
        pr_number: u64,
        pr_sha: &str,
    ) -> Result<()> {
        // Poll details
        let (copilot_comments, copilot_reviews) =
            self.fetch_copilot_activity(owner, repo, pr_number).await?;
        let ci_status = self.fetch_ci_status(owner, repo, pr_sha).await?;

        let copilot_count = copilot_comments.len() + copilot_reviews;

        // Check for state changes
        let mut state_guard = self.state.lock().await;

        if let Some(old_state) = state_guard.get_mut(&pr_number) {
            // Check Copilot changes
            if copilot_count != old_state.last_copilot_comment_count {
                if copilot_count > old_state.last_copilot_comment_count {
                    // New activity!
                    let message = self.format_copilot_message(&copilot_comments, copilot_reviews);
                    self.emit_event(branch, "copilot_review", &message).await;
                }
                // Update state even if count decreased (to sync with reality)
                old_state.last_copilot_comment_count = copilot_count;
            }

            // Check CI changes
            if ci_status != old_state.last_ci_status {
                // Status changed!
                let message = format!("[CI STATUS: {}] {}", branch, ci_status);
                self.emit_event(branch, &ci_status, &message).await;
                old_state.last_ci_status = ci_status;
            }
        } else {
            // New PR tracked - do not emit, just store
            state_guard.insert(
                pr_number,
                PRState {
                    last_copilot_comment_count: copilot_count,
                    last_ci_status: ci_status,
                    branch_name: branch.to_string(),
                },
            );
        }

        Ok(())
    }

    async fn fetch_copilot_activity(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<(Vec<String>, usize)> {
        // Comments
        let endpoint = format!("/repos/{}/{}/pulls/{}/comments", owner, repo, pr_number);
        let output = Command::new("gh").args(["api", &endpoint]).output().await?;
        let mut comments_bodies = Vec::new();

        if output.status.success() {
            #[derive(Deserialize)]
            struct Comment {
                body: String,
                user: User,
            }
            #[derive(Deserialize)]
            struct User {
                login: String,
            }

            // Use from_slice with error handling (e.g. if empty response)
            if let Ok(comments) = serde_json::from_slice::<Vec<Comment>>(&output.stdout) {
                for c in comments {
                    if c.user.login.to_lowercase().contains("copilot") {
                        comments_bodies.push(c.body);
                    }
                }
            }
        }

        // Reviews
        let endpoint_reviews = format!("/repos/{}/{}/pulls/{}/reviews", owner, repo, pr_number);
        let output_reviews = Command::new("gh")
            .args(["api", &endpoint_reviews])
            .output()
            .await?;
        let mut review_count = 0;

        if output_reviews.status.success() {
            #[derive(Deserialize)]
            struct Review {
                user: User,
            }
            #[derive(Deserialize)]
            struct User {
                login: String,
            }
            if let Ok(reviews) = serde_json::from_slice::<Vec<Review>>(&output_reviews.stdout) {
                review_count = reviews
                    .iter()
                    .filter(|r| r.user.login.to_lowercase().contains("copilot"))
                    .count();
            }
        }

        Ok((comments_bodies, review_count))
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

    fn format_copilot_message(&self, comments: &[String], review_count: usize) -> String {
        let mut msg = format!("Copilot Review ({} reviews)", review_count);
        if !comments.is_empty() {
            msg.push_str("\nComments:\n");
            for (i, c) in comments.iter().enumerate() {
                msg.push_str(&format!("{}. {}\n", i + 1, c));
            }
        }
        msg
    }

    async fn emit_event(&self, branch: &str, status: &str, message: &str) {
        info!(
            "Emitting event for branch {}: {} - {}",
            branch, status, message
        );

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
            "copilot_review" => Some(format!(
                "[Copilot Review Feedback]\n\
                 The following review comments were left on your PR. \
                 Read each comment, then address the issues raised.\n\n\
                 {}",
                message
            )),
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
            let tab_name = format!("\u{1F916} {}", slug);
            zellij_events::inject_input(&tab_name, &agent_message);
        }
    }
}
