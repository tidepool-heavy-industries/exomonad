//! GitHub effect handler for the `github.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::github`.

use crate::effects::{
    dispatch_github_effect, EffectError, EffectHandler, EffectResult, GitHubEffects,
};
use crate::services::github::{CreatePRSpec, GitHubService, IssueFilter, PRFilter, Repo};
use async_trait::async_trait;
use exomonad_proto::effects::github::*;

/// GitHub effect handler.
///
/// Handles all effects in the `github.*` namespace by delegating to
/// the generated `dispatch_github_effect` function.
pub struct GitHubHandler {
    service: GitHubService,
}

impl GitHubHandler {
    pub fn new(service: GitHubService) -> Self {
        Self { service }
    }
}

#[async_trait]
impl EffectHandler for GitHubHandler {
    fn namespace(&self) -> &str {
        "github"
    }

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_github_effect(self, effect_type, payload, ctx).await
    }
}

#[async_trait]
impl GitHubEffects for GitHubHandler {
    async fn list_issues(
        &self,
        req: ListIssuesRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ListIssuesResponse> {
        let repo = make_repo(&req.owner, &req.repo);
        tracing::info!(owner = %req.owner, repo = %req.repo, "[GitHub] list_issues starting");

        let state = issue_state_to_filter(req.state());
        let filter = if state.is_none() && req.labels.is_empty() {
            None
        } else {
            Some(IssueFilter {
                state: state.or(Some(crate::domain::FilterState::Open)),
                labels: if req.labels.is_empty() {
                    None
                } else {
                    Some(req.labels.clone())
                },
            })
        };

        let raw_issues = self
            .service
            .list_issues(&repo, filter.as_ref())
            .await
            .map_err(|e| EffectError::network_error(e.to_string()))?;

        let limit = if req.limit <= 0 {
            30
        } else {
            req.limit as usize
        };
        let issues: Vec<Issue> = raw_issues
            .into_iter()
            .take(limit)
            .map(convert_issue)
            .collect();

        tracing::info!(count = issues.len(), "[GitHub] list_issues complete");
        Ok(ListIssuesResponse { issues })
    }

    async fn get_issue(
        &self,
        req: GetIssueRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GetIssueResponse> {
        tracing::info!(owner = %req.owner, repo = %req.repo, number = req.number, "[GitHub] get_issue starting");
        let repo = make_repo(&req.owner, &req.repo);

        let raw_issue = self
            .service
            .get_issue(&repo, req.number as u64)
            .await
            .map_err(|e| EffectError::network_error(e.to_string()))?;

        let issue = convert_issue(raw_issue);
        let comments: Vec<IssueComment> = Vec::new();

        tracing::info!(number = req.number, "[GitHub] get_issue complete");
        Ok(GetIssueResponse {
            issue: Some(issue),
            comments,
        })
    }

    async fn list_pull_requests(
        &self,
        req: ListPullRequestsRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ListPullRequestsResponse> {
        tracing::info!(owner = %req.owner, repo = %req.repo, "[GitHub] list_pull_requests starting");
        let repo = make_repo(&req.owner, &req.repo);

        let state = issue_state_to_filter(req.state());
        let limit = if req.limit <= 0 { 30 } else { req.limit as u32 };

        let filter = Some(PRFilter {
            state: state.or(Some(crate::domain::FilterState::Open)),
            limit: Some(limit),
        });

        let raw_prs = self
            .service
            .list_prs(&repo, filter.as_ref())
            .await
            .map_err(|e| EffectError::network_error(e.to_string()))?;

        let pull_requests: Vec<PullRequest> = raw_prs.into_iter().map(convert_pr).collect();

        tracing::info!(count = pull_requests.len(), "[GitHub] list_pull_requests complete");
        Ok(ListPullRequestsResponse { pull_requests })
    }

    async fn get_pull_request(
        &self,
        req: GetPullRequestRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GetPullRequestResponse> {
        tracing::info!(owner = %req.owner, repo = %req.repo, number = req.number, "[GitHub] get_pull_request starting");
        let repo = make_repo(&req.owner, &req.repo);

        let raw_pr = self
            .service
            .get_pr(&repo, req.number as u64)
            .await
            .map_err(|e| EffectError::network_error(e.to_string()))?;

        let pull_request = convert_pr(raw_pr);
        let reviews: Vec<Review> = Vec::new();

        tracing::info!(number = req.number, "[GitHub] get_pull_request complete");
        Ok(GetPullRequestResponse {
            pull_request: Some(pull_request),
            reviews,
        })
    }

    async fn get_pull_request_for_branch(
        &self,
        req: GetPullRequestForBranchRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GetPullRequestForBranchResponse> {
        tracing::info!(owner = %req.owner, repo = %req.repo, branch = %req.branch, "[GitHub] get_pull_request_for_branch starting");
        let repo = make_repo(&req.owner, &req.repo);

        let result = self
            .service
            .get_pr_for_branch(&repo, &req.branch)
            .await
            .map_err(|e| EffectError::network_error(e.to_string()))?;

        let found = result.is_some();
        tracing::info!(found, branch = %req.branch, "[GitHub] get_pull_request_for_branch complete");
        Ok(GetPullRequestForBranchResponse {
            pull_request: result.map(convert_pr),
            found,
        })
    }

    async fn create_pull_request(
        &self,
        req: CreatePullRequestRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<CreatePullRequestResponse> {
        tracing::info!(owner = %req.owner, repo = %req.repo, head = %req.head, "[GitHub] create_pull_request starting");
        let repo = make_repo(&req.owner, &req.repo);

        let spec = CreatePRSpec {
            title: req.title,
            body: req.body,
            head: req.head.clone(),
            base: if req.base.is_empty() {
                "main".to_string()
            } else {
                req.base
            },
        };

        let raw_pr = self
            .service
            .create_pr(&repo, spec)
            .await
            .map_err(|e| EffectError::network_error(e.to_string()))?;

        let url = raw_pr.url.clone();
        let pull_request = convert_pr(raw_pr);

        tracing::info!(url = %url, "[GitHub] create_pull_request complete");
        Ok(CreatePullRequestResponse {
            pull_request: Some(pull_request),
            url,
        })
    }

    async fn get_pull_request_review_comments(
        &self,
        req: GetPullRequestReviewCommentsRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GetPullRequestReviewCommentsResponse> {
        // Review comments require additional API work - return empty for now
        tracing::debug!(
            owner = %req.owner,
            repo = %req.repo,
            number = req.number,
            "get_pull_request_review_comments: not yet implemented"
        );
        Ok(GetPullRequestReviewCommentsResponse {
            comments: Vec::new(),
        })
    }
}

fn make_repo(owner: &str, repo: &str) -> Repo {
    Repo {
        owner: owner.into(),
        name: repo.into(),
    }
}

fn issue_state_to_filter(state: IssueState) -> Option<crate::domain::FilterState> {
    match state {
        IssueState::Unspecified => None,
        IssueState::Open => Some(crate::domain::FilterState::Open),
        IssueState::Closed => Some(crate::domain::FilterState::Closed),
        IssueState::All => Some(crate::domain::FilterState::All),
    }
}

fn item_state_to_proto(state: crate::domain::ItemState) -> i32 {
    match state {
        crate::domain::ItemState::Open => IssueState::Open as i32,
        crate::domain::ItemState::Closed => IssueState::Closed as i32,
        crate::domain::ItemState::Unknown => IssueState::Unspecified as i32,
    }
}

fn convert_issue(i: crate::services::github::Issue) -> Issue {
    Issue {
        number: i.number as i32,
        title: i.title,
        body: i.body,
        state: item_state_to_proto(i.state),
        author: Some(User {
            login: i.author,
            id: 0,
            avatar_url: String::new(),
        }),
        labels: i
            .labels
            .into_iter()
            .map(|l| Label {
                name: l,
                color: String::new(),
                description: String::new(),
            })
            .collect(),
        created_at: 0,
        updated_at: 0,
        comments_count: 0,
    }
}

fn convert_pr(pr: crate::services::github::PullRequest) -> PullRequest {
    PullRequest {
        number: pr.number as i32,
        title: pr.title,
        body: pr.body,
        state: item_state_to_proto(pr.state),
        author: Some(User {
            login: pr.author,
            id: 0,
            avatar_url: String::new(),
        }),
        head_ref: pr.head_ref,
        base_ref: pr.base_ref,
        merged: pr.merged_at.is_some(),
        draft: false,
        labels: Vec::new(),
        created_at: 0,
        updated_at: 0,
    }
}
