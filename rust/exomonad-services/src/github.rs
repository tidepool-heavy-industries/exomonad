use crate::{ExternalService, ServiceError};
use async_trait::async_trait;
use exomonad_shared::domain::{ItemState, ReviewState, UpperItemState};
use exomonad_shared::protocol::{
    GitHubAuthorRef, GitHubDiscussionComment, GitHubLabelRef, GitHubPRRef, GitHubReviewComment,
};
use exomonad_shared::protocol::{GitHubIssueRef, IssueState, ServiceRequest, ServiceResponse};
use octocrab::{Octocrab, OctocrabBuilder};
use reqwest::Url;

/// Service client for the GitHub API.
///
/// Uses `octocrab` for robust interactions with GitHub, handling issues and pull requests.
pub struct GitHubService {
    client: Octocrab,
}

impl GitHubService {
    /// Create a new GitHub service with the given personal access token.
    pub fn new(token: String) -> Self {
        let client = OctocrabBuilder::new()
            .personal_token(token)
            .build()
            .expect("Failed to build Octocrab client");
        Self { client }
    }

    /// Create a new GitHub service with a custom base URL.
    ///
    /// Useful for GitHub Enterprise or testing (mock servers).
    pub fn with_base_url(token: String, base_url: Url) -> Self {
        let client = OctocrabBuilder::new()
            .personal_token(token)
            .base_uri(base_url.to_string())
            .expect("Invalid base URL")
            .build()
            .expect("Failed to build Octocrab client");
        Self { client }
    }

    /// Create a new GitHub service from environment variables.
    ///
    /// Required: `GITHUB_TOKEN`.
    /// Optional: `GITHUB_API_URL`.
    pub fn from_env() -> Result<Self, anyhow::Error> {
        let token = std::env::var("GITHUB_TOKEN")?;
        let base_url = std::env::var("GITHUB_API_URL")
            .ok()
            .and_then(|s| Url::parse(&s).ok())
            .unwrap_or_else(|| Url::parse("https://api.github.com").unwrap());

        Ok(Self::with_base_url(token, base_url))
    }
}

impl GitHubService {
    /// Fetch review thread comments via GraphQL for a pull request.
    async fn fetch_review_threads(
        &self,
        owner: &str,
        repo: &str,
        number: u32,
    ) -> Result<Vec<GitHubReviewComment>, ServiceError> {
        let query = serde_json::json!({
            "query": "query($owner: String!, $repo: String!, $number: Int!) { \
                repository(owner: $owner, name: $repo) { \
                    pullRequest(number: $number) { \
                        reviewThreads(last: 100) { \
                            nodes { \
                                isResolved \
                                comments(first: 100) { \
                                    nodes { \
                                        author { login } \
                                        body \
                                        path \
                                        line \
                                        state \
                                        createdAt \
                                    } \
                                } \
                            } \
                        } \
                    } \
                } \
            }",
            "variables": {
                "owner": owner,
                "repo": repo,
                "number": number
            }
        });

        let resp: serde_json::Value =
            self.client
                .graphql(&query)
                .await
                .map_err(|e| ServiceError::Api {
                    code: 500,
                    message: e.to_string(),
                })?;

        let threads = resp
            .get("data")
            .and_then(|d| d.get("repository"))
            .and_then(|r| r.get("pullRequest"))
            .and_then(|pr| pr.get("reviewThreads"))
            .and_then(|rt| rt.get("nodes"))
            .and_then(|n| n.as_array())
            .ok_or_else(|| ServiceError::Api {
                code: 500,
                message: "Failed to parse GraphQL review threads response".into(),
            })?;

        let mut reviews = Vec::new();
        for thread in threads {
            if let Some(comments_nodes) = thread
                .get("comments")
                .and_then(|c| c.get("nodes"))
                .and_then(|n| n.as_array())
            {
                for comment in comments_nodes {
                    let author = comment
                        .get("author")
                        .and_then(|a| a.get("login"))
                        .and_then(|l| l.as_str())
                        .unwrap_or("unknown")
                        .to_string();
                    let body = comment
                        .get("body")
                        .and_then(|b| b.as_str())
                        .unwrap_or("")
                        .to_string();
                    let path = comment
                        .get("path")
                        .and_then(|p| p.as_str())
                        .unwrap_or("")
                        .to_string();
                    let line = comment
                        .get("line")
                        .and_then(|l| l.as_u64())
                        .map(|u| u as u32);
                    let state_str = comment
                        .get("state")
                        .and_then(|s| s.as_str())
                        .unwrap_or("PENDING");
                    
                    let state = match state_str {
                        "PENDING" => ReviewState::Pending,
                        "SUBMITTED" => ReviewState::Commented, // Map SUBMITTED to Commented as closest match? Or assume SUBMITTED means it's a comment.
                        "APPROVED" => ReviewState::Approved,
                        "CHANGES_REQUESTED" => ReviewState::ChangesRequested,
                        "DISMISSED" => ReviewState::Dismissed,
                        _ => ReviewState::Commented, // Default fallback
                    };

                    let created_at = comment
                        .get("createdAt")
                        .and_then(|d| d.as_str())
                        .unwrap_or("")
                        .to_string();

                    reviews.push(GitHubReviewComment {
                        author,
                        body,
                        path,
                        line,
                        state,
                        created_at,
                    });
                }
            }
        }

        Ok(reviews)
    }
}

fn state_to_item_state(state: octocrab::models::IssueState) -> ItemState {
    match state {
        octocrab::models::IssueState::Open => ItemState::Open,
        octocrab::models::IssueState::Closed => ItemState::Closed,
        _ => ItemState::Unknown,
    }
}

fn issue_state_to_upper_item_state(state: octocrab::models::IssueState) -> UpperItemState {
    match state {
        octocrab::models::IssueState::Open => UpperItemState::Open,
        octocrab::models::IssueState::Closed => UpperItemState::Closed,
        _ => UpperItemState::Unknown,
    }
}

#[async_trait]
impl ExternalService for GitHubService {
    type Request = ServiceRequest;
    type Response = ServiceResponse;

    async fn call(&self, req: Self::Request) -> Result<Self::Response, ServiceError> {
        match req {
            ServiceRequest::GitHubGetIssue {
                owner,
                repo,
                number,
                include_comments,
            } => {
                let issue = self
                    .client
                    .issues(owner.as_str(), repo.as_str())
                    .get(number.into())
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                let comments = if include_comments {
                    self.client
                        .issues(owner.as_str(), repo.as_str())
                        .list_comments(number.into())
                        .send()
                        .await
                        .map_err(|e| ServiceError::Api {
                            code: 500,
                            message: e.to_string(),
                        })?
                        .items
                        .into_iter()
                        .map(|c| GitHubDiscussionComment {
                            author: c.user.login,
                            body: c.body.unwrap_or_default(),
                            created_at: c.created_at.to_rfc3339(),
                            replies: vec![],
                        })
                        .collect()
                } else {
                    vec![]
                };

                Ok(ServiceResponse::GitHubIssue {
                    number: issue.number as u32,
                    title: issue.title,
                    body: issue.body.unwrap_or_default(),
                    state: state_to_item_state(issue.state),
                    labels: issue.labels.into_iter().map(|l| l.name).collect(),
                    url: issue.html_url.to_string(),
                    author: issue.user.login.clone(),
                    comments,
                })
            }
            ServiceRequest::GitHubCreateIssue {
                owner,
                repo,
                title,
                body,
                labels,
            } => {
                let issue = self
                    .client
                    .issues(owner, repo)
                    .create(title)
                    .body(body)
                    .labels(labels)
                    .send()
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                Ok(ServiceResponse::GitHubIssue {
                    number: issue.number as u32,
                    title: issue.title,
                    body: issue.body.unwrap_or_default(),
                    state: state_to_item_state(issue.state),
                    labels: issue.labels.into_iter().map(|l| l.name).collect(),
                    url: issue.html_url.to_string(),
                    author: issue.user.login.clone(),
                    comments: vec![],
                })
            }
            ServiceRequest::GitHubListIssues {
                owner,
                repo,
                state,
                labels,
            } => {
                let state = match state {
                    Some(IssueState::Open) => octocrab::params::State::Open,
                    Some(IssueState::Closed) => octocrab::params::State::Closed,
                    Some(IssueState::All) | None => octocrab::params::State::All,
                };

                let page = self
                    .client
                    .issues(owner, repo)
                    .list()
                    .state(state)
                    .labels(&labels)
                    .per_page(100)
                    .send()
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                let all_issues =
                    self.client
                        .all_pages(page)
                        .await
                        .map_err(|e| ServiceError::Api {
                            code: 500,
                            message: e.to_string(),
                        })?;

                let issues = all_issues
                    .into_iter()
                    .map(|i| GitHubIssueRef {
                        number: i.number as u32,
                        title: i.title,
                        body: i.body.unwrap_or_default(),
                        state: issue_state_to_upper_item_state(i.state),
                        url: i.html_url.to_string(),
                        author: GitHubAuthorRef {
                            login: i.user.login,
                            name: None,
                        },
                        labels: i
                            .labels
                            .into_iter()
                            .map(|l| GitHubLabelRef { name: l.name })
                            .collect(),
                        comments: vec![],
                    })
                    .collect();

                Ok(ServiceResponse::GitHubIssues { issues })
            }
            ServiceRequest::GitHubUpdateIssue {
                owner,
                repo,
                number,
                title,
                body,
                state,
                labels,
                assignees,
            } => {
                let issues = self.client.issues(owner.as_str(), repo.as_str());
                let mut builder = issues.update(number.into());

                if let Some(ref t) = title {
                    builder = builder.title(t);
                }
                if let Some(ref b) = body {
                    builder = builder.body(b);
                }
                if let Some(ref s) = state {
                    let s_enum = match s {
                        ItemState::Open => octocrab::models::IssueState::Open,
                        ItemState::Closed => octocrab::models::IssueState::Closed,
                        ItemState::Unknown => {
                            return Err(ServiceError::Api {
                                code: 400,
                                message: "Cannot update issue state to 'unknown'".to_string(),
                            })
                        }
                    };
                    builder = builder.state(s_enum);
                }
                if let Some(ref l) = labels {
                    builder = builder.labels(l);
                }
                if let Some(ref a) = assignees {
                    builder = builder.assignees(a);
                }

                let issue = builder.send().await.map_err(|e| ServiceError::Api {
                    code: 500,
                    message: e.to_string(),
                })?;

                Ok(ServiceResponse::GitHubIssue {
                    number: issue.number as u32,
                    title: issue.title,
                    body: issue.body.unwrap_or_default(),
                    state: state_to_item_state(issue.state),
                    labels: issue.labels.into_iter().map(|l| l.name).collect(),
                    url: issue.html_url.to_string(),
                    author: issue.user.login.clone(),
                    comments: vec![],
                })
            }
            ServiceRequest::GitHubAddIssueLabel {
                owner,
                repo,
                number,
                label,
            } => {
                self.client
                    .issues(owner, repo)
                    .add_labels(number.into(), &[label])
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;
                Ok(ServiceResponse::OtelAck) // Simple ack
            }
            ServiceRequest::GitHubRemoveIssueLabel {
                owner,
                repo,
                number,
                label,
            } => {
                self.client
                    .issues(owner, repo)
                    .remove_label(number.into(), label)
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;
                Ok(ServiceResponse::OtelAck)
            }
            ServiceRequest::GitHubAddIssueAssignee {
                owner,
                repo,
                number,
                assignee,
            } => {
                self.client
                    .issues(owner, repo)
                    .add_assignees(number.into(), &[&assignee])
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;
                Ok(ServiceResponse::OtelAck)
            }
            ServiceRequest::GitHubRemoveIssueAssignee {
                owner: _,
                repo: _,
                number: _,
                assignee: _,
            } => {
                // FIXME: octocrab 0.38 seems to lack remove_assignees or I have the wrong name.
                // Disabling for now to fix build.
                return Err(ServiceError::Api {
                    code: 501,
                    message: "GitHubRemoveIssueAssignee not implemented in Rust service yet".into(),
                });
            }
            ServiceRequest::GitHubListPullRequests {
                owner,
                repo,
                state,
                limit,
            } => {
                let state_enum = match state.as_deref() {
                    Some("open") => octocrab::params::State::Open,
                    Some("closed") => octocrab::params::State::Closed,
                    Some("all") => octocrab::params::State::All,
                    None => octocrab::params::State::Open,
                    Some(s) => {
                        return Err(ServiceError::Api {
                            code: 400,
                            message: format!("Invalid state: {}", s),
                        })
                    }
                };

                let page = self
                    .client
                    .pulls(owner, repo)
                    .list()
                    .state(state_enum)
                    .per_page(limit.unwrap_or(30) as u8)
                    .send()
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                let prs = page
                    .into_iter()
                    .map(|pr| GitHubPRRef {
                        number: pr.number as u32,
                        title: pr.title.unwrap_or_default(),
                        state: state_to_item_state(
                            pr.state.unwrap_or(octocrab::models::IssueState::Open),
                        ),
                        url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
                    })
                    .collect();

                Ok(ServiceResponse::GitHubPullRequests { pull_requests: prs })
            }
            ServiceRequest::GitHubGetPullRequestReviews {
                owner,
                repo,
                number,
            } => {
                let reviews = self
                    .fetch_review_threads(owner.as_str(), repo.as_str(), number)
                    .await?;
                Ok(ServiceResponse::GitHubReviews { reviews })
            }
            ServiceRequest::GitHubGetDiscussion {
                owner,
                repo,
                number,
            } => {
                let query = serde_json::json!({
                    "query": "query($owner: String!, $repo: String!, $number: Int!) { \
                        repository(owner: $owner, name: $repo) { \
                            discussion(number: $number) { \
                                number \
                                title \
                                body \
                                url \
                                author { login } \
                                comments(first: 50) { \
                                    nodes { \
                                        author { login } \
                                        body \
                                        createdAt \
                                        replies(first: 20) { \
                                            nodes { \
                                                author { login } \
                                                body \
                                                createdAt \
                                            } \
                                        } \
                                    } \
                                } \
                            } \
                        } \
                    }",
                    "variables": {
                        "owner": owner,
                        "repo": repo,
                        "number": number
                    }
                });

                let resp: serde_json::Value =
                    self.client
                        .graphql(&query)
                        .await
                        .map_err(|e| ServiceError::Api {
                            code: 500,
                            message: e.to_string(),
                        })?;

                let discussion = resp
                    .get("data")
                    .and_then(|d| d.get("repository"))
                    .and_then(|r| r.get("discussion"))
                    .ok_or_else(|| ServiceError::Api {
                        code: 404,
                        message: "Discussion not found".into(),
                    })?;

                let number = discussion
                    .get("number")
                    .and_then(|n| n.as_u64())
                    .unwrap_or(0) as u32;
                let title = discussion
                    .get("title")
                    .and_then(|t| t.as_str())
                    .unwrap_or("")
                    .to_string();
                let body = discussion
                    .get("body")
                    .and_then(|b| b.as_str())
                    .unwrap_or("")
                    .to_string();
                let url = discussion
                    .get("url")
                    .and_then(|u| u.as_str())
                    .unwrap_or("")
                    .to_string();
                let author = discussion
                    .get("author")
                    .and_then(|a| a.get("login"))
                    .and_then(|l| l.as_str())
                    .unwrap_or("unknown")
                    .to_string();

                let mut comments = Vec::new();
                if let Some(nodes) = discussion
                    .get("comments")
                    .and_then(|c| c.get("nodes"))
                    .and_then(|n| n.as_array())
                {
                    for node in nodes {
                        let c_author = node
                            .get("author")
                            .and_then(|a| a.get("login"))
                            .and_then(|l| l.as_str())
                            .unwrap_or("unknown")
                            .to_string();
                        let c_body = node
                            .get("body")
                            .and_then(|b| b.as_str())
                            .unwrap_or("")
                            .to_string();
                        let c_created_at = node
                            .get("createdAt")
                            .and_then(|d| d.as_str())
                            .unwrap_or("")
                            .to_string();

                        let mut replies = Vec::new();
                        if let Some(reply_nodes) = node
                            .get("replies")
                            .and_then(|r| r.get("nodes"))
                            .and_then(|n| n.as_array())
                        {
                            for reply in reply_nodes {
                                let r_author = reply
                                    .get("author")
                                    .and_then(|a| a.get("login"))
                                    .and_then(|l| l.as_str())
                                    .unwrap_or("unknown")
                                    .to_string();
                                let r_body = reply
                                    .get("body")
                                    .and_then(|b| b.as_str())
                                    .unwrap_or("")
                                    .to_string();
                                let r_created_at = reply
                                    .get("createdAt")
                                    .and_then(|d| d.as_str())
                                    .unwrap_or("")
                                    .to_string();
                                replies.push(GitHubDiscussionComment {
                                    author: r_author,
                                    body: r_body,
                                    created_at: r_created_at,
                                    replies: Vec::new(),
                                });
                            }
                        }

                        comments.push(GitHubDiscussionComment {
                            author: c_author,
                            body: c_body,
                            created_at: c_created_at,
                            replies,
                        });
                    }
                }

                Ok(ServiceResponse::GitHubDiscussion {
                    number,
                    title,
                    body,
                    author,
                    url,
                    comments,
                })
            }
            ServiceRequest::GitHubCreatePR {
                owner,
                repo,
                title,
                body,
                head,
                base,
            } => {
                let pr = self
                    .client
                    .pulls(owner, repo)
                    .create(title, head, base)
                    .body(body)
                    .send()
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                Ok(ServiceResponse::GitHubPR {
                    number: pr.number as u32,
                    title: pr.title.unwrap_or_default(),
                    body: pr.body.unwrap_or_default(),
                    author: pr.user.map(|u| u.login).unwrap_or_else(|| "unknown".into()),
                    url: pr.html_url.expect("PR has no URL").to_string(),
                    state: state_to_item_state(pr.state.expect("PR has no state")),
                    head_ref_name: pr.head.ref_field,
                    base_ref_name: pr.base.ref_field,
                    created_at: pr.created_at.map(|t| t.to_rfc3339()).unwrap_or_default(),
                    merged_at: None,
                    labels: vec![],
                    comments: vec![],
                    reviews: vec![],
                })
            }
            ServiceRequest::GitHubGetPR {
                owner,
                repo,
                number,
                include_details,
            } => {
                let pr = self
                    .client
                    .pulls(owner.as_str(), repo.as_str())
                    .get(number.into())
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                let merged_at = pr.merged_at.map(|t| t.to_rfc3339());
                let labels = pr
                    .labels
                    .as_deref()
                    .unwrap_or(&[])
                    .iter()
                    .map(|l| l.name.clone())
                    .collect();

                let (comments, reviews) = if include_details {
                    // Fetch issue comments (PR discussions use the issues API)
                    let issue_comments = self
                        .client
                        .issues(owner.as_str(), repo.as_str())
                        .list_comments(number.into())
                        .send()
                        .await
                        .map_err(|e| ServiceError::Api {
                            code: 500,
                            message: format!("Failed to fetch PR comments: {}", e),
                        })?
                        .items
                        .into_iter()
                        .map(|c| GitHubDiscussionComment {
                            author: c.user.login,
                            body: c.body.unwrap_or_default(),
                            created_at: c.created_at.to_rfc3339(),
                            replies: vec![],
                        })
                        .collect();

                    // Fetch review threads via GraphQL
                    let review_comments = self
                        .fetch_review_threads(owner.as_str(), repo.as_str(), number)
                        .await
                        .unwrap_or_default();

                    (issue_comments, review_comments)
                } else {
                    (vec![], vec![])
                };

                Ok(ServiceResponse::GitHubPR {
                    number: pr.number as u32,
                    title: pr.title.unwrap_or_default(),
                    body: pr.body.unwrap_or_default(),
                    author: pr.user.map(|u| u.login).unwrap_or_else(|| "unknown".into()),
                    url: pr.html_url.expect("PR has no URL").to_string(),
                    state: state_to_item_state(pr.state.expect("PR has no state")),
                    head_ref_name: pr.head.ref_field,
                    base_ref_name: pr.base.ref_field,
                    created_at: pr.created_at.map(|t| t.to_rfc3339()).unwrap_or_default(),
                    merged_at,
                    labels,
                    comments,
                    reviews,
                })
            }
            _ => panic!("Invalid request type for GitHubService"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    // Octocrab uses reqwest under the hood, so we can still mock it
    // IF we can point it to the mock server.
    // OctocrabBuilder::base_uri takes a string.

    #[tokio::test]
    async fn test_github_get_issue() {
        let mock_server = MockServer::start().await;

        let mock_response = serde_json::json!({
            "id": 1001,
            "node_id": "MDU6SXNzdWUxMDAx",
            "url": "https://api.github.com/repos/owner/repo/issues/1",
            "repository_url": "https://api.github.com/repos/owner/repo",
            "labels_url": "https://api.github.com/repos/owner/repo/issues/1/labels{/name}",
            "comments_url": "https://api.github.com/repos/owner/repo/issues/1/comments",
            "events_url": "https://api.github.com/repos/owner/repo/issues/1/events",
            "html_url": "https://github.com/owner/repo/issues/1",
            "number": 1,
            "title": "Bug fix",
            "body": "Fixes a bug",
            "state": "open",
            "labels": [{
                "id": 123,
                "node_id": "MDU6TGFiZWwxMjM=",
                "url": "https://api.github.com/repos/owner/repo/labels/bug",
                "name": "bug",
                "color": "f29513",
                "default": true
            }],
            "user": {
                "login": "user",
                "id": 1,
                "node_id": "MDQ6VXNlcjE=",
                "avatar_url": "https://example.com/avatar",
                "gravatar_id": "",
                "url": "https://api.github.com/users/user",
                "html_url": "https://github.com/user",
                "followers_url": "https://api.github.com/users/user/followers",
                "following_url": "https://api.github.com/users/user/following{/other_user}",
                "gists_url": "https://api.github.com/users/user/gists{/gist_id}",
                "starred_url": "https://api.github.com/users/user/starred{/owner}{/repo}",
                "subscriptions_url": "https://api.github.com/users/user/subscriptions",
                "organizations_url": "https://api.github.com/users/user/orgs",
                "repos_url": "https://api.github.com/users/user/repos",
                "events_url": "https://api.github.com/users/user/events{/privacy}",
                "received_events_url": "https://api.github.com/users/user/received_events",
                "type": "User",
                "site_admin": false
            },
            "locked": false,
            "assignees": [],
            "milestone": null,
            "comments": 0,
            "created_at": "2021-01-01T00:00:00Z",
            "updated_at": "2021-01-01T00:00:00Z",
            "closed_at": null,
            "author_association": "NONE"
        });

        Mock::given(method("GET"))
            .and(path("/repos/owner/repo/issues/1"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let service =
            GitHubService::with_base_url("token".into(), mock_server.uri().parse().unwrap());

        let req = ServiceRequest::GitHubGetIssue {
            owner: "owner".into(),
            repo: "repo".into(),
            number: 1,
            include_comments: false,
        };

        match service.call(req).await.unwrap() {
            ServiceResponse::GitHubIssue { number, title, .. } => {
                assert_eq!(number, 1);
                assert_eq!(title, "Bug fix");
            }
            _ => panic!("Wrong response type"),
        }
    }
}
