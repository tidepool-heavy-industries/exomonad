use crate::{ExternalService, ServiceError};
use async_trait::async_trait;
use exomonad_shared::{GitHubIssueRef, IssueState, ServiceRequest, ServiceResponse};
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
}

fn state_to_string(state: octocrab::models::IssueState) -> String {
    match state {
        octocrab::models::IssueState::Open => "open".to_string(),
        octocrab::models::IssueState::Closed => "closed".to_string(),
        _ => "unknown".to_string(),
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
            } => {
                let issue = self
                    .client
                    .issues(owner, repo)
                    .get(number.into())
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                Ok(ServiceResponse::GitHubIssue {
                    number: issue.number as u32,
                    title: issue.title,
                    body: issue.body.unwrap_or_default(),
                    state: state_to_string(issue.state),
                    labels: issue.labels.into_iter().map(|l| l.name).collect(),
                    url: issue.html_url.to_string(),
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
                    state: state_to_string(issue.state),
                    labels: issue.labels.into_iter().map(|l| l.name).collect(),
                    url: issue.html_url.to_string(),
                })
            }
            ServiceRequest::GitHubListIssues {
                owner,
                repo,
                state,
                labels,
            } => {
                let state = match state {
                    IssueState::Open => octocrab::params::State::Open,
                    IssueState::Closed => octocrab::params::State::Closed,
                    IssueState::All => octocrab::params::State::All,
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

                let all_issues = self
                    .client
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
                        state: state_to_string(i.state),
                    })
                    .collect();

                Ok(ServiceResponse::GitHubIssues { issues })
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
                    url: pr.html_url.expect("PR has no URL").to_string(),
                    state: state_to_string(pr.state.expect("PR has no state")),
                })
            }
            ServiceRequest::GitHubGetPR {
                owner,
                repo,
                number,
            } => {
                let pr = self
                    .client
                    .pulls(owner, repo)
                    .get(number.into())
                    .await
                    .map_err(|e| ServiceError::Api {
                        code: 500,
                        message: e.to_string(),
                    })?;

                Ok(ServiceResponse::GitHubPR {
                    number: pr.number as u32,
                    url: pr.html_url.expect("PR has no URL").to_string(),
                    state: state_to_string(pr.state.expect("PR has no state")),
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

        let service = GitHubService::with_base_url("token".into(), mock_server.uri().parse().unwrap());
        
        let req = ServiceRequest::GitHubGetIssue {
            owner: "owner".into(),
            repo: "repo".into(),
            number: 1,
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
