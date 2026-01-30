use anyhow::Result;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::MemoryHandle;
use octocrab::{models, params, Octocrab, OctocrabBuilder};
use serde::{Deserialize, Serialize};

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Repo {
    pub owner: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IssueFilter {
    pub state: Option<String>,
    pub labels: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreatePRSpec {
    pub title: String,
    pub body: String,
    pub head: String,
    pub base: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PRFilter {
    pub state: Option<String>,
    pub limit: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Issue {
    pub number: u64,
    pub title: String,
    pub body: String,
    pub state: String,
    pub url: String,
    pub author: String,
    pub labels: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PullRequest {
    pub number: u64,
    pub title: String,
    pub body: String,
    pub state: String,
    pub url: String,
    pub author: String,
    pub head_ref: String,
    pub base_ref: String,
    pub created_at: String,
    pub merged_at: Option<String>,
}

// ============================================================================
// Service Implementation
// ============================================================================

pub struct GitHubService {
    client: Octocrab,
}

impl GitHubService {
    pub fn new(token: String) -> Result<Self> {
        let client = OctocrabBuilder::new().personal_token(token).build()?;
        Ok(Self { client })
    }

    pub async fn list_issues(
        &self,
        repo: &Repo,
        filter: Option<&IssueFilter>,
    ) -> Result<Vec<Issue>> {
        let issues_handler = self.client.issues(&repo.owner, &repo.name);
        let mut builder = issues_handler.list();

        if let Some(f) = filter {
            if let Some(state) = &f.state {
                let s = match state.as_str() {
                    "open" => params::State::Open,
                    "closed" => params::State::Closed,
                    _ => params::State::All,
                };
                builder = builder.state(s);
            }
            if let Some(labels) = &f.labels {
                if !labels.is_empty() {
                    // Octocrab expects generic iterable
                    builder = builder.labels(labels);
                }
            }
        }

        let page = builder.send().await?;
        let issues = self.client.all_pages(page).await?;

        Ok(issues
            .into_iter()
            .map(|i| Issue {
                number: i.number,
                title: i.title,
                body: i.body.unwrap_or_default(),
                state: match i.state {
                    models::IssueState::Open => "open".to_string(),
                    models::IssueState::Closed => "closed".to_string(),
                    _ => "unknown".to_string(),
                },
                url: i.html_url.to_string(),
                author: i.user.login,
                labels: i.labels.into_iter().map(|l| l.name).collect(),
            })
            .collect())
    }

    pub async fn get_issue(&self, repo: &Repo, number: u64) -> Result<Issue> {
        let issue = self
            .client
            .issues(&repo.owner, &repo.name)
            .get(number)
            .await?;

        Ok(Issue {
            number: issue.number,
            title: issue.title,
            body: issue.body.unwrap_or_default(),
            state: match issue.state {
                models::IssueState::Open => "open".to_string(),
                models::IssueState::Closed => "closed".to_string(),
                _ => "unknown".to_string(),
            },
            url: issue.html_url.to_string(),
            author: issue.user.login,
            labels: issue.labels.into_iter().map(|l| l.name).collect(),
        })
    }

    pub async fn create_pr(&self, repo: &Repo, spec: CreatePRSpec) -> Result<PullRequest> {
        let pr = self
            .client
            .pulls(&repo.owner, &repo.name)
            .create(spec.title, spec.head, spec.base)
            .body(spec.body)
            .send()
            .await?;

        Ok(PullRequest {
            number: pr.number,
            title: pr.title.unwrap_or_default(),
            body: pr.body.unwrap_or_default(),
            state: match pr.state {
                Some(models::IssueState::Open) => "open".to_string(),
                Some(models::IssueState::Closed) => "closed".to_string(),
                _ => "unknown".to_string(),
            },
            url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
            author: pr.user.map(|u| u.login).unwrap_or_else(|| "unknown".into()),
            head_ref: pr.head.ref_field,
            base_ref: pr.base.ref_field,
            created_at: pr.created_at.map(|t| t.to_rfc3339()).unwrap_or_default(),
            merged_at: pr.merged_at.map(|t| t.to_rfc3339()),
        })
    }

    pub async fn list_prs(
        &self,
        repo: &Repo,
        filter: Option<&PRFilter>,
    ) -> Result<Vec<PullRequest>> {
        let pulls_handler = self.client.pulls(&repo.owner, &repo.name);
        let mut builder = pulls_handler.list();

        if let Some(f) = filter {
            if let Some(state) = &f.state {
                let s = match state.as_str() {
                    "open" => params::State::Open,
                    "closed" => params::State::Closed,
                    _ => params::State::All,
                };
                builder = builder.state(s);
            }
            if let Some(limit) = f.limit {
                builder = builder.per_page(limit as u8);
            }
        }

        let page = builder.send().await?;
        // For PRs, we might not want all pages if a limit was set, but octocrab's list() returns a Page.
        // If limit was set, we used per_page.

        Ok(page
            .into_iter()
            .map(|pr| PullRequest {
                number: pr.number,
                title: pr.title.unwrap_or_default(),
                body: pr.body.unwrap_or_default(),
                state: match pr.state {
                    Some(models::IssueState::Open) => "open".to_string(),
                    Some(models::IssueState::Closed) => "closed".to_string(),
                    _ => "unknown".to_string(),
                },
                url: pr.html_url.map(|u| u.to_string()).unwrap_or_default(),
                author: pr.user.map(|u| u.login).unwrap_or_else(|| "unknown".into()),
                head_ref: pr.head.ref_field,
                base_ref: pr.base.ref_field,
                created_at: pr.created_at.map(|t| t.to_rfc3339()).unwrap_or_default(),
                merged_at: pr.merged_at.map(|t| t.to_rfc3339()),
            })
            .collect())
    }
}

// ============================================================================
// Host Functions
// ============================================================================

#[derive(Serialize, Deserialize)]
struct HostResult<T> {
    kind: String,
    payload: Option<T>,
    error: Option<HostError>,
}

#[derive(Serialize, Deserialize)]
struct HostError {
    message: String,
    code: String,
}

impl<T> HostResult<T> {
    fn success(payload: T) -> Self {
        Self {
            kind: "Success".to_string(),
            payload: Some(payload),
            error: None,
        }
    }

    fn error(message: String, code: String) -> Self {
        Self {
            kind: "Error".to_string(),
            payload: None,
            error: Some(HostError { message, code }),
        }
    }
}

fn map_error(e: anyhow::Error) -> (String, String) {
    let msg = e.to_string();
    if msg.contains("403") || msg.contains("rate limit") {
        (msg, "rate_limited".to_string())
    } else if msg.contains("404") {
        (msg, "not_found".to_string())
    } else if msg.contains("401") {
        (msg, "unauthorized".to_string())
    } else {
        (msg, "internal_error".to_string())
    }
}

// Define the host functions
pub fn register_host_functions() -> Vec<Function> {
    vec![
        Function::new(
            "github_list_issues",
            [ValType::I64, ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_list_issues,
        ),
        Function::new(
            "github_get_issue",
            [ValType::I64, ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_get_issue,
        ),
        Function::new(
            "github_create_pr",
            [ValType::I64, ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_create_pr,
        ),
        Function::new(
            "github_list_prs",
            [ValType::I64, ValType::I64],
            [ValType::I64],
            UserData::new(()),
            github_list_prs,
        ),
    ]
}

fn github_list_issues(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    let offset = inputs[0].unwrap_i64() as u64;
    let len = inputs[1].unwrap_i64() as u64;
    let handle = unsafe { MemoryHandle::new(offset, len) };
    let input_str = plugin.memory_str(handle)?;

    #[derive(Deserialize)]
    struct Input {
        repo: Repo,
        filter: Option<IssueFilter>,
    }

    let input: Input = serde_json::from_str(input_str)?;

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(service.list_issues(&input.repo, input.filter.as_ref()));

    let response = match result {
        Ok(issues) => HostResult::success(issues),
        Err(e) => {
            let (msg, code) = map_error(e);
            HostResult::error(msg, code)
        }
    };

    let json = serde_json::to_string(&response)?;
    let handle = plugin.memory_alloc(json.len() as u64)?;
    let output_offset = handle.offset();

    {
        let mem_str = plugin.memory_str_mut(handle)?;
        unsafe {
            let slice = mem_str.as_bytes_mut();
            slice.copy_from_slice(json.as_bytes());
        }
    }

    outputs[0] = Val::I64(output_offset as i64);

    Ok(())
}

fn github_get_issue(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    let offset = inputs[0].unwrap_i64() as u64;
    let len = inputs[1].unwrap_i64() as u64;
    let handle = unsafe { MemoryHandle::new(offset, len) };
    let input_str = plugin.memory_str(handle)?;

    #[derive(Deserialize)]
    struct Input {
        repo: Repo,
        number: u64,
    }

    let input: Input = serde_json::from_str(input_str)?;

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(service.get_issue(&input.repo, input.number));

    let response = match result {
        Ok(issue) => HostResult::success(issue),
        Err(e) => {
            let (msg, code) = map_error(e);
            HostResult::error(msg, code)
        }
    };

    let json = serde_json::to_string(&response)?;
    let handle = plugin.memory_alloc(json.len() as u64)?;
    let output_offset = handle.offset();

    {
        let mem_str = plugin.memory_str_mut(handle)?;
        unsafe {
            let slice = mem_str.as_bytes_mut();
            slice.copy_from_slice(json.as_bytes());
        }
    }

    outputs[0] = Val::I64(output_offset as i64);

    Ok(())
}

fn github_create_pr(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    let offset = inputs[0].unwrap_i64() as u64;
    let len = inputs[1].unwrap_i64() as u64;
    let handle = unsafe { MemoryHandle::new(offset, len) };
    let input_str = plugin.memory_str(handle)?;

    #[derive(Deserialize)]
    struct Input {
        repo: Repo,
        spec: CreatePRSpec,
    }

    let input: Input = serde_json::from_str(input_str)?;

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(service.create_pr(&input.repo, input.spec));

    let response = match result {
        Ok(pr) => HostResult::success(pr),
        Err(e) => {
            let (msg, code) = map_error(e);
            HostResult::error(msg, code)
        }
    };

    let json = serde_json::to_string(&response)?;
    let handle = plugin.memory_alloc(json.len() as u64)?;
    let output_offset = handle.offset();

    {
        let mem_str = plugin.memory_str_mut(handle)?;
        unsafe {
            let slice = mem_str.as_bytes_mut();
            slice.copy_from_slice(json.as_bytes());
        }
    }

    outputs[0] = Val::I64(output_offset as i64);

    Ok(())
}

fn github_list_prs(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    let offset = inputs[0].unwrap_i64() as u64;
    let len = inputs[1].unwrap_i64() as u64;
    let handle = unsafe { MemoryHandle::new(offset, len) };
    let input_str = plugin.memory_str(handle)?;

    #[derive(Deserialize)]
    struct Input {
        repo: Repo,
        filter: Option<PRFilter>,
    }

    let input: Input = serde_json::from_str(input_str)?;

    let service = GitHubService::new(std::env::var("GITHUB_TOKEN").unwrap_or_default())
        .map_err(|e| Error::msg(e.to_string()))?;

    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(service.list_prs(&input.repo, input.filter.as_ref()));

    let response = match result {
        Ok(prs) => HostResult::success(prs),
        Err(e) => {
            let (msg, code) = map_error(e);
            HostResult::error(msg, code)
        }
    };

    let json = serde_json::to_string(&response)?;
    let handle = plugin.memory_alloc(json.len() as u64)?;
    let output_offset = handle.offset();

    {
        let mem_str = plugin.memory_str_mut(handle)?;
        unsafe {
            let slice = mem_str.as_bytes_mut();
            slice.copy_from_slice(json.as_bytes());
        }
    }

    outputs[0] = Val::I64(output_offset as i64);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    async fn create_mock_service() -> (GitHubService, MockServer) {
        let mock_server = MockServer::start().await;
        let client = OctocrabBuilder::new()
            .personal_token("test_token".to_string())
            .base_uri(mock_server.uri())
            .unwrap()
            .build()
            .unwrap();
        (GitHubService { client }, mock_server)
    }

    #[tokio::test]
    async fn test_list_issues() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!([
            {
                "id": 1,
                "node_id": "MDU6SXNzdWUx",
                "number": 1,
                "title": "Test Issue",
                "state": "open",
                "html_url": "http://github.com/owner/repo/issues/1",
                "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
                "labels": [],
                "body": "Test Body",
                "created_at": "2023-01-01T00:00:00Z",
                "updated_at": "2023-01-01T00:00:00Z",
                "url": "http://api.github.com/repos/owner/repo/issues/1",
                "repository_url": "http://api.github.com/repos/owner/repo",
                "labels_url": "http://api.github.com/repos/owner/repo/issues/1/labels{/name}",
                "comments_url": "http://api.github.com/repos/owner/repo/issues/1/comments",
                "events_url": "http://api.github.com/repos/owner/repo/issues/1/events",
                "comments": 0,
                "assignees": [],
                "author_association": "NONE",
                "locked": false
            }
        ]);

        Mock::given(method("GET"))
            .and(path("/repos/owner/repo/issues"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".to_string(),
            name: "repo".to_string(),
        };

        let issues = service.list_issues(&repo, None).await.unwrap();
        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].title, "Test Issue");
        assert_eq!(issues[0].author, "testuser");
    }

    #[tokio::test]
    async fn test_get_issue() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!({
            "id": 1,
            "node_id": "MDU6SXNzdWUx",
            "number": 1,
            "title": "Test Issue",
            "state": "open",
            "html_url": "http://github.com/owner/repo/issues/1",
            "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
            "labels": [],
            "body": "Test Body",
            "created_at": "2023-01-01T00:00:00Z",
            "updated_at": "2023-01-01T00:00:00Z",
            "url": "http://api.github.com/repos/owner/repo/issues/1",
            "repository_url": "http://api.github.com/repos/owner/repo",
            "labels_url": "http://api.github.com/repos/owner/repo/issues/1/labels{/name}",
            "comments_url": "http://api.github.com/repos/owner/repo/issues/1/comments",
            "events_url": "http://api.github.com/repos/owner/repo/issues/1/events",
            "comments": 0,
            "assignees": [],
            "author_association": "NONE",
            "locked": false
        });

        Mock::given(method("GET"))
            .and(path("/repos/owner/repo/issues/1"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".to_string(),
            name: "repo".to_string(),
        };

        let issue = service.get_issue(&repo, 1).await.unwrap();
        assert_eq!(issue.number, 1);
        assert_eq!(issue.title, "Test Issue");
    }

    #[tokio::test]
    async fn test_create_pr() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!({
            "id": 2,
            "node_id": "MDExOlB1bGxSZXF1ZXN0Mg==",
            "number": 2,
            "title": "New PR",
            "state": "open",
            "html_url": "http://github.com/owner/repo/pulls/2",
            "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
            "body": "PR Body",
            "head": { "ref": "feature", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
            "base": { "ref": "main", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
            "created_at": "2023-01-01T00:00:00Z",
            "updated_at": "2023-01-01T00:00:00Z",
            "url": "http://api.github.com/repos/owner/repo/pulls/2",
            "diff_url": "http://github.com/owner/repo/pulls/2.diff",
            "patch_url": "http://github.com/owner/repo/pulls/2.patch",
            "issue_url": "http://api.github.com/repos/owner/repo/issues/2",
            "commits_url": "http://api.github.com/repos/owner/repo/pulls/2/commits",
            "review_comments_url": "http://api.github.com/repos/owner/repo/pulls/2/comments",
            "review_comment_url": "http://api.github.com/repos/owner/repo/pulls/comments{/number}",
            "comments_url": "http://api.github.com/repos/owner/repo/issues/2/comments",
            "statuses_url": "http://api.github.com/repos/owner/repo/statuses/sha",
            "author_association": "NONE"
        });

        Mock::given(method("POST"))
            .and(path("/repos/owner/repo/pulls"))
            .respond_with(ResponseTemplate::new(201).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".to_string(),
            name: "repo".to_string(),
        };

        let spec = CreatePRSpec {
            title: "New PR".to_string(),
            body: "PR Body".to_string(),
            head: "feature".to_string(),
            base: "main".to_string(),
        };

        let pr = service.create_pr(&repo, spec).await.unwrap();
        assert_eq!(pr.number, 2);
        assert_eq!(pr.title, "New PR");
    }

    #[tokio::test]
    async fn test_list_prs() {
        let (service, mock_server) = create_mock_service().await;

        let mock_response = serde_json::json!([
            {
                "id": 2,
                "node_id": "MDExOlB1bGxSZXF1ZXN0Mg==",
                "number": 2,
                "title": "New PR",
                "state": "open",
                "html_url": "http://github.com/owner/repo/pulls/2",
                "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false },
                "body": "PR Body",
                "head": { "ref": "feature", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
                "base": { "ref": "main", "sha": "sha", "repo": { "id": 1, "node_id": "MDEwOlJlcG9zaXRvcnkx", "url": "http://example.com", "name": "repo", "full_name": "owner/repo", "owner": { "login": "owner", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false } }, "user": { "login": "testuser", "id": 1, "node_id": "MDQ6VXNlcjE=", "gravatar_id": "", "url": "http://example.com", "avatar_url": "http://example.com", "html_url": "http://example.com", "followers_url": "http://example.com", "following_url": "http://example.com", "gists_url": "http://example.com", "starred_url": "http://example.com", "subscriptions_url": "http://example.com", "organizations_url": "http://example.com", "repos_url": "http://example.com", "events_url": "http://example.com", "received_events_url": "http://example.com", "type": "User", "site_admin": false }, "label": "label" },
                "created_at": "2023-01-01T00:00:00Z",
                "updated_at": "2023-01-01T00:00:00Z",
                "url": "http://api.github.com/repos/owner/repo/pulls/2",
                "diff_url": "http://github.com/owner/repo/pulls/2.diff",
                "patch_url": "http://github.com/owner/repo/pulls/2.patch",
                "issue_url": "http://api.github.com/repos/owner/repo/issues/2",
                "commits_url": "http://api.github.com/repos/owner/repo/pulls/2/commits",
                "review_comments_url": "http://api.github.com/repos/owner/repo/pulls/2/comments",
                "review_comment_url": "http://api.github.com/repos/owner/repo/pulls/comments{/number}",
                "comments_url": "http://api.github.com/repos/owner/repo/issues/2/comments",
                "statuses_url": "http://api.github.com/repos/owner/repo/statuses/sha",
                "author_association": "NONE"
            }
        ]);

        Mock::given(method("GET"))
            .and(path("/repos/owner/repo/pulls"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let repo = Repo {
            owner: "owner".to_string(),
            name: "repo".to_string(),
        };

        let prs = service.list_prs(&repo, None).await.unwrap();
        assert_eq!(prs.len(), 1);
        assert_eq!(prs[0].title, "New PR");
    }
}
