//! Service request/response types.
//!
//! Types for external service integrations (LLM, GitHub, Observability, User Interaction).

use crate::domain::{GithubOwner, GithubRepo, ItemState, ReviewState};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

// ============================================================================
// Service Protocol Types
// ============================================================================

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum ServiceRequest {
    // LLM
    AnthropicChat {
        model: String,
        messages: Vec<ChatMessage>,
        max_tokens: u32,
        #[serde(skip_serializing_if = "Option::is_none")]
        tools: Option<Vec<Tool>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        system: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        thinking: Option<Value>,
    },
    OllamaGenerate {
        model: String,
        prompt: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        system: Option<String>,
    },

    // GitHub
    GitHubGetIssue {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
        #[serde(default)]
        include_comments: bool,
    },
    GitHubCreateIssue {
        owner: GithubOwner,
        repo: GithubRepo,
        title: String,
        body: String,
        labels: Vec<String>,
    },
    GitHubUpdateIssue {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
        #[serde(skip_serializing_if = "Option::is_none")]
        title: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        body: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<ItemState>,
        #[serde(skip_serializing_if = "Option::is_none")]
        labels: Option<Vec<String>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        assignees: Option<Vec<String>>,
    },
    GitHubAddIssueLabel {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
        label: String,
    },
    GitHubRemoveIssueLabel {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
        label: String,
    },
    GitHubAddIssueAssignee {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
        assignee: String,
    },
    GitHubListIssues {
        owner: GithubOwner,
        repo: GithubRepo,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<IssueState>,
        labels: Vec<String>,
    },
    GitHubGetPR {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
        #[serde(default)]
        include_details: bool,
    },
    GitHubListPullRequests {
        owner: GithubOwner,
        repo: GithubRepo,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<String>, // open, closed, merged, all
        #[serde(skip_serializing_if = "Option::is_none")]
        limit: Option<u32>,
        /// Filter by head branch (e.g., "feature/my-branch" or "owner:feature/my-branch")
        #[serde(skip_serializing_if = "Option::is_none")]
        head: Option<String>,
    },
    GitHubGetPullRequestReviews {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
    },
    GitHubGetDiscussion {
        owner: GithubOwner,
        repo: GithubRepo,
        number: u32,
    },
    GitHubCheckAuth,

    // Observability
    OtelSpan {
        trace_id: String,
        span_id: String,
        name: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        start_ns: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        end_ns: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        attributes: Option<HashMap<String, String>>,
    },
    OtelMetric {
        name: String,
        value: f64,
        labels: HashMap<String, String>,
    },

    // User Interaction (from Zellij plugin)
    UserInteraction {
        request_id: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        payload: Option<Value>,
        #[serde(default)]
        cancel: bool,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum ServiceResponse {
    // LLM
    #[serde(rename = "AnthropicChatResponse")]
    AnthropicChat {
        content: Vec<ContentBlock>,
        #[serde(rename = "stop_reason")]
        stop_reason: StopReason,
        usage: Usage,
    },
    #[serde(rename = "OllamaGenerateResponse")]
    OllamaGenerate { response: String, done: bool },

    // GitHub
    #[serde(rename = "GitHubIssueResponse")]
    GitHubIssue {
        number: u32,
        title: String,
        body: String,
        state: ItemState,
        labels: Vec<String>,
        url: String,
        author: String,
        #[serde(default)]
        comments: Vec<GitHubDiscussionComment>,
    },
    #[serde(rename = "GitHubIssuesResponse")]
    GitHubIssues { issues: Vec<GitHubIssueRef> },
    #[serde(rename = "GitHubPRResponse")]
    GitHubPR {
        number: u32,
        title: String,
        body: String,
        author: String,
        url: String,
        state: ItemState,
        head_ref_name: String,
        base_ref_name: String,
        created_at: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        merged_at: Option<String>,
        labels: Vec<String>,
        #[serde(default)]
        comments: Vec<GitHubDiscussionComment>,
        #[serde(default)]
        reviews: Vec<GitHubReviewComment>,
    },
    #[serde(rename = "GitHubPullRequestsResponse")]
    GitHubPullRequests { pull_requests: Vec<GitHubPRRef> },
    #[serde(rename = "GitHubReviewsResponse")]
    GitHubReviews { reviews: Vec<GitHubReviewComment> },
    #[serde(rename = "GitHubDiscussionResponse")]
    GitHubDiscussion {
        number: u32,
        title: String,
        body: String,
        author: String,
        url: String,
        comments: Vec<GitHubDiscussionComment>,
    },
    #[serde(rename = "GitHubAuthResponse")]
    GitHubAuth {
        authenticated: bool,
        user: Option<String>,
    },

    // Generic acknowledgment (label/assignee operations, OTLP export, etc.)
    #[serde(rename = "AckResponse", alias = "OtelAckResponse")]
    Ack,

    // Error
    #[serde(rename = "ErrorResponse")]
    Error { code: i32, message: String },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ChatMessage {
    pub role: String,
    pub content: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Tool {
    pub name: String,
    pub description: String,
    pub input_schema: Value,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum StopReason {
    EndTurn,
    MaxTokens,
    StopSequence,
    ToolUse,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ContentBlock {
    #[serde(rename = "type")]
    pub block_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input: Option<Value>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Usage {
    pub input_tokens: u32,
    pub output_tokens: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "lowercase")]
pub enum IssueState {
    Open,
    Closed,
    All,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubIssueRef {
    pub number: u32,
    pub title: String,
    pub body: String,
    pub state: ItemState,
    pub url: String,
    pub author: GitHubAuthorRef,
    pub labels: Vec<GitHubLabelRef>,
    #[serde(default)]
    pub comments: Vec<GitHubDiscussionComment>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubLabelRef {
    pub name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubAuthorRef {
    pub login: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubPRRef {
    pub number: u32,
    pub title: String,
    pub state: ItemState,
    pub url: String,
    #[serde(default)]
    pub head_ref_name: String,
    #[serde(default)]
    pub base_ref_name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubReviewComment {
    pub author: String,
    pub body: String,
    pub path: String,
    pub line: Option<u32>,
    pub state: ReviewState,
    pub created_at: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GitHubDiscussionComment {
    pub author: String,
    pub body: String,
    pub created_at: String,
    pub replies: Vec<GitHubDiscussionComment>,
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // ServiceRequest/ServiceResponse GitHub roundtrip tests
    // =========================================================================

    #[test]
    fn test_github_get_issue_request_roundtrip() {
        let req = ServiceRequest::GitHubGetIssue {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            number: 42,
            include_comments: true,
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetIssue {
                owner,
                repo,
                number,
                include_comments,
            } => {
                assert_eq!(owner.as_str(), "octocat");
                assert_eq!(repo.as_str(), "hello-world");
                assert_eq!(number, 42);
                assert!(include_comments);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_get_issue_request_defaults() {
        // include_comments should default to false when absent
        let json = r#"{"type":"GitHubGetIssue","owner":"o","repo":"r","number":1}"#;
        let parsed: ServiceRequest = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetIssue {
                include_comments, ..
            } => {
                assert!(!include_comments);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_issue_response_roundtrip() {
        let resp = ServiceResponse::GitHubIssue {
            number: 42,
            title: "Fix the bug".into(),
            body: "It's broken".into(),
            state: ItemState::Open,
            labels: vec!["bug".into(), "critical".into()],
            url: "https://github.com/octocat/hello-world/issues/42".into(),
            author: "octocat".into(),
            comments: vec![GitHubDiscussionComment {
                author: "reviewer".into(),
                body: "Looks good".into(),
                created_at: "2024-01-15T10:00:00Z".into(),
                replies: vec![],
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubIssue {
                number,
                title,
                author,
                comments,
                labels,
                ..
            } => {
                assert_eq!(number, 42);
                assert_eq!(title, "Fix the bug");
                assert_eq!(author, "octocat");
                assert_eq!(comments.len(), 1);
                assert_eq!(comments[0].author, "reviewer");
                assert_eq!(labels, vec!["bug", "critical"]);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_issue_response_empty_comments_default() {
        // comments should default to empty vec when absent
        let json = r#"{"type":"GitHubIssueResponse","number":1,"title":"t","body":"b","state":"open","labels":[],"url":"u","author":"a"}"#;
        let parsed: ServiceResponse = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceResponse::GitHubIssue { comments, .. } => {
                assert!(comments.is_empty());
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_get_pr_request_roundtrip() {
        let req = ServiceRequest::GitHubGetPR {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            number: 99,
            include_details: true,
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetPR {
                owner,
                repo,
                number,
                include_details,
            } => {
                assert_eq!(owner.as_str(), "octocat");
                assert_eq!(repo.as_str(), "hello-world");
                assert_eq!(number, 99);
                assert!(include_details);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_get_pr_request_defaults() {
        let json = r#"{"type":"GitHubGetPR","owner":"o","repo":"r","number":1}"#;
        let parsed: ServiceRequest = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceRequest::GitHubGetPR {
                include_details, ..
            } => {
                assert!(!include_details);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_pr_response_roundtrip() {
        let resp = ServiceResponse::GitHubPR {
            number: 99,
            title: "Add feature".into(),
            body: "This adds X".into(),
            author: "octocat".into(),
            url: "https://github.com/octocat/hello-world/pull/99".into(),
            state: ItemState::Open,
            head_ref_name: "feature-branch".into(),
            base_ref_name: "main".into(),
            created_at: "2024-01-15T10:00:00Z".into(),
            merged_at: Some("2024-01-16T12:00:00Z".into()),
            labels: vec!["enhancement".into()],
            comments: vec![GitHubDiscussionComment {
                author: "reviewer".into(),
                body: "LGTM".into(),
                created_at: "2024-01-15T11:00:00Z".into(),
                replies: vec![],
            }],
            reviews: vec![GitHubReviewComment {
                author: "reviewer".into(),
                body: "Approved".into(),
                path: "src/main.rs".into(),
                line: Some(42),
                state: ReviewState::Approved,
                created_at: "2024-01-15T12:00:00Z".into(),
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubPR {
                number,
                title,
                author,
                merged_at,
                labels,
                comments,
                reviews,
                ..
            } => {
                assert_eq!(number, 99);
                assert_eq!(title, "Add feature");
                assert_eq!(author, "octocat");
                assert_eq!(merged_at, Some("2024-01-16T12:00:00Z".into()));
                assert_eq!(labels, vec!["enhancement"]);
                assert_eq!(comments.len(), 1);
                assert_eq!(reviews.len(), 1);
                assert_eq!(reviews[0].state, ReviewState::Approved);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_pr_response_optional_defaults() {
        // merged_at absent, comments/reviews absent should all default
        let json = r#"{
            "type": "GitHubPRResponse",
            "number": 1, "title": "t", "body": "b", "author": "a",
            "url": "u", "state": "open", "head_ref_name": "h",
            "base_ref_name": "main", "created_at": "2024-01-01T00:00:00Z",
            "labels": []
        }"#;
        let parsed: ServiceResponse = serde_json::from_str(json).unwrap();
        match parsed {
            ServiceResponse::GitHubPR {
                merged_at,
                comments,
                reviews,
                ..
            } => {
                assert_eq!(merged_at, None);
                assert!(comments.is_empty());
                assert!(reviews.is_empty());
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_create_issue_request_roundtrip() {
        let req = ServiceRequest::GitHubCreateIssue {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            title: "New bug".into(),
            body: "Details here".into(),
            labels: vec!["bug".into()],
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubCreateIssue {
                owner,
                title,
                labels,
                ..
            } => {
                assert_eq!(owner.as_str(), "octocat");
                assert_eq!(title, "New bug");
                assert_eq!(labels, vec!["bug"]);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_list_issues_request_roundtrip() {
        let req = ServiceRequest::GitHubListIssues {
            owner: "octocat".into(),
            repo: "hello-world".into(),
            state: Some(IssueState::Open),
            labels: vec!["bug".into()],
        };
        let json = serde_json::to_string(&req).unwrap();
        let parsed: ServiceRequest = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceRequest::GitHubListIssues { labels, .. } => {
                assert_eq!(labels, vec!["bug"]);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_issues_response_roundtrip() {
        let resp = ServiceResponse::GitHubIssues {
            issues: vec![
                GitHubIssueRef {
                    number: 1,
                    title: "Bug".into(),
                    body: "b".into(),
                    state: ItemState::Open,
                    url: "u".into(),
                    author: GitHubAuthorRef {
                        login: "a".into(),
                        name: None,
                    },
                    labels: vec![],
                    comments: vec![],
                },
                GitHubIssueRef {
                    number: 2,
                    title: "Feature".into(),
                    body: "b".into(),
                    state: ItemState::Closed,
                    url: "u".into(),
                    author: GitHubAuthorRef {
                        login: "a".into(),
                        name: None,
                    },
                    labels: vec![],
                    comments: vec![],
                },
            ],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubIssues { issues } => {
                assert_eq!(issues.len(), 2);
                assert_eq!(issues[0].number, 1);
                assert_eq!(issues[1].state, ItemState::Closed);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_reviews_response_roundtrip() {
        let resp = ServiceResponse::GitHubReviews {
            reviews: vec![GitHubReviewComment {
                author: "reviewer".into(),
                body: "Changes requested".into(),
                path: "lib.rs".into(),
                line: None,
                state: ReviewState::ChangesRequested,
                created_at: "2024-01-15T10:00:00Z".into(),
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubReviews { reviews } => {
                assert_eq!(reviews.len(), 1);
                assert_eq!(reviews[0].state, ReviewState::ChangesRequested);
                assert_eq!(reviews[0].line, None);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_discussion_response_roundtrip() {
        let resp = ServiceResponse::GitHubDiscussion {
            number: 10,
            title: "RFC: New API".into(),
            body: "Proposal details".into(),
            author: "octocat".into(),
            url: "https://github.com/octocat/hello-world/discussions/10".into(),
            comments: vec![GitHubDiscussionComment {
                author: "commenter".into(),
                body: "Great idea".into(),
                created_at: "2024-01-15T10:00:00Z".into(),
                replies: vec![GitHubDiscussionComment {
                    author: "octocat".into(),
                    body: "Thanks!".into(),
                    created_at: "2024-01-15T11:00:00Z".into(),
                    replies: vec![],
                }],
            }],
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubDiscussion {
                number, comments, ..
            } => {
                assert_eq!(number, 10);
                assert_eq!(comments.len(), 1);
                assert_eq!(comments[0].replies.len(), 1);
                assert_eq!(comments[0].replies[0].author, "octocat");
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_github_auth_response_roundtrip() {
        let resp = ServiceResponse::GitHubAuth {
            authenticated: true,
            user: Some("octocat".into()),
        };
        let json = serde_json::to_string(&resp).unwrap();
        let parsed: ServiceResponse = serde_json::from_str(&json).unwrap();
        match parsed {
            ServiceResponse::GitHubAuth {
                authenticated,
                user,
            } => {
                assert!(authenticated);
                assert_eq!(user, Some("octocat".into()));
            }
            _ => panic!("Wrong variant"),
        }
    }

    /// Verify the JSON wire format uses the exact field names
    /// the Haskell SocketClient.hs FromJSON instance expects.
    #[test]
    fn test_github_issue_response_wire_format() {
        let resp = ServiceResponse::GitHubIssue {
            number: 1,
            title: "t".into(),
            body: "b".into(),
            state: ItemState::Open,
            labels: vec![],
            url: "u".into(),
            author: "a".into(),
            comments: vec![],
        };
        let val: Value = serde_json::to_value(&resp).unwrap();
        let obj = val.as_object().unwrap();
        // Verify the type tag matches Haskell's FromJSON dispatch
        assert_eq!(obj["type"], "GitHubIssueResponse");
        // Verify all expected field names are present
        assert!(obj.contains_key("number"));
        assert!(obj.contains_key("title"));
        assert!(obj.contains_key("body"));
        assert!(obj.contains_key("state"));
        assert!(obj.contains_key("labels"));
        assert!(obj.contains_key("url"));
        assert!(obj.contains_key("author"));
        assert!(obj.contains_key("comments"));
    }

    /// Verify the PR JSON wire format uses the exact field names
    /// the Haskell SocketClient.hs FromJSON instance expects.
    #[test]
    fn test_github_pr_response_wire_format() {
        let resp = ServiceResponse::GitHubPR {
            number: 1,
            title: "t".into(),
            body: "b".into(),
            author: "a".into(),
            url: "u".into(),
            state: ItemState::Open,
            head_ref_name: "h".into(),
            base_ref_name: "main".into(),
            created_at: "2024-01-01T00:00:00Z".into(),
            merged_at: None,
            labels: vec![],
            comments: vec![],
            reviews: vec![],
        };
        let val: Value = serde_json::to_value(&resp).unwrap();
        let obj = val.as_object().unwrap();
        assert_eq!(obj["type"], "GitHubPRResponse");
        assert!(obj.contains_key("number"));
        assert!(obj.contains_key("title"));
        assert!(obj.contains_key("body"));
        assert!(obj.contains_key("author"));
        assert!(obj.contains_key("url"));
        assert!(obj.contains_key("state"));
        assert!(obj.contains_key("head_ref_name"));
        assert!(obj.contains_key("base_ref_name"));
        assert!(obj.contains_key("created_at"));
        assert!(obj.contains_key("labels"));
        assert!(obj.contains_key("comments"));
        assert!(obj.contains_key("reviews"));
        // merged_at is None so should be absent (skip_serializing_if)
        assert!(!obj.contains_key("merged_at"));
    }
}

#[cfg(test)]
mod proptest_tests {
    use super::*;
    use proptest::prelude::*;

    fn arb_github_owner() -> impl Strategy<Value = GithubOwner> {
        "[a-zA-Z0-9-]+".prop_map(|s| GithubOwner::try_from(s).unwrap())
    }

    fn arb_github_repo() -> impl Strategy<Value = GithubRepo> {
        "[a-zA-Z0-9-_.]+".prop_map(|s| GithubRepo::try_from(s).unwrap())
    }

    proptest! {
        #[test]
        fn test_github_get_issue_request_roundtrip_proptest(
            owner in arb_github_owner(),
            repo in arb_github_repo(),
            number in any::<u32>(),
            include_comments in any::<bool>()
        ) {
            let req = ServiceRequest::GitHubGetIssue {
                owner,
                repo,
                number,
                include_comments,
            };
            let json = serde_json::to_string(&req).unwrap();
            let back: ServiceRequest = serde_json::from_str(&json).unwrap();
            match (req, back) {
                (ServiceRequest::GitHubGetIssue { owner: o1, repo: r1, number: n1, include_comments: i1 },
                 ServiceRequest::GitHubGetIssue { owner: o2, repo: r2, number: n2, include_comments: i2 }) => {
                    prop_assert_eq!(o1, o2);
                    prop_assert_eq!(r1, r2);
                    prop_assert_eq!(n1, n2);
                    prop_assert_eq!(i1, i2);
                },
                _ => prop_assert!(false, "Wrong variant"),
            }
        }

        #[test]
        fn test_github_create_issue_request_roundtrip_proptest(
            owner in arb_github_owner(),
            repo in arb_github_repo(),
            title in any::<String>(),
            body in any::<String>(),
            labels in prop::collection::vec(any::<String>(), 0..5)
        ) {
            let req = ServiceRequest::GitHubCreateIssue {
                owner,
                repo,
                title,
                body,
                labels,
            };
            let json = serde_json::to_string(&req).unwrap();
            let back: ServiceRequest = serde_json::from_str(&json).unwrap();
            match (req, back) {
                (ServiceRequest::GitHubCreateIssue { owner: o1, repo: r1, title: t1, body: b1, labels: l1 },
                 ServiceRequest::GitHubCreateIssue { owner: o2, repo: r2, title: t2, body: b2, labels: l2 }) => {
                    prop_assert_eq!(o1, o2);
                    prop_assert_eq!(r1, r2);
                    prop_assert_eq!(t1, t2);
                    prop_assert_eq!(b1, b2);
                    prop_assert_eq!(l1, l2);
                },
                _ => prop_assert!(false, "Wrong variant"),
            }
        }

        #[test]
        fn test_error_response_roundtrip_proptest(
            code in any::<i32>(),
            message in any::<String>()
        ) {
            let resp = ServiceResponse::Error { code, message };
            let json = serde_json::to_string(&resp).unwrap();
            let back: ServiceResponse = serde_json::from_str(&json).unwrap();
            match (resp, back) {
                (ServiceResponse::Error { code: c1, message: m1 },
                 ServiceResponse::Error { code: c2, message: m2 }) => {
                    prop_assert_eq!(c1, c2);
                    prop_assert_eq!(m1, m2);
                },
                _ => prop_assert!(false, "Wrong variant"),
            }
        }
    }
}
