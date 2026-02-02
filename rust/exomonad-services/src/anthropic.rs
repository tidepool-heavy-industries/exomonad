use crate::{ExternalService, ServiceError};
use async_trait::async_trait;
use exomonad_shared::protocol::{
    ChatMessage, ContentBlock, ServiceRequest, ServiceResponse, StopReason, Tool, Usage,
};
use reqwest::{Client, Url};
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tracing::warn;

/// Service client for the Anthropic Messages API.
///
/// Handles chat completions with support for tools, system prompts, and
/// automatic retry (exponential backoff) for 529 Overloaded errors.
pub struct AnthropicService {
    client: Client,
    api_key: String,
    base_url: Url,
}

impl AnthropicService {
    /// Create a new Anthropic service with the given API key.
    ///
    /// Uses the default endpoint: `https://api.anthropic.com`.
    pub fn new(api_key: String) -> Self {
        Self {
            client: Client::new(),
            api_key,
            base_url: Url::parse("https://api.anthropic.com")
                .expect("hardcoded Anthropic API URL should be valid"),
        }
    }

    /// Create a new Anthropic service with a custom base URL.
    ///
    /// Useful for testing (mock servers) or proxies.
    pub fn with_base_url(api_key: String, base_url: Url) -> Self {
        Self {
            client: Client::new(),
            api_key,
            base_url,
        }
    }

    /// Create a new Anthropic service from environment variables.
    ///
    /// Required: `ANTHROPIC_API_KEY`.
    /// Optional: `ANTHROPIC_BASE_URL`.
    pub fn from_env() -> Result<Self, anyhow::Error> {
        let api_key = std::env::var("ANTHROPIC_API_KEY")?;
        let base_url = std::env::var("ANTHROPIC_BASE_URL")
            .ok()
            .and_then(|s| Url::parse(&s).ok())
            .unwrap_or_else(|| {
                Url::parse("https://api.anthropic.com")
                    .expect("hardcoded Anthropic API URL should be valid")
            });

        Ok(Self::with_base_url(api_key, base_url))
    }
}

#[derive(Serialize)]
struct AnthropicRequestPayload {
    model: String,
    messages: Vec<ChatMessage>,
    max_tokens: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    tools: Option<Vec<Tool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    system: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    thinking: Option<serde_json::Value>,
}

#[derive(Deserialize)]
struct AnthropicResponsePayload {
    content: Vec<ContentBlock>,
    stop_reason: Option<String>,
    usage: Usage,
}

#[async_trait]
impl ExternalService for AnthropicService {
    type Request = ServiceRequest;
    type Response = ServiceResponse;

    async fn call(&self, req: Self::Request) -> Result<Self::Response, ServiceError> {
        let (model, messages, max_tokens, tools, system, thinking) = match req {
            ServiceRequest::AnthropicChat {
                model,
                messages,
                max_tokens,
                tools,
                system,
                thinking,
            } => (model, messages, max_tokens, tools, system, thinking),
            _ => panic!("Invalid request type for AnthropicService"),
        };

        let payload = AnthropicRequestPayload {
            model,
            messages,
            max_tokens,
            tools,
            system,
            thinking,
        };
        let url = self.base_url.join("/v1/messages").unwrap();
        let mut attempts = 0;
        let max_attempts = 3;
        let mut backoff = Duration::from_millis(500);

        loop {
            attempts += 1;
            let response = self
                .client
                .post(url.clone())
                .header("x-api-key", &self.api_key)
                .header("anthropic-version", "2023-06-01")
                .header("content-type", "application/json")
                .json(&payload)
                .send()
                .await?;

            if response.status().as_u16() == 529 {
                if attempts >= max_attempts {
                    return Err(ServiceError::RateLimited {
                        retry_after_ms: backoff.as_millis() as u64,
                    });
                }
                warn!("Anthropic overloaded (529), retrying in {:?}...", backoff);
                tokio::time::sleep(backoff).await;
                backoff *= 2;
                continue;
            }

            if !response.status().is_success() {
                return Err(ServiceError::Api {
                    code: response.status().as_u16() as i32,
                    message: response.text().await.unwrap_or_default(),
                });
            }

            let body: AnthropicResponsePayload = response.json().await?;

            let stop_reason = match body.stop_reason.as_deref() {
                Some("end_turn") => StopReason::EndTurn,
                Some("max_tokens") => StopReason::MaxTokens,
                Some("stop_sequence") => StopReason::StopSequence,
                Some("tool_use") => StopReason::ToolUse,
                _ => StopReason::EndTurn, // Default or unknown
            };

            return Ok(ServiceResponse::AnthropicChat {
                content: body.content,
                stop_reason,
                usage: body.usage,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    fn get_fixture_path(subpath: &str) -> PathBuf {
        let root = std::env::current_dir().unwrap();
        // Handle running from workspace root or crate root
        if root.ends_with("exomonad-services") {
            root.join("../../test/fixtures/claude-api").join(subpath)
        } else {
            root.join("test/fixtures/claude-api").join(subpath)
        }
    }

    fn load_fixture(subpath: &str) -> serde_json::Value {
        let path = get_fixture_path(subpath);
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("Failed to read fixture {:?}: {}", path, err));
        serde_json::from_str(&content).expect("Invalid JSON in fixture")
    }

    #[tokio::test]
    async fn test_anthropic_chat() {
        let mock_server = MockServer::start().await;

        let mock_response = serde_json::json!({
            "content": [{"type": "text", "text": "Hello"}],
            "stop_reason": "end_turn",
            "usage": {"input_tokens": 10, "output_tokens": 5}
        });

        Mock::given(method("POST"))
            .and(path("/v1/messages"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let service =
            AnthropicService::with_base_url("test-key".into(), mock_server.uri().parse().unwrap());

        let req = ServiceRequest::AnthropicChat {
            model: "claude-3-opus".into(),
            messages: vec![ChatMessage {
                role: "user".into(),
                content: "Hi".into(),
            }],
            max_tokens: 100,
            tools: None,
            system: None,
            thinking: None,
        };

        match service.call(req).await.unwrap() {
            ServiceResponse::AnthropicChat {
                content,
                stop_reason,
                ..
            } => {
                assert_eq!(content[0].text.as_deref(), Some("Hello"));
                assert_eq!(stop_reason, StopReason::EndTurn);
            }
            _ => panic!("Wrong response type"),
        }
    }

    #[tokio::test]
    async fn test_request_golden_simple_message() {
        let mock_server = MockServer::start().await;
        let expected_json = load_fixture("request/simple_message.json");

        // Verify the request body matches the golden fixture
        Mock::given(method("POST"))
            .and(path("/v1/messages"))
            .and(move |req: &wiremock::Request| {
                let body: serde_json::Value = req.body_json().unwrap();
                // Compare body with expected_json
                // Note: expected_json has "system" but our simple request might not if None.
                // The fixture has "system": "You are a helpful assistant."

                // We need to construct the ServiceRequest to match the fixture exactly.
                body == expected_json
            })
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "content": [], "stop_reason": "end_turn", "usage": {"input_tokens": 0, "output_tokens": 0}
            })))
            .mount(&mock_server)
            .await;

        let service =
            AnthropicService::with_base_url("test-key".into(), mock_server.uri().parse().unwrap());

        // Construct request matching request/simple_message.json
        let req = ServiceRequest::AnthropicChat {
            model: "claude-3-opus-20240229".into(),
            max_tokens: 1024,
            system: Some("You are a helpful assistant.".into()),
            messages: vec![ChatMessage {
                role: "user".into(),
                content: "Hello".into(),
            }],
            tools: None,
            thinking: None,
        };

        service.call(req).await.unwrap();
    }

    #[tokio::test]
    async fn test_request_golden_with_tools() {
        let mock_server = MockServer::start().await;
        let expected_json = load_fixture("request/with_tools.json");

        Mock::given(method("POST"))
            .and(path("/v1/messages"))
            .and(move |req: &wiremock::Request| {
                let body: serde_json::Value = req.body_json().unwrap();
                body == expected_json
            })
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "content": [], "stop_reason": "end_turn", "usage": {"input_tokens": 0, "output_tokens": 0}
            })))
            .mount(&mock_server)
            .await;

        let service =
            AnthropicService::with_base_url("test-key".into(), mock_server.uri().parse().unwrap());

        // Construct request matching request/with_tools.json
        let req = ServiceRequest::AnthropicChat {
            model: "claude-3-opus-20240229".into(),
            max_tokens: 1024,
            messages: vec![ChatMessage {
                role: "user".into(),
                content: "What is the weather?".into(),
            }],
            tools: Some(vec![Tool {
                name: "get_weather".into(),
                description: "Get weather for a location".into(),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "location": {"type": "string"}
                    },
                    "required": ["location"]
                }),
            }]),
            system: None, // Fixture doesn't have system
            thinking: None,
        };

        service.call(req).await.unwrap();
    }

    #[tokio::test]
    async fn test_response_golden_text() {
        let mock_server = MockServer::start().await;
        let response_json = load_fixture("response/text_response.json");

        Mock::given(method("POST"))
            .and(path("/v1/messages"))
            .respond_with(ResponseTemplate::new(200).set_body_json(response_json))
            .mount(&mock_server)
            .await;

        let service =
            AnthropicService::with_base_url("test-key".into(), mock_server.uri().parse().unwrap());

        let req = ServiceRequest::AnthropicChat {
            model: "claude-3-opus".into(),
            messages: vec![],
            max_tokens: 10,
            tools: None,
            system: None,
            thinking: None,
        };

        let resp = service.call(req).await.unwrap();

        match resp {
            ServiceResponse::AnthropicChat {
                content,
                stop_reason,
                usage,
            } => {
                assert_eq!(content.len(), 1);
                assert_eq!(content[0].block_type, "text");
                assert_eq!(content[0].text.as_deref(), Some("Hello!"));
                assert_eq!(stop_reason, StopReason::EndTurn);
                assert_eq!(usage.input_tokens, 10);
                assert_eq!(usage.output_tokens, 5);
            }
            _ => panic!("Wrong response type"),
        }
    }

    #[tokio::test]
    async fn test_response_golden_tool_use() {
        let mock_server = MockServer::start().await;
        let response_json = load_fixture("response/tool_use_response.json");

        Mock::given(method("POST"))
            .and(path("/v1/messages"))
            .respond_with(ResponseTemplate::new(200).set_body_json(response_json))
            .mount(&mock_server)
            .await;

        let service =
            AnthropicService::with_base_url("test-key".into(), mock_server.uri().parse().unwrap());

        let req = ServiceRequest::AnthropicChat {
            model: "claude-3-opus".into(),
            messages: vec![],
            max_tokens: 10,
            tools: None,
            system: None,
            thinking: None,
        };

        let resp = service.call(req).await.unwrap();

        match resp {
            ServiceResponse::AnthropicChat {
                content,
                stop_reason,
                usage,
            } => {
                assert_eq!(content.len(), 2);

                // Block 1: Text
                assert_eq!(content[0].block_type, "text");
                assert_eq!(
                    content[0].text.as_deref(),
                    Some("I will check the weather.")
                );

                // Block 2: Tool Use
                assert_eq!(content[1].block_type, "tool_use");
                assert_eq!(content[1].id.as_deref(), Some("toolu_01234"));
                assert_eq!(content[1].name.as_deref(), Some("get_weather"));

                let input = content[1].input.as_ref().unwrap();
                assert_eq!(input["location"], "San Francisco");

                assert_eq!(stop_reason, StopReason::ToolUse);
                assert_eq!(usage.input_tokens, 20);
                assert_eq!(usage.output_tokens, 30);
            }
            _ => panic!("Wrong response type"),
        }
    }
}
