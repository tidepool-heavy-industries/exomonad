use crate::{ExternalService, ServiceError};
use async_trait::async_trait;
use exomonad_shared::{ChatMessage, ContentBlock, ServiceRequest, ServiceResponse, StopReason, Tool, Usage};
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
            base_url: Url::parse("https://api.anthropic.com").unwrap(),
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
        let (model, messages, max_tokens, tools, system) = match req {
            ServiceRequest::AnthropicChat {
                model,
                messages,
                max_tokens,
                tools,
                system,
            } => (model, messages, max_tokens, tools, system),
            _ => panic!("Invalid request type for AnthropicService"),
        };

        let payload = AnthropicRequestPayload {
            model,
            messages,
            max_tokens,
            tools,
            system,
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
                     return Err(ServiceError::RateLimited { retry_after_ms: backoff.as_millis() as u64 });
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
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

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

        let service = AnthropicService::with_base_url("test-key".into(), mock_server.uri().parse().unwrap());
        
        let req = ServiceRequest::AnthropicChat {
            model: "claude-3-opus".into(),
            messages: vec![ChatMessage { role: "user".into(), content: "Hi".into() }],
            max_tokens: 100,
            tools: None,
            system: None,
        };

        match service.call(req).await.unwrap() {
            ServiceResponse::AnthropicChat { content, stop_reason, .. } => {
                assert_eq!(content[0].text.as_deref(), Some("Hello"));
                assert_eq!(stop_reason, StopReason::EndTurn);
            }
            _ => panic!("Wrong response type"),
        }
    }
}