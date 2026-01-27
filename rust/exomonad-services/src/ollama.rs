use crate::{ExternalService, ServiceError};
use async_trait::async_trait;
use exomonad_shared::{ServiceRequest, ServiceResponse};
use ollama_rs::generation::completion::request::GenerationRequest;
use ollama_rs::Ollama;
use reqwest::Url;

/// Service client for the Ollama API.
///
/// Handles local LLM generation and chat using the `ollama-rs` crate.
/// Defaults to `http://localhost:11434` if no endpoint is specified.
#[derive(Default)]
pub struct OllamaService {
    client: Ollama,
}

impl OllamaService {
    /// Create a new Ollama service with an optional custom endpoint.
    ///
    /// If `endpoint` is `None`, defaults to `http://localhost:11434`.
    pub fn new(endpoint: Option<Url>) -> Self {
        match endpoint {
            Some(url) => {
                let host = url.scheme().to_string() + "://" + url.host_str().unwrap();
                let port = url.port().unwrap_or(11434);
                Self {
                    client: Ollama::new(host, port),
                }
            }
            None => Self::default(),
        }
    }

    /// Create a new Ollama service from environment variables.
    ///
    /// Optional: `OLLAMA_HOST` (e.g., `http://localhost:11434`).
    pub fn from_env() -> Result<Self, anyhow::Error> {
        let endpoint = std::env::var("OLLAMA_HOST")
            .ok()
            .and_then(|s| Url::parse(&s).ok());
        Ok(Self::new(endpoint))
    }
}

#[async_trait]
impl ExternalService for OllamaService {
    type Request = ServiceRequest;
    type Response = ServiceResponse;

    async fn call(&self, req: Self::Request) -> Result<Self::Response, ServiceError> {
        match req {
            ServiceRequest::OllamaGenerate {
                model,
                prompt,
                system,
            } => {
                let mut request = GenerationRequest::new(model, prompt);
                if let Some(s) = system {
                    request = request.system(s);
                }

                let response = self.client.generate(request).await.map_err(|e| ServiceError::Api {
                    code: 500,
                    message: e.to_string(),
                })?;

                Ok(ServiceResponse::OllamaGenerate {
                    response: response.response,
                    done: response.done,
                })
            }
            _ => panic!("Invalid request type for OllamaService"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wiremock::matchers::{method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    #[tokio::test]
    async fn test_ollama_generate() {
        let mock_server = MockServer::start().await;

        let mock_response = serde_json::json!({
            "response": "Hello from Ollama",
            "done": true,
            "model": "llama2",
            "created_at": "2023-01-01T00:00:00Z",
            "context": [],
            "total_duration": 100,
            "load_duration": 10,
            "prompt_eval_count": 10,
            "prompt_eval_duration": 10,
            "eval_count": 10,
            "eval_duration": 10
        });

        Mock::given(method("POST"))
            .and(path("/api/generate"))
            .respond_with(ResponseTemplate::new(200).set_body_json(mock_response))
            .mount(&mock_server)
            .await;

        let service = OllamaService::new(Some(mock_server.uri().parse().unwrap()));
        
        let req = ServiceRequest::OllamaGenerate {
            model: "llama2".into(),
            prompt: "Hi".into(),
            system: None,
        };

        match service.call(req).await.unwrap() {
            ServiceResponse::OllamaGenerate { response, done } => {
                assert_eq!(response, "Hello from Ollama");
                assert!(done);
            }
            _ => panic!("Wrong response type"),
        }
    }
}
