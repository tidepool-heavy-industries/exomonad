use crate::{
    AnthropicService, ExternalService, GitHubService, OllamaService, OtelService, ServiceError,
};
use exomonad_shared::protocol::{ServiceRequest, ServiceResponse};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{UnixListener, UnixStream};
use tracing::{error, info};

/// A Unix socket server that routes service requests to external providers.
#[derive(Clone)]
pub struct ServiceServer {
    anthropic: Arc<AnthropicService>,
    github: Arc<GitHubService>,
    ollama: Arc<OllamaService>,
    otel: Arc<OtelService>,
}

impl ServiceServer {
    pub fn new(
        anthropic: AnthropicService,
        github: GitHubService,
        ollama: OllamaService,
        otel: OtelService,
    ) -> Self {
        Self {
            anthropic: Arc::new(anthropic),
            github: Arc::new(github),
            ollama: Arc::new(ollama),
            otel: Arc::new(otel),
        }
    }

    /// Run the server, listening on the specified Unix socket path.
    pub async fn run(self, socket_path: &str) -> Result<(), std::io::Error> {
        // Ensure the parent directory for the socket exists, if any.
        if let Some(parent) = std::path::Path::new(socket_path).parent() {
            if !parent.as_os_str().is_empty() {
                tokio::fs::create_dir_all(parent).await?;
            }
        }

        // Remove existing socket file if it exists
        let _ = tokio::fs::remove_file(socket_path).await;

        let listener = UnixListener::bind(socket_path)?;
        info!("Service server listening on {}", socket_path);

        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    let server = self.clone();
                    tokio::spawn(async move {
                        if let Err(e) = server.handle_connection(stream).await {
                            if e.downcast_ref::<serde_json::Error>().is_some() {
                                error!(
                                    "Critical JSON (de)serialization error in connection: {:#}",
                                    e
                                );
                            } else {
                                error!("Connection error: {:#}", e);
                            }
                        }
                    });
                }
                Err(e) => {
                    error!("Accept error: {}", e);
                }
            }
        }
    }

    async fn handle_connection(&self, stream: UnixStream) -> Result<(), anyhow::Error> {
        let (reader, mut writer) = stream.into_split();
        let mut lines = BufReader::new(reader).lines();

        while let Some(line) = lines.next_line().await? {
            let request: ServiceRequest = serde_json::from_str(&line)?;
            let response = self.route(request).await;
            let mut json = serde_json::to_string(&response)?;
            json.push('\n');
            writer.write_all(json.as_bytes()).await?;
        }
        Ok(())
    }

    async fn route(&self, req: ServiceRequest) -> ServiceResponse {
        let result = match &req {
            ServiceRequest::AnthropicChat { .. } => self.anthropic.call(req).await,
            ServiceRequest::GitHubGetIssue { .. }
            | ServiceRequest::GitHubCreateIssue { .. }
            | ServiceRequest::GitHubUpdateIssue { .. }
            | ServiceRequest::GitHubListIssues { .. }
            | ServiceRequest::GitHubAddIssueLabel { .. }
            | ServiceRequest::GitHubRemoveIssueLabel { .. }
            | ServiceRequest::GitHubAddIssueAssignee { .. }
            | ServiceRequest::GitHubRemoveIssueAssignee { .. }
            | ServiceRequest::GitHubCreatePR { .. }
            | ServiceRequest::GitHubGetPR { .. }
            | ServiceRequest::GitHubListPullRequests { .. }
            | ServiceRequest::GitHubGetPullRequestReviews { .. }
            | ServiceRequest::GitHubGetDiscussion { .. }
            | ServiceRequest::GitHubCheckAuth => self.github.call(req).await,
            ServiceRequest::OllamaGenerate { .. } => self.ollama.call(req).await,
            ServiceRequest::OtelSpan { .. } | ServiceRequest::OtelMetric { .. } => {
                self.otel.call(req).await
            }
            ServiceRequest::UserInteraction { .. } => Err(ServiceError::Api {
                code: 400,
                message: "UserInteraction not supported by ServiceServer".to_string(),
            }),
        };

        match result {
            Ok(res) => res,
            Err(e) => match e {
                ServiceError::Api { code, message } => ServiceResponse::Error { code, message },
                _ => ServiceResponse::Error {
                    code: 500,
                    message: e.to_string(),
                },
            },
        }
    }
}
