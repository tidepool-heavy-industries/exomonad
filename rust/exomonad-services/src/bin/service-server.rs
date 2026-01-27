use anyhow::Result;
use exomonad_services::{
    AnthropicService, GitHubService, OllamaService, OtelService, ServiceServer,
};
use tracing::info;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()))
        .with(tracing_subscriber::fmt::layer())
        .init();

    let socket_path = std::env::var("EXOMONAD_SERVICE_SOCKET")
        .unwrap_or_else(|_| ".exomonad/sockets/services.sock".to_string());

    info!("Initializing services...");

    let anthropic = AnthropicService::from_env().unwrap_or_else(|e| {
        tracing::warn!("Failed to initialize Anthropic service: {}. Falling back to dummy.", e);
        AnthropicService::new("dummy".into())
    });

    let github = GitHubService::from_env().unwrap_or_else(|e| {
        tracing::warn!("Failed to initialize GitHub service: {}. Falling back to dummy.", e);
        GitHubService::new("dummy".into())
    });

    let ollama = OllamaService::from_env().unwrap_or_else(|e| {
        tracing::warn!("Failed to initialize Ollama service: {}. Falling back to default.", e);
        OllamaService::default()
    });

    let otel = OtelService::from_env().unwrap_or_else(|e| {
        tracing::warn!("Failed to initialize Otel service: {}. Falling back to dummy.", e);
        OtelService::new("http://localhost:4318".parse().unwrap(), std::collections::HashMap::new())
    });

    let server = ServiceServer::new(anthropic, github, ollama, otel);

    info!("Starting service server on {}...", socket_path);
    server.run(&socket_path).await?;

    Ok(())
}
