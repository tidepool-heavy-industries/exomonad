use crate::plugin_manager::PluginManager;
use crate::services::Services;
use anyhow::Result;
use axum::{
    extract::{Path, State},
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};
use serde_json::Value;
use std::sync::Arc;
use tracing::info;

#[derive(Clone)]
pub struct AppState {
    pub plugin_manager: PluginManager,
    pub services: Arc<Services>,
}

pub async fn start_server(
    port: u16,
    plugin_manager: PluginManager,
    services: Arc<Services>,
) -> Result<()> {
    let state = AppState {
        plugin_manager,
        services,
    };

    let app = Router::new()
        .route("/health", get(health_check))
        .route("/role/:role/mcp/call", post(mcp_call))
        .route("/hook/:event", post(hook_call))
        .route("/reload", post(reload))
        .with_state(state);

    let listener = tokio::net::TcpListener::bind(format!("127.0.0.1:{}", port)).await?;
    info!("Server listening on {}", listener.local_addr()?);
    axum::serve(listener, app).await?;
    Ok(())
}

async fn health_check() -> &'static str {
    "OK"
}

async fn mcp_call(
    State(state): State<AppState>,
    Path(role): Path<String>,
    Json(payload): Json<Value>,
) -> Result<Json<Value>, (StatusCode, Json<Value>)> {
    info!("MCP call for role: {}", role);

    // Include role in the payload so plugins can dispatch on it.
    let input = serde_json::json!({
        "role": role,
        "payload": payload
    });

    match state.plugin_manager.call("handle_mcp_call", &input).await {
        Ok(result) => Ok(Json(result)),
        Err(e) => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({ "error": e.to_string() })),
        )),
    }
}

async fn hook_call(
    State(state): State<AppState>,
    Path(event): Path<String>,
    Json(payload): Json<Value>,
) -> Result<Json<Value>, (StatusCode, Json<Value>)> {
    info!("Hook call: {}", event);

    // Pass both the event name and the original payload to the plugin.
    let input = serde_json::json!({
        "event": event,
        "payload": payload,
    });

    match state.plugin_manager.call("handle_hook", &input).await {
        Ok(result) => Ok(Json(result)),
        Err(e) => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({ "error": e.to_string() })),
        )),
    }
}

async fn reload(State(state): State<AppState>) -> Result<Json<Value>, (StatusCode, Json<Value>)> {
    info!("Reloading plugin...");
    match state.plugin_manager.reload(state.services.clone()).await {
        Ok(_) => Ok(Json(serde_json::json!({ "status": "reloaded" }))),
        Err(e) => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({ "error": e.to_string() })),
        )),
    }
}
