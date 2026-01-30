use axum::{
    extract::{Path, State},
    routing::{get, post},
    Json, Router,
};
use serde_json::Value;
use std::sync::Arc;
use crate::plugin_manager::PluginManager;
use crate::services::Services;
use tracing::info;

#[derive(Clone)]
pub struct AppState {
    pub plugin_manager: PluginManager,
    pub services: Arc<Services>,
}

pub async fn start_server(port: u16, plugin_manager: PluginManager, services: Arc<Services>) {
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

    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{}", port)).await.unwrap();
    info!("Server listening on {}", listener.local_addr().unwrap());
    axum::serve(listener, app).await.unwrap();
}

async fn health_check() -> &'static str {
    "OK"
}

async fn mcp_call(
    State(state): State<AppState>,
    Path(role): Path<String>,
    Json(payload): Json<Value>,
) -> Json<Value> {
    info!("MCP call for role: {}", role);
    // Assuming the plugin has a function "handle_mcp_call"
    // We might wrap payload in { role: ..., args: ... }
    
    // For now pass payload directly
    let result: Value = state.plugin_manager.call("handle_mcp_call", &payload).await.unwrap_or_else(|e| {
        serde_json::json!({ "error": e.to_string() })
    });
    Json(result)
}

async fn hook_call(
    State(state): State<AppState>,
    Path(event): Path<String>,
    Json(payload): Json<Value>,
) -> Json<Value> {
    info!("Hook call: {}", event);
    // Assuming plugin function "handle_hook"
    let result: Value = state.plugin_manager.call("handle_hook", &payload).await.unwrap_or_else(|e| {
        serde_json::json!({ "error": e.to_string() })
    });
    Json(result)
}

async fn reload(State(state): State<AppState>) -> Json<Value> {
    info!("Reloading plugin...");
    match state.plugin_manager.reload(state.services.clone()).await {
        Ok(_) => Json(serde_json::json!({ "status": "reloaded" })),
        Err(e) => Json(serde_json::json!({ "error": e.to_string() })),
    }
}
