//! HTTP MCP transport.
//!
//! Implements JSON-RPC over HTTP using axum. Reuses the same `handle_request`
//! dispatch logic as the stdio transport.

use super::stdio::{handle_request, JsonRpcRequest, JsonRpcResponse};
use super::McpState;
use anyhow::Result;
use axum::{extract::State, Json, Router};
use serde_json::json;
use std::sync::Arc;
use tokio::net::TcpListener;
use tower_http::cors::{Any, CorsLayer};
use tracing::{info, warn};

// ============================================================================
// Axum State
// ============================================================================

/// Shared state wrapper for axum (requires Clone).
#[derive(Clone)]
struct HttpState {
    mcp: Arc<McpState>,
}

// ============================================================================
// Handlers
// ============================================================================

/// POST /mcp — JSON-RPC endpoint.
///
/// Accepts a JSON-RPC request, dispatches to the shared `handle_request`,
/// and returns the JSON-RPC response.
async fn mcp_handler(
    State(state): State<HttpState>,
    Json(request): Json<JsonRpcRequest>,
) -> Json<JsonRpcResponse> {
    let method = request.method.clone();
    let start = std::time::Instant::now();

    // Hot reload: check if WASM file changed since last call
    match state.mcp.plugin.reload_if_changed().await {
        Ok(true) => info!("WASM hot-reloaded before handling request"),
        Ok(false) => {} // No change, fast path
        Err(e) => warn!(error = %e, "WASM reload check failed, using existing plugin"),
    }

    let response = handle_request(&state.mcp, request).await;

    let elapsed = start.elapsed();
    info!(method = %method, duration_ms = %elapsed.as_millis(), "HTTP request handled");

    Json(response)
}

/// GET /health — Health check endpoint.
async fn health_handler() -> Json<serde_json::Value> {
    Json(json!({
        "status": "ok",
        "version": env!("CARGO_PKG_VERSION")
    }))
}

// ============================================================================
// Server
// ============================================================================

/// Run the HTTP MCP server on the given port.
///
/// Shares `McpState` via `Arc` across all request handlers.
/// Shuts down gracefully on ctrl-c.
pub async fn run_http_server(state: McpState, port: u16) -> Result<()> {
    let http_state = HttpState {
        mcp: Arc::new(state),
    };

    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    let app = Router::new()
        .route("/mcp", axum::routing::post(mcp_handler))
        .route("/health", axum::routing::get(health_handler))
        .layer(cors)
        .with_state(http_state);

    let listener = TcpListener::bind(format!("0.0.0.0:{}", port)).await?;
    info!(port = %port, "HTTP MCP server listening");

    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    info!("HTTP MCP server shut down");
    Ok(())
}

async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("failed to install ctrl-c handler");
    info!("Received ctrl-c, initiating graceful shutdown");
}
