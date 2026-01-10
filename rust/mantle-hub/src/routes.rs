//! HTTP API routes.

use axum::{
    extract::{Path, State, WebSocketUpgrade},
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use tower_http::services::ServeDir;

use crate::db;
use crate::error::{HubError, Result};
use crate::state::AppState;
use crate::types::{HubEvent, SessionInfo, SessionRegister, SessionResult};

/// Build the router with all routes.
pub fn router(state: AppState, static_dir: &std::path::Path) -> Router {
    Router::new()
        // API routes
        .route("/api/sessions", get(list_sessions).post(register_session))
        .route("/api/sessions/{id}", get(get_session).delete(delete_session))
        .route("/api/sessions/{id}/result", post(submit_result).get(poll_result))
        .route("/api/graph", get(get_graph))
        // WebSocket for live updates
        .route("/ws", get(websocket_handler))
        // Static files (frontend) - use fallback_service for root-level serving
        .fallback_service(ServeDir::new(static_dir))
        .with_state(state)
}

// ============================================================================
// Session Endpoints
// ============================================================================

/// List all sessions.
async fn list_sessions(State(state): State<AppState>) -> Result<Json<Vec<SessionInfo>>> {
    let sessions = db::list_sessions(&state.pool).await?;
    Ok(Json(sessions))
}

/// Register a new session.
///
/// Session ID is generated server-side and returned in the response.
async fn register_session(
    State(state): State<AppState>,
    Json(req): Json<SessionRegister>,
) -> Result<Json<SessionInfo>> {
    let session_id = db::insert_session(
        &state.pool,
        &req.branch,
        &req.worktree.display().to_string(),
        &req.prompt,
        &req.model,
        req.parent_id.as_deref(),
    )
    .await?;

    let session = db::get_session(&state.pool, &session_id).await?;

    // Broadcast event
    state.broadcast(HubEvent::SessionStarted {
        session: session.clone(),
    });

    Ok(Json(session))
}

/// Get session by ID.
async fn get_session(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<Json<SessionInfo>> {
    let session = db::get_session(&state.pool, &id).await?;
    Ok(Json(session))
}

/// Delete session.
async fn delete_session(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<impl IntoResponse> {
    db::delete_session(&state.pool, &id).await?;
    Ok(axum::http::StatusCode::NO_CONTENT)
}

/// Submit session result (typically from container via socket, but also accessible via HTTP).
async fn submit_result(
    State(state): State<AppState>,
    Path(id): Path<String>,
    Json(mut result): Json<SessionResult>,
) -> Result<impl IntoResponse> {
    // Ensure session_id matches path
    result.session_id = id.clone();

    db::insert_result(&state.pool, &result).await?;

    // Broadcast event
    state.broadcast(HubEvent::SessionCompleted {
        session_id: id,
        result,
    });

    Ok(axum::http::StatusCode::OK)
}

/// Poll for session result (blocking with timeout).
async fn poll_result(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<Json<SessionResult>> {
    // Simple implementation: check if result exists
    // TODO: Add long-polling with timeout
    let session = db::get_session(&state.pool, &id).await?;

    session
        .result
        .ok_or_else(|| HubError::BadRequest(format!("Session {} has no result yet", id)))
        .map(Json)
}

/// Get graph data for visualization.
async fn get_graph(
    State(state): State<AppState>,
) -> Result<Json<crate::types::GraphData>> {
    let graph = db::get_graph_data(&state.pool).await?;
    Ok(Json(graph))
}

// ============================================================================
// WebSocket
// ============================================================================

/// WebSocket handler for live updates.
async fn websocket_handler(
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> impl IntoResponse {
    ws.on_upgrade(|socket| handle_websocket(socket, state))
}

async fn handle_websocket(
    mut socket: axum::extract::ws::WebSocket,
    state: AppState,
) {
    use axum::extract::ws::Message;

    let mut rx = state.subscribe();

    // Send initial graph data
    if let Ok(graph) = db::get_graph_data(&state.pool).await {
        let msg = serde_json::json!({
            "type": "init",
            "graph": graph
        });
        if socket.send(Message::Text(msg.to_string().into())).await.is_err() {
            return;
        }
    }

    // Forward events to WebSocket
    while let Ok(event) = rx.recv().await {
        let json = match serde_json::to_string(&event) {
            Ok(j) => j,
            Err(_) => continue,
        };

        if socket.send(Message::Text(json.into())).await.is_err() {
            break;
        }
    }
}
