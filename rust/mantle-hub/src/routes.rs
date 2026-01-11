//! HTTP API routes.
//!
//! URL structure:
//! - GET  /                              → redirect to /sessions
//! - GET  /sessions                      → sessions list page (HTML)
//! - GET  /sessions/{sid}                → session tree page (HTML)
//!
//! API:
//! - POST /api/sessions                  → create session + root node
//! - GET  /api/sessions                  → list sessions
//! - GET  /api/sessions/{sid}            → session with all nodes
//! - DELETE /api/sessions/{sid}          → delete session
//! - POST /api/sessions/{sid}/nodes      → add child node
//! - GET  /api/sessions/{sid}/nodes/{nid} → node detail
//! - GET  /api/sessions/{sid}/nodes/{nid}/events → node events
//! - POST /api/sessions/{sid}/nodes/{nid}/result → submit node result
//! - GET  /api/sessions/{sid}/graph      → graph data for visualization
//!
//! WebSocket:
//! - WS /ws                              → frontend subscribes for updates
//! - WS /ws/push/{sid}/{nid}             → mantle pushes events for a node

use axum::{
    extract::{Path, State, WebSocketUpgrade},
    http::header,
    response::{IntoResponse, Redirect, Response},
    routing::{get, post},
    Json, Router,
};
use tower_http::services::ServeDir;

use crate::db;
use crate::error::Result;
use crate::state::AppState;
use crate::types::{
    GraphData, HubEvent, NodeCreateResponse, NodeEvent, NodeInfo, NodeRegister, NodeResult,
    NodeState, SessionCreateResponse, SessionInfo, SessionRegister, SessionWithNodes,
};

/// Build the router with all routes.
pub fn router(state: AppState, static_dir: &std::path::Path) -> Router {
    // Capture static_dir for page handlers
    let index_html_path = static_dir.join("index.html");
    let session_html_path = static_dir.join("session.html");

    Router::new()
        // Page routes
        .route("/", get(|| async { Redirect::permanent("/sessions") }))
        .route(
            "/sessions",
            get({
                let path = index_html_path.clone();
                move || serve_html_page(path.clone())
            }),
        )
        .route(
            "/sessions/{sid}",
            get(move || serve_html_page(session_html_path.clone())),
        )
        // API routes - Sessions
        .route("/api/sessions", get(list_sessions).post(create_session))
        .route(
            "/api/sessions/{sid}",
            get(get_session).delete(delete_session),
        )
        .route("/api/sessions/{sid}/graph", get(get_graph))
        // API routes - Nodes
        .route("/api/sessions/{sid}/nodes", post(create_node))
        .route("/api/sessions/{sid}/nodes/{nid}", get(get_node))
        .route("/api/sessions/{sid}/nodes/{nid}/events", get(get_node_events).post(post_node_event))
        .route(
            "/api/sessions/{sid}/nodes/{nid}/result",
            post(submit_result),
        )
        // WebSocket for live updates (frontend subscribes)
        .route("/ws", get(websocket_handler))
        // WebSocket for event push (mantle pushes node events)
        .route("/ws/push/{sid}/{nid}", get(push_websocket_handler))
        // Static files (frontend) - use fallback_service for root-level serving
        .fallback_service(ServeDir::new(static_dir))
        .with_state(state)
}

// ============================================================================
// Page Handlers
// ============================================================================

/// Serve an HTML page from the static directory.
async fn serve_html_page(path: std::path::PathBuf) -> Response {
    match tokio::fs::read(&path).await {
        Ok(contents) => Response::builder()
            .status(200)
            .header(header::CONTENT_TYPE, "text/html; charset=utf-8")
            .body(axum::body::Body::from(contents))
            .unwrap(),
        Err(_) => Response::builder()
            .status(404)
            .body(axum::body::Body::from("Page not found"))
            .unwrap(),
    }
}

// ============================================================================
// Session Endpoints
// ============================================================================

/// List all sessions.
async fn list_sessions(State(state): State<AppState>) -> Result<Json<Vec<SessionInfo>>> {
    let sessions = db::list_sessions(&state.pool).await?;
    Ok(Json(sessions))
}

/// Create a new session with its root node.
async fn create_session(
    State(state): State<AppState>,
    Json(req): Json<SessionRegister>,
) -> Result<Json<SessionCreateResponse>> {
    let (session_id, node_id) = db::create_session(
        &state.pool,
        &req.branch,
        &req.worktree.display().to_string(),
        &req.prompt,
        &req.model,
    )
    .await?;

    let session = db::get_session(&state.pool, &session_id).await?;
    let root_node = db::get_node(&state.pool, &node_id).await?;

    // Broadcast events
    state.broadcast(HubEvent::SessionCreated {
        session: session.clone(),
    });
    state.broadcast(HubEvent::NodeCreated {
        node: root_node.clone(),
    });

    Ok(Json(SessionCreateResponse { session, root_node }))
}

/// Get session with all its nodes.
async fn get_session(
    State(state): State<AppState>,
    Path(sid): Path<String>,
) -> Result<Json<SessionWithNodes>> {
    let session = db::get_session_with_nodes(&state.pool, &sid).await?;
    Ok(Json(session))
}

/// Delete session.
async fn delete_session(
    State(state): State<AppState>,
    Path(sid): Path<String>,
) -> Result<impl IntoResponse> {
    db::delete_session(&state.pool, &sid).await?;
    Ok(axum::http::StatusCode::NO_CONTENT)
}

/// Get graph data for a session.
async fn get_graph(
    State(state): State<AppState>,
    Path(sid): Path<String>,
) -> Result<Json<GraphData>> {
    let graph = db::get_graph_data(&state.pool, &sid).await?;
    Ok(Json(graph))
}

// ============================================================================
// Node Endpoints
// ============================================================================

/// Add a child node to a session.
async fn create_node(
    State(state): State<AppState>,
    Path(sid): Path<String>,
    Json(req): Json<NodeRegister>,
) -> Result<Json<NodeCreateResponse>> {
    let node_id = db::create_node(
        &state.pool,
        &sid,
        &req.parent_node_id,
        &req.branch,
        &req.worktree.display().to_string(),
        &req.prompt,
        &req.model,
    )
    .await?;

    let node = db::get_node(&state.pool, &node_id).await?;

    // Broadcast event
    state.broadcast(HubEvent::NodeCreated { node: node.clone() });

    // Also update session (node count changed)
    if let Ok(session) = db::get_session(&state.pool, &sid).await {
        state.broadcast(HubEvent::SessionUpdated { session });
    }

    Ok(Json(NodeCreateResponse { node }))
}

/// Get node by ID.
async fn get_node(
    State(state): State<AppState>,
    Path((sid, nid)): Path<(String, String)>,
) -> Result<Json<NodeInfo>> {
    let node = db::get_node(&state.pool, &nid).await?;

    // Verify node belongs to session
    if node.session_id != sid {
        return Err(crate::error::HubError::BadRequest(format!(
            "Node {} does not belong to session {}",
            nid, sid
        )));
    }

    Ok(Json(node))
}

/// Get events for a node.
async fn get_node_events(
    State(state): State<AppState>,
    Path((sid, nid)): Path<(String, String)>,
) -> Result<Json<Vec<NodeEvent>>> {
    // First verify the node exists and belongs to this session
    let node = db::get_node(&state.pool, &nid).await?;
    if node.session_id != sid {
        return Err(crate::error::HubError::BadRequest(format!(
            "Node {} does not belong to session {}",
            nid, sid
        )));
    }

    let events = db::get_node_events(&state.pool, &nid).await?;
    Ok(Json(events))
}

/// Post an event for a node (for testing - broadcasts to WebSocket).
async fn post_node_event(
    State(state): State<AppState>,
    Path((sid, nid)): Path<(String, String)>,
    Json(event): Json<mantle_shared::events::StreamEvent>,
) -> Result<impl IntoResponse> {
    // Verify node exists and belongs to session
    let node = db::get_node(&state.pool, &nid).await?;
    if node.session_id != sid {
        return Err(crate::error::HubError::BadRequest(format!(
            "Node {} does not belong to session {}",
            nid, sid
        )));
    }

    // Persist event
    db::insert_event(&state.pool, &nid, &event).await?;

    // Broadcast to WebSocket subscribers
    let timestamp = chrono::Utc::now().to_rfc3339();
    state.broadcast(HubEvent::NodeEvent {
        session_id: sid,
        node_id: nid,
        event,
        timestamp,
    });

    Ok(axum::http::StatusCode::CREATED)
}

/// Submit node result.
async fn submit_result(
    State(state): State<AppState>,
    Path((sid, nid)): Path<(String, String)>,
    Json(mut result): Json<NodeResult>,
) -> Result<impl IntoResponse> {
    // Ensure node_id matches path
    result.node_id = nid.clone();

    db::insert_result(&state.pool, &result).await?;

    // Broadcast event
    state.broadcast(HubEvent::NodeCompleted {
        node_id: nid.clone(),
        result,
    });

    // Update session state
    if let Ok(session) = db::get_session(&state.pool, &sid).await {
        state.broadcast(HubEvent::SessionUpdated { session });
    }

    Ok(axum::http::StatusCode::OK)
}

// ============================================================================
// WebSocket - Frontend Subscription
// ============================================================================

/// WebSocket handler for live updates.
async fn websocket_handler(
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> impl IntoResponse {
    ws.on_upgrade(|socket| handle_websocket(socket, state))
}

async fn handle_websocket(mut socket: axum::extract::ws::WebSocket, state: AppState) {
    use axum::extract::ws::Message;

    let mut rx = state.subscribe();

    // Send initial sessions list
    if let Ok(sessions) = db::list_sessions(&state.pool).await {
        let msg = serde_json::json!({
            "type": "init",
            "sessions": sessions
        });
        if socket
            .send(Message::Text(msg.to_string().into()))
            .await
            .is_err()
        {
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

// ============================================================================
// WebSocket - Mantle Event Push
// ============================================================================

/// WebSocket handler for mantle to push node events.
///
/// mantle connects to this endpoint and sends StreamEvent objects.
/// Hub persists them and broadcasts to all subscribers.
async fn push_websocket_handler(
    State(state): State<AppState>,
    Path((sid, nid)): Path<(String, String)>,
    ws: WebSocketUpgrade,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_push_websocket(socket, state, sid, nid))
}

async fn handle_push_websocket(
    mut socket: axum::extract::ws::WebSocket,
    state: AppState,
    session_id: String,
    node_id: String,
) {
    use axum::extract::ws::Message;
    use mantle_shared::events::StreamEvent;

    tracing::info!(session_id = %session_id, node_id = %node_id, "Push WebSocket connected");

    // Update node state to Running
    if let Err(e) = db::update_node_state(&state.pool, &node_id, NodeState::Running).await {
        tracing::warn!(node_id = %node_id, error = %e, "Failed to update node state");
    }

    // Also update session
    if let Ok(session) = db::get_session(&state.pool, &session_id).await {
        state.broadcast(HubEvent::SessionUpdated { session });
    }

    // Receive events from mantle and broadcast to subscribers
    while let Some(msg) = socket.recv().await {
        match msg {
            Ok(Message::Text(text)) => {
                match serde_json::from_str::<StreamEvent>(&text) {
                    Ok(event) => {
                        // Persist event to database
                        if let Err(e) = db::insert_event(&state.pool, &node_id, &event).await {
                            tracing::warn!(
                                node_id = %node_id,
                                error = %e,
                                "Failed to persist stream event"
                            );
                        }

                        let timestamp = chrono::Utc::now().to_rfc3339();
                        state.broadcast(HubEvent::NodeEvent {
                            session_id: session_id.clone(),
                            node_id: node_id.clone(),
                            event,
                            timestamp,
                        });
                    }
                    Err(e) => {
                        tracing::warn!(
                            node_id = %node_id,
                            error = %e,
                            "Failed to parse stream event"
                        );
                    }
                }
            }
            Ok(Message::Close(_)) => {
                tracing::info!(node_id = %node_id, "Push WebSocket closed");
                break;
            }
            Err(e) => {
                tracing::warn!(node_id = %node_id, error = %e, "WebSocket error");
                break;
            }
            _ => {} // Ignore ping/pong/binary
        }
    }

    tracing::info!(node_id = %node_id, "Push WebSocket disconnected");
}
