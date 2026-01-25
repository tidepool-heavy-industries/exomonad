mod spawner;

use axum::{
    extract::{Path, State},
    routing::{get, post},
    Json, Router,
};
use spawner::{Spawner, SpawnRequest, SpawnResponse, StatusResponse};
use std::sync::Arc;
use tracing::{error, info};
use tower_http::trace::TraceLayer;

struct AppState {
    spawner: Spawner,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let spawner = Spawner::new()?;
    let state = Arc::new(AppState { spawner });

    let app = Router::new()
        .route("/health", get(health_check))
        .route("/spawn", post(spawn_container))
        .route("/status/:id", get(get_status))
        .route("/stop/:id", post(stop_container))
        .layer(TraceLayer::new_for_http())
        .with_state(state);

    let addr = "0.0.0.0:7435";
    let listener = tokio::net::TcpListener::bind(addr).await?;
    info!("Starting docker-spawner on {}", addr);
    axum::serve(listener, app).await?;

    Ok(())
}

async fn health_check() -> &'static str {
    "OK"
}

async fn spawn_container(
    State(state): State<Arc<AppState>>,
    Json(payload): Json<SpawnRequest>,
) -> Result<Json<SpawnResponse>, (axum::http::StatusCode, String)> {
    match state.spawner.spawn(payload).await {
        Ok(response) => Ok(Json(response)),
        Err(e) => {
            error!("Failed to spawn container: {}", e);
            Err((axum::http::StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))
        }
    }
}

async fn get_status(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<StatusResponse>, (axum::http::StatusCode, String)> {
    match state.spawner.status(&id).await {
        Ok(status) => Ok(Json(StatusResponse { status })),
        Err(e) => {
            error!("Failed to get container status: {}", e);
            Err((axum::http::StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))
        }
    }
}

async fn stop_container(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<axum::http::StatusCode, (axum::http::StatusCode, String)> {
    match state.spawner.stop(&id).await {
        Ok(_) => Ok(axum::http::StatusCode::OK),
        Err(e) => {
            error!("Failed to stop container: {}", e);
            Err((axum::http::StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))
        }
    }
}
