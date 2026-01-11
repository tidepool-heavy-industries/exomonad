//! Error types for mantle-hub.

use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use thiserror::Error;

#[derive(Debug, Error)]
#[allow(dead_code)]
pub enum HubError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Session not found: {0}")]
    SessionNotFound(String),

    #[error("Node not found: {0}")]
    NodeNotFound(String),

    #[error("Invalid request: {0}")]
    BadRequest(String),

    #[error("Internal error: {0}")]
    Internal(String),
}

impl IntoResponse for HubError {
    fn into_response(self) -> Response {
        let (status, message) = match &self {
            HubError::Database(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()),
            HubError::SessionNotFound(id) => {
                (StatusCode::NOT_FOUND, format!("Session not found: {}", id))
            }
            HubError::NodeNotFound(id) => {
                (StatusCode::NOT_FOUND, format!("Node not found: {}", id))
            }
            HubError::BadRequest(msg) => (StatusCode::BAD_REQUEST, msg.clone()),
            HubError::Internal(msg) => (StatusCode::INTERNAL_SERVER_ERROR, msg.clone()),
        };

        let body = serde_json::json!({
            "error": message
        });

        (status, axum::Json(body)).into_response()
    }
}

pub type Result<T> = std::result::Result<T, HubError>;
