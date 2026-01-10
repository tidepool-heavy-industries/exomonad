//! Application state shared across handlers.

use sqlx::SqlitePool;
use std::path::Path;
use tokio::sync::broadcast;

use crate::db;
use crate::error::Result;
use crate::types::HubEvent;

/// Shared application state.
#[derive(Clone)]
pub struct AppState {
    pub pool: SqlitePool,
    pub tx: broadcast::Sender<HubEvent>,
}

impl AppState {
    /// Create new app state with database connection.
    pub async fn new(db_path: &Path) -> Result<Self> {
        let pool = db::init_pool(db_path).await?;
        let (tx, _) = broadcast::channel(100);

        Ok(Self { pool, tx })
    }

    /// Broadcast an event to all WebSocket subscribers.
    pub fn broadcast(&self, event: HubEvent) {
        // Ignore send errors (no subscribers)
        let _ = self.tx.send(event);
    }

    /// Subscribe to hub events.
    pub fn subscribe(&self) -> broadcast::Receiver<HubEvent> {
        self.tx.subscribe()
    }
}
