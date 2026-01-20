//! SQLite database operations.
//!
//! Two-table schema:
//! - `sessions`: Orchestration runs (tree containers)
//! - `nodes`: Individual Claude Code executions
//! - `results`: Node completion data
//! - `events`: StreamEvent log per node

use sqlx::sqlite::{SqliteConnectOptions, SqlitePool, SqlitePoolOptions};
use std::path::Path;
use std::str::FromStr;

use crate::error::{HubError, Result};
use crate::types::{
    GraphData, GraphEdge, GraphNode, NodeEvent, NodeInfo, NodeResult, NodeState, SessionInfo,
    SessionState, SessionWithNodes,
};

/// Initialize database pool and run migrations.
pub async fn init_pool(db_path: &Path) -> Result<SqlitePool> {
    let db_url = format!("sqlite:{}?mode=rwc", db_path.display());

    let options = SqliteConnectOptions::from_str(&db_url)
        .map_err(HubError::Database)?
        .create_if_missing(true);

    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect_with(options)
        .await?;

    // Run migrations
    run_migrations(&pool).await?;

    Ok(pool)
}

/// Run database migrations.
async fn run_migrations(pool: &SqlitePool) -> Result<()> {
    // Enable foreign keys
    sqlx::query("PRAGMA foreign_keys = ON")
        .execute(pool)
        .await?;

    // Sessions table (the tree container)
    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS sessions (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL,
            created_at TEXT NOT NULL,
            updated_at TEXT NOT NULL
        )
        "#,
    )
    .execute(pool)
    .await?;

    // Nodes table (individual Claude runs)
    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS nodes (
            id TEXT PRIMARY KEY,
            session_id TEXT NOT NULL,
            parent_node_id TEXT,
            branch TEXT NOT NULL,
            worktree TEXT NOT NULL,
            prompt TEXT NOT NULL,
            model TEXT NOT NULL,
            state TEXT NOT NULL DEFAULT 'pending',
            metadata TEXT,
            created_at TEXT NOT NULL,
            updated_at TEXT NOT NULL,
            FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,
            FOREIGN KEY (parent_node_id) REFERENCES nodes(id) ON DELETE SET NULL
        )
        "#,
    )
    .execute(pool)
    .await?;

    // Migration: add metadata column if not exists (for existing databases)
    // SQLite doesn't support IF NOT EXISTS for columns, so we check first
    let has_metadata: Option<(String,)> = sqlx::query_as(
        "SELECT name FROM pragma_table_info('nodes') WHERE name = 'metadata'",
    )
    .fetch_optional(pool)
    .await?;

    if has_metadata.is_none() {
        sqlx::query("ALTER TABLE nodes ADD COLUMN metadata TEXT")
            .execute(pool)
            .await
            .ok(); // Ignore error if column exists
    }

    // Results table (node completion data)
    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS results (
            node_id TEXT PRIMARY KEY,
            exit_code INTEGER NOT NULL,
            is_error INTEGER NOT NULL,
            result_text TEXT,
            structured_output TEXT,
            total_cost_usd REAL NOT NULL,
            num_turns INTEGER NOT NULL,
            cc_session_id TEXT NOT NULL,
            duration_secs REAL NOT NULL,
            model_usage TEXT,
            created_at TEXT NOT NULL,
            FOREIGN KEY (node_id) REFERENCES nodes(id) ON DELETE CASCADE
        )
        "#,
    )
    .execute(pool)
    .await?;

    // Events table (StreamEvent log)
    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS events (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            node_id TEXT NOT NULL,
            event_type TEXT NOT NULL,
            event_data TEXT NOT NULL,
            timestamp TEXT NOT NULL,
            FOREIGN KEY (node_id) REFERENCES nodes(id) ON DELETE CASCADE
        )
        "#,
    )
    .execute(pool)
    .await?;

    // Indexes
    sqlx::query("CREATE INDEX IF NOT EXISTS idx_nodes_session ON nodes(session_id)")
        .execute(pool)
        .await?;

    sqlx::query("CREATE INDEX IF NOT EXISTS idx_nodes_parent ON nodes(parent_node_id)")
        .execute(pool)
        .await?;

    sqlx::query("CREATE INDEX IF NOT EXISTS idx_nodes_state ON nodes(state)")
        .execute(pool)
        .await?;

    sqlx::query("CREATE INDEX IF NOT EXISTS idx_events_node ON events(node_id)")
        .execute(pool)
        .await?;

    // Index for querying nodes by execution_id in metadata
    sqlx::query(
        "CREATE INDEX IF NOT EXISTS idx_nodes_metadata_execution ON nodes(json_extract(metadata, '$.execution_id'))",
    )
    .execute(pool)
    .await?;

    Ok(())
}

// ============================================================================
// Session Operations
// ============================================================================

/// Create a new session with its root node atomically.
///
/// Returns (session_id, root_node_id).
pub async fn create_session(
    pool: &SqlitePool,
    branch: &str,
    worktree: &str,
    prompt: &str,
    model: &str,
) -> Result<(String, String)> {
    let session_id = uuid::Uuid::new_v4().to_string();
    let node_id = uuid::Uuid::new_v4().to_string();
    let now = chrono::Utc::now().to_rfc3339();

    // Use branch as session name
    let name = branch.to_string();

    // Insert session
    sqlx::query(
        r#"
        INSERT INTO sessions (id, name, created_at, updated_at)
        VALUES (?, ?, ?, ?)
        "#,
    )
    .bind(&session_id)
    .bind(&name)
    .bind(&now)
    .bind(&now)
    .execute(pool)
    .await?;

    // Insert root node (no parent)
    sqlx::query(
        r#"
        INSERT INTO nodes (id, session_id, parent_node_id, branch, worktree, prompt, model, state, created_at, updated_at)
        VALUES (?, ?, NULL, ?, ?, ?, ?, 'running', ?, ?)
        "#,
    )
    .bind(&node_id)
    .bind(&session_id)
    .bind(branch)
    .bind(worktree)
    .bind(prompt)
    .bind(model)
    .bind(&now)
    .bind(&now)
    .execute(pool)
    .await?;

    Ok((session_id, node_id))
}

/// Create an empty session without a root node.
///
/// Used for graph execution tracking where Haskell creates the session first,
/// then spawns nodes that register into this session.
///
/// Returns the session_id.
pub async fn create_empty_session(pool: &SqlitePool, name: &str) -> Result<String> {
    let session_id = uuid::Uuid::new_v4().to_string();
    let now = chrono::Utc::now().to_rfc3339();

    sqlx::query(
        r#"
        INSERT INTO sessions (id, name, created_at, updated_at)
        VALUES (?, ?, ?, ?)
        "#,
    )
    .bind(&session_id)
    .bind(name)
    .bind(&now)
    .bind(&now)
    .execute(pool)
    .await?;

    Ok(session_id)
}

/// Get session by ID.
pub async fn get_session(pool: &SqlitePool, id: &str) -> Result<SessionInfo> {
    let row = sqlx::query_as::<_, SessionRow>(
        r#"
        SELECT s.id, s.name, s.created_at, s.updated_at,
               COUNT(n.id) as node_count,
               SUM(CASE WHEN n.state = 'running' THEN 1 ELSE 0 END) as running_count,
               SUM(CASE WHEN n.state = 'failed' THEN 1 ELSE 0 END) as failed_count
        FROM sessions s
        LEFT JOIN nodes n ON s.id = n.session_id
        WHERE s.id = ?
        GROUP BY s.id
        "#,
    )
    .bind(id)
    .fetch_optional(pool)
    .await?
    .ok_or_else(|| HubError::SessionNotFound(id.to_string()))?;

    Ok(row.into_session_info())
}

/// Get session with all its nodes.
pub async fn get_session_with_nodes(pool: &SqlitePool, id: &str) -> Result<SessionWithNodes> {
    let session = get_session(pool, id).await?;
    let nodes = list_nodes_for_session(pool, id).await?;

    Ok(SessionWithNodes { session, nodes })
}

/// List all sessions.
pub async fn list_sessions(pool: &SqlitePool) -> Result<Vec<SessionInfo>> {
    let rows = sqlx::query_as::<_, SessionRow>(
        r#"
        SELECT s.id, s.name, s.created_at, s.updated_at,
               COUNT(n.id) as node_count,
               SUM(CASE WHEN n.state = 'running' THEN 1 ELSE 0 END) as running_count,
               SUM(CASE WHEN n.state = 'failed' THEN 1 ELSE 0 END) as failed_count
        FROM sessions s
        LEFT JOIN nodes n ON s.id = n.session_id
        GROUP BY s.id
        ORDER BY s.created_at DESC
        "#,
    )
    .fetch_all(pool)
    .await?;

    Ok(rows.into_iter().map(|r| r.into_session_info()).collect())
}

/// Delete session (cascades to nodes, results, events).
pub async fn delete_session(pool: &SqlitePool, id: &str) -> Result<()> {
    let result = sqlx::query("DELETE FROM sessions WHERE id = ?")
        .bind(id)
        .execute(pool)
        .await?;

    if result.rows_affected() == 0 {
        return Err(HubError::SessionNotFound(id.to_string()));
    }

    Ok(())
}

// ============================================================================
// Node Operations
// ============================================================================

/// Add a node to a session.
///
/// # Arguments
/// * `parent_node_id` - Optional parent for tree structure. If None, this is a root node.
/// * `metadata` - Optional JSON metadata (execution_id, node_path, node_type, depth).
pub async fn create_node(
    pool: &SqlitePool,
    session_id: &str,
    parent_node_id: Option<&str>,
    branch: &str,
    worktree: &str,
    prompt: &str,
    model: &str,
    metadata: Option<&serde_json::Value>,
) -> Result<String> {
    // Verify session exists
    let _ = get_session(pool, session_id).await?;

    // Verify parent node exists and belongs to this session (if provided)
    if let Some(parent_id) = parent_node_id {
        let parent = get_node(pool, parent_id).await?;
        if parent.session_id != session_id {
            return Err(HubError::BadRequest(format!(
                "Parent node {} does not belong to session {}",
                parent_id, session_id
            )));
        }
    }

    let node_id = uuid::Uuid::new_v4().to_string();
    let now = chrono::Utc::now().to_rfc3339();
    let metadata_json = metadata.map(|m| serde_json::to_string(m).unwrap_or_default());

    sqlx::query(
        r#"
        INSERT INTO nodes (id, session_id, parent_node_id, branch, worktree, prompt, model, state, metadata, created_at, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, 'running', ?, ?, ?)
        "#,
    )
    .bind(&node_id)
    .bind(session_id)
    .bind(parent_node_id)
    .bind(branch)
    .bind(worktree)
    .bind(prompt)
    .bind(model)
    .bind(&metadata_json)
    .bind(&now)
    .bind(&now)
    .execute(pool)
    .await?;

    // Update session timestamp
    sqlx::query("UPDATE sessions SET updated_at = ? WHERE id = ?")
        .bind(&now)
        .bind(session_id)
        .execute(pool)
        .await?;

    Ok(node_id)
}

/// Get node by ID.
pub async fn get_node(pool: &SqlitePool, id: &str) -> Result<NodeInfo> {
    let row = sqlx::query_as::<_, NodeRow>(
        r#"
        SELECT n.id, n.session_id, n.parent_node_id, n.branch, n.worktree, n.prompt, n.model, n.state, n.metadata, n.created_at, n.updated_at,
               r.exit_code, r.is_error, r.result_text, r.structured_output, r.total_cost_usd, r.num_turns, r.cc_session_id, r.duration_secs, r.model_usage
        FROM nodes n
        LEFT JOIN results r ON n.id = r.node_id
        WHERE n.id = ?
        "#,
    )
    .bind(id)
    .fetch_optional(pool)
    .await?
    .ok_or_else(|| HubError::NodeNotFound(id.to_string()))?;

    Ok(row.into_node_info())
}

/// List all nodes for a session.
pub async fn list_nodes_for_session(pool: &SqlitePool, session_id: &str) -> Result<Vec<NodeInfo>> {
    let rows = sqlx::query_as::<_, NodeRow>(
        r#"
        SELECT n.id, n.session_id, n.parent_node_id, n.branch, n.worktree, n.prompt, n.model, n.state, n.metadata, n.created_at, n.updated_at,
               r.exit_code, r.is_error, r.result_text, r.structured_output, r.total_cost_usd, r.num_turns, r.cc_session_id, r.duration_secs, r.model_usage
        FROM nodes n
        LEFT JOIN results r ON n.id = r.node_id
        WHERE n.session_id = ?
        ORDER BY n.created_at ASC
        "#,
    )
    .bind(session_id)
    .fetch_all(pool)
    .await?;

    Ok(rows.into_iter().map(|r| r.into_node_info()).collect())
}

/// Update node state.
pub async fn update_node_state(pool: &SqlitePool, id: &str, state: NodeState) -> Result<()> {
    let now = chrono::Utc::now().to_rfc3339();

    let result = sqlx::query("UPDATE nodes SET state = ?, updated_at = ? WHERE id = ?")
        .bind(state.to_string())
        .bind(&now)
        .bind(id)
        .execute(pool)
        .await?;

    if result.rows_affected() == 0 {
        return Err(HubError::NodeNotFound(id.to_string()));
    }

    // Update session timestamp
    sqlx::query(
        r#"
        UPDATE sessions SET updated_at = ?
        WHERE id = (SELECT session_id FROM nodes WHERE id = ?)
        "#,
    )
    .bind(&now)
    .bind(id)
    .execute(pool)
    .await?;

    Ok(())
}

/// Insert node result.
pub async fn insert_result(pool: &SqlitePool, result: &NodeResult) -> Result<()> {
    let now = chrono::Utc::now().to_rfc3339();
    let structured_output = result
        .structured_output
        .as_ref()
        .map(|v| serde_json::to_string(v).unwrap_or_default());
    let model_usage = serde_json::to_string(&result.model_usage).unwrap_or_default();

    sqlx::query(
        r#"
        INSERT OR REPLACE INTO results
        (node_id, exit_code, is_error, result_text, structured_output, total_cost_usd, num_turns, cc_session_id, duration_secs, model_usage, created_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        "#,
    )
    .bind(&result.node_id)
    .bind(result.exit_code)
    .bind(result.is_error as i32)
    .bind(&result.result_text)
    .bind(&structured_output)
    .bind(result.total_cost_usd)
    .bind(result.num_turns)
    .bind(&result.cc_session_id)
    .bind(result.duration_secs)
    .bind(&model_usage)
    .bind(&now)
    .execute(pool)
    .await?;

    // Update node state
    let state = if result.is_error {
        NodeState::Failed
    } else {
        NodeState::Completed
    };
    let _ = update_node_state(pool, &result.node_id, state).await;

    Ok(())
}

// ============================================================================
// Event Operations
// ============================================================================

/// Insert a stream event for a node.
pub async fn insert_event(
    pool: &SqlitePool,
    node_id: &str,
    event: &mantle_shared::events::StreamEvent,
) -> Result<i64> {
    let now = chrono::Utc::now().to_rfc3339();
    let event_type = match event {
        mantle_shared::events::StreamEvent::System(_) => "system",
        mantle_shared::events::StreamEvent::Assistant(_) => "assistant",
        mantle_shared::events::StreamEvent::User(_) => "user",
        mantle_shared::events::StreamEvent::Result(_) => "result",
    };
    let event_data =
        serde_json::to_string(event).map_err(|e| HubError::BadRequest(e.to_string()))?;

    let result = sqlx::query(
        r#"
        INSERT INTO events (node_id, event_type, event_data, timestamp)
        VALUES (?, ?, ?, ?)
        "#,
    )
    .bind(node_id)
    .bind(event_type)
    .bind(&event_data)
    .bind(&now)
    .execute(pool)
    .await?;

    Ok(result.last_insert_rowid())
}

/// Get events for a node.
pub async fn get_node_events(pool: &SqlitePool, node_id: &str) -> Result<Vec<NodeEvent>> {
    let rows = sqlx::query_as::<_, EventRow>(
        r#"
        SELECT id, node_id, event_type, event_data, timestamp
        FROM events
        WHERE node_id = ?
        ORDER BY id ASC
        "#,
    )
    .bind(node_id)
    .fetch_all(pool)
    .await?;

    rows.into_iter()
        .map(|r| r.try_into_node_event())
        .collect::<Result<Vec<_>>>()
}

// ============================================================================
// Graph Operations
// ============================================================================

/// Get graph data for a session.
pub async fn get_graph_data(pool: &SqlitePool, session_id: &str) -> Result<GraphData> {
    let session = get_session(pool, session_id).await?;
    let nodes = list_nodes_for_session(pool, session_id).await?;

    let graph_nodes: Vec<GraphNode> = nodes
        .iter()
        .map(|n| GraphNode {
            id: n.id.clone(),
            branch: n.branch.clone(),
            state: n.state,
            prompt: n.prompt.clone(),
            parent_id: n.parent_node_id.clone(),
            result_text: n.result.as_ref().and_then(|r| r.result_text.clone()),
            structured_output: n.result.as_ref().and_then(|r| r.structured_output.clone()),
            total_cost_usd: n.result.as_ref().map(|r| r.total_cost_usd),
            duration_secs: n.result.as_ref().map(|r| r.duration_secs),
        })
        .collect();

    let edges: Vec<GraphEdge> = nodes
        .iter()
        .filter_map(|n| {
            n.parent_node_id.as_ref().map(|parent| GraphEdge {
                source: parent.clone(),
                target: n.id.clone(),
            })
        })
        .collect();

    Ok(GraphData {
        session,
        nodes: graph_nodes,
        edges,
    })
}

// ============================================================================
// Internal Row Types
// ============================================================================

#[derive(sqlx::FromRow)]
struct SessionRow {
    id: String,
    name: String,
    created_at: String,
    updated_at: String,
    node_count: i64,
    running_count: i64,
    failed_count: i64,
}

impl SessionRow {
    fn into_session_info(self) -> SessionInfo {
        // Derive state from node states
        let state = if self.running_count > 0 {
            SessionState::Running
        } else if self.failed_count > 0 {
            SessionState::Failed
        } else {
            SessionState::Completed
        };

        SessionInfo {
            id: self.id,
            name: self.name,
            state,
            created_at: self.created_at,
            updated_at: self.updated_at,
            node_count: self.node_count,
        }
    }
}

#[derive(sqlx::FromRow)]
struct NodeRow {
    id: String,
    session_id: String,
    parent_node_id: Option<String>,
    branch: String,
    worktree: String,
    prompt: String,
    model: String,
    state: String,
    metadata: Option<String>,
    created_at: String,
    updated_at: String,
    // Result fields (nullable from LEFT JOIN)
    exit_code: Option<i32>,
    is_error: Option<i32>,
    result_text: Option<String>,
    structured_output: Option<String>,
    total_cost_usd: Option<f64>,
    num_turns: Option<i64>,
    cc_session_id: Option<String>,
    duration_secs: Option<f64>,
    model_usage: Option<String>,
}

impl NodeRow {
    fn into_node_info(self) -> NodeInfo {
        let result = if let (Some(exit_code), Some(is_error), Some(cc_session_id)) =
            (self.exit_code, self.is_error, self.cc_session_id.clone())
        {
            Some(NodeResult {
                node_id: self.id.clone(),
                exit_code,
                is_error: is_error != 0,
                result_text: self.result_text,
                structured_output: self.structured_output.and_then(|s| {
                    serde_json::from_str(&s).map_err(|e| {
                        tracing::warn!(
                            node_id = %self.id,
                            error = %e,
                            "Failed to parse structured_output JSON"
                        );
                        e
                    }).ok()
                }),
                total_cost_usd: self.total_cost_usd.unwrap_or(0.0),
                num_turns: self.num_turns.unwrap_or(0),
                cc_session_id,
                duration_secs: self.duration_secs.unwrap_or(0.0),
                model_usage: self.model_usage.and_then(|s| {
                    serde_json::from_str(&s).map_err(|e| {
                        tracing::warn!(
                            node_id = %self.id,
                            error = %e,
                            "Failed to parse model_usage JSON"
                        );
                        e
                    }).ok()
                }).unwrap_or_default(),
            })
        } else {
            None
        };

        // Parse metadata JSON if present
        let metadata = self.metadata.and_then(|s| {
            serde_json::from_str(&s).map_err(|e| {
                tracing::warn!(
                    node_id = %self.id,
                    error = %e,
                    "Failed to parse node metadata JSON"
                );
                e
            }).ok()
        });

        NodeInfo {
            id: self.id,
            session_id: self.session_id,
            parent_node_id: self.parent_node_id,
            branch: self.branch,
            worktree: self.worktree.into(),
            prompt: self.prompt,
            model: self.model,
            state: self.state.parse().unwrap_or(NodeState::Pending),
            metadata,
            created_at: self.created_at,
            updated_at: self.updated_at,
            result,
        }
    }
}

#[derive(sqlx::FromRow)]
struct EventRow {
    id: i64,
    node_id: String,
    event_type: String,
    event_data: String,
    timestamp: String,
}

impl EventRow {
    fn try_into_node_event(self) -> Result<NodeEvent> {
        let event: mantle_shared::events::StreamEvent =
            serde_json::from_str(&self.event_data).map_err(|e| {
                HubError::BadRequest(format!("Failed to parse event data: {}", e))
            })?;

        Ok(NodeEvent {
            id: self.id,
            node_id: self.node_id,
            event_type: self.event_type,
            event,
            timestamp: self.timestamp,
        })
    }
}
