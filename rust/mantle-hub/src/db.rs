//! SQLite database operations.

use sqlx::sqlite::{SqliteConnectOptions, SqlitePool, SqlitePoolOptions};
use std::path::Path;
use std::str::FromStr;

use crate::error::{HubError, Result};
use crate::types::{GraphData, GraphEdge, GraphNode, SessionInfo, SessionResult, SessionState};

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

    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS sessions (
            id TEXT PRIMARY KEY,
            branch TEXT NOT NULL,
            worktree TEXT NOT NULL,
            prompt TEXT NOT NULL,
            model TEXT NOT NULL,
            parent_id TEXT,
            state TEXT NOT NULL DEFAULT 'pending',
            created_at TEXT NOT NULL,
            updated_at TEXT NOT NULL
        )
        "#,
    )
    .execute(pool)
    .await?;

    // Results table without foreign key - allows results before session registration
    // This handles the race condition where container finishes before host registers
    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS results (
            session_id TEXT PRIMARY KEY,
            exit_code INTEGER NOT NULL,
            is_error INTEGER NOT NULL,
            result_text TEXT,
            structured_output TEXT,
            total_cost_usd REAL NOT NULL,
            num_turns INTEGER NOT NULL,
            cc_session_id TEXT NOT NULL,
            duration_secs REAL NOT NULL,
            model_usage TEXT,
            created_at TEXT NOT NULL
        )
        "#,
    )
    .execute(pool)
    .await?;

    sqlx::query("CREATE INDEX IF NOT EXISTS idx_sessions_parent ON sessions(parent_id)")
        .execute(pool)
        .await?;

    sqlx::query("CREATE INDEX IF NOT EXISTS idx_sessions_state ON sessions(state)")
        .execute(pool)
        .await?;

    Ok(())
}

/// Insert a new session.
///
/// Generates a UUID for the session and returns it.
pub async fn insert_session(
    pool: &SqlitePool,
    branch: &str,
    worktree: &str,
    prompt: &str,
    model: &str,
    parent_id: Option<&str>,
) -> Result<String> {
    let id = uuid::Uuid::new_v4().to_string();
    let now = chrono::Utc::now().to_rfc3339();

    sqlx::query(
        r#"
        INSERT INTO sessions (id, branch, worktree, prompt, model, parent_id, state, created_at, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, 'running', ?, ?)
        "#,
    )
    .bind(&id)
    .bind(branch)
    .bind(worktree)
    .bind(prompt)
    .bind(model)
    .bind(parent_id)
    .bind(&now)
    .bind(&now)
    .execute(pool)
    .await?;

    Ok(id)
}

/// Update session state.
pub async fn update_session_state(pool: &SqlitePool, id: &str, state: SessionState) -> Result<()> {
    let now = chrono::Utc::now().to_rfc3339();

    let result = sqlx::query("UPDATE sessions SET state = ?, updated_at = ? WHERE id = ?")
        .bind(state.to_string())
        .bind(&now)
        .bind(id)
        .execute(pool)
        .await?;

    if result.rows_affected() == 0 {
        return Err(HubError::SessionNotFound(id.to_string()));
    }

    Ok(())
}

/// Insert session result.
///
/// Results can be inserted even if the session doesn't exist yet (race condition handling).
/// If the session exists, its state will be updated. If not, the result is stored
/// and the session state will be updated when it's registered (via get_session JOIN).
pub async fn insert_result(pool: &SqlitePool, result: &SessionResult) -> Result<()> {
    let now = chrono::Utc::now().to_rfc3339();
    let structured_output = result
        .structured_output
        .as_ref()
        .map(|v| serde_json::to_string(v).unwrap_or_default());
    let model_usage = serde_json::to_string(&result.model_usage).unwrap_or_default();

    sqlx::query(
        r#"
        INSERT OR REPLACE INTO results
        (session_id, exit_code, is_error, result_text, structured_output, total_cost_usd, num_turns, cc_session_id, duration_secs, model_usage, created_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        "#,
    )
    .bind(&result.session_id)
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

    // Update session state IF session exists (ignore error if it doesn't)
    // This handles the race condition where result arrives before session registration
    let state = if result.is_error {
        SessionState::Failed
    } else {
        SessionState::Completed
    };
    let _ = update_session_state(pool, &result.session_id, state).await;

    Ok(())
}

/// Get session by ID.
pub async fn get_session(pool: &SqlitePool, id: &str) -> Result<SessionInfo> {
    let row = sqlx::query_as::<_, SessionRow>(
        r#"
        SELECT s.id, s.branch, s.worktree, s.prompt, s.model, s.parent_id, s.state, s.created_at, s.updated_at,
               r.exit_code, r.is_error, r.result_text, r.structured_output, r.total_cost_usd, r.num_turns, r.cc_session_id, r.duration_secs, r.model_usage
        FROM sessions s
        LEFT JOIN results r ON s.id = r.session_id
        WHERE s.id = ?
        "#,
    )
    .bind(id)
    .fetch_optional(pool)
    .await?
    .ok_or_else(|| HubError::SessionNotFound(id.to_string()))?;

    Ok(row.into_session_info())
}

/// List all sessions.
pub async fn list_sessions(pool: &SqlitePool) -> Result<Vec<SessionInfo>> {
    let rows = sqlx::query_as::<_, SessionRow>(
        r#"
        SELECT s.id, s.branch, s.worktree, s.prompt, s.model, s.parent_id, s.state, s.created_at, s.updated_at,
               r.exit_code, r.is_error, r.result_text, r.structured_output, r.total_cost_usd, r.num_turns, r.cc_session_id, r.duration_secs, r.model_usage
        FROM sessions s
        LEFT JOIN results r ON s.id = r.session_id
        ORDER BY s.created_at DESC
        "#,
    )
    .fetch_all(pool)
    .await?;

    Ok(rows.into_iter().map(|r| r.into_session_info()).collect())
}

/// Delete session.
pub async fn delete_session(pool: &SqlitePool, id: &str) -> Result<()> {
    // Delete result first (foreign key constraint)
    sqlx::query("DELETE FROM results WHERE session_id = ?")
        .bind(id)
        .execute(pool)
        .await?;

    let result = sqlx::query("DELETE FROM sessions WHERE id = ?")
        .bind(id)
        .execute(pool)
        .await?;

    if result.rows_affected() == 0 {
        return Err(HubError::SessionNotFound(id.to_string()));
    }

    Ok(())
}

/// Get graph data for visualization.
pub async fn get_graph_data(pool: &SqlitePool) -> Result<GraphData> {
    let sessions = list_sessions(pool).await?;

    let nodes: Vec<GraphNode> = sessions
        .iter()
        .map(|s| GraphNode {
            id: s.id.clone(),
            branch: s.branch.clone(),
            state: s.state,
            prompt: s.prompt.clone(),
            result_text: s.result.as_ref().and_then(|r| r.result_text.clone()),
            structured_output: s.result.as_ref().and_then(|r| r.structured_output.clone()),
            total_cost_usd: s.result.as_ref().map(|r| r.total_cost_usd),
            duration_secs: s.result.as_ref().map(|r| r.duration_secs),
        })
        .collect();

    let edges: Vec<GraphEdge> = sessions
        .iter()
        .filter_map(|s| {
            s.parent_id.as_ref().map(|parent| GraphEdge {
                source: parent.clone(),
                target: s.id.clone(),
            })
        })
        .collect();

    Ok(GraphData { nodes, edges })
}

// ============================================================================
// Internal Row Types
// ============================================================================

#[derive(sqlx::FromRow)]
struct SessionRow {
    id: String,
    branch: String,
    worktree: String,
    prompt: String,
    model: String,
    parent_id: Option<String>,
    state: String,
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

impl SessionRow {
    fn into_session_info(self) -> SessionInfo {
        let result = if let (Some(exit_code), Some(is_error), Some(cc_session_id)) =
            (self.exit_code, self.is_error, self.cc_session_id.clone())
        {
            Some(SessionResult {
                session_id: self.id.clone(),
                exit_code,
                is_error: is_error != 0,
                result_text: self.result_text,
                structured_output: self
                    .structured_output
                    .and_then(|s| serde_json::from_str(&s).ok()),
                total_cost_usd: self.total_cost_usd.unwrap_or(0.0),
                num_turns: self.num_turns.unwrap_or(0),
                cc_session_id,
                duration_secs: self.duration_secs.unwrap_or(0.0),
                model_usage: self
                    .model_usage
                    .and_then(|s| serde_json::from_str(&s).ok())
                    .unwrap_or_default(),
            })
        } else {
            None
        };

        SessionInfo {
            id: self.id,
            branch: self.branch,
            worktree: self.worktree.into(),
            prompt: self.prompt,
            model: self.model,
            state: self.state.parse().unwrap_or(SessionState::Pending),
            parent_id: self.parent_id,
            created_at: self.created_at,
            updated_at: self.updated_at,
            result,
        }
    }
}
