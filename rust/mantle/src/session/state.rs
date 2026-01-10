//! State management for mantle sessions.
//!
//! Handles persistence of session metadata to `.mantle/sessions.json`
//! with file locking for concurrent access safety.

use fs2::FileExt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use super::types::{SessionMetadata, SessionState};

/// Error types for state operations.
#[derive(Debug, thiserror::Error)]
pub enum StateError {
    #[error("Failed to create .mantle directory: {0}")]
    CreateDir(#[source] std::io::Error),

    #[error("Failed to open state file: {0}")]
    OpenFile(#[source] std::io::Error),

    #[error("Failed to acquire file lock: {0}")]
    Lock(#[source] std::io::Error),

    #[error("Failed to read state file: {0}")]
    ReadFile(#[source] std::io::Error),

    #[error("Failed to parse state file: {0}")]
    Parse(#[source] serde_json::Error),

    #[error("Failed to serialize state: {0}")]
    Serialize(#[source] serde_json::Error),

    #[error("Failed to write state file: {0}")]
    WriteFile(#[source] std::io::Error),

    #[error("Failed to rename temp file: {0}")]
    Rename(#[source] std::io::Error),

    #[error("Session not found: {0}")]
    NotFound(String),

    #[error("Branch not found: {0}")]
    BranchNotFound(String),

    #[error("Invalid state file format")]
    InvalidFormat,
}

pub type Result<T> = std::result::Result<T, StateError>;

/// Persisted state file format.
///
/// Stored at `.mantle/sessions.json` relative to repository root.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StateFile {
    /// All sessions indexed by ID.
    pub sessions: HashMap<String, SessionMetadata>,

    /// Reverse index: branch name → session ID.
    /// Enables quick lookup when resuming by branch.
    #[serde(default)]
    pub branch_to_session: HashMap<String, String>,

    /// Reverse index: slug → session IDs (multiple sessions can share a slug base).
    #[serde(default)]
    pub slug_to_sessions: HashMap<String, Vec<String>>,
}

impl StateFile {
    /// Create empty state.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a session, updating all indices.
    pub fn insert(&mut self, session: SessionMetadata) {
        let id = session.id.clone();
        let branch = session.branch.clone();
        let slug = session.slug.clone();

        // Update branch index
        self.branch_to_session.insert(branch, id.clone());

        // Update slug index
        self.slug_to_sessions
            .entry(slug)
            .or_default()
            .push(id.clone());

        // Insert session
        self.sessions.insert(id, session);
    }

    /// Get a session by ID.
    pub fn get(&self, id: &str) -> Option<&SessionMetadata> {
        self.sessions.get(id)
    }

    /// Get a mutable reference to a session by ID.
    pub fn get_mut(&mut self, id: &str) -> Option<&mut SessionMetadata> {
        self.sessions.get_mut(id)
    }

    /// Get a session by branch name.
    pub fn get_by_branch(&self, branch: &str) -> Option<&SessionMetadata> {
        self.branch_to_session
            .get(branch)
            .and_then(|id| self.sessions.get(id))
    }

    /// Get all sessions with a given slug.
    pub fn get_by_slug(&self, slug: &str) -> Vec<&SessionMetadata> {
        self.slug_to_sessions
            .get(slug)
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| self.sessions.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Remove a session, updating all indices.
    pub fn remove(&mut self, id: &str) -> Option<SessionMetadata> {
        if let Some(session) = self.sessions.remove(id) {
            // Remove from branch index
            self.branch_to_session.remove(&session.branch);

            // Remove from slug index
            if let Some(ids) = self.slug_to_sessions.get_mut(&session.slug) {
                ids.retain(|s| s != id);
                if ids.is_empty() {
                    self.slug_to_sessions.remove(&session.slug);
                }
            }

            Some(session)
        } else {
            None
        }
    }

    /// List all sessions, optionally filtered by state.
    pub fn list(&self, state_filter: Option<SessionState>) -> Vec<&SessionMetadata> {
        self.sessions
            .values()
            .filter(|s| state_filter.map_or(true, |st| s.state == st))
            .collect()
    }

    /// Add a child ID to a parent session.
    pub fn add_child(&mut self, parent_id: &str, child_id: &str) -> Result<()> {
        let parent = self
            .sessions
            .get_mut(parent_id)
            .ok_or_else(|| StateError::NotFound(parent_id.to_string()))?;

        if !parent.child_ids.contains(&child_id.to_string()) {
            parent.child_ids.push(child_id.to_string());
        }

        Ok(())
    }
}

/// Manager for state file operations with file locking.
///
/// Provides safe concurrent access to `.mantle/sessions.json`.
pub struct StateManager {
    /// Path to the .mantle directory.
    mantle_dir: PathBuf,
    /// Path to sessions.json within .mantle.
    state_file_path: PathBuf,
}

impl StateManager {
    /// Create a new state manager for the given repository root.
    ///
    /// Creates `.mantle/` directory if it doesn't exist.
    pub fn new(repo_root: &Path) -> Result<Self> {
        let mantle_dir = repo_root.join(".mantle");
        let state_file_path = mantle_dir.join("sessions.json");

        // Ensure .mantle directory exists
        if !mantle_dir.exists() {
            fs::create_dir_all(&mantle_dir).map_err(StateError::CreateDir)?;
        }

        Ok(Self {
            mantle_dir,
            state_file_path,
        })
    }

    /// Path to the .mantle directory.
    pub fn mantle_dir(&self) -> &Path {
        &self.mantle_dir
    }

    /// Path to the worktrees directory.
    pub fn worktrees_dir(&self) -> PathBuf {
        self.mantle_dir.join("worktrees")
    }

    /// Load state file with shared (read) lock.
    pub fn load(&self) -> Result<StateFile> {
        if !self.state_file_path.exists() {
            return Ok(StateFile::new());
        }

        let file = File::open(&self.state_file_path).map_err(StateError::OpenFile)?;

        // Acquire shared lock for reading
        file.lock_shared().map_err(StateError::Lock)?;

        let mut contents = String::new();
        let mut reader = std::io::BufReader::new(&file);
        reader.read_to_string(&mut contents).map_err(StateError::ReadFile)?;

        // Lock is released when file is dropped
        drop(file);

        if contents.is_empty() {
            return Ok(StateFile::new());
        }

        serde_json::from_str(&contents).map_err(StateError::Parse)
    }

    /// Save state file with exclusive (write) lock.
    ///
    /// Uses atomic write pattern (temp file + rename) to prevent data loss
    /// if the process crashes mid-write.
    pub fn save(&self, state: &StateFile) -> Result<()> {
        // Ensure directory exists
        if !self.mantle_dir.exists() {
            fs::create_dir_all(&self.mantle_dir).map_err(StateError::CreateDir)?;
        }

        // Serialize with pretty printing for readability
        let contents = serde_json::to_string_pretty(state).map_err(StateError::Serialize)?;

        // Atomic write: write to temp file, sync, then rename
        let temp_path = self.state_file_path.with_extension("json.tmp");

        // Write to temp file
        {
            let mut temp_file = File::create(&temp_path).map_err(StateError::WriteFile)?;
            temp_file
                .write_all(contents.as_bytes())
                .map_err(StateError::WriteFile)?;
            // Sync to ensure data is on disk before rename
            temp_file.sync_all().map_err(StateError::WriteFile)?;
        }

        // Atomic rename (on POSIX systems)
        fs::rename(&temp_path, &self.state_file_path).map_err(StateError::Rename)?;

        Ok(())
    }

    /// Execute a read-modify-write operation with exclusive lock.
    ///
    /// This is the primary method for updating state. It:
    /// 1. Acquires exclusive lock on the state file
    /// 2. Reads current state
    /// 3. Applies the provided function
    /// 4. Writes updated state atomically (temp + rename)
    /// 5. Releases lock
    ///
    /// Uses atomic write pattern to prevent data loss on crash.
    pub fn with_state<F, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce(&mut StateFile) -> Result<T>,
    {
        // Ensure directory exists
        if !self.mantle_dir.exists() {
            fs::create_dir_all(&self.mantle_dir).map_err(StateError::CreateDir)?;
        }

        // Open file for read (we'll write atomically via temp file)
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&self.state_file_path)
            .map_err(StateError::OpenFile)?;

        // Acquire exclusive lock - this coordinates with other processes
        file.lock_exclusive().map_err(StateError::Lock)?;

        // Read current state
        let mut contents = String::new();
        let mut reader = std::io::BufReader::new(&file);
        reader.read_to_string(&mut contents).map_err(StateError::ReadFile)?;

        let mut state = if contents.is_empty() {
            StateFile::new()
        } else {
            serde_json::from_str(&contents).map_err(StateError::Parse)?
        };

        // Apply the modification
        let result = f(&mut state)?;

        // Serialize for atomic write
        let new_contents = serde_json::to_string_pretty(&state).map_err(StateError::Serialize)?;

        // Atomic write: temp file + rename
        // This ensures we never have a partially written state file
        let temp_path = self.state_file_path.with_extension("json.tmp");
        {
            let mut temp_file = File::create(&temp_path).map_err(StateError::WriteFile)?;
            temp_file
                .write_all(new_contents.as_bytes())
                .map_err(StateError::WriteFile)?;
            temp_file.sync_all().map_err(StateError::WriteFile)?;
        }

        // Atomic rename
        fs::rename(&temp_path, &self.state_file_path).map_err(StateError::Rename)?;

        // Lock is released when file is dropped
        Ok(result)
    }

    /// Get a session by ID.
    pub fn get_session(&self, id: &str) -> Result<SessionMetadata> {
        let state = self.load()?;
        state
            .get(id)
            .cloned()
            .ok_or_else(|| StateError::NotFound(id.to_string()))
    }

    /// Get a session by branch name.
    pub fn get_session_by_branch(&self, branch: &str) -> Result<SessionMetadata> {
        let state = self.load()?;
        state
            .get_by_branch(branch)
            .cloned()
            .ok_or_else(|| StateError::BranchNotFound(branch.to_string()))
    }

    /// Insert a new session.
    pub fn insert_session(&self, session: SessionMetadata) -> Result<()> {
        self.with_state(|state| {
            state.insert(session);
            Ok(())
        })
    }

    /// Update an existing session.
    pub fn update_session<F>(&self, id: &str, f: F) -> Result<SessionMetadata>
    where
        F: FnOnce(&mut SessionMetadata),
    {
        self.with_state(|state| {
            let session = state
                .get_mut(id)
                .ok_or_else(|| StateError::NotFound(id.to_string()))?;
            f(session);
            Ok(session.clone())
        })
    }

    /// Remove a session.
    pub fn remove_session(&self, id: &str) -> Result<SessionMetadata> {
        self.with_state(|state| {
            state
                .remove(id)
                .ok_or_else(|| StateError::NotFound(id.to_string()))
        })
    }

    /// List all sessions, optionally filtered by state.
    pub fn list_sessions(&self, state_filter: Option<SessionState>) -> Result<Vec<SessionMetadata>> {
        let state = self.load()?;
        Ok(state.list(state_filter).into_iter().cloned().collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use tempfile::TempDir;

    fn create_test_session(id: &str, slug: &str) -> SessionMetadata {
        SessionMetadata::new(
            id.to_string(),
            slug.to_string(),
            format!("{}-abc123", slug),
            PathBuf::from("/tmp/worktree"),
            "sonnet".to_string(),
        )
    }

    #[test]
    fn test_state_file_insert_and_get() {
        let mut state = StateFile::new();
        let session = create_test_session("sess-1", "implement/auth");

        state.insert(session.clone());

        assert!(state.get("sess-1").is_some());
        assert_eq!(state.get("sess-1").unwrap().slug, "implement/auth");
    }

    #[test]
    fn test_state_file_branch_index() {
        let mut state = StateFile::new();
        let session = create_test_session("sess-1", "implement/auth");

        state.insert(session);

        let by_branch = state.get_by_branch("implement/auth-abc123");
        assert!(by_branch.is_some());
        assert_eq!(by_branch.unwrap().id, "sess-1");
    }

    #[test]
    fn test_state_file_slug_index() {
        let mut state = StateFile::new();

        // Add multiple sessions with same slug base
        let session1 = create_test_session("sess-1", "implement/auth");
        let mut session2 = create_test_session("sess-2", "implement/auth");
        session2.branch = "implement/auth-def456".to_string();

        state.insert(session1);
        state.insert(session2);

        let by_slug = state.get_by_slug("implement/auth");
        assert_eq!(by_slug.len(), 2);
    }

    #[test]
    fn test_state_file_remove() {
        let mut state = StateFile::new();
        let session = create_test_session("sess-1", "implement/auth");

        state.insert(session);
        assert!(state.get("sess-1").is_some());

        let removed = state.remove("sess-1");
        assert!(removed.is_some());
        assert!(state.get("sess-1").is_none());
        assert!(state.get_by_branch("implement/auth-abc123").is_none());
    }

    #[test]
    fn test_state_file_list_filter() {
        let mut state = StateFile::new();

        let mut session1 = create_test_session("sess-1", "feat-a");
        session1.state = SessionState::Completed;

        let mut session2 = create_test_session("sess-2", "feat-b");
        session2.branch = "feat-b-abc123".to_string();
        session2.state = SessionState::Running;

        state.insert(session1);
        state.insert(session2);

        let all = state.list(None);
        assert_eq!(all.len(), 2);

        let completed = state.list(Some(SessionState::Completed));
        assert_eq!(completed.len(), 1);
        assert_eq!(completed[0].id, "sess-1");

        let running = state.list(Some(SessionState::Running));
        assert_eq!(running.len(), 1);
        assert_eq!(running[0].id, "sess-2");
    }

    #[test]
    fn test_state_manager_persistence() {
        let temp_dir = TempDir::new().unwrap();
        let manager = StateManager::new(temp_dir.path()).unwrap();

        // Insert a session
        let session = create_test_session("sess-1", "test/persist");
        manager.insert_session(session).unwrap();

        // Load and verify
        let loaded = manager.get_session("sess-1").unwrap();
        assert_eq!(loaded.slug, "test/persist");

        // Create new manager instance (simulates new process)
        let manager2 = StateManager::new(temp_dir.path()).unwrap();
        let loaded2 = manager2.get_session("sess-1").unwrap();
        assert_eq!(loaded2.slug, "test/persist");
    }

    #[test]
    fn test_state_manager_update() {
        let temp_dir = TempDir::new().unwrap();
        let manager = StateManager::new(temp_dir.path()).unwrap();

        let session = create_test_session("sess-1", "test/update");
        manager.insert_session(session).unwrap();

        // Update session
        manager
            .update_session("sess-1", |s| {
                s.mark_running(Some("container-123".to_string()));
            })
            .unwrap();

        let updated = manager.get_session("sess-1").unwrap();
        assert_eq!(updated.state, SessionState::Running);
        assert_eq!(updated.container_id, Some("container-123".to_string()));
    }

    #[test]
    fn test_state_manager_not_found() {
        let temp_dir = TempDir::new().unwrap();
        let manager = StateManager::new(temp_dir.path()).unwrap();

        let result = manager.get_session("nonexistent");
        assert!(matches!(result, Err(StateError::NotFound(_))));
    }

    #[test]
    fn test_state_file_serialization_roundtrip() {
        let mut state = StateFile::new();
        let session = create_test_session("sess-1", "implement/auth");
        state.insert(session);

        let json = serde_json::to_string_pretty(&state).unwrap();
        let loaded: StateFile = serde_json::from_str(&json).unwrap();

        assert!(loaded.get("sess-1").is_some());
        assert!(loaded.get_by_branch("implement/auth-abc123").is_some());
    }

    #[test]
    fn test_state_manager_creates_mantle_dir() {
        let temp_dir = TempDir::new().unwrap();
        let mantle_path = temp_dir.path().join(".mantle");

        assert!(!mantle_path.exists());

        let _manager = StateManager::new(temp_dir.path()).unwrap();

        assert!(mantle_path.exists());
    }
}
