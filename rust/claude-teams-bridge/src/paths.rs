use std::path::{Path, PathBuf};

/// Claude Code Teams base directory (~/.claude/teams/).
pub fn teams_base_dir() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".claude").join("teams"))
}

/// Team directory path.
pub fn team_dir(team: &str) -> Option<PathBuf> {
    teams_base_dir().map(|b| b.join(team))
}

/// Team config.json path.
pub fn config_path(team: &str) -> Option<PathBuf> {
    team_dir(team).map(|d| d.join("config.json"))
}

/// Inboxes directory for a team.
pub fn inbox_dir(team: &str) -> Option<PathBuf> {
    team_dir(team).map(|d| d.join("inboxes"))
}

/// Inbox file path for a specific recipient.
pub fn inbox_path(team: &str, recipient: &str) -> Option<PathBuf> {
    inbox_dir(team).map(|d| d.join(format!("{}.json", recipient)))
}

// --- Internal helpers for testing with custom base paths ---

#[allow(dead_code)]
pub(crate) fn inbox_dir_at(base: &Path, team: &str) -> PathBuf {
    base.join(".claude").join("teams").join(team).join("inboxes")
}

#[allow(dead_code)]
pub(crate) fn inbox_path_at(base: &Path, team: &str, recipient: &str) -> PathBuf {
    inbox_dir_at(base, team).join(format!("{}.json", recipient))
}

#[allow(dead_code)]
pub(crate) fn config_path_at(base: &Path, team: &str) -> PathBuf {
    base.join(".claude").join("teams").join(team).join("config.json")
}

#[allow(dead_code)]
pub(crate) fn teams_base_dir_at(base: &Path) -> PathBuf {
    base.join(".claude").join("teams")
}
