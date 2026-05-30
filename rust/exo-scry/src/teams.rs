//! Resolve a team's identity from its on-disk `config.json`. This is the
//! contract-based, fully-portable layer — it reads fields Claude Code commits
//! to (`name`, `leadAgentId`, `leadSessionId`, `members`).

use crate::error::{Result, ScryError};
use serde::Deserialize;
use std::path::{Path, PathBuf};

/// `~/.claude` for the current user.
pub fn claude_home() -> Result<PathBuf> {
    std::env::var_os("HOME")
        .map(|h| PathBuf::from(h).join(".claude"))
        .ok_or(ScryError::HomeUnknown)
}

/// `~/.claude/tasks` — the per-team dirs Claude Code inotify-watches.
pub fn tasks_root() -> Result<PathBuf> {
    Ok(claude_home()?.join("tasks"))
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawConfig {
    #[serde(default)]
    lead_agent_id: String,
    #[serde(default)]
    lead_session_id: String,
    #[serde(default)]
    members: Vec<RawMember>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawMember {
    #[serde(default)]
    name: String,
    #[serde(default)]
    agent_id: String,
}

/// The routing-relevant bits of a team config.
pub struct TeamConfig {
    /// The lead member's inbox name — where `notify_parent`-to-root must write.
    pub lead_inbox: Option<String>,
    /// CC-assigned lead session UUID — the globally-unique team handle.
    pub lead_session_id: Option<String>,
}

/// Read and parse `~/.claude/teams/{team}/config.json`.
pub fn load_team_config(team: &str) -> Result<TeamConfig> {
    let path = claude_home()?.join("teams").join(team).join("config.json");
    load_team_config_at(&path)
}

fn load_team_config_at(path: &Path) -> Result<TeamConfig> {
    let bytes = std::fs::read(path).map_err(|source| ScryError::TeamConfigRead {
        path: path.to_path_buf(),
        source,
    })?;
    let raw: RawConfig =
        serde_json::from_slice(&bytes).map_err(|source| ScryError::TeamConfigParse {
            path: path.to_path_buf(),
            source,
        })?;
    let lead_inbox = raw
        .members
        .iter()
        .find(|m| m.agent_id == raw.lead_agent_id)
        .map(|m| m.name.clone());
    Ok(TeamConfig {
        lead_inbox,
        lead_session_id: (!raw.lead_session_id.is_empty()).then_some(raw.lead_session_id),
    })
}
