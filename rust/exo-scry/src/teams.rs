//! Read team identity and membership from the on-disk Teams substrate.
//!
//! This is the contract-based, fully-portable layer: it reads the fields Claude
//! Code commits to `~/.claude/teams/{team}/config.json` — `name`, `leadAgentId`,
//! `leadSessionId`, and `members[]` (each with `name`, `agentId`, `agentType`,
//! `model`, `cwd`).

use crate::error::{Result, ScryError};
use serde::{Deserialize, Serialize};
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

/// `~/.claude/teams` — the per-team config dirs.
pub fn teams_root() -> Result<PathBuf> {
    Ok(claude_home()?.join("teams"))
}

/// `~/.claude/projects` — per-cwd dirs holding session transcripts.
pub fn projects_root() -> Result<PathBuf> {
    Ok(claude_home()?.join("projects"))
}

/// One member of a team.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Teammate {
    pub agent_id: String,
    pub name: String,
    #[serde(default)]
    pub agent_type: String,
    #[serde(default)]
    pub model: String,
    #[serde(default)]
    pub cwd: String,
    #[serde(default)]
    pub tmux_pane_id: String,
    #[serde(default)]
    pub backend_type: String,
    /// Claude Code's own per-member liveness flag. Absent for the human lead.
    #[serde(default)]
    pub is_active: Option<bool>,
}

/// A team's full identity and membership.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Team {
    pub name: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub lead_agent_id: String,
    #[serde(default)]
    lead_session_id: String,
    #[serde(default)]
    pub members: Vec<Teammate>,
}

impl Team {
    /// The lead member's inbox name (the member whose `agentId == leadAgentId`).
    pub fn lead_inbox(&self) -> Option<&str> {
        self.members
            .iter()
            .find(|m| m.agent_id == self.lead_agent_id)
            .map(|m| m.name.as_str())
    }

    /// The lead session UUID, or `None` if unset.
    pub fn lead_session_id(&self) -> Option<&str> {
        (!self.lead_session_id.is_empty()).then_some(self.lead_session_id.as_str())
    }

    /// Look up a member by name.
    pub fn member(&self, name: &str) -> Option<&Teammate> {
        self.members.iter().find(|m| m.name == name)
    }
}

/// Read and parse `~/.claude/teams/{team}/config.json`.
pub fn load_team(team: &str) -> Result<Team> {
    load_team_at(&teams_root()?.join(team).join("config.json"))
}

fn load_team_at(path: &Path) -> Result<Team> {
    let bytes = std::fs::read(path).map_err(|source| ScryError::TeamConfigRead {
        path: path.to_path_buf(),
        source,
    })?;
    serde_json::from_slice(&bytes).map_err(|source| ScryError::TeamConfigParse {
        path: path.to_path_buf(),
        source,
    })
}

/// Find the team whose `leadSessionId` matches `session_id`, by scanning team
/// configs. Fully portable — no `/proc`, no inotify, just the persisted field
/// Claude Code commits at team creation. CC enforces one team per leader, so at
/// most one live config matches (the mapping is 1:1).
pub fn find_team_by_session(session_id: &str) -> Result<Option<String>> {
    find_team_by_session_in(&teams_root()?, session_id)
}

/// Find the `(team, member)` whose member `tmuxPaneId` equals `pane`.
///
/// The durable, unambiguous self-key for a tmux-backed teammate: pane ids are
/// unique per session (so they disambiguate multiple sessions in one cwd) and
/// survive session-id churn (unlike the spawn-frozen `CLAUDE_CODE_SESSION_ID`).
/// The human lead is *not* found here — its member entry has no pane; resolve it
/// via the live watch instead.
pub fn find_member_by_pane(pane: &str) -> Result<Option<(Team, Teammate)>> {
    find_member_by_pane_in(&teams_root()?, pane)
}

fn find_member_by_pane_in(teams_root: &Path, pane: &str) -> Result<Option<(Team, Teammate)>> {
    if pane.is_empty() {
        return Ok(None);
    }
    let entries = match std::fs::read_dir(teams_root) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(ScryError::Io(e)),
    };
    for entry in entries {
        let Ok(entry) = entry else { continue };
        let Ok(team) = load_team_at(&entry.path().join("config.json")) else {
            continue;
        };
        if let Some(m) = team
            .members
            .iter()
            .find(|m| !m.tmux_pane_id.is_empty() && m.tmux_pane_id == pane)
        {
            let me = m.clone();
            return Ok(Some((team, me)));
        }
    }
    Ok(None)
}

fn find_team_by_session_in(teams_root: &Path, session_id: &str) -> Result<Option<String>> {
    let entries = match std::fs::read_dir(teams_root) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(ScryError::Io(e)),
    };
    for entry in entries {
        let Ok(entry) = entry else { continue };
        let cfg = entry.path().join("config.json");
        // A malformed/half-written sibling config must not abort the scan.
        let Ok(team) = load_team_at(&cfg) else { continue };
        if team.lead_session_id() == Some(session_id) {
            if let Some(name) = entry.file_name().to_str() {
                return Ok(Some(name.to_string()));
            }
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_team(root: &Path, name: &str, sid: &str) {
        let dir = root.join(name);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("config.json"),
            format!(
                r#"{{"name":"{name}","leadSessionId":"{sid}","leadAgentId":"lead@{name}",
                    "members":[{{"agentId":"lead@{name}","name":"team-lead","agentType":"team-lead","cwd":"/x"}}]}}"#
            ),
        )
        .unwrap();
    }

    #[test]
    fn finds_team_by_lead_session() {
        let root = std::env::temp_dir().join(format!("exo-scry-teams-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        write_team(&root, "alpha", "uuid-aaa");
        write_team(&root, "beta", "uuid-bbb");
        assert_eq!(
            find_team_by_session_in(&root, "uuid-bbb").unwrap().as_deref(),
            Some("beta")
        );
        assert_eq!(find_team_by_session_in(&root, "uuid-zzz").unwrap(), None);
        std::fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn missing_teams_root_is_none() {
        let root = std::env::temp_dir().join("exo-scry-teams-nonexistent-xyzzy");
        let _ = std::fs::remove_dir_all(&root);
        assert_eq!(find_team_by_session_in(&root, "any").unwrap(), None);
    }

    #[test]
    fn finds_member_by_pane() {
        let root = std::env::temp_dir().join(format!("exo-scry-pane-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(root.join("alpha")).unwrap();
        std::fs::write(
            root.join("alpha").join("config.json"),
            r#"{"name":"alpha","leadAgentId":"lead@alpha","members":[
                {"agentId":"lead@alpha","name":"team-lead","tmuxPaneId":""},
                {"agentId":"scout@alpha","name":"scout","tmuxPaneId":"%312","isActive":false}
            ]}"#,
        )
        .unwrap();
        let (team, me) = find_member_by_pane_in(&root, "%312").unwrap().unwrap();
        assert_eq!(team.name, "alpha");
        assert_eq!(me.name, "scout");
        // Empty pane must never match the lead's empty tmuxPaneId.
        assert!(find_member_by_pane_in(&root, "").unwrap().is_none());
        assert!(find_member_by_pane_in(&root, "%999").unwrap().is_none());
        std::fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn parses_members_and_lead_inbox() {
        let root = std::env::temp_dir().join(format!("exo-scry-team2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        write_team(&root, "alpha", "uuid-aaa");
        let team = load_team_at(&root.join("alpha").join("config.json")).unwrap();
        assert_eq!(team.lead_inbox(), Some("team-lead"));
        assert_eq!(team.lead_session_id(), Some("uuid-aaa"));
        assert_eq!(team.members.len(), 1);
        assert_eq!(team.members[0].agent_type, "team-lead");
        std::fs::remove_dir_all(&root).unwrap();
    }
}
