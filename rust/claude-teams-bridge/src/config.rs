use crate::paths;
use serde::{Deserialize, Serialize};
use std::io;
use std::path::Path;
use tracing::info;

/// Claude Code team configuration (matches CC's config.json format).
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TeamConfig {
    pub name: String,
    pub description: String,
    pub created_at: u64,
    pub lead_agent_id: String,
    pub lead_session_id: String,
    pub members: Vec<TeamMember>,
}

/// A member of a Claude Code team.
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TeamMember {
    pub agent_id: String,
    pub name: String,
    pub agent_type: String,
    pub model: String,
    pub joined_at: u64,
    pub cwd: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub backend_type: Option<String>,
}

/// Read team config from `~/.claude/teams/{team}/config.json`.
pub fn read_team_config(team: &str) -> io::Result<TeamConfig> {
    let path = paths::config_path(team)
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "HOME directory not found"))?;
    let content = std::fs::read_to_string(&path)?;
    serde_json::from_str(&content)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

/// Write team config atomically.
pub fn write_team_config(team: &str, config: &TeamConfig) -> io::Result<()> {
    let path = paths::config_path(team)
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "HOME directory not found"))?;
    write_team_config_at(&path, config)
}

fn write_team_config_at(path: &Path, config: &TeamConfig) -> io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let json = serde_json::to_string_pretty(config)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;
    let tmp = path.with_extension("json.tmp");
    std::fs::write(&tmp, &json)?;
    std::fs::rename(&tmp, path)?;
    info!(path = %path.display(), "Wrote team config");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn sample_config() -> TeamConfig {
        TeamConfig {
            name: "test-team".into(),
            description: "A test team".into(),
            created_at: 1700000000,
            lead_agent_id: "lead-1".into(),
            lead_session_id: "session-1".into(),
            members: vec![TeamMember {
                agent_id: "agent-1".into(),
                name: "worker".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1700000001,
                cwd: "/tmp".into(),
                backend_type: None,
            }],
        }
    }

    #[test]
    fn test_roundtrip() {
        let tmp = tempdir().unwrap();
        let path = tmp.path().join("config.json");
        let config = sample_config();
        write_team_config_at(&path, &config).unwrap();

        let content = std::fs::read_to_string(&path).unwrap();
        let loaded: TeamConfig = serde_json::from_str(&content).unwrap();
        assert_eq!(loaded.name, "test-team");
        assert_eq!(loaded.members.len(), 1);
        assert_eq!(loaded.members[0].name, "worker");
    }

    #[test]
    fn test_camel_case_serialization() {
        let config = sample_config();
        let json = serde_json::to_string(&config).unwrap();
        assert!(json.contains("createdAt"));
        assert!(json.contains("leadAgentId"));
        assert!(json.contains("leadSessionId"));
        assert!(json.contains("agentType"));
        assert!(json.contains("joinedAt"));
        assert!(!json.contains("created_at"));
    }
}
