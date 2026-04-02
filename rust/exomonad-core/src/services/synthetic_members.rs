use crate::domain::{AgentName, TeamName};
use anyhow::{Context, Result};
use claude_teams_bridge::file_lock::{fsync_dir, FileLock};
use serde_json::Value;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;
use tracing::{info, warn};

/// Register a synthetic member in a Claude Teams config.json.
///
/// Reads the existing config, appends to the members array, writes back atomically.
pub fn register_synthetic_member(
    team_name: &TeamName,
    member_name: &AgentName,
    agent_type: &str,
) -> Result<()> {
    let config_path = team_config_path(team_name)?;
    register_synthetic_member_at_path(&config_path, team_name, member_name.as_str(), agent_type)
}

fn register_synthetic_member_at_path(
    config_path: &PathBuf,
    team_name: &TeamName,
    member_name: &str,
    agent_type: &str,
) -> Result<()> {
    // Read existing config
    let _lock = FileLock::acquire(config_path, Duration::from_secs(30)).with_context(|| {
        format!(
            "Failed to acquire lock on team config: {}",
            config_path.display()
        )
    })?;
    let content = std::fs::read_to_string(config_path)
        .with_context(|| format!("Failed to read team config: {}", config_path.display()))?;
    let mut config: Value =
        serde_json::from_str(&content).context("Failed to parse team config")?;

    let members = config
        .get_mut("members")
        .and_then(|m| m.as_array_mut())
        .ok_or_else(|| anyhow::anyhow!("No members array in team config"))?;

    // Check if member already exists
    if members
        .iter()
        .any(|m| m.get("name").and_then(|n| n.as_str()) == Some(member_name))
    {
        info!(member = %member_name, "Synthetic member already registered");
        return Ok(());
    }

    let now = chrono::Utc::now().timestamp_millis();
    let agent_id = format!("{}@{}", member_name, team_name);

    let entry = serde_json::json!({
        "agentId": agent_id,
        "name": member_name,
        "agentType": agent_type,
        "model": "gemini",
        "color": "green",
        "planModeRequired": false,
        "joinedAt": now,
        "tmuxPaneId": "synthetic",
        "cwd": std::env::current_dir().unwrap_or_default(),
        "subscriptions": [],
        "backendType": "exomonad"
    });

    members.push(entry);

    // Atomic write: fsync temp file before persist for crash safety
    let content = serde_json::to_string_pretty(&config)?;
    let tmp_dir = config_path.parent().ok_or_else(|| {
        anyhow::anyhow!("No parent dir for config path: {}", config_path.display())
    })?;
    let tmp = tempfile::NamedTempFile::new_in(tmp_dir)?;
    {
        let mut writer = std::io::BufWriter::new(tmp.as_file());
        writer.write_all(content.as_bytes())?;
        writer.flush()?;
        tmp.as_file().sync_all()?;
    }
    tmp.persist(config_path)?;

    if let Some(parent) = config_path.parent() {
        if let Err(e) = fsync_dir(parent) {
            warn!(error = %e, "fsync on config dir failed");
        }
    }

    info!(team = %team_name, member = %member_name, "Synthetic member registered");
    Ok(())
}

/// Remove a synthetic member from a Claude Teams config.json.
pub fn remove_synthetic_member(team_name: &TeamName, member_name: &AgentName) -> Result<()> {
    let config_path = team_config_path(team_name)?;
    remove_synthetic_member_at_path(&config_path, team_name, member_name.as_str())
}

fn remove_synthetic_member_at_path(
    config_path: &PathBuf,
    team_name: &TeamName,
    member_name: &str,
) -> Result<()> {
    let _lock = FileLock::acquire(config_path, Duration::from_secs(30)).with_context(|| {
        format!(
            "Failed to acquire lock on team config: {}",
            config_path.display()
        )
    })?;
    let content = std::fs::read_to_string(config_path)
        .with_context(|| format!("Failed to read team config: {}", config_path.display()))?;
    let mut config: Value =
        serde_json::from_str(&content).context("Failed to parse team config")?;

    if let Some(members) = config.get_mut("members").and_then(|m| m.as_array_mut()) {
        members.retain(|m| m.get("name").and_then(|n| n.as_str()) != Some(member_name));
    }

    // Atomic write: fsync temp file before persist for crash safety
    let content = serde_json::to_string_pretty(&config)?;
    let tmp_dir = config_path.parent().ok_or_else(|| {
        anyhow::anyhow!("No parent dir for config path: {}", config_path.display())
    })?;
    let tmp = tempfile::NamedTempFile::new_in(tmp_dir)?;
    {
        let mut writer = std::io::BufWriter::new(tmp.as_file());
        writer.write_all(content.as_bytes())?;
        writer.flush()?;
        tmp.as_file().sync_all()?;
    }
    tmp.persist(config_path)?;

    if let Some(parent) = config_path.parent() {
        if let Err(e) = fsync_dir(parent) {
            warn!(error = %e, "fsync on config dir failed");
        }
    }

    info!(team = %team_name, member = %member_name, "Synthetic member removed");
    Ok(())
}

/// Remove all synthetic (exomonad-managed) members from a Claude Teams config.json.
///
/// Retains CC-native members (those without `"backendType": "exomonad"`).
/// Used during team teardown to prevent ghost members from blocking TeamDelete.
pub fn remove_all_synthetic_members(team_name: &TeamName) -> Result<usize> {
    let config_path = team_config_path(team_name)?;
    remove_all_synthetic_members_at_path(&config_path, team_name)
}

fn remove_all_synthetic_members_at_path(
    config_path: &PathBuf,
    team_name: &TeamName,
) -> Result<usize> {
    if !config_path.exists() {
        info!(team = %team_name, "Team config not found, nothing to clean");
        return Ok(0);
    }

    let _lock = FileLock::acquire(config_path, Duration::from_secs(30)).with_context(|| {
        format!(
            "Failed to acquire lock on team config: {}",
            config_path.display()
        )
    })?;
    let content = std::fs::read_to_string(config_path)
        .with_context(|| format!("Failed to read team config: {}", config_path.display()))?;
    let mut config: Value =
        serde_json::from_str(&content).context("Failed to parse team config")?;

    let removed = if let Some(members) = config.get_mut("members").and_then(|m| m.as_array_mut()) {
        let before = members.len();
        members.retain(|m| m.get("backendType").and_then(|b| b.as_str()) != Some("exomonad"));
        before - members.len()
    } else {
        0
    };

    if removed > 0 {
        let content = serde_json::to_string_pretty(&config)?;
        let tmp_dir = config_path.parent().ok_or_else(|| {
            anyhow::anyhow!("No parent dir for config path: {}", config_path.display())
        })?;
        let tmp = tempfile::NamedTempFile::new_in(tmp_dir)?;
        {
            let mut writer = std::io::BufWriter::new(tmp.as_file());
            writer.write_all(content.as_bytes())?;
            writer.flush()?;
            tmp.as_file().sync_all()?;
        }
        tmp.persist(config_path)?;

        if let Some(parent) = config_path.parent() {
            if let Err(e) = fsync_dir(parent) {
                warn!(error = %e, "fsync on config dir failed");
            }
        }
    }

    info!(team = %team_name, removed, "Removed all synthetic members");
    Ok(removed)
}

fn team_config_path(team_name: &TeamName) -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| anyhow::anyhow!("HOME directory not found"))?;
    Ok(home
        .join(".claude")
        .join("teams")
        .join(team_name.as_str())
        .join("config.json"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_synthetic_member_lifecycle() -> Result<()> {
        let dir = tempdir()?;
        let team_name = TeamName::from("test-team");
        let config_path = dir.path().join("config.json");

        let initial_config = serde_json::json!({
            "members": [
                {
                    "agentId": "real@test-team",
                    "name": "real",
                    "agentType": "human"
                }
            ]
        });
        fs::write(&config_path, serde_json::to_string_pretty(&initial_config)?)?;

        // Register new member
        register_synthetic_member_at_path(&config_path, &team_name, "gemini-1", "gemini-worker")?;

        let content = fs::read_to_string(&config_path)?;
        let config: Value = serde_json::from_str(&content)?;
        let members = config["members"].as_array().unwrap();
        assert_eq!(members.len(), 2);
        assert!(members.iter().any(|m| m["name"] == "gemini-1"));

        // Idempotent registration
        register_synthetic_member_at_path(&config_path, &team_name, "gemini-1", "gemini-worker")?;
        let content = fs::read_to_string(&config_path)?;
        let config: Value = serde_json::from_str(&content)?;
        assert_eq!(config["members"].as_array().unwrap().len(), 2);

        // Remove member
        remove_synthetic_member_at_path(&config_path, &team_name, "gemini-1")?;
        let content = fs::read_to_string(&config_path)?;
        let config: Value = serde_json::from_str(&content)?;
        let members = config["members"].as_array().unwrap();
        assert_eq!(members.len(), 1);
        assert!(!members.iter().any(|m| m["name"] == "gemini-1"));

        Ok(())
    }

    #[test]
    fn test_remove_all_synthetic_members() -> Result<()> {
        let dir = tempdir()?;
        let team_name = TeamName::from("test-team");
        let config_path = dir.path().join("config.json");

        let initial_config = serde_json::json!({
            "members": [
                {
                    "agentId": "lead@test-team",
                    "name": "lead",
                    "agentType": "human"
                },
                {
                    "agentId": "gemini-1@test-team",
                    "name": "gemini-1",
                    "agentType": "gemini-worker",
                    "backendType": "exomonad"
                },
                {
                    "agentId": "gemini-2@test-team",
                    "name": "gemini-2",
                    "agentType": "gemini-worker",
                    "backendType": "exomonad"
                },
                {
                    "agentId": "claude-sub@test-team",
                    "name": "claude-sub",
                    "agentType": "claude",
                    "backendType": "exomonad"
                }
            ]
        });
        fs::write(&config_path, serde_json::to_string_pretty(&initial_config)?)?;

        let removed = remove_all_synthetic_members_at_path(&config_path, &team_name)?;
        assert_eq!(removed, 3);

        let content = fs::read_to_string(&config_path)?;
        let config: Value = serde_json::from_str(&content)?;
        let members = config["members"].as_array().unwrap();
        assert_eq!(members.len(), 1);
        assert_eq!(members[0]["name"], "lead");

        // Idempotent — no synthetic members left
        let removed = remove_all_synthetic_members_at_path(&config_path, &team_name)?;
        assert_eq!(removed, 0);

        Ok(())
    }

    #[test]
    fn test_remove_all_synthetic_members_missing_config() -> Result<()> {
        let team_name = TeamName::from("nonexistent-team");
        let config_path = PathBuf::from("/tmp/nonexistent-config.json");
        let removed = remove_all_synthetic_members_at_path(&config_path, &team_name)?;
        assert_eq!(removed, 0);
        Ok(())
    }
}
