use anyhow::{Context, Result};
use serde_json::Value;
use std::path::PathBuf;
use tracing::info;

/// Register a synthetic member in a Claude Teams config.json.
///
/// Reads the existing config, appends to the members array, writes back atomically.
pub fn register_synthetic_member(
    team_name: &str,
    member_name: &str,
    agent_type: &str,
) -> Result<()> {
    let config_path = team_config_path(team_name)?;
    register_synthetic_member_at_path(&config_path, team_name, member_name, agent_type)
}

fn register_synthetic_member_at_path(
    config_path: &PathBuf,
    team_name: &str,
    member_name: &str,
    agent_type: &str,
) -> Result<()> {
    // Read existing config
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

    // Atomic write
    let tmp = config_path.with_extension("json.tmp");
    let content = serde_json::to_string_pretty(&config)?;
    std::fs::write(&tmp, &content)?;
    std::fs::rename(&tmp, config_path)?;

    info!(team = %team_name, member = %member_name, "Synthetic member registered");
    Ok(())
}

/// Remove a synthetic member from a Claude Teams config.json.
pub fn remove_synthetic_member(team_name: &str, member_name: &str) -> Result<()> {
    let config_path = team_config_path(team_name)?;
    remove_synthetic_member_at_path(&config_path, team_name, member_name)
}

fn remove_synthetic_member_at_path(
    config_path: &PathBuf,
    team_name: &str,
    member_name: &str,
) -> Result<()> {
    let content = std::fs::read_to_string(config_path)
        .with_context(|| format!("Failed to read team config: {}", config_path.display()))?;
    let mut config: Value =
        serde_json::from_str(&content).context("Failed to parse team config")?;

    if let Some(members) = config.get_mut("members").and_then(|m| m.as_array_mut()) {
        members.retain(|m| m.get("name").and_then(|n| n.as_str()) != Some(member_name));
    }

    let tmp = config_path.with_extension("json.tmp");
    let content = serde_json::to_string_pretty(&config)?;
    std::fs::write(&tmp, &content)?;
    std::fs::rename(&tmp, config_path)?;

    info!(team = %team_name, member = %member_name, "Synthetic member removed");
    Ok(())
}

fn team_config_path(team_name: &str) -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| anyhow::anyhow!("HOME directory not found"))?;
    Ok(home
        .join(".claude")
        .join("teams")
        .join(team_name)
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
        let team_name = "test-team";
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
        register_synthetic_member_at_path(&config_path, team_name, "gemini-1", "gemini-worker")?;

        let content = fs::read_to_string(&config_path)?;
        let config: Value = serde_json::from_str(&content)?;
        let members = config["members"].as_array().unwrap();
        assert_eq!(members.len(), 2);
        assert!(members.iter().any(|m| m["name"] == "gemini-1"));

        // Idempotent registration
        register_synthetic_member_at_path(&config_path, team_name, "gemini-1", "gemini-worker")?;
        let content = fs::read_to_string(&config_path)?;
        let config: Value = serde_json::from_str(&content)?;
        assert_eq!(config["members"].as_array().unwrap().len(), 2);

        // Remove member
        remove_synthetic_member_at_path(&config_path, team_name, "gemini-1")?;
        let content = fs::read_to_string(&config_path)?;
        let config: Value = serde_json::from_str(&content)?;
        let members = config["members"].as_array().unwrap();
        assert_eq!(members.len(), 1);
        assert!(!members.iter().any(|m| m["name"] == "gemini-1"));

        Ok(())
    }
}
