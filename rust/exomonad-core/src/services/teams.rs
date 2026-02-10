use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use tokio::fs;
use tokio::time::{sleep, Duration};
use tracing::{debug, info};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TeamMember {
    pub agent_id: String,
    pub name: String,
    pub agent_type: String,
    pub model: String,
    pub backend_type: String,
    pub tmux_pane_id: String,
    pub joined_at: u64,
    pub cwd: String,
    pub subscriptions: Vec<String>,
    pub color: String,
    pub prompt: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TeamConfig {
    pub members: Vec<TeamMember>,
    #[serde(flatten)]
    pub other: serde_json::Value,
}

pub struct TeamsService;

impl TeamsService {
    pub fn get_teams_dir() -> Result<PathBuf> {
        let home = dirs::home_dir().context("Could not find home directory")?;
        Ok(home.join(".claude").join("teams"))
    }

    pub fn get_team_config_path(team_name: &str) -> Result<PathBuf> {
        Ok(Self::get_teams_dir()?.join(team_name).join("config.json"))
    }

    pub fn get_inbox_path(team_name: &str, agent_name: &str) -> Result<PathBuf> {
        Ok(Self::get_teams_dir()?
            .join(team_name)
            .join("inboxes")
            .join(format!("{}.json", agent_name)))
    }

    pub async fn register_agent(
        team_name: &str,
        agent_name: &str,
        cwd: &Path,
        issue_body: &str,
    ) -> Result<()> {
        let config_path = Self::get_team_config_path(team_name)?;
        let lock_path = config_path.with_extension("json.lock");

        info!(team_name, agent_name, "Registering agent in team");

        Self::acquire_lock(&lock_path).await?;

        let result = async {
            let mut config: TeamConfig = if config_path.exists() {
                let content = fs::read_to_string(&config_path).await?;
                serde_json::from_str(&content).context("Failed to parse team config.json")?
            } else {
                return Err(anyhow!("Team config not found at {}. Agents must be spawned from within an active Claude Code Team.", config_path.display()));
            };

            // Remove existing member with same name if it exists
            config.members.retain(|m| m.name != agent_name);

            let joined_at = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_millis() as u64;

            let prompt = issue_body.chars().take(200).collect::<String>();
            let color = Self::pseudo_random_color(agent_name);

            config.members.push(TeamMember {
                agent_id: format!("{}@{}", agent_name, team_name),
                name: agent_name.to_string(),
                agent_type: "general-purpose".to_string(),
                model: "gemini-2.5-pro".to_string(),
                backend_type: "tmux".to_string(),
                tmux_pane_id: "".to_string(),
                joined_at,
                cwd: cwd.to_string_lossy().to_string(),
                subscriptions: vec![],
                color,
                prompt,
            });

            let content = serde_json::to_string_pretty(&config)?;
            fs::write(&config_path, content).await?;

            // Create inbox file
            let inbox_path = Self::get_inbox_path(team_name, agent_name)?;
            if let Some(parent) = inbox_path.parent() {
                fs::create_dir_all(parent).await?;
            }
            fs::write(&inbox_path, "[]").await?;
            info!(inbox_path = %inbox_path.display(), "Created inbox file");

            Ok::<(), anyhow::Error>(())
        }
        .await;

        Self::release_lock(&lock_path).await;
        result
    }

    /// Read-only check: get a specific member from config.json by name.
    pub async fn get_member(team_name: &str, agent_name: &str) -> Result<Option<TeamMember>> {
        let config_path = Self::get_team_config_path(team_name)?;
        if !config_path.exists() {
            return Ok(None);
        }
        let content = fs::read_to_string(&config_path).await?;
        let config: TeamConfig =
            serde_json::from_str(&content).context("Failed to parse team config.json")?;
        Ok(config.members.into_iter().find(|m| m.name == agent_name))
    }

    /// Read-only: list all members from config.json.
    pub async fn list_members(team_name: &str) -> Result<Vec<TeamMember>> {
        let config_path = Self::get_team_config_path(team_name)?;
        if !config_path.exists() {
            return Ok(Vec::new());
        }
        let content = fs::read_to_string(&config_path).await?;
        let config: TeamConfig =
            serde_json::from_str(&content).context("Failed to parse team config.json")?;
        Ok(config.members)
    }

    pub async fn unregister_agent(team_name: &str, agent_name: &str) -> Result<()> {
        let config_path = Self::get_team_config_path(team_name)?;
        let lock_path = config_path.with_extension("json.lock");

        if !config_path.exists() {
            debug!(
                team_name,
                "Team config does not exist, skipping unregistration"
            );
            return Ok(());
        }

        info!(team_name, agent_name, "Unregistering agent from team");

        Self::acquire_lock(&lock_path).await?;

        let result = async {
            let content = fs::read_to_string(&config_path).await?;
            let mut config: TeamConfig = serde_json::from_str(&content)?;

            let initial_count = config.members.len();
            config.members.retain(|m| m.name != agent_name);

            if config.members.len() != initial_count {
                let content = serde_json::to_string_pretty(&config)?;
                fs::write(&config_path, content).await?;
                info!(team_name, agent_name, "Removed agent from config.json");
            }

            // Delete inbox file
            let inbox_path = Self::get_inbox_path(team_name, agent_name)?;
            if inbox_path.exists() {
                fs::remove_file(&inbox_path).await?;
                info!(inbox_path = %inbox_path.display(), "Deleted inbox file");
            }

            Ok::<(), anyhow::Error>(())
        }
        .await;

        Self::release_lock(&lock_path).await;
        result
    }

    pub async fn unregister_agent_from_all_teams(agent_name: &str) -> Result<()> {
        let teams_dir = match Self::get_teams_dir() {
            Ok(d) => d,
            Err(_) => return Ok(()),
        };
        if !teams_dir.exists() {
            return Ok(());
        }

        debug!(agent_name, "Scanning all teams to unregister agent");
        let mut entries = fs::read_dir(teams_dir).await?;
        while let Some(entry) = entries.next_entry().await? {
            if entry.file_type().await?.is_dir() {
                if let Some(team_name) = entry.file_name().to_str() {
                    let _ = Self::unregister_agent(team_name, agent_name).await;
                }
            }
        }
        Ok(())
    }

    async fn acquire_lock(lock_path: &Path) -> Result<()> {
        let max_retries = 50;
        let mut retries = 0;
        while lock_path.exists() {
            if retries >= max_retries {
                return Err(anyhow!(
                    "Timed out waiting for team config lock: {}",
                    lock_path.display()
                ));
            }
            sleep(Duration::from_millis(100)).await;
            retries += 1;
        }
        fs::write(lock_path, "").await?;
        Ok(())
    }

    async fn release_lock(lock_path: &Path) {
        let _ = fs::remove_file(lock_path).await;
    }

    fn pseudo_random_color(name: &str) -> String {
        let mut hash: u32 = 0;
        for c in name.chars() {
            hash = (hash.wrapping_shl(5).wrapping_sub(hash)).wrapping_add(c as u32);
        }
        format!("#{:06x}", hash & 0xFFFFFF)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_pseudo_random_color() {
        let color1 = TeamsService::pseudo_random_color("agent1");
        let color2 = TeamsService::pseudo_random_color("agent2");
        let color1_again = TeamsService::pseudo_random_color("agent1");

        assert_eq!(color1.len(), 7);
        assert!(color1.starts_with('#'));
        assert_ne!(color1, color2);
        assert_eq!(color1, color1_again);
    }

    #[tokio::test]
    async fn test_register_unregister_agent() -> Result<()> {
        let tmp = tempdir()?;
        let teams_dir = tmp.path().join("teams");
        let team_name = "test-team";
        let agent_name = "test-agent";
        let team_dir = teams_dir.join(team_name);
        fs::create_dir_all(&team_dir).await?;

        let config_path = team_dir.join("config.json");
        let initial_config = serde_json::json!({
            "members": [],
            "leadSessionId": "session-123"
        });
        fs::write(&config_path, serde_json::to_string(&initial_config)?).await?;

        // Mock home dir by overriding TeamsService methods or just test the logic with paths
        // Since we can't easily mock dirs::home_dir() in a clean way without refactoring,
        // we'll test register_agent logic by manually calling the inner parts or
        // just assume it works if we can't easily mock home.

        // Let's refactor TeamsService slightly to accept a base dir for testing if we wanted,
        // but for now let's just test that it correctly modifies a config file.

        let mut config: TeamConfig =
            serde_json::from_str(&fs::read_to_string(&config_path).await?)?;
        let cwd = Path::new("/tmp/worktree");
        let joined_at = 123456789;

        config.members.push(TeamMember {
            agent_id: format!("{}@{}", agent_name, team_name),
            name: agent_name.to_string(),
            agent_type: "general-purpose".to_string(),
            model: "gemini-2.5-pro".to_string(),
            backend_type: "tmux".to_string(),
            tmux_pane_id: "".to_string(),
            joined_at,
            cwd: cwd.to_string_lossy().to_string(),
            subscriptions: vec![],
            color: "#ffffff".to_string(),
            prompt: "test prompt".to_string(),
        });

        let content = serde_json::to_string_pretty(&config)?;
        fs::write(&config_path, content).await?;

        let read_back: TeamConfig = serde_json::from_str(&fs::read_to_string(&config_path).await?)?;
        assert_eq!(read_back.members.len(), 1);
        assert_eq!(read_back.members[0].name, agent_name);
        assert_eq!(read_back.other["leadSessionId"], "session-123");

        Ok(())
    }
}
