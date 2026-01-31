//! Configuration discovery from .exomonad/config.toml

use anyhow::{Context, Result};
use serde::Deserialize;
use std::path::PathBuf;
use tracing::info;

/// Sidecar configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct Config {
    /// Agent role (tl, pm, dev).
    pub role: String,

    /// Project directory for git operations.
    #[serde(default = "default_project_dir")]
    pub project_dir: PathBuf,
}

fn default_project_dir() -> PathBuf {
    PathBuf::from(".")
}

impl Config {
    /// Discover configuration from .exomonad/config.toml in current directory.
    pub fn discover() -> Result<Self> {
        let path = PathBuf::from(".exomonad/config.toml");
        Self::from_path(&path)
    }

    /// Load configuration from a specific path.
    pub fn from_path(path: &PathBuf) -> Result<Self> {
        info!(path = %path.display(), "Loading config");

        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read config file: {}", path.display()))?;

        let config: Config = toml::from_str(&content)
            .with_context(|| format!("Failed to parse config file: {}", path.display()))?;

        info!(role = %config.role, project_dir = %config.project_dir.display(), "Config loaded");

        Ok(config)
    }

    /// Create a default config (for when no config file exists).
    pub fn default_for_role(role: &str) -> Self {
        Self {
            role: role.to_string(),
            project_dir: PathBuf::from("."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn test_parse_minimal_config() {
        let toml = r#"
role = "tl"
"#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.role, "tl");
        assert_eq!(config.project_dir, PathBuf::from("."));
    }

    #[test]
    fn test_parse_full_config() {
        let toml = r#"
role = "pm"
project_dir = "/home/user/project"
"#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.role, "pm");
        assert_eq!(config.project_dir, PathBuf::from("/home/user/project"));
    }

    #[test]
    fn test_from_path() {
        let dir = tempdir().unwrap();
        let config_path = dir.path().join("config.toml");

        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, r#"role = "dev""#).unwrap();

        let config = Config::from_path(&config_path).unwrap();
        assert_eq!(config.role, "dev");
    }

    #[test]
    fn test_default_for_role() {
        let config = Config::default_for_role("tl");
        assert_eq!(config.role, "tl");
        assert_eq!(config.project_dir, PathBuf::from("."));
    }
}
