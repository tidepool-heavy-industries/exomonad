//! Configuration discovery from .exomonad/config.toml

use anyhow::{Context, Result};
use serde::Deserialize;
use std::path::PathBuf;
use tracing::info;

/// Sidecar configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct Config {
    /// Project directory for git operations.
    #[serde(default = "default_project_dir")]
    pub project_dir: PathBuf,

    /// Agent role (dev, tl, pm).
    #[serde(default = "default_role")]
    pub role: String,
}

fn default_project_dir() -> PathBuf {
    PathBuf::from(".")
}

fn default_role() -> String {
    "dev".to_string()
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

        info!(project_dir = %config.project_dir.display(), "Config loaded");

        Ok(config)
    }

    /// Create a default config (for when no config file exists).
    pub fn default() -> Self {
        Self {
            project_dir: default_project_dir(),
            role: default_role(),
        }
    }

    /// Resolve WASM path based on role.
    pub fn wasm_path(&self) -> PathBuf {
        let home = std::env::var("HOME").expect("HOME not set");
        PathBuf::from(home)
            .join(".exomonad/wasm")
            .join(format!("wasm-guest-{}.wasm", self.role))
    }

    /// Validate WASM exists, fail with helpful error.
    pub fn validate_wasm_exists(&self) -> Result<PathBuf> {
        let path = self.wasm_path();
        if !path.exists() {
            anyhow::bail!(
                "WASM plugin not found: {}\n\
                 Expected role: {}\n\
                 Run: just wasm {}",
                path.display(),
                self.role,
                self.role
            );
        }
        Ok(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn test_parse_minimal_config() {
        // Empty config should use defaults
        let toml = "";
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.project_dir, PathBuf::from("."));
        assert_eq!(config.role, "dev");
    }

    #[test]
    fn test_parse_full_config() {
        let toml = r#"
project_dir = "/home/user/project"
"#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.project_dir, PathBuf::from("/home/user/project"));
    }

    #[test]
    fn test_from_path() {
        let dir = tempdir().unwrap();
        let config_path = dir.path().join("config.toml");

        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, r#"project_dir = "/tmp/test""#).unwrap();

        let config = Config::from_path(&config_path).unwrap();
        assert_eq!(config.project_dir, PathBuf::from("/tmp/test"));
    }

    #[test]
    fn test_default() {
        let config = Config::default();
        assert_eq!(config.project_dir, PathBuf::from("."));
        assert_eq!(config.role, "dev");
    }

    #[test]
    fn test_wasm_path() {
        let config = Config {
            role: "tl".to_string(),
            project_dir: PathBuf::from("."),
        };

        let expected = PathBuf::from(std::env::var("HOME").unwrap())
            .join(".exomonad/wasm/wasm-guest-tl.wasm");

        assert_eq!(config.wasm_path(), expected);
    }

    #[test]
    fn test_wasm_path_dev_role() {
        let config = Config::default();

        let expected = PathBuf::from(std::env::var("HOME").unwrap())
            .join(".exomonad/wasm/wasm-guest-dev.wasm");

        assert_eq!(config.wasm_path(), expected);
    }
}
