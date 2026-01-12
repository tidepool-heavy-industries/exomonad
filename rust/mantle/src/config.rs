//! Configuration file support for mantle.
//!
//! Reads configuration from `~/.config/mantle/config.toml`.
//!
//! ## Example config
//!
//! ```toml
//! [docker]
//! auth_volume = "tidepool-claude-auth"
//! image = "tidepool/claude-code:latest"
//! ```
//!
//! ## Hub Configuration
//!
//! Hub configuration is loaded separately from `~/.tidepool/config.toml`
//! or the `MANTLE_HUB_URL` environment variable. See `mantle_shared::hub::HubConfig`.
//!
//! Hub is **required** - sessions will fail to start if hub is unreachable.
//! Default: `http://localhost:7433`
//!
//! ```toml
//! # ~/.tidepool/config.toml
//! [hub]
//! http_url = "http://localhost:7433"
//! ```

use serde::Deserialize;
use std::path::PathBuf;

/// Docker-related configuration.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct DockerConfig {
    /// Named Docker volume containing Claude auth credentials.
    /// When set, this volume is mounted to `/home/user/.claude` in containers
    /// instead of the host's `~/.claude` directory.
    pub auth_volume: Option<String>,

    /// Docker image to use for Claude Code containers.
    /// Defaults to "mantle-agent:latest" if not specified.
    pub image: Option<String>,

    /// Named Docker volume for shared cabal package store.
    /// When set, this volume is mounted to `/home/user/.cabal/store` in containers.
    /// This allows all containers to share built Haskell packages, avoiding redundant builds.
    pub cabal_store_volume: Option<String>,
}

/// Root configuration structure.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct Config {
    /// Docker configuration.
    #[serde(default)]
    pub docker: DockerConfig,
}

impl Config {
    /// Load configuration from the default path (`~/.config/mantle/config.toml`).
    ///
    /// Returns default config if file doesn't exist.
    /// Returns error only if file exists but is malformed.
    pub fn load() -> Result<Self, ConfigError> {
        let config_path = Self::default_path();

        eprintln!("[DEBUG] Config path: {:?}", config_path);
        eprintln!("[DEBUG] Config exists: {}", config_path.exists());

        if !config_path.exists() {
            eprintln!("[DEBUG] No config file, using defaults");
            tracing::debug!("No config file at {:?}, using defaults", config_path);
            return Ok(Self::default());
        }

        eprintln!("[DEBUG] Loading config from {:?}", config_path);
        Self::load_from(&config_path)
    }

    /// Load configuration from a specific path.
    pub fn load_from(path: &PathBuf) -> Result<Self, ConfigError> {
        let contents = std::fs::read_to_string(path).map_err(|e| ConfigError::Read {
            path: path.clone(),
            source: e,
        })?;

        let config: Config = toml::from_str(&contents).map_err(|e| ConfigError::Parse {
            path: path.clone(),
            source: e,
        })?;

        tracing::info!("Loaded config from {:?}", path);
        if let Some(ref vol) = config.docker.auth_volume {
            tracing::info!("  docker.auth_volume = {:?}", vol);
        }
        if let Some(ref img) = config.docker.image {
            tracing::info!("  docker.image = {:?}", img);
        }
        if let Some(ref vol) = config.docker.cabal_store_volume {
            tracing::info!("  docker.cabal_store_volume = {:?}", vol);
        }

        Ok(config)
    }

    /// Default config file path: `~/.config/mantle/config.toml`
    pub fn default_path() -> PathBuf {
        dirs::config_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("mantle")
            .join("config.toml")
    }
}

/// Configuration errors.
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("Failed to read config file {path}: {source}")]
    Read {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Failed to parse config file {path}: {source}")]
    Parse {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_config() {
        let toml = r#"
[docker]
auth_volume = "tidepool-claude-auth"
"#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(
            config.docker.auth_volume,
            Some("tidepool-claude-auth".to_string())
        );
        assert_eq!(config.docker.image, None);
    }

    #[test]
    fn test_parse_full_config() {
        let toml = r#"
[docker]
auth_volume = "my-auth-vol"
image = "my-image:latest"
"#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.docker.auth_volume, Some("my-auth-vol".to_string()));
        assert_eq!(config.docker.image, Some("my-image:latest".to_string()));
    }

    #[test]
    fn test_empty_config() {
        let toml = "";
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.docker.auth_volume, None);
        assert_eq!(config.docker.image, None);
    }

    #[test]
    fn test_default_path() {
        let path = Config::default_path();
        assert!(path.ends_with("mantle/config.toml"));
    }
}
