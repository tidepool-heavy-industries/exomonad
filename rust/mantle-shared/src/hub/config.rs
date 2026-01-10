//! Configuration for hub connection.
//!
//! Configuration is resolved in the following order (later overrides earlier):
//! 1. Defaults (http://localhost:7433, /tmp/mantle.sock)
//! 2. ~/.tidepool/config.toml
//! 3. MANTLE_HUB_URL environment variable

use serde::Deserialize;
use std::path::PathBuf;

/// Default HTTP port for hub.
pub const DEFAULT_HUB_PORT: u16 = 7433;

/// Default Unix socket path for hub.
pub const DEFAULT_SOCKET_PATH: &str = "/tmp/mantle.sock";

/// Hub configuration.
#[derive(Debug, Clone)]
pub struct HubConfig {
    /// HTTP URL for hub API (e.g., http://localhost:7433).
    pub http_url: String,
    /// Unix socket path for container communication.
    pub socket_path: PathBuf,
}

impl Default for HubConfig {
    fn default() -> Self {
        Self {
            http_url: format!("http://localhost:{}", DEFAULT_HUB_PORT),
            socket_path: PathBuf::from(DEFAULT_SOCKET_PATH),
        }
    }
}

/// TOML structure for ~/.tidepool/config.toml.
#[derive(Debug, Deserialize, Default)]
struct ConfigFile {
    #[serde(default)]
    hub: HubSection,
}

#[derive(Debug, Deserialize, Default)]
struct HubSection {
    http_url: Option<String>,
    socket_path: Option<PathBuf>,
}

impl HubConfig {
    /// Load configuration from file and environment.
    pub fn load() -> Self {
        let mut config = HubConfig::default();

        // Try to load from ~/.tidepool/config.toml
        if let Some(config_path) = Self::config_file_path() {
            if config_path.exists() {
                if let Ok(contents) = std::fs::read_to_string(&config_path) {
                    if let Ok(file_config) = toml::from_str::<ConfigFile>(&contents) {
                        if let Some(url) = file_config.hub.http_url {
                            config.http_url = url;
                        }
                        if let Some(path) = file_config.hub.socket_path {
                            config.socket_path = path;
                        }
                    }
                }
            }
        }

        // Environment variable overrides everything
        if let Ok(url) = std::env::var("MANTLE_HUB_URL") {
            config.http_url = url;
        }

        config
    }

    /// Get the path to the config file (~/.tidepool/config.toml).
    pub fn config_file_path() -> Option<PathBuf> {
        directories::BaseDirs::new().map(|dirs| dirs.home_dir().join(".tidepool").join("config.toml"))
    }

    /// Get the path to the tidepool data directory (~/.tidepool/).
    pub fn data_dir() -> Option<PathBuf> {
        directories::BaseDirs::new().map(|dirs| dirs.home_dir().join(".tidepool"))
    }
}
