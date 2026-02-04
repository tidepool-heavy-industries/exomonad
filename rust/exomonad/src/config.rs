//! Configuration discovery from .exomonad/config.toml and config.local.toml

use anyhow::{Context, Result};
use exomonad_shared::{PathError, Role, WasmPath};
use serde::Deserialize;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tracing::debug;

/// Configuration validation errors.
#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("project directory does not exist: {path}")]
    ProjectDirNotFound { path: PathBuf },

    #[error("project directory is not a directory: {path}")]
    ProjectDirNotDirectory { path: PathBuf },

    #[error("HOME environment variable not set")]
    HomeNotSet,

    #[error("WASM plugin validation failed: {0}")]
    WasmValidation(#[from] PathError),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("required config field '{field}' is missing")]
    MissingField { field: String },
}

/// Raw configuration from file (supports both config.toml and config.local.toml fields).
#[derive(Debug, Clone, Deserialize, Default)]
pub struct RawConfig {
    /// Project directory for git operations.
    pub project_dir: Option<PathBuf>,

    /// Specific role for this worktree (local config).
    pub role: Option<Role>,

    /// Project-wide default role.
    pub default_role: Option<Role>,

    /// Absolute path to WASM artifacts.
    pub wasm_path: Option<PathBuf>,
}

/// Final resolved configuration.
#[derive(Debug, Clone)]
pub struct Config {
    pub project_dir: PathBuf,
    pub role: Role,
    pub wasm_path_override: Option<PathBuf>,
}

/// Validated configuration wrapper.
#[derive(Debug, Clone)]
pub struct ValidatedConfig {
    config: Config,
    wasm_path: WasmPath,
}

impl Config {
    /// Discover configuration by merging local and global project config.
    ///
    /// Resolution Order:
    /// 1. config.local.toml (role, wasm_path)
    /// 2. config.toml (default_role, project_dir)
    /// 3. Environment defaults
    pub fn discover() -> Result<Self> {
        let local_path = PathBuf::from(".exomonad/config.local.toml");
        let global_path = PathBuf::from(".exomonad/config.toml");

        let local_raw = if local_path.exists() {
            Self::load_raw(&local_path)?
        } else {
            RawConfig::default()
        };

        let global_raw = if global_path.exists() {
            Self::load_raw(&global_path)?
        } else {
            RawConfig::default()
        };

        // Resolve role: local.role > global.default_role
        let role = local_raw
            .role
            .or(global_raw.default_role)
            .ok_or_else(|| anyhow::anyhow!("No active role defined. Please set 'role' in .exomonad/config.local.toml or 'default_role' in .exomonad/config.toml"))?;

        // Resolve project_dir: global.project_dir > "."
        let project_dir = global_raw
            .project_dir
            .or(local_raw.project_dir)
            .unwrap_or_else(|| PathBuf::from("."));

        Ok(Self {
            project_dir,
            role,
            wasm_path_override: local_raw.wasm_path,
        })
    }

    fn load_raw(path: &Path) -> Result<RawConfig> {
        debug!(path = %path.display(), "Loading raw config");
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read config file: {}", path.display()))?;

        let config: RawConfig = toml::from_str(&content)
            .with_context(|| format!("Failed to parse config file: {}", path.display()))?;

        Ok(config)
    }

    /// Validate configuration and return a ValidatedConfig.
    pub fn validate(self) -> Result<ValidatedConfig, ConfigError> {
        // Check project_dir exists
        if !self.project_dir.exists() {
            return Err(ConfigError::ProjectDirNotFound {
                path: self.project_dir.clone(),
            });
        }

        // Check project_dir is a directory
        if !self.project_dir.is_dir() {
            return Err(ConfigError::ProjectDirNotDirectory {
                path: self.project_dir.clone(),
            });
        }

        // Resolve and validate WASM path
        let wasm_path_buf = self.resolve_wasm_path()?;
        let wasm_path = WasmPath::try_from(wasm_path_buf)?;

        Ok(ValidatedConfig {
            config: self,
            wasm_path,
        })
    }

    /// Resolve absolute WASM path based on role and overrides.
    fn resolve_wasm_path(&self) -> Result<PathBuf, ConfigError> {
        if let Some(ref over) = self.wasm_path_override {
            return Ok(over.join(format!("wasm-guest-{}.wasm", self.role)));
        }

        let home = std::env::var("HOME").map_err(|_| ConfigError::HomeNotSet)?;
        Ok(PathBuf::from(home)
            .join(".exomonad/wasm")
            .join(format!("wasm-guest-{}.wasm", self.role)))
    }
}

impl ValidatedConfig {
    /// Get the validated project directory.
    pub fn project_dir(&self) -> &PathBuf {
        &self.config.project_dir
    }

    /// Get the validated role.
    pub fn role(&self) -> Role {
        self.config.role
    }

    /// Get the WASM path as a PathBuf.
    pub fn wasm_path_buf(&self) -> PathBuf {
        self.wasm_path.as_path().to_path_buf()
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            project_dir: PathBuf::from("."),
            role: Role::Dev,
            wasm_path_override: None,
        }
    }
}
