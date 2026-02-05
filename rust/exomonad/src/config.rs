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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn setup_test_dir() -> TempDir {
        TempDir::new().expect("Failed to create temp dir")
    }

    #[test]
    fn test_raw_config_parse_local() {
        let content = r#"
            role = "dev"
            wasm_path = "/custom/path"
        "#;
        let raw: RawConfig = toml::from_str(content).unwrap();
        assert_eq!(raw.role, Some(Role::Dev));
        assert_eq!(raw.wasm_path, Some(PathBuf::from("/custom/path")));
    }

    #[test]
    fn test_raw_config_parse_global() {
        let content = r#"
            project_dir = "/my/project"
            default_role = "tl"
        "#;
        let raw: RawConfig = toml::from_str(content).unwrap();
        assert_eq!(raw.project_dir, Some(PathBuf::from("/my/project")));
        assert_eq!(raw.default_role, Some(Role::TL));
    }

    #[test]
    fn test_raw_config_empty() {
        let raw: RawConfig = toml::from_str("").unwrap();
        assert!(raw.role.is_none());
        assert!(raw.default_role.is_none());
        assert!(raw.project_dir.is_none());
        assert!(raw.wasm_path.is_none());
    }

    #[test]
    fn test_config_default() {
        let config = Config::default();
        assert_eq!(config.project_dir, PathBuf::from("."));
        assert_eq!(config.role, Role::Dev);
        assert!(config.wasm_path_override.is_none());
    }

    #[test]
    fn test_config_validate_project_dir_not_found() {
        let config = Config {
            project_dir: PathBuf::from("/nonexistent/path/that/should/not/exist"),
            role: Role::Dev,
            wasm_path_override: None,
        };

        let result = config.validate();
        assert!(result.is_err());
        match result.unwrap_err() {
            ConfigError::ProjectDirNotFound { path } => {
                assert_eq!(path, PathBuf::from("/nonexistent/path/that/should/not/exist"));
            }
            other => panic!("Expected ProjectDirNotFound, got {:?}", other),
        }
    }

    #[test]
    fn test_config_validate_project_dir_not_directory() {
        let temp = setup_test_dir();
        let file_path = temp.path().join("not_a_dir");
        fs::write(&file_path, "content").unwrap();

        let config = Config {
            project_dir: file_path.clone(),
            role: Role::Dev,
            wasm_path_override: None,
        };

        let result = config.validate();
        assert!(result.is_err());
        match result.unwrap_err() {
            ConfigError::ProjectDirNotDirectory { path } => {
                assert_eq!(path, file_path);
            }
            other => panic!("Expected ProjectDirNotDirectory, got {:?}", other),
        }
    }

    #[test]
    fn test_config_resolve_wasm_path_with_override() {
        let config = Config {
            project_dir: PathBuf::from("."),
            role: Role::TL,
            wasm_path_override: Some(PathBuf::from("/custom/wasm")),
        };

        let path = config.resolve_wasm_path().unwrap();
        assert_eq!(path, PathBuf::from("/custom/wasm/wasm-guest-tl.wasm"));
    }

    #[test]
    fn test_config_resolve_wasm_path_default() {
        let config = Config {
            project_dir: PathBuf::from("."),
            role: Role::Dev,
            wasm_path_override: None,
        };

        // This test requires HOME to be set
        if std::env::var("HOME").is_ok() {
            let path = config.resolve_wasm_path().unwrap();
            assert!(path.to_string_lossy().contains(".exomonad/wasm"));
            assert!(path.to_string_lossy().contains("wasm-guest-dev.wasm"));
        }
    }

    #[test]
    fn test_config_error_display() {
        let err = ConfigError::ProjectDirNotFound {
            path: PathBuf::from("/test"),
        };
        assert!(err.to_string().contains("/test"));

        let err = ConfigError::HomeNotSet;
        assert!(err.to_string().contains("HOME"));

        let err = ConfigError::MissingField {
            field: "role".to_string(),
        };
        assert!(err.to_string().contains("role"));
    }

    #[test]
    fn test_validated_config_accessors() {
        let temp = setup_test_dir();
        let project_dir = temp.path().to_path_buf();

        // Create a mock WASM file
        let wasm_dir = temp.path().join("wasm");
        fs::create_dir_all(&wasm_dir).unwrap();
        let wasm_file = wasm_dir.join("wasm-guest-dev.wasm");
        fs::write(&wasm_file, b"mock wasm").unwrap();

        let config = Config {
            project_dir: project_dir.clone(),
            role: Role::Dev,
            wasm_path_override: Some(wasm_dir),
        };

        let validated = config.validate().unwrap();
        assert_eq!(validated.project_dir(), &project_dir);
        assert_eq!(validated.role(), Role::Dev);
        assert!(validated.wasm_path_buf().to_string_lossy().contains("wasm-guest-dev.wasm"));
    }
}
