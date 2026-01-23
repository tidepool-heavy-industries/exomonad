use serde::Deserialize;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum HangarError {
    #[error("Hangar.toml not found (searched up from {start_dir})")]
    NotFound { start_dir: PathBuf },

    #[error("Failed to read Hangar.toml at {path}: {source}")]
    ReadError {
        path: PathBuf,
        source: std::io::Error,
    },

    #[error("Failed to parse Hangar.toml at {path}: {source}")]
    ParseError {
        path: PathBuf,
        source: toml::de::Error,
    },

    #[error("Failed to resolve path {path}: {source}")]
    PathResolution {
        path: PathBuf,
        source: std::io::Error,
    },
}

/// Hangar configuration parsed from Hangar.toml
#[derive(Debug, Clone, Deserialize)]
pub struct HangarConfig {
    pub hangar: HangarMeta,
    pub project: ProjectPaths,
    pub runtime: RuntimePaths,
    pub auth: AuthConfig,
}

#[derive(Debug, Clone, Deserialize)]
pub struct HangarMeta {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ProjectPaths {
    pub repo: String,
    pub worktrees: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RuntimePaths {
    pub tool_source: String,
    pub bin: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct AuthConfig {
    pub env_file: String,
}

/// Discovered Hangar with absolute paths resolved
#[derive(Debug, Clone)]
pub struct Hangar {
    /// Absolute path to Hangar root (directory containing Hangar.toml)
    pub root: PathBuf,
    /// Parsed configuration
    pub config: HangarConfig,
}

impl Hangar {
    /// Discover Hangar by walking up from current directory
    pub fn discover() -> Result<Self, HangarError> {
        let start_dir = std::env::current_dir().map_err(|e| HangarError::PathResolution {
            path: PathBuf::from("."),
            source: e,
        })?;
        Self::discover_from(&start_dir)
    }

    /// Discover Hangar by walking up from a specific directory
    pub fn discover_from(start_dir: &Path) -> Result<Self, HangarError> {
        let mut current = start_dir.to_path_buf();

        loop {
            let hangar_toml = current.join("Hangar.toml");

            if hangar_toml.exists() {
                return Self::load_from(hangar_toml);
            }

            // Stop at filesystem root or home directory
            if current.parent().is_none() || Some(&current) == dirs::home_dir().as_ref() {
                return Err(HangarError::NotFound {
                    start_dir: start_dir.to_path_buf(),
                });
            }

            current = current.parent().unwrap().to_path_buf();
        }
    }

    /// Load Hangar from a specific Hangar.toml path
    fn load_from(hangar_toml_path: PathBuf) -> Result<Self, HangarError> {
        let contents =
            std::fs::read_to_string(&hangar_toml_path).map_err(|e| HangarError::ReadError {
                path: hangar_toml_path.clone(),
                source: e,
            })?;

        let config: HangarConfig =
            toml::from_str(&contents).map_err(|e| HangarError::ParseError {
                path: hangar_toml_path.clone(),
                source: e,
            })?;

        let root = hangar_toml_path
            .parent()
            .unwrap()
            .canonicalize()
            .map_err(|e| HangarError::PathResolution {
                path: hangar_toml_path.clone(),
                source: e,
            })?;

        Ok(Hangar { root, config })
    }

    /// Get absolute path to runtime/bin directory
    pub fn bin_dir(&self) -> PathBuf {
        self.root.join(&self.config.runtime.bin)
    }

    /// Get absolute path to a specific binary
    pub fn bin_path(&self, binary_name: &str) -> PathBuf {
        self.bin_dir().join(binary_name)
    }

    /// Get absolute path to repo directory
    pub fn repo_dir(&self) -> PathBuf {
        self.root.join(&self.config.project.repo)
    }

    /// Get absolute path to worktrees directory
    pub fn worktrees_dir(&self) -> PathBuf {
        self.root.join(&self.config.project.worktrees)
    }

    /// Get absolute path to .env file
    pub fn env_file(&self) -> PathBuf {
        self.root.join(&self.config.auth.env_file)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover() {
        // This will fail in CI if not in a Hangar, which is fine
        match Hangar::discover() {
            Ok(hangar) => {
                assert!(hangar.root.join("Hangar.toml").exists());
                assert!(!hangar.config.hangar.name.is_empty());
            }
            Err(HangarError::NotFound { .. }) => {
                // OK if not running inside a Hangar
            }
            Err(e) => panic!("Unexpected error: {}", e),
        }
    }
}
