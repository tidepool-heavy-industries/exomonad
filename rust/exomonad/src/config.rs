//! Configuration discovery from .exo/config.toml and config.local.toml

use anyhow::{Context, Result};
use exomonad_core::Role;
use serde::Deserialize;
use std::path::{Path, PathBuf};
use tracing::debug;

/// Raw configuration from file (supports both config.toml and config.local.toml fields).
#[derive(Debug, Clone, Deserialize, Default)]
pub struct RawConfig {
    /// Project directory for git operations.
    pub project_dir: Option<PathBuf>,

    /// Specific role for this worktree (local config).
    pub role: Option<Role>,

    /// Project-wide default role.
    pub default_role: Option<Role>,

    /// Canonical Zellij session name for this project.
    pub zellij_session: Option<String>,

    /// TCP port for the HTTP MCP server.
    pub port: Option<u16>,

    /// Base directory for worktrees (default: .exo/worktrees).
    pub worktree_base: Option<PathBuf>,

    /// Shell command to wrap environment (e.g. "nix develop"). TL tab runs this as shell.
    pub shell_command: Option<String>,

    /// WASM directory override (default: ~/.exo/wasm/).
    pub wasm_dir: Option<PathBuf>,
}

/// Final resolved configuration.
#[derive(Debug, Clone)]
pub struct Config {
    pub project_dir: PathBuf,
    pub role: Role,
    /// Canonical Zellij session name (required after discovery).
    pub zellij_session: String,
    /// TCP port for the HTTP MCP server (default: 7432).
    pub port: u16,
    /// Base directory for worktrees.
    pub worktree_base: PathBuf,
    /// Shell command to wrap environment (e.g. "nix develop").
    pub shell_command: Option<String>,
    /// Resolved WASM directory.
    pub wasm_dir: PathBuf,
}

impl Config {
    /// Discover configuration by merging local and global project config.
    ///
    /// Searches upward from CWD for `.exo/config.toml`.
    ///
    /// Resolution Order:
    /// 1. config.local.toml (role)
    /// 2. config.toml (default_role, project_dir)
    /// 3. Environment defaults
    pub fn discover() -> Result<Self> {
        let project_root = find_project_root()?;

        let local_path = project_root.join(".exo/config.local.toml");
        let global_path = project_root.join(".exo/config.toml");

        let local_raw = if local_path.exists() {
            debug!(path = %local_path.display(), "Loaded local config");
            Self::load_raw(&local_path)?
        } else {
            RawConfig::default()
        };

        let global_raw = if global_path.exists() {
            debug!(path = %global_path.display(), "Loaded global config");
            Self::load_raw(&global_path)?
        } else {
            RawConfig::default()
        };

        // Resolve role: local.role > global.default_role > TL
        let role = local_raw
            .role
            .or(global_raw.default_role)
            .unwrap_or(Role::TL);

        // Resolve project_dir: global.project_dir > project_root
        let project_dir = global_raw
            .project_dir
            .or(local_raw.project_dir)
            .map(|p| {
                if p.is_absolute() {
                    p
                } else {
                    project_root.join(p)
                }
            })
            .unwrap_or_else(|| project_root.clone());

        // Resolve zellij_session: config > directory name
        let zellij_session = local_raw
            .zellij_session
            .or(global_raw.zellij_session)
            .unwrap_or_else(|| {
                project_root
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("exomonad")
                    .to_string()
            });
        let zellij_session = sanitize_session_name(zellij_session);

        // Resolve port: local > global > default (7432)
        let port = local_raw.port.or(global_raw.port).unwrap_or(7432);

        // Resolve worktree_base: global > local > default (.exo/worktrees)
        let worktree_base = global_raw
            .worktree_base
            .or(local_raw.worktree_base)
            .map(|p| {
                if p.is_absolute() {
                    p
                } else {
                    project_root.join(p)
                }
            })
            .unwrap_or_else(|| project_root.join(".exo/worktrees"));

        // Resolve shell_command: local > global
        let shell_command = local_raw.shell_command.or(global_raw.shell_command);

        // Resolve wasm_dir: config > ~/.exo/wasm/
        let wasm_dir = global_raw
            .wasm_dir
            .or(local_raw.wasm_dir)
            .map(|p| {
                if p.is_absolute() {
                    p
                } else {
                    project_root.join(p)
                }
            })
            .unwrap_or_else(global_wasm_dir);

        Ok(Self {
            project_dir,
            role,
            zellij_session,
            port,
            worktree_base,
            shell_command,
            wasm_dir,
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
}

impl Default for Config {
    fn default() -> Self {
        Self {
            project_dir: PathBuf::from("."),
            role: Role::TL,
            zellij_session: "default".to_string(),
            port: 7432,
            worktree_base: PathBuf::from(".exo/worktrees"),
            shell_command: None,
            wasm_dir: global_wasm_dir(),
        }
    }
}

/// Walk up from CWD to find the project root containing `.exo/config.toml`.
/// Falls back to CWD if not found (bootstrap case).
fn find_project_root() -> Result<PathBuf> {
    let start = std::env::current_dir()?;
    let mut current = start.as_path();
    loop {
        if current.join(".exo/config.toml").exists() {
            return Ok(current.to_path_buf());
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => {
                debug!("No .exo/config.toml found, using CWD as project root");
                return Ok(start);
            }
        }
    }
}

/// Global WASM directory: ~/.exo/wasm/
pub fn global_wasm_dir() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(".exo/wasm")
}

/// Sanitize session name per Zellij constraints.
/// - Max 36 characters
/// - Replace . with _ (dots cause issues)
fn sanitize_session_name(name: String) -> String {
    name.replace('.', "_").chars().take(36).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raw_config_parse_local() {
        let content = r#"
            role = "dev"
        "#;
        let raw: RawConfig = toml::from_str(content).unwrap();
        assert_eq!(raw.role, Some(Role::Dev));
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
    }

    #[test]
    fn test_config_default() {
        let config = Config::default();
        assert_eq!(config.project_dir, PathBuf::from("."));
        assert_eq!(config.role, Role::TL);
    }

    #[test]
    fn test_sanitize_session_name() {
        // Dots replaced with underscores
        assert_eq!(
            sanitize_session_name("my.project".to_string()),
            "my_project"
        );

        // Max 36 characters
        let long_name = "a".repeat(50);
        assert_eq!(sanitize_session_name(long_name).len(), 36);

        // Clean name unchanged
        assert_eq!(sanitize_session_name("tidepool".to_string()), "tidepool");
    }

    #[test]
    fn test_raw_config_parse_with_zellij_session() {
        let content = r#"
            default_role = "tl"
            zellij_session = "tidepool"
        "#;
        let raw: RawConfig = toml::from_str(content).unwrap();
        assert_eq!(raw.zellij_session, Some("tidepool".to_string()));
    }
}
