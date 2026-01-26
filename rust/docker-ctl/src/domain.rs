//! Domain model for Tidepool agent containers.
//!
//! Haskell owns "what" (DockerSpawner effect, SpawnConfig).
//! Rust owns "how" (volume mounting, TTY allocation, network config).

use bollard::models::{Mount, MountTypeEnum};

// =============================================================================
// Volume Configuration
// =============================================================================

/// Standard volume set for Tidepool agent containers.
///
/// **SYNC POINT:** Keep in sync with `docker-compose.yml` volumes section.
/// When adding a volume here, also add it to:
/// - `docker-compose.yml` (volumes: section + service mounts)
/// - Control-server's SpawnAgents tool (passes env vars via Docker API)
///
/// Reference: `/docker-compose.yml:250-270` (volume definitions)
#[derive(Debug, Clone)]
pub struct AgentVolumes {
    /// Main git repository - required for worktree linkage.
    /// Worktrees' `.git` files point to `/repo/.git/worktrees/...`
    pub repo: VolumeMount,

    /// Worktrees directory - agent workspaces live here.
    pub worktrees: VolumeMount,

    /// Unix sockets for IPC - subagents create `/sockets/{issue_id}/`
    pub sockets: VolumeMount,

    /// GitHub CLI auth - shared across all agents.
    pub gh_auth: VolumeMount,
}

impl Default for AgentVolumes {
    fn default() -> Self {
        Self {
            repo: VolumeMount::REPO,
            worktrees: VolumeMount::WORKTREES,
            sockets: VolumeMount::SOCKETS,
            gh_auth: VolumeMount::GH_AUTH,
        }
    }
}

impl AgentVolumes {
    pub fn to_mounts(&self) -> Vec<Mount> {
        vec![
            self.repo.to_bollard(),
            self.worktrees.to_bollard(),
            self.sockets.to_bollard(),
            self.gh_auth.to_bollard(),
        ]
    }
}

/// A named Docker volume mount.
#[derive(Debug, Clone)]
pub struct VolumeMount {
    /// Docker volume name (source)
    pub volume: &'static str,
    /// Path inside container (target)
    pub path: &'static str,
}

impl VolumeMount {
    // -------------------------------------------------------------------------
    // Standard volumes - these are the source of truth
    // -------------------------------------------------------------------------

    /// Main repository volume.
    /// **docker-compose.yml:** `tidepool-repo:/workspace/tl` (for tl service)
    pub const REPO: Self = Self {
        volume: "tidepool-repo",
        path: "/repo",
    };

    /// Worktrees volume - spawned agents work here.
    /// **docker-compose.yml:** `tidepool-worktrees:/worktrees`
    pub const WORKTREES: Self = Self {
        volume: "tidepool-worktrees",
        path: "/worktrees",
    };

    /// Sockets volume - IPC between agents and control-server.
    /// **docker-compose.yml:** `tidepool-sockets:/sockets`
    pub const SOCKETS: Self = Self {
        volume: "tidepool-sockets",
        path: "/sockets",
    };

    /// GitHub auth volume - shared credentials.
    /// **docker-compose.yml:** `tidepool-gh-auth:/home/agent/.config/gh`
    pub const GH_AUTH: Self = Self {
        volume: "tidepool-gh-auth",
        path: "/home/agent/.config/gh",
    };

    // -------------------------------------------------------------------------
    // Conversion
    // -------------------------------------------------------------------------

    pub fn to_bollard(&self) -> Mount {
        Mount {
            target: Some(self.path.to_string()),
            source: Some(self.volume.to_string()),
            typ: Some(MountTypeEnum::VOLUME),
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_volumes_has_all_four() {
        let vols = AgentVolumes::default();
        let mounts = vols.to_mounts();
        assert_eq!(mounts.len(), 4);
    }

    #[test]
    fn volume_names_match_docker_compose() {
        // These names must match docker-compose.yml volume definitions
        assert_eq!(VolumeMount::REPO.volume, "tidepool-repo");
        assert_eq!(VolumeMount::WORKTREES.volume, "tidepool-worktrees");
        assert_eq!(VolumeMount::SOCKETS.volume, "tidepool-sockets");
        assert_eq!(VolumeMount::GH_AUTH.volume, "tidepool-gh-auth");
    }
}
