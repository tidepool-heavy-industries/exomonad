//! Claude project-dir linking for `--resume --fork-session` — the single source
//! shared by classic mode (`AgentControlService`) and node mode (`exo-runtime`'s
//! Spawner).
//!
//! When a child worktree launches with `--resume <parent-uuid> --fork-session`,
//! Claude Code looks up the parent's conversation under `~/.claude/projects/<encoded-cwd>`,
//! where `<encoded-cwd>` is the child's own working directory encoded by Claude's
//! lossy path scheme. The child's encoded dir does not exist yet (it's a fresh
//! worktree), so the lookup fails with "no conversation ID found". Symlinking the
//! child's project dir to the parent's lets the child discover the parent's sessions.

use std::path::Path;
use tracing::{info, warn};

/// Encode a filesystem path the way Claude Code stores it in `~/.claude/projects/`.
/// Non-alphanumeric ASCII becomes `-` (lossy regex replacement `[^a-zA-Z0-9] → '-'`).
pub fn encode_claude_project_dir(p: &Path) -> String {
    p.to_string_lossy()
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
        .collect()
}

/// Symlink the child worktree's Claude project dir to the parent's, so a child
/// launched with `--resume --fork-session` can discover the parent's sessions.
///
/// Without this symlink, `--resume --fork-session` fails with "no conversation ID found".
/// The parent path is canonicalized (matching the path Claude records for the parent);
/// the child worktree path is used as-is (matching the cwd Claude is launched in).
///
/// Best-effort: a missing parent project dir or an existing child link is a no-op, and a
/// symlink failure is logged at `warn` and swallowed (fork-session may not work, but the
/// spawn proceeds). The only hard error is an unresolvable home directory.
pub fn link_parent_project_dir(parent_project_dir: &Path, child_worktree: &Path) -> std::io::Result<()> {
    let home = dirs::home_dir().ok_or_else(|| {
        std::io::Error::new(std::io::ErrorKind::NotFound, "home directory not set")
    })?;
    let claude_projects_dir = home.join(".claude").join("projects");
    let canonical_parent = parent_project_dir
        .canonicalize()
        .unwrap_or_else(|_| parent_project_dir.to_path_buf());
    let parent_encoded = encode_claude_project_dir(&canonical_parent);
    let child_encoded = encode_claude_project_dir(child_worktree);
    let parent_project = claude_projects_dir.join(&parent_encoded);
    let child_project = claude_projects_dir.join(&child_encoded);
    if parent_project.exists() && !child_project.exists() {
        match std::os::unix::fs::symlink(&parent_project, &child_project) {
            Ok(()) => info!(
                parent = %parent_encoded,
                child = %child_encoded,
                "Symlinked Claude project dir for session inheritance"
            ),
            Err(e) => warn!(
                parent = %parent_encoded,
                child = %child_encoded,
                error = %e,
                "Failed to symlink Claude project dir (fork-session may not work)"
            ),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_claude_project_path_encoding() {
        // Claude Code encodes paths via [^a-zA-Z0-9] → '-'
        // Verified against actual ~/.claude/projects/ directory names.

        // Basic path
        assert_eq!(
            encode_claude_project_dir(Path::new("/home/inanna/dev/exomonad")),
            "-home-inanna-dev-exomonad"
        );
        // Worktree path (dots and hyphens in segments)
        assert_eq!(
            encode_claude_project_dir(Path::new(
                "/home/inanna/dev/exomonad/.exo/worktrees/fork-session"
            )),
            "-home-inanna-dev-exomonad--exo-worktrees-fork-session"
        );
        // Hidden dir (leading dot → double dash after parent separator)
        assert_eq!(
            encode_claude_project_dir(Path::new("/home/inanna/.config/home-manager")),
            "-home-inanna--config-home-manager"
        );
        // Deep nested path with hyphens
        assert_eq!(
            encode_claude_project_dir(Path::new(
                "/home/inanna/dev/aegis-binder-diagnostic-framework"
            )),
            "-home-inanna-dev-aegis-binder-diagnostic-framework"
        );
        // Path with spaces
        assert_eq!(
            encode_claude_project_dir(Path::new("/home/user/My Projects/app")),
            "-home-user-My-Projects-app"
        );
    }
}
