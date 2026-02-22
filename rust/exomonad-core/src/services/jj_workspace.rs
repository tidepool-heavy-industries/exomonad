//! Jujutsu workspace management via jj-lib.
//!
//! Replaces git worktree operations with native jj workspace/bookmark operations.
//! Every mutating method calls `export_refs()` after committing the transaction
//! to keep git refs in sync for `gh` CLI compatibility (colocated mode).

use crate::domain::{BranchName, Revision};
use anyhow::{Context, Result};
use jj_lib::config::{ConfigSource, StackedConfig};
use jj_lib::git::{self, GitImportOptions};
use jj_lib::backend::CommitId;
use jj_lib::op_store::RefTarget;
use jj_lib::ref_name::{RefName, WorkspaceNameBuf};
use jj_lib::repo::Repo;
use jj_lib::settings::UserSettings;
use jj_lib::workspace::{default_working_copy_factories, Workspace};
use std::path::{Path, PathBuf};
use tracing::{error, info, warn};

/// Service for jj workspace and bookmark operations via jj-lib.
///
/// Each method loads the workspace fresh (jj-lib types aren't Send/Sync).
/// All mutating methods export refs to git after committing transactions.
pub struct JjWorkspaceService {
    project_dir: PathBuf,
}

impl JjWorkspaceService {
    pub fn new(project_dir: PathBuf) -> Self {
        Self { project_dir }
    }

    /// Load jj settings from standard config locations.
    fn load_settings(&self) -> Result<UserSettings> {
        let mut config = StackedConfig::with_defaults();

        // Load user config (~/.config/jj/config.toml or equivalent)
        if let Some(config_dir) = dirs::config_dir() {
            let user_config = config_dir.join("jj").join("config.toml");
            if user_config.exists() {
                if let Err(e) = config.load_file(ConfigSource::User, &user_config) {
                    warn!(path = %user_config.display(), error = %e, "Failed to load jj user config");
                }
            }
        }

        // Load repo config
        let repo_config = self.project_dir.join(".jj").join("repo").join("config.toml");
        if repo_config.exists() {
            if let Err(e) = config.load_file(ConfigSource::Repo, &repo_config) {
                warn!(path = %repo_config.display(), error = %e, "Failed to load jj repo config");
            }
        }

        UserSettings::from_config(config).context("Failed to create jj UserSettings")
    }

    /// Load the workspace at the project root.
    fn load_root_workspace(&self) -> Result<Workspace> {
        let settings = self.load_settings()?;
        Workspace::load(
            &settings,
            &self.project_dir,
            &jj_lib::repo::StoreFactories::default(),
            &default_working_copy_factories(),
        )
        .map_err(|e| anyhow::anyhow!("Failed to load jj workspace at {}: {}", self.project_dir.display(), e))
    }

    /// Load a workspace at a specific path (for child workspaces).
    fn load_workspace_at(&self, path: &Path) -> Result<Workspace> {
        let settings = self.load_settings()?;
        Workspace::load(
            &settings,
            path,
            &jj_lib::repo::StoreFactories::default(),
            &default_working_copy_factories(),
        )
        .map_err(|e| anyhow::anyhow!("Failed to load jj workspace at {}: {}", path.display(), e))
    }

    /// Resolve a revision string to a CommitId.
    ///
    /// Tries local bookmark first, then full commit ID hex.
    fn resolve_revision(&self, repo: &dyn Repo, rev: &Revision) -> Result<CommitId> {
        let rev_str = rev.as_str();
        // Try as local bookmark
        let bookmark_ref: &RefName = rev_str.as_ref();
        if let Some(id) = repo.view().get_local_bookmark(bookmark_ref).as_normal() {
            return Ok(id.clone());
        }

        // Try as full commit ID hex
        if let Some(id) = CommitId::try_from_hex(rev_str) {
            // Verify the commit exists
            repo.store()
                .get_commit(&id)
                .with_context(|| format!("Commit '{}' not found in repo", rev))?;
            return Ok(id);
        }

        Err(anyhow::anyhow!(
            "Could not resolve revision '{}': not a known bookmark or valid commit ID",
            rev
        ))
    }

    /// Create a new jj workspace with a bookmark pointing to a base commit.
    ///
    /// Replaces: `git worktree add -b {bookmark} {path} {base}`
    pub fn create_workspace(&self, path: &Path, bookmark: &BranchName, base: &BranchName) -> Result<()> {
        info!(path = %path.display(), bookmark = %bookmark, base = %base, "Creating jj workspace");

        let root_ws = self.load_root_workspace()?;
        let repo = root_ws.repo_loader().load_at_head()
            .context("Failed to load repo at head")?;

        // Sync jj's view with git refs before resolving bookmarks.
        // Without this, direct git commits (e.g., Wave 1 committed via `git commit`)
        // won't be visible to jj, causing workspaces to fork from stale state.
        let repo = {
            let mut tx = repo.start_transaction();
            let import_opts = GitImportOptions {
                auto_local_bookmark: true,
                abandon_unreachable_commits: false,
                remote_auto_track_bookmarks: std::collections::HashMap::new(),
            };
            match git::import_refs(tx.repo_mut(), &import_opts) {
                Ok(stats) => {
                    if !stats.changed_remote_bookmarks.is_empty() {
                        info!(
                            changed_bookmarks = stats.changed_remote_bookmarks.len(),
                            "Imported git refs into jj"
                        );
                    }
                    tx.commit("import git refs before workspace creation")?
                }
                Err(e) => {
                    warn!(error = %e, "Failed to import git refs (non-fatal, using existing jj state)");
                    repo
                }
            }
        };

        // Resolve the base bookmark to a commit
        let base_ref: &RefName = base.as_str().as_ref();
        let base_target = repo.view().get_local_bookmark(base_ref);
        let base_commit_id = base_target
            .as_normal()
            .ok_or_else(|| anyhow::anyhow!(
                "Base bookmark '{}' not found or is conflicted", base
            ))?;
        let base_commit = repo.store().get_commit(base_commit_id)
            .context("Failed to load base commit")?;

        // Derive workspace name from the path's last component
        let ws_name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("workspace");
        let workspace_name: WorkspaceNameBuf = ws_name.into();

        // Create the workspace directory
        std::fs::create_dir_all(path)
            .with_context(|| format!("Failed to create workspace dir: {}", path.display()))?;

        // Initialize workspace with existing repo
        let working_copy_factory = jj_lib::local_working_copy::LocalWorkingCopyFactory {};
        let (new_ws, new_repo) = Workspace::init_workspace_with_existing_repo(
            path,
            root_ws.repo_path(),
            &repo,
            &working_copy_factory,
            workspace_name,
        )
        .map_err(|e| anyhow::anyhow!("Failed to init workspace: {}", e))?;

        // Start transaction to create bookmark and set up working copy
        let mut tx = new_repo.start_transaction();

        // Create the bookmark pointing to base commit
        let bookmark_ref: &RefName = bookmark.as_str().as_ref();
        tx.repo_mut().set_local_bookmark_target(
            bookmark_ref,
            RefTarget::normal(base_commit_id.clone()),
        );

        // Check out the base commit in the new workspace
        let ws_id = new_ws.workspace_name().to_owned();
        tx.repo_mut()
            .edit(ws_id, &base_commit)
            .context("Failed to edit workspace working copy")?;

        // Export refs to git (keeps gh CLI happy in colocated mode)
        if let Err(e) = git::export_refs(tx.repo_mut()) {
            warn!(error = %e, "Failed to export refs to git");
        }

        tx.repo_mut().rebase_descendants()
            .map_err(|e| anyhow::anyhow!("Failed to rebase descendants: {}", e))?;
        tx.commit(format!("create workspace with bookmark '{}'", bookmark))?;
        info!(path = %path.display(), bookmark = %bookmark, "Workspace created successfully");
        Ok(())
    }

    /// Remove a jj workspace.
    ///
    /// Replaces: `git worktree remove --force {path}`
    pub fn remove_workspace(&self, path: &Path) -> Result<()> {
        info!(path = %path.display(), "Removing jj workspace");

        // Remove the workspace from jj's tracking
        if path.join(".jj").exists() {
            let ws = self.load_workspace_at(path)?;
            let ws_name = ws.workspace_name().to_owned();
            let repo = ws.repo_loader().load_at_head()
                .context("Failed to load repo")?;

            let mut tx = repo.start_transaction();
            tx.repo_mut().remove_wc_commit(&ws_name)?;
            if let Err(e) = git::export_refs(tx.repo_mut()) {
                warn!(error = %e, "Failed to export refs to git after removing workspace");
            }
            tx.repo_mut().rebase_descendants()
                .map_err(|e| anyhow::anyhow!("Failed to rebase descendants: {}", e))?;
            tx.commit(format!("remove workspace '{}'", ws_name.as_str()))?;
        }

        // Remove the directory
        if path.exists() {
            std::fs::remove_dir_all(path)
                .with_context(|| format!("Failed to remove workspace dir: {}", path.display()))?;
        }

        info!(path = %path.display(), "Workspace removed");
        Ok(())
    }

    /// Create a bookmark in the given workspace pointing to its working copy commit,
    /// or to the specified revision if provided.
    pub fn create_bookmark(
        &self,
        workspace_path: &Path,
        name: &BranchName,
        revision: Option<&Revision>,
    ) -> Result<()> {
        info!(bookmark = %name, revision = ?revision, path = %workspace_path.display(), "Creating jj bookmark");

        let ws = self.load_workspace_at(workspace_path)?;
        let repo = ws.repo_loader().load_at_head()?;

        let target_id = match revision {
            Some(rev) => self.resolve_revision(repo.as_ref(), rev)?,
            None => {
                let ws_name = ws.workspace_name();
                repo.view()
                    .get_wc_commit_id(ws_name)
                    .ok_or_else(|| anyhow::anyhow!("No working copy commit for workspace"))?
                    .clone()
            }
        };

        let mut tx = repo.start_transaction();
        let bookmark_ref: &RefName = name.as_str().as_ref();
        tx.repo_mut().set_local_bookmark_target(
            bookmark_ref,
            RefTarget::normal(target_id),
        );

        if let Err(e) = git::export_refs(tx.repo_mut()) {
            error!(error = %e, "Failed to export refs to git after creating bookmark");
        }
        tx.commit(format!("create bookmark '{}'", name))?;

        info!(bookmark = %name, "Bookmark created");
        Ok(())
    }

    /// Delete a bookmark.
    pub fn delete_bookmark(&self, name: &BranchName) -> Result<()> {
        info!(bookmark = %name, "Deleting jj bookmark");

        let ws = self.load_root_workspace()?;
        let repo = ws.repo_loader().load_at_head()?;

        let mut tx = repo.start_transaction();
        let bookmark_ref: &RefName = name.as_str().as_ref();
        tx.repo_mut().set_local_bookmark_target(
            bookmark_ref,
            RefTarget::absent(),
        );

        if let Err(e) = git::export_refs(tx.repo_mut()) {
            error!(error = %e, "Failed to export refs to git after deleting bookmark");
        }
        tx.commit(format!("delete bookmark '{}'", name))?;

        info!(bookmark = %name, "Bookmark deleted");
        Ok(())
    }

    /// Push a bookmark to the remote.
    ///
    /// Uses jj CLI for push — the library API requires complex callback/refspec
    /// setup (matches the approach used in `fetch()`).
    pub fn push_bookmark(&self, workspace_path: &Path, bookmark: &BranchName) -> Result<()> {
        info!(bookmark = %bookmark, path = %workspace_path.display(), "Pushing jj bookmark");

        let output = std::process::Command::new("jj")
            .args(["git", "push", "--bookmark", bookmark.as_str()])
            .current_dir(workspace_path)
            .output()
            .context("Failed to run jj git push")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "jj git push failed");
            return Err(anyhow::anyhow!("git push failed: {}", stderr));
        }

        info!(bookmark = %bookmark, "Bookmark pushed successfully");
        Ok(())
    }

    /// Fetch from remote.
    ///
    /// Replaces: `jj git fetch`
    pub fn fetch(&self, workspace_path: &Path) -> Result<()> {
        info!(path = %workspace_path.display(), "jj git fetch");

        // Use jj CLI for fetch — the library API requires complex callback/refspec setup
        let output = std::process::Command::new("jj")
            .args(["git", "fetch"])
            .current_dir(workspace_path)
            .output()
            .context("Failed to run jj git fetch")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "jj git fetch failed");
            return Err(anyhow::anyhow!("jj git fetch failed: {}", stderr));
        }

        info!("jj git fetch succeeded");
        Ok(())
    }

    /// Get the bookmark name associated with a workspace.
    ///
    /// Tries jj bookmarks first (exact match on wc commit), then falls back
    /// to `git rev-parse --abbrev-ref HEAD` for colocated repos where the
    /// working copy has advanced past the bookmark target.
    pub fn get_workspace_bookmark(&self, workspace_path: &Path) -> Result<Option<String>> {
        let ws = self.load_workspace_at(workspace_path)?;
        let repo = ws.repo_loader().load_at_head()?;

        let ws_name = ws.workspace_name();
        let wc_commit_id = match repo.view().get_wc_commit_id(ws_name) {
            Some(id) => id.clone(),
            None => return self.git_branch_fallback(workspace_path),
        };

        // Find bookmark pointing to working copy commit (exact match)
        for (name, target) in repo.view().local_bookmarks() {
            if target.as_normal() == Some(&wc_commit_id) {
                return Ok(Some(name.as_str().to_string()));
            }
        }

        // Exact match failed — working copy has likely advanced past the bookmark.
        // Fall back to git branch detection (reliable in colocated mode).
        self.git_branch_fallback(workspace_path)
    }

    /// Fall back to `git rev-parse --abbrev-ref HEAD` for branch detection.
    fn git_branch_fallback(&self, workspace_path: &Path) -> Result<Option<String>> {
        let output = std::process::Command::new("git")
            .args(["rev-parse", "--abbrev-ref", "HEAD"])
            .current_dir(workspace_path)
            .output()
            .context("Failed to run git rev-parse")?;
        if output.status.success() {
            let branch = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if branch != "HEAD" && !branch.is_empty() {
                info!(workspace = %workspace_path.display(), branch = %branch, "Resolved branch via git fallback");
                return Ok(Some(branch));
            }
        }
        Ok(None)
    }

    /// Ensure the project has a colocated jj repo. If only .git/ exists, initialize jj.
    ///
    /// Used by `exomonad init`.
    pub fn ensure_colocated(&self) -> Result<()> {
        let jj_dir = self.project_dir.join(".jj");
        let git_dir = self.project_dir.join(".git");

        if jj_dir.exists() {
            info!("jj repo already exists, verifying colocated mode");
            let _ws = self.load_root_workspace()?;
            Ok(())
        } else if git_dir.exists() {
            info!("Initializing jj colocated repo from existing git repo");
            let output = std::process::Command::new("jj")
                .args(["git", "init", "--colocate"])
                .current_dir(&self.project_dir)
                .output()
                .context("Failed to run jj git init --colocate")?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                anyhow::bail!("jj git init --colocate failed: {}", stderr);
            }
            info!("jj colocated repo initialized");
            Ok(())
        } else {
            Err(anyhow::anyhow!(
                "Neither .jj/ nor .git/ found in {}. Initialize a git repo first.",
                self.project_dir.display()
            ))
        }
    }
}
