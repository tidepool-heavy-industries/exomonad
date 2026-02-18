//! Jujutsu workspace management via jj-lib.
//!
//! Replaces git worktree operations with native jj workspace/bookmark operations.
//! Every mutating method calls `export_refs()` after committing the transaction
//! to keep git refs in sync for `gh` CLI compatibility (colocated mode).

use anyhow::{Context, Result};
use jj_lib::config::{ConfigSource, StackedConfig};
use jj_lib::git;
use jj_lib::git::{GitSubprocessCallback, GitSidebandLineTerminator};
use jj_lib::op_store::RefTarget;
use jj_lib::ref_name::{RefName, RefNameBuf, RemoteName, WorkspaceNameBuf};
use jj_lib::refs::BookmarkPushUpdate;
use jj_lib::repo::Repo;
use jj_lib::settings::UserSettings;
use jj_lib::workspace::{default_working_copy_factories, Workspace};
use std::io;
use std::path::{Path, PathBuf};
use tracing::{error, info};

/// No-op callback for non-interactive git push/fetch operations.
struct SilentCallback;

impl GitSubprocessCallback for SilentCallback {
    fn needs_progress(&self) -> bool {
        false
    }

    fn progress(&mut self, _progress: &jj_lib::git::GitProgress) -> io::Result<()> {
        Ok(())
    }

    fn local_sideband(
        &mut self,
        _message: &[u8],
        _term: Option<GitSidebandLineTerminator>,
    ) -> io::Result<()> {
        Ok(())
    }

    fn remote_sideband(
        &mut self,
        _message: &[u8],
        _term: Option<GitSidebandLineTerminator>,
    ) -> io::Result<()> {
        Ok(())
    }
}

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
                let _ = config.load_file(ConfigSource::User, &user_config);
            }
        }

        // Load repo config
        let repo_config = self.project_dir.join(".jj").join("repo").join("config.toml");
        if repo_config.exists() {
            let _ = config.load_file(ConfigSource::Repo, &repo_config);
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

    /// Create a new jj workspace with a bookmark pointing to a base commit.
    ///
    /// Replaces: `git worktree add -b {bookmark} {path} {base}`
    pub fn create_workspace(&self, path: &Path, bookmark: &str, base: &str) -> Result<()> {
        info!(path = %path.display(), bookmark, base, "Creating jj workspace");

        let root_ws = self.load_root_workspace()?;
        let repo = root_ws.repo_loader().load_at_head()
            .context("Failed to load repo at head")?;

        // Resolve the base bookmark to a commit
        let base_ref: &RefName = base.as_ref();
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
        let bookmark_ref: &RefName = bookmark.as_ref();
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
            error!(error = %e, "Failed to export refs to git");
        }

        tx.commit(format!("create workspace with bookmark '{bookmark}'"))?;
        info!(path = %path.display(), bookmark, "Workspace created successfully");
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

    /// Create a bookmark in the given workspace pointing to its working copy commit.
    pub fn create_bookmark(&self, workspace_path: &Path, name: &str) -> Result<()> {
        info!(bookmark = name, path = %workspace_path.display(), "Creating jj bookmark");

        let ws = self.load_workspace_at(workspace_path)?;
        let repo = ws.repo_loader().load_at_head()?;

        // Get the working copy commit for this workspace
        let ws_name = ws.workspace_name();
        let wc_commit_id = repo
            .view()
            .get_wc_commit_id(ws_name)
            .ok_or_else(|| anyhow::anyhow!("No working copy commit for workspace"))?
            .clone();

        let mut tx = repo.start_transaction();
        let bookmark_ref: &RefName = name.as_ref();
        tx.repo_mut().set_local_bookmark_target(
            bookmark_ref,
            RefTarget::normal(wc_commit_id),
        );

        let _ = git::export_refs(tx.repo_mut());
        tx.commit(format!("create bookmark '{}'", name))?;

        info!(bookmark = name, "Bookmark created");
        Ok(())
    }

    /// Delete a bookmark.
    pub fn delete_bookmark(&self, name: &str) -> Result<()> {
        info!(bookmark = name, "Deleting jj bookmark");

        let ws = self.load_root_workspace()?;
        let repo = ws.repo_loader().load_at_head()?;

        let mut tx = repo.start_transaction();
        let bookmark_ref: &RefName = name.as_ref();
        tx.repo_mut().set_local_bookmark_target(
            bookmark_ref,
            RefTarget::absent(),
        );

        let _ = git::export_refs(tx.repo_mut());
        tx.commit(format!("delete bookmark '{}'", name))?;

        info!(bookmark = name, "Bookmark deleted");
        Ok(())
    }

    /// Push a bookmark to the remote.
    ///
    /// Replaces: `git push -u origin HEAD` / `jj git push --bookmark {bookmark}`
    pub fn push_bookmark(&self, workspace_path: &Path, bookmark: &str) -> Result<()> {
        info!(bookmark, path = %workspace_path.display(), "Pushing jj bookmark");

        let settings = self.load_settings()?;
        let ws = self.load_workspace_at(workspace_path)?;
        let repo = ws.repo_loader().load_at_head()?;

        let mut tx = repo.start_transaction();

        // Export refs first to ensure git sees current state
        let _ = git::export_refs(tx.repo_mut());

        let bookmark_ref: &RefName = bookmark.as_ref();
        let local_target = tx.repo().view().get_local_bookmark(bookmark_ref);
        let remote_name: &RemoteName = "origin".as_ref();
        let remote_symbol = bookmark_ref.to_remote_symbol(remote_name);
        let remote_ref = tx.repo().view().get_remote_bookmark(remote_symbol);

        // Build push targets
        let update = BookmarkPushUpdate {
            old_target: remote_ref.target.as_normal().cloned(),
            new_target: local_target.as_normal().cloned(),
        };
        let bookmark_name: RefNameBuf = bookmark.into();
        let targets = git::GitBranchPushTargets {
            branch_updates: vec![(bookmark_name, update)],
        };

        let subprocess_options = git::GitSubprocessOptions::from_settings(&settings)
            .context("Failed to create git subprocess options")?;

        let mut callback = SilentCallback;
        let _stats = git::push_branches(
            tx.repo_mut(),
            subprocess_options,
            remote_name,
            &targets,
            &mut callback,
        )
        .map_err(|e| anyhow::anyhow!("git push failed: {}", e))?;

        tx.commit(format!("push bookmark '{}'", bookmark))?;
        info!(bookmark, "Bookmark pushed successfully");
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
    /// Looks for a bookmark whose target matches the workspace's working copy commit.
    pub fn get_workspace_bookmark(&self, workspace_path: &Path) -> Result<Option<String>> {
        let ws = self.load_workspace_at(workspace_path)?;
        let repo = ws.repo_loader().load_at_head()?;

        let ws_name = ws.workspace_name();
        let wc_commit_id = match repo.view().get_wc_commit_id(ws_name) {
            Some(id) => id.clone(),
            None => return Ok(None),
        };

        // Find bookmark pointing to working copy commit
        for (name, target) in repo.view().local_bookmarks() {
            if target.as_normal() == Some(&wc_commit_id) {
                return Ok(Some(name.as_str().to_string()));
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
            let settings = self.load_settings()?;
            let (_ws, _repo) = Workspace::init_colocated_git(&settings, &self.project_dir)
                .map_err(|e| anyhow::anyhow!("Failed to init colocated jj repo: {}", e))?;
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
