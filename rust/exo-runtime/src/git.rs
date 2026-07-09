//! `impl Git for Runtime` — local git operations via `tokio::process::Command`
//! (NEVER block the tokio executor inside an `async fn`).

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Branch, Git, GitError};
use std::path::Path;
use tokio::process::Command;
use tracing::debug;

#[async_trait]
impl Git for Runtime {
    async fn current_branch(&self) -> Result<Branch, GitError> {
        let output = self.git(&["rev-parse", "--abbrev-ref", "HEAD"]).await?;
        let s = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Branch::new(s).map_err(|e| GitError::Failed {
            op: "current_branch",
            detail: e.to_string(),
        })
    }

    async fn head_sha(&self) -> Result<String, GitError> {
        let output = self.git(&["rev-parse", "HEAD"]).await?;
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    }

    async fn merge_base(&self, refish: &str) -> Result<Option<String>, GitError> {
        // A non-zero exit — `refish` doesn't resolve, or HEAD and `refish` share no history —
        // is `Ok(None)` (caller falls back to another base), not an error.
        let Some(output) = self.git_optional(&["merge-base", "HEAD", refish]).await? else {
            return Ok(None);
        };
        let sha = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Ok((!sha.is_empty()).then_some(sha))
    }

    async fn fork_point(&self) -> Result<Option<String>, GitError> {
        let head = self.head_sha().await?;
        // current branch name (may be detached — then current is empty, fine)
        let cur_out = self
            .git_optional(&["rev-parse", "--abbrev-ref", "HEAD"])
            .await?;
        let current = cur_out
            .as_ref()
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .unwrap_or_default();
        // all local branch short names
        let Some(refs_out) = self
            .git_optional(&["for-each-ref", "--format=%(refname:short)", "refs/heads"])
            .await?
        else {
            return Ok(None);
        };
        let branches = String::from_utf8_lossy(&refs_out.stdout);
        let mut best: Option<(usize, String)> = None; // (ancestor-count, sha)
        for b in branches.lines().map(str::trim).filter(|b| !b.is_empty()) {
            if b == current {
                continue;
            }
            // merge-base HEAD <b>
            let Some(mb_out) = self.git_optional(&["merge-base", "HEAD", b]).await? else {
                continue;
            };
            let mb = String::from_utf8_lossy(&mb_out.stdout).trim().to_string();
            if mb.is_empty() || mb == head {
                continue; // exclude HEAD itself (descendant branches) and no-shared-history
            }
            // recency = number of commits reachable from mb (more ancestors = closer to HEAD)
            let count: usize = match self.git_optional(&["rev-list", "--count", &mb]).await? {
                Some(cnt_out) => String::from_utf8_lossy(&cnt_out.stdout)
                    .trim()
                    .parse()
                    .unwrap_or(0),
                None => 0,
            };
            if best.as_ref().is_none_or(|(c, _)| count > *c) {
                best = Some((count, mb));
            }
        }
        Ok(best.map(|(_, sha)| sha))
    }

    async fn is_clean(&self) -> Result<bool, GitError> {
        let output = self.git(&["status", "--porcelain"]).await?;
        Ok(String::from_utf8_lossy(&output.stdout).trim().is_empty())
    }

    async fn is_ahead_of(&self, base: &str) -> Result<bool, GitError> {
        let Some(output) = self
            .git_optional(&["rev-list", "--count", &format!("{base}..HEAD")])
            .await?
        else {
            tracing::warn!(base = %base, "is_ahead_of: git rev-list failed, treating as not ahead");
            return Ok(false);
        };
        let count: usize = String::from_utf8_lossy(&output.stdout)
            .trim()
            .parse()
            .unwrap_or(0);
        Ok(count > 0)
    }

    async fn is_behind(&self, base: &str) -> Result<bool, GitError> {
        // Mirror of `is_ahead_of` with the range reversed: `HEAD..base` counts commits reachable
        // from `base` but not HEAD — i.e. the parent advanced since we forked. Fail-open to
        // `Ok(false)` when `base` doesn't resolve (e.g. the root's `root`, which is never a real
        // git branch), so a submit is never blocked over an unresolvable parent name.
        let Some(output) = self
            .git_optional(&["rev-list", "--count", &format!("HEAD..{base}")])
            .await?
        else {
            tracing::warn!(base = %base, "is_behind: git rev-list failed, treating as not behind");
            return Ok(false);
        };
        let count: usize = String::from_utf8_lossy(&output.stdout)
            .trim()
            .parse()
            .unwrap_or(0);
        Ok(count > 0)
    }

    async fn fetch(&self) -> Result<(), GitError> {
        self.git(&["fetch"]).await?;
        Ok(())
    }

    async fn merge(&self, branch: &Branch) -> Result<(), GitError> {
        // Local fold of a child's branch into this node's branch; non-interactive so a clean
        // merge commit doesn't block on $EDITOR.
        match self.git(&["merge", "--no-edit", branch.as_str()]).await {
            Ok(_) => Ok(()),
            Err(e) => {
                // A conflict (or other failure) leaves the worktree half-merged (MERGE_HEAD +
                // markers), which would then wedge this node's own stop clean-gate. Restore a
                // clean tree before surfacing the error so the parent stays un-wedged and can
                // re-decompose. Best-effort: ignore the abort's own result.
                let _ = self.git(&["merge", "--abort"]).await;
                Err(match e {
                    GitError::Failed { detail, .. } => GitError::Failed {
                        op: "merge",
                        detail,
                    },
                    other => other,
                })
            }
        }
    }

    async fn worktree_add(&self, branch: &Branch, at: &Path) -> Result<(), GitError> {
        self.git(&[
            "worktree",
            "add",
            "-b",
            branch.as_str(),
            &at.to_string_lossy(),
        ])
        .await?;
        Ok(())
    }

    async fn worktree_remove(&self, at: &Path) -> Result<(), GitError> {
        // `--force` is the cap's contracted reclaim semantics: discard whatever state the
        // worktree DIRECTORY holds (dirty files, untracked artifacts) — the branch ref is
        // untouched, so committed work survives. Without it, a child that left any dirt
        // would wedge both callers (birth rollback, post-merge reclaim).
        self.git(&["worktree", "remove", "--force", &at.to_string_lossy()])
            .await?;
        Ok(())
    }
}

impl Runtime {
    async fn git(&self, args: &[&str]) -> Result<std::process::Output, GitError> {
        // DEBUG (not info/error): `git()` is generic and some callers treat a non-zero exit as
        // expected control flow (e.g. `fork_point` probes merge-bases that legitimately fail), so
        // an error-level log here would cry wolf. The stderr still rides in `GitError::Failed`, so
        // callers that treat the failure as fatal surface it themselves.
        debug!(cwd = %self.working_dir().display(), args = ?args, "git: exec");
        let output = Command::new("git")
            .current_dir(self.working_dir())
            .args(args)
            .output()
            .await?;

        if !output.status.success() {
            debug!(
                cwd = %self.working_dir().display(),
                args = ?args,
                exit = ?output.status.code(),
                stderr = %String::from_utf8_lossy(&output.stderr).trim(),
                "git: non-zero exit"
            );
            return Err(GitError::Failed {
                op: "git",
                detail: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            });
        }

        Ok(output)
    }

    /// Run git and treat a non-zero exit as `Ok(None)` rather than an error. Use for
    /// operations where "not found" or "no shared history" is expected control flow.
    async fn git_optional(&self, args: &[&str]) -> Result<Option<std::process::Output>, GitError> {
        debug!(cwd = %self.working_dir().display(), args = ?args, "git_optional: exec");
        let output = Command::new("git")
            .current_dir(self.working_dir())
            .args(args)
            .output()
            .await?;
        if !output.status.success() {
            return Ok(None);
        }
        Ok(Some(output))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, NodePath, PaneId};
    use std::process::Command;
    use tempfile::tempdir;

    fn run_git(dir: &std::path::Path, args: &[&str]) {
        let ok = Command::new("git")
            .current_dir(dir)
            .args(args)
            .status()
            .unwrap()
            .success();
        assert!(ok, "git {:?} failed", args);
    }

    fn runtime_at(dir: &std::path::Path) -> Runtime {
        Runtime::new(
            NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
            Branch::new("main".into()).unwrap(),
            dir.to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        )
    }

    fn init_repo(dir: &std::path::Path) {
        run_git(dir, &["init", "-q", "-b", "main"]);
        run_git(dir, &["config", "user.email", "t@t"]);
        run_git(dir, &["config", "user.name", "t"]);
        std::fs::write(dir.join("f.txt"), "base\n").unwrap();
        run_git(dir, &["add", "f.txt"]);
        run_git(dir, &["commit", "-q", "-m", "base"]);
    }

    #[tokio::test]
    async fn is_ahead_of_detects_commits_ahead_of_base() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        run_git(p, &["checkout", "-q", "-b", "feature"]);
        std::fs::write(p.join("extra.txt"), "extra\n").unwrap();
        run_git(p, &["add", "extra.txt"]);
        run_git(p, &["commit", "-q", "-m", "extra"]);

        let rt = runtime_at(p);
        assert!(rt.is_ahead_of("main").await.unwrap());
    }

    #[tokio::test]
    async fn is_ahead_of_false_when_not_ahead() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        let rt = runtime_at(p);
        assert!(!rt.is_ahead_of("main").await.unwrap());
    }

    #[tokio::test]
    async fn is_ahead_of_false_on_unresolved_base() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        let rt = runtime_at(p);
        assert!(!rt.is_ahead_of("nonexistent").await.unwrap());
    }

    #[tokio::test]
    async fn is_behind_detects_base_advanced_past_fork_point() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        // feature forks from main, then main advances (a sibling merged, say).
        run_git(p, &["checkout", "-q", "-b", "feature"]);
        run_git(p, &["checkout", "-q", "main"]);
        std::fs::write(p.join("sibling.txt"), "merged\n").unwrap();
        run_git(p, &["add", "sibling.txt"]);
        run_git(p, &["commit", "-q", "-m", "sibling merged"]);
        run_git(p, &["checkout", "-q", "feature"]);

        let rt = runtime_at(p);
        assert!(
            rt.is_behind("main").await.unwrap(),
            "feature should be behind main after main advanced"
        );
    }

    #[tokio::test]
    async fn is_behind_false_when_up_to_date() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        // feature is ahead of main but main hasn't moved → not behind.
        run_git(p, &["checkout", "-q", "-b", "feature"]);
        std::fs::write(p.join("extra.txt"), "extra\n").unwrap();
        run_git(p, &["add", "extra.txt"]);
        run_git(p, &["commit", "-q", "-m", "extra"]);

        let rt = runtime_at(p);
        assert!(!rt.is_behind("main").await.unwrap());
    }

    #[tokio::test]
    async fn is_behind_false_on_unresolved_base() {
        // The root's `root` parent name is never a real branch — must fail open, never block.
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        let rt = runtime_at(p);
        assert!(!rt.is_behind("root").await.unwrap());
    }

    #[tokio::test]
    async fn merge_conflict_aborts_and_leaves_clean_tree() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        // feature: conflicting change
        run_git(p, &["checkout", "-q", "-b", "feature"]);
        std::fs::write(p.join("f.txt"), "feature\n").unwrap();
        run_git(p, &["commit", "-q", "-am", "feature"]);
        // main: conflicting change
        run_git(p, &["checkout", "-q", "main"]);
        std::fs::write(p.join("f.txt"), "main\n").unwrap();
        run_git(p, &["commit", "-q", "-am", "main"]);

        let rt = runtime_at(p);
        match rt.merge(&Branch::new("feature".into()).unwrap()).await {
            Err(GitError::Failed { op, .. }) => assert_eq!(op, "merge"),
            other => panic!("expected a labelled merge failure, got {:?}", other),
        }
        // The conflict was --abort'd: the worktree is clean again (stop-gate won't wedge).
        assert!(
            rt.is_clean().await.unwrap(),
            "worktree should be clean after an aborted conflicting merge"
        );
    }

    #[tokio::test]
    async fn merge_succeeds_brings_in_branch() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        run_git(p, &["checkout", "-q", "-b", "feature"]);
        std::fs::write(p.join("g.txt"), "new\n").unwrap();
        run_git(p, &["add", "g.txt"]);
        run_git(p, &["commit", "-q", "-m", "feature"]);
        run_git(p, &["checkout", "-q", "main"]);

        let rt = runtime_at(p);
        rt.merge(&Branch::new("feature".into()).unwrap())
            .await
            .unwrap();
        assert!(
            p.join("g.txt").exists(),
            "merge should fold in feature's file"
        );
        assert!(rt.is_clean().await.unwrap());
    }
}
