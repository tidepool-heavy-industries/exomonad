//! `Git` capability — local git operations. Signatures firm up in Wave 1 (adapt
//! exomonad-core `GitService`).

use crate::error::CapResult;
use crate::types::Branch;
use async_trait::async_trait;
use std::path::Path;

#[async_trait]
pub trait Git {
    async fn current_branch(&self) -> CapResult<Branch>;
    async fn is_clean(&self) -> CapResult<bool>;
    async fn worktree_add(&self, branch: &Branch, at: &Path) -> CapResult<()>;
    async fn worktree_remove(&self, at: &Path) -> CapResult<()>;
}
