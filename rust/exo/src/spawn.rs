//! [`ExoSpawn`] — the `exo` domain's spawn intent (its `D::Spawn`), a plain struct implementing
//! [`SpawnSpec`]. The domain's spawn tools each build one of these with the `(role, kind)` pair
//! fixed (so an illegal pairing is unnameable at the tool boundary); the engine's single generic
//! `Spawner::spawn` consumes it. The rendered prompt is built by the tool (the spec just carries it).
//!
//! Until the `Spawner` collapse (P4) wires the engine to `spawn(D::Spawn)`, this is the concrete
//! `Exomonad::Spawn` type that proves the seam; the per-op tool wrappers populate it in P4.

use exo_caps::{AgentName, ChildKind, Fs, NodeKind, SpawnSpec};
use std::path::PathBuf;

/// One spawn intent: which role/kind to birth, the (optional) name, the rendered task body, and the
/// opt-in context-inheritance flag.
#[derive(Debug, Clone)]
pub struct ExoSpawn {
    /// The role the child is born as (fixes its tool set + launch backend).
    pub role: NodeKind,
    /// Own worktree vs inline pane.
    pub kind: ChildKind,
    /// Explicit child name, or `None` to auto-generate from `name_prefix`.
    pub name: Option<AgentName>,
    /// Auto-increment prefix used when `name` is `None`.
    pub name_prefix: &'static str,
    /// The fully-rendered prompt/task body delivered to the child.
    pub task: String,
    /// Opt-in Claude context inheritance (`--resume --fork-session`).
    pub fork_session: bool,
}

impl SpawnSpec for ExoSpawn {
    type Role = NodeKind;

    fn role(&self) -> NodeKind {
        self.role
    }
    fn child_kind(&self) -> ChildKind {
        self.kind
    }
    fn name(&self) -> Option<AgentName> {
        self.name.clone()
    }
    fn name_prefix(&self) -> &str {
        self.name_prefix
    }
    fn fork_session(&self) -> bool {
        self.fork_session
    }
    fn into_task(self) -> String {
        self.task
    }
}

/// Render a structured spec (task + the optional sections) into the single prompt body an
/// [`ExoSpawn`] carries. Domain-owned (moved out of the runtime with the Spawner collapse) — the
/// engine births a child from an already-rendered task, so the formatting is the domain's concern.
#[allow(clippy::too_many_arguments)]
pub fn render_spec_prompt(
    task: &str,
    read_first: &[String],
    steps: &[String],
    verify: &[String],
    boundary: &[String],
    context: Option<&String>,
    done_criteria: &[String],
) -> String {
    let mut prompt = task.to_string();

    if !boundary.is_empty() {
        prompt.push_str("\n\nBOUNDARY (DO NOT):");
        for b in boundary {
            prompt.push_str(&format!("\n- {b}"));
        }
    }
    if !read_first.is_empty() {
        prompt.push_str("\n\nREAD FIRST:");
        for rf in read_first {
            prompt.push_str(&format!("\n- {rf}"));
        }
    }
    if !steps.is_empty() {
        prompt.push_str("\n\nSTEPS:");
        for (i, step) in steps.iter().enumerate() {
            prompt.push_str(&format!("\n{}. {}", i + 1, step));
        }
    }
    if !verify.is_empty() {
        prompt.push_str("\n\nVERIFY:");
        for v in verify {
            prompt.push_str(&format!("\n- {v}"));
        }
    }
    if let Some(ctx) = context {
        if !ctx.is_empty() {
            prompt.push_str("\n\nCONTEXT:\n");
            prompt.push_str(ctx);
        }
    }
    if !done_criteria.is_empty() {
        prompt.push_str("\n\nDONE CRITERIA:");
        for d in done_criteria {
            prompt.push_str(&format!("\n- {d}"));
        }
    }
    prompt
}

/// Persist a spawned Worktree child's spec as its `.exo/acceptance.md` (the bar its later
/// `submit_branch` hands to a reviewer). Domain-owned via the `Fs` cap (relocated out of the runtime
/// birth, which no longer knows the review-gate's filename). Best-effort — the path is relative to
/// the spawning node's worktree (`birth` already created the child's `.exo/`); a write failure only
/// costs the reviewer its acceptance context.
pub async fn write_acceptance<C: Fs>(ctx: &C, child: &AgentName, spec_task: &str) {
    let path = PathBuf::from(format!(".exo/worktrees/{}/.exo/acceptance.md", child.as_str()));
    if let Err(e) = ctx.write_atomic(&path, spec_task.as_bytes()).await {
        tracing::warn!(
            "failed to persist .exo/acceptance.md for {}: {e}",
            child.as_str()
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_spec_prompt_sections() {
        let p = render_spec_prompt(
            "do work",
            &["README.md".into()],
            &["step 1".into(), "step 2".into()],
            &["cargo test".into()],
            &["no delete".into()],
            Some(&"some context".to_string()),
            &["all green".into()],
        );
        assert!(p.contains("do work"));
        assert!(p.contains("BOUNDARY (DO NOT):\n- no delete"));
        assert!(p.contains("READ FIRST:\n- README.md"));
        assert!(p.contains("STEPS:\n1. step 1\n2. step 2"));
        assert!(p.contains("VERIFY:\n- cargo test"));
        assert!(p.contains("CONTEXT:\nsome context"));
        assert!(p.contains("DONE CRITERIA:\n- all green"));
        assert_eq!(render_spec_prompt("task", &[], &[], &[], &[], None, &[]), "task");
    }
}
