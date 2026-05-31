//! `Log` capability — structured logging to a file at the **git worktree-root** (the one
//! dir not removed when a worktree is reclaimed). OTel was dropped (broken OTLP export).

pub trait Log {
    fn info(&self, msg: &str);
    fn error(&self, msg: &str);
}
