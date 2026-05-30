//! How a caller points exo-scry at a process.

/// What to resolve the active team *for*.
#[derive(Debug, Clone)]
pub enum ProbeTarget {
    /// This process — walk ancestors to the owning Claude Code session.
    SelfProcess,
    /// An explicit pid — if it isn't Claude itself, walk ancestors to find it.
    Pid(i32),
    /// A tmux pane id (e.g. `"%306"`) — resolve its pane pid, then walk the
    /// subtree *down* to the Claude Code process the pane is running.
    TmuxPane(String),
}
