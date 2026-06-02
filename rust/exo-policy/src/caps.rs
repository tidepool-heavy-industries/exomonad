//! [`PolicyCaps`] — a **static bound-union** marker for the concrete runtime `R`. This is
//! NOT a `&dyn Caps` god-trait: there is no trait object, no dynamic dispatch, and tools/
//! hooks still declare their *own* narrow per-cap bounds (`fn run<C: Git>`). The
//! union exists only so the `role_def<R: PolicyCaps>` table can name one bound that
//! guarantees every cap is present at the dispatch boundary. A blanket impl means any type
//! implementing all the caps (the real `exo-runtime::Runtime`, the test `MockRuntime`) is
//! `PolicyCaps` automatically — nothing manual to keep in sync.
//!
//! `Send + Sync + 'static` is required because the sidecar drives tools/hooks across tokio
//! tasks (doc 07: "`R: Send + Sync + 'static` at the dispatch boundary").

use exo_caps::{Bus, Fs, Git, Kv, Log, Process, Spawner, Tmux};

/// The full cap set a runtime must provide to back a role. A blanket impl (below) makes this
/// automatic for any type that impls all the caps — never implemented by hand.
pub trait PolicyCaps:
    Git + Bus + Spawner + Kv + Fs + Tmux + Process + Log + Send + Sync + 'static
{
}

impl<T> PolicyCaps for T where
    T: Git + Bus + Spawner + Kv + Fs + Tmux + Process + Log + Send + Sync + 'static
{
}
