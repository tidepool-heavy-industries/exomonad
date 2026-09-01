//! `exo-runtime` — the IO side of the capability seam.
//!
//! Implements the [`exo_caps`] capability traits (`Git`, `Bus`, `Spawner`, `Tmux`, `Fs`,
//! `Process`, `Log`, `Kv`) against a single concrete [`Runtime`] struct that carries the
//! node's identity. Policy monomorphizes against this `R`. Convergence is on-disk (local
//! `git merge`); there is no GitHub cap.
//!
//! One file per cap (`impl <Cap> for Runtime`), so cap leaves never collide. The `Runtime`
//! struct + accessors live in [`runtime`]; every other module is a trait impl.
//!
//! **Cap set.** All `exo-caps` traits are implemented. The "provisional" caps
//! (`Tmux`/`Fs`/`Process`/`Log`) are kept because each has a *runtime* consumer — `Bus` uses
//! `Tmux::paste` + `Fs` side-files; `Spawner` uses `Tmux` panes + `Process`; `Log` is
//! universal. They are runtime-internal (not policy-facing), but NOT zero-consumer.
//!
//! `Runtime` implements every cap (git, tmux, fs/process/log/kv, bus, Spawner). The `Bus`
//! append half is done here; the read/cursor/`notify`-watch half is the inbound loop
//! (`exo-node`).

mod runtime;
mod util;

mod bus;
pub mod codex;
mod fs;
mod git;
mod kv;
mod liveness;
pub mod node_config;
mod process;
pub mod protocol;
pub mod session_boot;
mod spawner;
mod tmux;
mod topology;

pub use node_config::write_node_agent_config;
pub use runtime::Runtime;
pub use session_boot::boot_root_session;
pub use spawner::retry_teardown;
