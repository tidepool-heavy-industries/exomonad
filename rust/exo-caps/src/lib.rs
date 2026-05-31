//! `exo-caps` — the capability seam.
//!
//! Policy (`exo-policy`) is written generic over these traits; the runtime
//! (`exo-runtime`) implements them. Unlike the old WASM boundary this is NOT enforced —
//! policy MAY drop to raw IO as an escape hatch; good caps make it rarely want to.
//! See `docs/design/swarm/03-capabilities.md`.
//!
//! **Status: Wave-0 scaffold (the frozen contract).** The domain types and the
//! `Bus`/`Spawner` seam are settled; newtype constructor *bodies* and the IO cap method
//! *signatures* firm up in Wave 1 (adapt from exomonad-core services). The skeleton
//! `cargo check`s — that is the Wave-0 gate everything forks from.

pub mod error;
pub mod types;
pub mod bus;
pub mod spawner;
pub mod lifecycle;

// IO caps — one trait per file (see `docs/design/swarm/05-crates-and-binary.md`).
pub mod git;
pub mod github;
pub mod tmux;
pub mod fs;
pub mod process;
pub mod log;
pub mod clock;
pub mod kv;

pub use error::{CapError, CapResult};
pub use types::{
    AgentName, AgentType, Branch, ChildKind, ControlKind, InboxPath, Message, MessageBody,
    MessageId, MessageKind, NodeKind, NodePath, PaneId, Persona, SyntheticName,
};
pub use bus::{Addressee, Bus};
pub use spawner::{ForkSpec, GeminiSpec, Spawner, WorkerSpec};
pub use lifecycle::{fold_children, Child, ChildLifecycle, ChildRecord};
pub use git::Git;
pub use github::GitHub;
pub use tmux::Tmux;
pub use fs::Fs;
pub use process::Process;
pub use log::Log;
pub use clock::Clock;
pub use kv::Kv;
