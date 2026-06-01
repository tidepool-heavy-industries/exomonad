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

pub mod bus;
pub mod error;
pub mod lifecycle;
pub mod papers;
pub mod paths;
pub mod spawner;
pub mod types;

// IO caps — one trait per file (see `docs/design/swarm/05-crates-and-binary.md`).
pub mod fs;
pub mod git;
pub mod github;
pub mod kv;
pub mod log;
pub mod process;
pub mod tmux;

pub use bus::{Addressee, Bus, BusError};
pub use error::{CapError, CapResult};
pub use fs::{Fs, FsError};
pub use git::{Git, GitError};
pub use github::{GitHub, GitHubError};
pub use kv::{Kv, KvError};
pub use lifecycle::{fold_children, Child, ChildLifecycle, ChildRecord};
pub use log::Log;
pub use papers::NodePapers;
pub use process::{Process, ProcessError};
pub use spawner::{ForkSpec, GeminiSpec, SpawnError, Spawner, WorkerSpec};
pub use tmux::{Tmux, TmuxError};
pub use types::{
    AgentName, AgentType, Branch, ChildKind, ControlKind, InboxPath, IngestionEntry, Message,
    MessageBody, MessageKind, NodeKind, NodePath, PaneId, Persona, Summary, SyntheticName,
};
