//! `exo-caps` — the capability seam.
//!
//! Policy (`exo`) is written generic over these traits; the runtime
//! (`exo-runtime`) implements them. Unlike the old WASM boundary this is NOT enforced —
//! policy MAY drop to raw IO as an escape hatch; good caps make it rarely want to.
//!
//! **Status: Wave-0 scaffold (the frozen contract).** The domain types and the
//! `Bus`/`Spawner` seam are settled; newtype constructor *bodies* and the IO cap method
//! *signatures* firm up in Wave 1 (adapt from exomonad-core services). The skeleton
//! `cargo check`s — that is the Wave-0 gate everything forks from.

pub mod bus;
pub mod error;
pub mod hook_rpc;
pub mod invocation;
pub mod lifecycle;
pub mod liveness;
pub mod papers;
pub mod paths;
pub mod spawner;
pub mod topology;
pub mod types;

// IO caps — one trait per file.
pub mod fs;
pub mod git;
pub mod kv;
pub mod log;
pub mod process;
pub mod tmux;

pub use bus::{Addressee, Bus, BusError};
pub use error::{CapError, CapResult};
pub use fs::{Fs, FsError};
pub use git::{Git, GitError};
pub use hook_rpc::{HookEvent, HookRequest, HookVerdict};
pub use kv::{Kv, KvError};
pub use lifecycle::{fold_children, Child, ChildLifecycle, ChildRecord};
pub use liveness::ChildLiveness;
pub use log::Log;
pub use papers::NodePapers;
pub use process::{Process, ProcessError};
pub use spawner::{ForkSpec, GeminiSpec, SpawnError, Spawner, WorkerSpec};
pub use tmux::{Tmux, TmuxError};
pub use topology::{Topology, TopologyError, TopologyView, TreeNode};
pub use types::{
    AgentName, AgentType, Branch, ChildKind, ChildStatus, ControlKind, InboxPath, IngestionEntry,
    Message, MessageBody, MessageKind, NodeKind, NodePath, NodeStatus, PaneId, Persona, Summary,
    SyntheticName, SystemMessage,
};
