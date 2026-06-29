//! `exo-caps` — the capability seam.
//!
//! Policy (`exo`) is written generic over these traits; the runtime
//! (`exo-runtime`) implements them. Unlike the old WASM boundary this is NOT enforced —
//! policy MAY drop to raw IO as an escape hatch; good caps make it rarely want to.
//!
//! The contract is settled: nine caps (one trait per file), validated domain newtypes,
//! and the message/identity vocabulary. `exo-runtime::Runtime` implements every cap;
//! `exo::testing::MockRuntime` mocks every cap.
//!
//! The caps form **two tiers**. *Primitive* caps (`Git`, `Tmux`, `Fs`, `Kv`, `Process`) each
//! own one external resource. *Composite* caps (`Spawner`, `Bus`, `Topology`, `ChildLiveness`)
//! orchestrate across resources and declare the primitives they stand on as **supertraits** —
//! so an impl of a composite must also impl its primitives, and a composite can never quietly
//! re-shell a domain a primitive already owns.

pub mod bus;
pub mod domain;
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
pub mod process;
pub mod tmux;

pub use bus::{Addressee, Bus, BusError};
pub use domain::{deliver_domain, DomainSystem, RoleKind, SpawnSpec};
pub use error::{CapError, CapResult};
pub use fs::{Fs, FsError};
pub use git::{Git, GitError};
pub use hook_rpc::{HookEvent, HookRequest, HookVerdict};
pub use kv::{Kv, KvError};
pub use lifecycle::{fold_children, Child, ChildLifecycle, ChildRecord};
pub use liveness::ChildLiveness;
pub use papers::{NodePapers, RoleRecord};
pub use process::{Process, ProcessError};
pub use spawner::{SpawnError, Spawner};
pub use tmux::{Tmux, TmuxError};
pub use topology::{Topology, TopologyError, TopologyView, TreeNode};
pub use types::{
    AgentName, AgentType, Branch, ChildKind, ChildStatus, ControlKind, DomainPayload, InboxPath,
    IngestionEntry, Lifecycle, Message, MessageBody, MessageKind, NodePath, NodeStatus, PaneId,
    Persona, Reason, ShutdownStatus, Summary, SyntheticName, ToolName,
};
