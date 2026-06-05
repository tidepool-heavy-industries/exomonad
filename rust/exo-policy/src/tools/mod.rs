//! The MCP tools — one module per tool file (the type-per-tool layout). Each tool is a
//! **type** with an `Args` struct (deriving `Deserialize + JsonSchema`), a generic-over-caps
//! `run` whose cap bounds *are* its least-privilege spec, and a hand-written `Tool<R>` adapter
//! (NO macro). Each ships mock-cap unit tests in the same file.
//!
//! The tools, one file each: `messaging` (notify_parent / send_message), `spawn` (the three
//! per-op spawn tools), `merge` (the local on-disk fold), `submit` (a leaf's done / ready-to-
//! merge signal). They are wired into [`role_def`](crate::roles::role_def).

pub mod merge;
pub mod messaging;
pub mod spawn;
pub mod submit;
