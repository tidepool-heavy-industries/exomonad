//! The MCP tools — one module per tool file (the type-per-tool layout). Each tool is a
//! **type** with an `Args` struct (deriving `Deserialize + JsonSchema`) and an `impl Tool<R>`
//! (the typed authoring trait) whose cap bounds are its least-privilege spec. The framework's
//! `Adapter` (via `tool(X)` in the roster) handles JSON erasure. No per-tool adapter, no macro.
//! Each ships mock-cap unit tests in the same file.
//!
//! The tools, one file each: `messaging` (notify_parent / send_message), `spawn` (the three
//! per-op spawn tools), `merge` (the local on-disk fold), `submit` (a leaf's done / ready-to-
//! merge signal), `tree` (the caller's subtree + parent + liveness), `verdict` (the reviewer's
//! approve/deny/changes signal). They are wired into [`role_def`](crate::roles::role_def).

pub mod merge;
pub mod messaging;
pub mod spawn;
pub mod submit;
pub mod tree;
pub mod verdict;
