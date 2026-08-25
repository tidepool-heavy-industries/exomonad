//! The MCP tools — one module per tool file (the type-per-tool layout). Each tool is a
//! **type** with an `Args` struct (deriving `Deserialize + JsonSchema`) and an `impl Tool<R>`
//! (the typed authoring trait) whose cap bounds are its least-privilege spec. The framework's
//! `Adapter` (via `tool(X)` in the roster) handles JSON erasure. No per-tool adapter, no macro.
//! Each ships mock-cap unit tests in the same file.
//!
//! The tools, one file each: `messaging` (notify_parent / send_message), `broadcast` (flat
//! fan-out to every live direct child), `spawn` (the three per-op spawn tools), `merge` (the
//! local on-disk fold), `submit` (a leaf's done / ready-to-merge signal), `tree` (the caller's
//! subtree + parent + liveness), `verdict` (the reviewer's approve/deny/changes signal),
//! `request_review` (mid-flight review-gate flip) + `amend_boundary` (fix a recorded file
//! boundary) — the two parent-side amend tools. They are wired into
//! [`role_def`](crate::roles::role_def).

pub mod amend_boundary;
pub mod broadcast;
pub mod dismiss;
pub mod merge;
pub mod messaging;
pub mod request_review;
pub mod spawn;
pub mod submit;
pub mod tree;
pub mod verdict;
