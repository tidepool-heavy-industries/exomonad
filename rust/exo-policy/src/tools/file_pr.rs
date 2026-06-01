//! **P4 leaf.** `file_pr` — create/update a PR over the [`Git`] + [`GitHub`] caps (base branch
//! auto-detected from the dot-separated branch name). A type with an `Args`, a generic-over-caps
//! `run<C: Git + GitHub>(ctx, args) -> CapResult<ToolOutput>`, and a `Tool<R>` adapter. Ships
//! mock-cap unit tests (assert `GitHub::file_pr` recorded with the derived base) in this file.
//! See `docs/design/swarm/04-policy.md`.
//!
//! Empty until the P4 leaf lands.

#![allow(unused_imports)]
use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::{Git, GitHub};
