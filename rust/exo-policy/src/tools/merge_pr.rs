//! **P5 leaf — COMPLEX.** `merge_pr` — merge a child's PR over [`Git`] + [`GitHub`]. Ports
//! the dense `MergePR.hs` (~364 LOC): rebase-on-conflict, retry, and the guard heuristics
//! (CI-green, review-clean). A type with an `Args`, a generic-over-caps `run<C: Git + GitHub>`,
//! and a `Tool<R>` adapter. Ships mock-cap unit tests covering the happy path AND the
//! error/guard branches (forced-failure mock). If the conflict/retry path proves gnarly,
//! sub-split rather than cram. See `docs/design/swarm/04-policy.md`.
//!
//! Empty until the P5 leaf lands.

#![allow(unused_imports)]
use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::{Git, GitHub};
