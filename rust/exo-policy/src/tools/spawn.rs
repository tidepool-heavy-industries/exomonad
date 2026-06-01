//! **P3 leaf.** The three **per-op** spawn tools over the [`Spawner`] cap: `spawn_worker`
//! (→ Inline/Worker/Gemini), `spawn_gemini` (→ Worktree/Dev/Gemini), `fork_wave`
//! (→ Worktree/Tl/Claude). Each is a thin wrapper type: an `Args` carrying ONLY task content
//! (the `(role, agent_type, kind)` triple is fixed by which op, never a caller field), a
//! generic-over-caps `run<C: Spawner>`, and a `Tool<R>` adapter. Ships mock-cap unit tests
//! (assert the right `Spawner` method recorded) in this file. See `docs/design/swarm/03-capabilities.md`.
//!
//! Empty until the P3 leaf lands.

#![allow(unused_imports)]
use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::Spawner;
