//! **P2 leaf.** `task_list` / `task_get` / `task_update` — the shared-task-list tools. Each
//! is a type with an `Args`, a generic-over-caps `run` (the task store is reached via a cap —
//! likely `Kv` or `Fs`; pick the narrowest that fits and document it), and a `Tool<R>`
//! adapter. Ships mock-cap unit tests in this file. See `docs/design/swarm/04-policy.md`.
//!
//! Empty until the P2 leaf lands.

#![allow(unused_imports)]
use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
