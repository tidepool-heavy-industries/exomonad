//! **P1 leaf.** `notify_parent` + `send_message` — the messaging tools, over the [`Bus`]
//! cap (port from `teams-mcp`). Each is a type with an `Args` (derive `Deserialize +
//! JsonSchema`), a generic-over-caps `run<C: Bus>(ctx, args) -> CapResult<ToolOutput>`, and a
//! hand-written `Tool<R>` adapter. Ships mock-cap unit tests (assert `Bus::deliver` recorded
//! with the right `Addressee`/`Message`) in this file. See `docs/design/swarm/04-policy.md`.
//!
//! Empty until the P1 leaf lands.

#![allow(unused_imports)]
use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::Bus;
