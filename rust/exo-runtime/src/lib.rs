//! `exo-runtime` — the IO side of the capability seam.
//!
//! Implements the [`exo_caps`] capability traits (`Git`, `GitHub`, `Bus`, `Spawner`,
//! `Tmux`, `Fs`, `Process`, `Log`, `Kv`) against a single concrete `Runtime` struct that
//! carries the node's identity (`EffectContext`). Policy monomorphizes against this `R`.
//!
//! **Status: Wave-0 stub.** Only the crate shell + pinned deps exist — the gate everyone
//! forks from. The Wave-1 Runtime TL scaffolds the `Runtime` struct and the per-cap
//! `impl <Cap> for Runtime` modules (one file each, adapting exomonad-core's
//! Git/GitHub/Tmux services + the jsonl `Bus`). See `docs/design/swarm/06-migration.md`.
