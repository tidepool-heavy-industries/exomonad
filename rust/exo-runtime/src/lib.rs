//! `exo-runtime` — the IO side of the capability seam.
//!
//! Implements the [`exo_caps`] capability traits (`Git`, `GitHub`, `Bus`, `Spawner`,
//! `Tmux`, `Fs`, `Process`, `Log`, `Kv`) against a single concrete [`Runtime`] struct that
//! carries the node's identity. Policy monomorphizes against this `R`.
//!
//! One file per cap (`impl <Cap> for Runtime`), so cap leaves never collide. The `Runtime`
//! struct + accessors live in [`runtime`]; every other module is a trait impl.
//!
//! **Cap set (Wave 1).** All nine `exo-caps` traits are implemented. The "provisional"
//! caps from doc 03 (`Tmux`/`Fs`/`Process`/`Log`) are kept because each has a *runtime*
//! consumer — `Bus` uses `Tmux::paste` + `Fs` side-files; `Spawner` uses `Tmux` panes +
//! `Process`; `Log` is universal. They are runtime-internal (not policy-facing), but NOT
//! zero-consumer, so none is cut. (Cutting one would also churn the frozen `exo-caps`
//! contract that the Policy TL is forking from concurrently — out of scope for Wave 1.)
//!
//! **Status: Wave 1 complete.** `Runtime` implements every cap (R1 git/github, R2 tmux,
//! R3 fs/process/log/kv, R4 bus; Spawner S1–S3). The `Bus` append half is done here; the
//! read/cursor/`notify`-watch half is the Wave-2 inbound loop. See
//! `docs/design/swarm/06-migration.md`.

mod runtime;

mod bus;
mod fs;
mod git;
mod github;
mod kv;
mod log;
mod process;
mod spawner;
mod tmux;

pub use runtime::Runtime;
