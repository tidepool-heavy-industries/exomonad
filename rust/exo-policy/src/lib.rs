//! `exo-policy` — the Bucket-C decision logic (tools / roles / hooks / events).
//!
//! Written generic over the [`exo_caps`] capability traits it needs (no `dyn Caps`), so
//! least-privilege is compiler-checked and every tool is unit-testable against mock caps
//! with zero IO. No phases, no DSL, no macros. See `docs/design/swarm/04-policy.md`.
//!
//! **Status: Wave-0 stub.** Only the crate shell + pinned deps exist. The Wave-3 Policy
//! TL — gated solely on the exo-caps signature-freeze (done), so it runs concurrent with
//! the Runtime TL — scaffolds the policy contract (`Tool<R>`, `RoleDef<R>`, `HookDecision`,
//! `EventAction`, `WorldEvent`) and forks one Gemini leaf per tool file. See
//! `docs/design/swarm/06-migration.md`.
