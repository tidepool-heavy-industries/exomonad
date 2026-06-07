//! [`ExoDomain`] — the `exo` domain's [`Exomonad`] impl, the binary's composition root for the
//! engine. A ZST that fixes the four associated types to the concrete `exo` choices (`Runtime` caps,
//! `NodeKind` roles, the [`ReviewSystem`] inter-node payload, the [`ExoSpawn`] spawn intent) and
//! resolves roles / handles system messages through the lib. The binary monomorphizes the whole
//! engine once here (`run_node::<ExoDomain>`); nothing else names a concrete domain.
//!
//! This is the one place that fixes `Caps = Runtime`, so it is bin-only (it links `exo-runtime`);
//! the lib stays generic over the caps and IO-free. The substantive logic it points at —
//! [`role_def`](exo::role_def) and [`handle_review_system`](exo::handle_review_system) — lives in
//! the lib and is unit-tested there against mocks.

use exo::{handle_review_system, role_def, ExoRole, ExoSpawn, ReviewSystem};
use exo_caps::{CapResult, Persona};
use exo_framework::{BoxFuture, Exomonad, RoleDef, SystemCtx, SystemOutcome};
use exo_runtime::Runtime;

/// The `exo` domain — the ZST the binary instantiates the engine at.
pub struct ExoDomain;

impl Exomonad for ExoDomain {
    type Caps = Runtime;
    type Role = ExoRole;
    type System = ReviewSystem;
    type Spawn = ExoSpawn;

    fn role_def(role: ExoRole) -> RoleDef<Runtime> {
        role_def::<Runtime>(role)
    }

    fn handle_system<'a, C: SystemCtx>(
        ctx: &'a C,
        from: &'a Persona,
        system: &'a ReviewSystem,
    ) -> BoxFuture<'a, CapResult<SystemOutcome>> {
        Box::pin(handle_review_system(ctx, from, system))
    }
}
