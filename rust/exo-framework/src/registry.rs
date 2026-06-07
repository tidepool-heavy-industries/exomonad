//! [`RoleRegistry`] — the seam that inverts the engine→domain dependency.
//!
//! The engine (`exo-node`) must not know the concrete roles. Instead of calling a domain
//! `role_def(kind)` directly, it holds a `RoleRegistry<R>` **injected by the binary** and asks
//! it to resolve a [`RoleDef<R>`](crate::roles::RoleDef) per [`NodeKind`]. The `exo` usage crate
//! builds the registry from its own roster and hands it in. After this, `exo-node` depends only
//! on `exo-framework` for these types and never on the domain crate.
//!
//! The registry is a thin wrapper over a `fn(NodeKind) -> RoleDef<R>` pointer — monomorphized at
//! the concrete `R` when the binary instantiates it — so it is `Copy` and carries no state.

use crate::roles::RoleDef;
use exo_caps::NodeKind;

/// Resolves a [`RoleDef<R>`] for a given [`NodeKind`]. Injected into the engine by the binary;
/// constructed from the domain's roster via [`RoleRegistry::new`].
pub struct RoleRegistry<R: Send + Sync> {
    resolver: fn(NodeKind) -> RoleDef<R>,
}

impl<R: Send + Sync> RoleRegistry<R> {
    /// Wrap a domain roster fn (e.g. `exo::role_def::<R>`) as a registry.
    pub const fn new(resolver: fn(NodeKind) -> RoleDef<R>) -> Self {
        Self { resolver }
    }

    /// Resolve the role definition (tools + hooks) for `kind`.
    pub fn role_def(&self, kind: NodeKind) -> RoleDef<R> {
        (self.resolver)(kind)
    }
}

// A `fn` pointer is always `Copy`/`Clone` regardless of `R`; the derives would spuriously demand
// `R: Clone`, so impl by hand.
impl<R: Send + Sync> Clone for RoleRegistry<R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<R: Send + Sync> Copy for RoleRegistry<R> {}

impl<R: Send + Sync> std::fmt::Debug for RoleRegistry<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RoleRegistry").finish_non_exhaustive()
    }
}
