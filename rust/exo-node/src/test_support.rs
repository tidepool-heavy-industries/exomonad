//! Shared test fixtures for the engine. The inversion means `exo-node` must not depend on the
//! domain crate, so its tests inject a trivial roster here — the same seam production fills with
//! `exo::roster()`. Each gate is a minimal allow/default; tests that need specific hook behavior
//! build their own `RoleDef` inline.

use exo_caps::NodeKind;
use exo_framework::{
    BoxFuture, HookDecision, HookInput, RoleDef, RoleRegistry, SessionStartOutput, StopDecision,
};
use exo_runtime::Runtime;

pub(crate) fn test_pre_tool_use<'a>(
    _: &'a Runtime,
    _: &'a HookInput,
) -> BoxFuture<'a, HookDecision> {
    Box::pin(async { HookDecision::Allow })
}

pub(crate) fn test_stop(_: &Runtime) -> BoxFuture<'_, StopDecision> {
    Box::pin(async { StopDecision::Allow })
}

pub(crate) fn test_session_start(_: &Runtime) -> BoxFuture<'_, SessionStartOutput> {
    Box::pin(async { SessionStartOutput::default() })
}

pub(crate) fn test_role_def(_kind: NodeKind) -> RoleDef<Runtime> {
    RoleDef {
        tools: vec![],
        pre_tool_use: test_pre_tool_use,
        stop: test_stop,
        session_start: test_session_start,
    }
}

pub(crate) fn test_roster() -> RoleRegistry<Runtime> {
    RoleRegistry::new(test_role_def)
}
