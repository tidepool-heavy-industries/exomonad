//! Shared test fixtures for the engine. The seam means `exo-node` must not depend on the domain
//! crate, so its tests inject a trivial [`TestDomain`] — the same `Exomonad` seam production fills
//! with `exo::ExoDomain`. Each gate is a minimal allow/default; tests that need specific hook
//! behavior build their own `RoleDef` inline.

use exo_caps::{
    AgentName, CapResult, ChildKind, NodeKind, Persona, SpawnSpec,
};
use exo_framework::{
    BoxFuture, Exomonad, HookDecision, HookInput, RoleDef, SessionStartOutput, StopDecision,
    SystemCtx, SystemOutcome,
};
use exo_runtime::Runtime;
use serde::{Deserialize, Serialize};

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

/// A trivial domain `System` payload (the engine deserializes it but `handle_system` is a no-op).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSystem;

/// A trivial spawn intent satisfying [`SpawnSpec`].
#[derive(Debug, Clone)]
pub struct TestSpawn;

impl SpawnSpec for TestSpawn {
    type Role = NodeKind;
    fn role(&self) -> NodeKind {
        NodeKind::Worker
    }
    fn child_kind(&self) -> ChildKind {
        ChildKind::Inline
    }
    fn name(&self) -> Option<AgentName> {
        None
    }
    fn name_prefix(&self) -> &str {
        "test"
    }
    fn fork_session(&self) -> bool {
        false
    }
    fn into_task(self) -> String {
        String::new()
    }
}

/// The minimal domain that the engine's own tests run against.
pub struct TestDomain;

impl Exomonad for TestDomain {
    type Caps = Runtime;
    type Role = NodeKind;
    type System = TestSystem;
    type Spawn = TestSpawn;

    fn role_def(role: NodeKind) -> RoleDef<Runtime> {
        test_role_def(role)
    }

    fn handle_system<'a, C: SystemCtx>(
        _ctx: &'a C,
        _from: &'a Persona,
        _system: &'a TestSystem,
    ) -> BoxFuture<'a, CapResult<SystemOutcome>> {
        Box::pin(async { Ok(SystemOutcome::Done) })
    }
}
