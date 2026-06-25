//! Shared test fixtures for the engine. The seam means `exo-node` must not depend on the domain
//! crate, so its tests inject a trivial [`TestDomain`] with its own [`TestRole`] — the same
//! `Exomonad` seam production fills with `exo::ExoDomain` / `exo::ExoRole`. `TestRole` mirrors the
//! `exo` archetypes (role_str + agent_type) so the engine's identity/last-hop tests are realistic.

use exo_caps::{AgentName, AgentType, CapResult, ChildKind, Persona, RoleKind, SpawnSpec};
use exo_framework::{
    BoxFuture, Exomonad, HookDecision, HookInput, RoleDef, SessionStartOutput, StopDecision,
    SystemCtx, SystemOutcome,
};
use exo_runtime::Runtime;
use serde::{Deserialize, Serialize};

/// A stand-in domain role for the engine tests (mirrors `exo::ExoRole`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TestRole {
    Root,
    Tl,
    Dev,
    Worker,
    Reviewer,
}

impl RoleKind for TestRole {
    fn all() -> &'static [Self] {
        &[
            TestRole::Root,
            TestRole::Tl,
            TestRole::Dev,
            TestRole::Worker,
            TestRole::Reviewer,
        ]
    }
    fn agent_type(&self) -> AgentType {
        // Mirrors `exo::ExoRole`: every tree node is a Claude instance.
        AgentType::Claude
    }
    fn role_str(&self) -> &'static str {
        match self {
            TestRole::Root => "root",
            TestRole::Tl => "tl",
            TestRole::Dev => "dev",
            TestRole::Worker => "worker",
            TestRole::Reviewer => "reviewer",
        }
    }
    fn protocol(&self) -> &'static str {
        // Distinct markers per role so the engine's injection tests can assert which prose lands
        // (delivered via the launch-time --append-system-prompt, NOT session-start additionalContext).
        match self {
            TestRole::Tl => "TEST-TL-PROTOCOL-MARKER",
            TestRole::Dev => "TEST-DEV-PROTOCOL-MARKER",
            _ => "",
        }
    }
}

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

pub(crate) fn test_role_def(_kind: TestRole) -> RoleDef<Runtime> {
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
    type Role = TestRole;
    fn role(&self) -> TestRole {
        TestRole::Worker
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
    type Role = TestRole;
    type System = TestSystem;
    type Spawn = TestSpawn;

    fn role_def(role: TestRole) -> RoleDef<Runtime> {
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
