//! Shared fixtures for exo-node integration tests. The seam means the engine must not depend on
//! the `exo` domain crate, so its tests inject a trivial [`TestDomain`] — the same `Exomonad` seam
//! production fills with `exo::ExoDomain`. Its `pre_tool_use` denies a shell call so the socket
//! layer's `Deny → nudge` shaping is exercised without coupling the engine test to the domain's
//! concrete antipattern rules (those are unit-tested in `exo`).

use exo_caps::{AgentName, AgentType, CapResult, ChildKind, Persona, Reason, RoleKind, SpawnSpec};
use exo_framework::{
    BoxFuture, Exomonad, HookDecision, HookInput, RoleDef, SessionStartOutput, SystemCtx,
    SystemOutcome,
};
use exo_runtime::Runtime;
use serde::{Deserialize, Serialize};

/// A stand-in domain role for the integration tests (mirrors `exo::ExoRole`).
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
}

fn pre<'a>(_: &'a Runtime, input: &'a HookInput) -> BoxFuture<'a, HookDecision> {
    let deny =
        input.tool_name.as_str() == "Bash" || input.tool_name.as_str() == "run_shell_command";
    Box::pin(async move {
        if deny {
            HookDecision::Deny {
                reason: Reason::new("test gate: shell denied".into()).unwrap(),
            }
        } else {
            HookDecision::Allow
        }
    })
}
fn session(_: &Runtime) -> BoxFuture<'_, SessionStartOutput> {
    Box::pin(async { SessionStartOutput::default() })
}

/// A trivial domain `System` payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSystem;

/// A trivial spawn intent.
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

/// The minimal domain the engine integration tests run against.
pub struct TestDomain;

impl Exomonad for TestDomain {
    type Caps = Runtime;
    type Role = TestRole;
    type System = TestSystem;
    type Spawn = TestSpawn;

    fn role_def(_role: TestRole) -> RoleDef<Runtime> {
        RoleDef {
            tools: vec![],
            pre_tool_use: pre,
            session_start: session,
        }
    }

    fn handle_system<'a, C: SystemCtx>(
        _ctx: &'a C,
        _from: &'a Persona,
        _system: &'a TestSystem,
    ) -> BoxFuture<'a, CapResult<SystemOutcome>> {
        Box::pin(async { Ok(SystemOutcome::Done) })
    }
}
