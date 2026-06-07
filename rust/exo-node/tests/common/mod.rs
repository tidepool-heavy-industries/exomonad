//! Shared fixtures for exo-node integration tests. The seam means the engine must not depend on
//! the `exo` domain crate, so its tests inject a trivial [`TestDomain`] — the same `Exomonad` seam
//! production fills with `exo::ExoDomain`. Its `pre_tool_use` denies a shell call so the socket
//! layer's `Deny → nudge` shaping is exercised without coupling the engine test to the domain's
//! concrete antipattern rules (those are unit-tested in `exo`).

use exo_caps::{AgentName, CapResult, ChildKind, NodeKind, Persona, SpawnSpec};
use exo_framework::{
    BoxFuture, Exomonad, HookDecision, HookInput, RoleDef, SessionStartOutput, StopDecision,
    SystemCtx, SystemOutcome,
};
use exo_runtime::Runtime;
use serde::{Deserialize, Serialize};

fn pre<'a>(_: &'a Runtime, input: &'a HookInput) -> BoxFuture<'a, HookDecision> {
    let deny = input.tool_name == "Bash" || input.tool_name == "run_shell_command";
    Box::pin(async move {
        if deny {
            HookDecision::Deny {
                reason: "test gate: shell denied".into(),
            }
        } else {
            HookDecision::Allow
        }
    })
}
fn stop(_: &Runtime) -> BoxFuture<'_, StopDecision> {
    Box::pin(async { StopDecision::Allow })
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

/// The minimal domain the engine integration tests run against.
pub struct TestDomain;

impl Exomonad for TestDomain {
    type Caps = Runtime;
    type Role = NodeKind;
    type System = TestSystem;
    type Spawn = TestSpawn;

    fn role_def(_role: NodeKind) -> RoleDef<Runtime> {
        RoleDef {
            tools: vec![],
            pre_tool_use: pre,
            stop,
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
