//! Shared fixtures for exo-node integration tests. The inversion means the engine must not
//! depend on the `exo` domain crate, so its tests inject a trivial all-allow roster — the same
//! seam production fills with `exo::roster()`.

use exo_caps::NodeKind;
use exo_framework::{
    BoxFuture, HookDecision, HookInput, RoleDef, RoleRegistry, SessionStartOutput, StopDecision,
};
use exo_runtime::Runtime;

/// A roster for transport tests: `stop`/`session_start` allow/default; `pre_tool_use` denies a
/// shell call so the socket layer's `Deny → nudge` shaping is exercised without coupling the
/// engine test to the domain's concrete antipattern rules (those are unit-tested in `exo`).
pub fn test_roster() -> RoleRegistry<Runtime> {
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
    fn role(_kind: NodeKind) -> RoleDef<Runtime> {
        RoleDef {
            tools: vec![],
            pre_tool_use: pre,
            stop,
            session_start: session,
        }
    }
    RoleRegistry::new(role)
}
