//! `dismiss_worker` tool — unconditionally tear down an inline worker by name.
//!
//! Reuses `Spawner::kill_pane` (ledger-based parent-side pane kill — the same path FIX C
//! takes for native shutdown). Served by Root and Tl (the roles that spawn workers).

use exo_caps::{AgentName, CapResult, Spawner};
use exo_framework::{Tool, ToolOutput};
use schemars::JsonSchema;
use serde::Deserialize;

/// The `dismiss_worker` tool.
pub struct DismissWorker;

#[derive(Deserialize, JsonSchema)]
pub struct DismissWorkerArgs {
    /// Name of the inline worker to dismiss (the name you passed to `spawn_worker`).
    pub name: String,
}

#[async_trait::async_trait]
impl<R: Spawner + Send + Sync> Tool<R> for DismissWorker {
    const NAME: &'static str = "dismiss_worker";
    const DESCRIPTION: &'static str =
        "Dismiss an inline worker you spawned: kill its pane by name. Unconditional, \
         parent-side; for ephemeral workers you're done with.";
    type Args = DismissWorkerArgs;

    async fn run(ctx: &R, args: DismissWorkerArgs) -> CapResult<ToolOutput> {
        let name = AgentName::new(args.name)?;
        // UFCS: `Spawner::kill_pane` (by child name) — `Tmux::kill_pane` (by pane id)
        // is also in scope via the supertrait, so the bare method call is ambiguous.
        Spawner::kill_pane(ctx, &name).await?;
        Ok(ToolOutput::text(format!(
            "dismissed worker {}",
            name.as_str()
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_framework::Tool;

    #[tokio::test]
    async fn test_dismiss_worker_kills_pane() {
        let mock = MockRuntime::default();
        let out = DismissWorker::run(
            &mock,
            DismissWorkerArgs {
                name: "my-worker".into(),
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("my-worker"));
        let calls = mock.calls_made();
        assert!(
            calls
                .iter()
                .any(|c| matches!(c, Call::KillPane { child } if child.as_str() == "my-worker")),
            "expected KillPane for my-worker, got: {:?}",
            calls
        );
    }

    #[tokio::test]
    async fn test_dismiss_worker_error_propagates() {
        let mock = MockRuntime::failing("kill_pane");
        let res = DismissWorker::run(
            &mock,
            DismissWorkerArgs {
                name: "gone".into(),
            },
        )
        .await;
        assert!(res.is_err());
    }
}
