//! **P4 leaf.** `file_pr` — create/update a PR over the [`Git`] + [`GitHub`] caps (base branch
//! auto-detected from the dot-separated branch name). A type with an `Args`, a generic-over-caps
//! `run<C: Git + GitHub>(ctx, args) -> CapResult<ToolOutput>`, and a `Tool<R>` adapter. Ships
//! mock-cap unit tests (assert `GitHub::file_pr` recorded with the derived base) in this file.
//! See `docs/design/swarm/04-policy.md`.

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::{Branch, Git, GitHub};
use schemars::JsonSchema;
use serde::Deserialize;
use serde_json::json;

/// Arguments for the `file_pr` tool.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct FilePrArgs {
    /// The PR title.
    pub title: String,
    /// The PR body/description.
    pub body: String,
}

/// The `file_pr` tool.
pub struct FilePr;

impl FilePr {
    /// Implementation of the `file_pr` tool, generic over capabilities.
    pub async fn run<C: Git + GitHub>(ctx: &C, args: FilePrArgs) -> exo_caps::CapResult<ToolOutput> {
        let current = ctx.current_branch().await?;
        let branch_str = current.as_str();

        // Derive base branch: strip last dot segment, or default to "main".
        let base_name = if let Some(last_dot) = branch_str.rfind('.') {
            &branch_str[..last_dot]
        } else {
            "main"
        };

        let base = Branch::new(base_name.to_string())?;

        // Check for an existing PR (as per spec, although file_pr is idempotent).
        let _existing = ctx.pr_for_branch(&current).await?;

        // Create or update the PR.
        let n = ctx.file_pr(&args.title, &args.body, &base).await?;

        Ok(ToolOutput::with_data(
            format!("PR #{n}"),
            json!({ "pr": n }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Git + GitHub + Send + Sync> Tool<R> for FilePr {
    fn name(&self) -> &str {
        "file_pr"
    }

    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(FilePrArgs))
    }

    async fn call(&self, ctx: &R, args: serde_json::Value) -> exo_caps::CapResult<serde_json::Value> {
        ok_json(Self::run(ctx, parse(args)?).await?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};

    #[tokio::test]
    async fn test_file_pr_dotted_branch() {
        let mut mock = MockRuntime::default();
        mock.current_branch = Branch::new("main.policy-claude.p4".into()).unwrap();
        mock.next_pr = 123;

        let args = FilePrArgs {
            title: "Test PR".into(),
            body: "Test Body".into(),
        };

        let res = FilePr::run(&mock, args).await.unwrap();
        assert_eq!(res.text, "PR #123");
        assert_eq!(res.data, Some(json!({ "pr": 123 })));

        let calls = mock.calls_made();
        let found = calls.iter().any(|c| matches!(c, Call::FilePr { title, body, base }
            if title == "Test PR" && body == "Test Body" && base.as_str() == "main.policy-claude"));
        assert!(found, "Expected FilePr call with base 'main.policy-claude', got: {:?}", calls);
    }

    #[tokio::test]
    async fn test_file_pr_no_dot_branch() {
        let mut mock = MockRuntime::default();
        mock.current_branch = Branch::new("feature-branch".into()).unwrap();
        mock.next_pr = 456;

        let args = FilePrArgs {
            title: "Standalone PR".into(),
            body: "Body".into(),
        };

        let res = FilePr::run(&mock, args).await.unwrap();
        assert_eq!(res.text, "PR #456");

        let calls = mock.calls_made();
        let found = calls.iter().any(|c| matches!(c, Call::FilePr { base, .. } if base.as_str() == "main"));
        assert!(found, "Expected FilePr call with base 'main', got: {:?}", calls);
    }
}
