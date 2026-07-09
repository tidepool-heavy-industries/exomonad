//! **P3 leaf.** The three **per-op** spawn tools over the [`Spawner`] cap: `spawn_worker`
//! (→ Inline/Worker), `spawn_dev` (→ Worktree/Dev), `fork_wave` (→ Worktree/Tl). Every spawn is a
//! Claude instance — the role's model is what varies (leaves on Sonnet, TLs on the session
//! default; see [`ExoRole::model`](crate::ExoRole)). Each tool is a thin wrapper type: an `Args`
//! carrying ONLY task content (the `(role, kind)` pair is fixed by which op, never a caller field),
//! a generic-over-caps `run<C: Spawner>`, and a `Tool<R>` adapter. Ships mock-cap unit tests
//! (assert the right `Spawner` method recorded) in this file.

use crate::roles::ExoRole;
use crate::spawn::{render_spec_prompt, write_acceptance, ExoSpawn};
use exo_caps::{AgentName, CapResult, ChildKind, Fs, Spawner};
use exo_framework::{Tool, ToolOutput};
use schemars::JsonSchema;
use serde::Deserialize;

/// Internal fields shared by all three spawn tools. Callers convert their specific Args into this
/// and pass it to [`build_spawn`].
struct SpawnArgs {
    name: Option<String>,
    task: String,
    steps: Vec<String>,
    verify: Vec<String>,
    done_criteria: Vec<String>,
    context: Option<String>,
    boundary: Vec<String>,
    read_first: Vec<String>,
    fork_session: bool,
}

/// Resolve the name, render the spec prompt, and assemble an [`ExoSpawn`]. The `(role, kind,
/// name_prefix)` triple is fixed by the calling tool — callers provide only the task content.
fn build_spawn(
    role: ExoRole,
    kind: ChildKind,
    name_prefix: &str,
    args: SpawnArgs,
) -> CapResult<ExoSpawn> {
    let name = match args.name {
        Some(n) => Some(AgentName::new(n)?),
        None => None,
    };
    Ok(ExoSpawn {
        role,
        kind,
        name,
        name_prefix: name_prefix.into(),
        task: render_spec_prompt(
            &args.task,
            &args.read_first,
            &args.steps,
            &args.verify,
            &args.boundary,
            args.context.as_ref(),
            &args.done_criteria,
        ),
        fork_session: args.fork_session,
    })
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SpawnWorkerArgs {
    pub name: Option<String>,
    pub task: String,
    #[serde(default)]
    pub steps: Vec<String>,
    #[serde(default)]
    pub verify: Vec<String>,
    #[serde(default)]
    pub done_criteria: Vec<String>,
    #[serde(default)]
    pub context: Option<String>,
    #[serde(default)]
    pub boundary: Vec<String>,
    #[serde(default)]
    pub read_first: Vec<String>,
}

pub struct SpawnWorker;

#[async_trait::async_trait]
impl<R: Spawner + Send + Sync> Tool<R> for SpawnWorker {
    const NAME: &'static str = "spawn_worker";
    const DESCRIPTION: &'static str =
        "Spawn an ephemeral Sonnet worker in a pane inside YOUR worktree (no own branch, no \
         review). PREFER DELEGATING OVER DOING WORK YOURSELF — a Sonnet leaf costs far less than \
         your own tokens, so every line you implement yourself is wasted budget. Give it \
         acceptance criteria, key file paths, and anti-patterns — not line-by-line code. For \
         research or non-conflicting in-place edits; it reports back with `notify_parent`. There \
         is nothing to merge — for work that should land on its own branch, use `spawn_dev`. \
         After spawning, return immediately — idle and wait, do not poll.";
    type Args = SpawnWorkerArgs;

    async fn run(ctx: &R, args: SpawnWorkerArgs) -> CapResult<ToolOutput> {
        // The tool fixes the (role, kind): an ephemeral inline worker (Sonnet Claude).
        let spec = build_spawn(
            ExoRole::Worker,
            ChildKind::Inline,
            "worker",
            SpawnArgs {
                name: args.name,
                task: args.task,
                steps: args.steps,
                verify: args.verify,
                done_criteria: args.done_criteria,
                context: args.context,
                boundary: args.boundary,
                read_first: args.read_first,
                fork_session: false,
            },
        )?;
        let spawned = ctx.spawn(spec).await?;
        Ok(ToolOutput::with_data(
            format!("Spawned worker {}", spawned.as_str()),
            serde_json::json!({ "spawned": spawned.as_str() }),
        ))
    }
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SpawnDevArgs {
    pub name: Option<String>,
    pub task: String,
    #[serde(default)]
    pub steps: Vec<String>,
    #[serde(default)]
    pub verify: Vec<String>,
    #[serde(default)]
    pub done_criteria: Vec<String>,
    #[serde(default)]
    pub context: Option<String>,
    #[serde(default)]
    pub boundary: Vec<String>,
    #[serde(default)]
    pub read_first: Vec<String>,
}

pub struct SpawnDev;

#[async_trait::async_trait]
impl<R: Spawner + Fs + Send + Sync> Tool<R> for SpawnDev {
    const NAME: &'static str = "spawn_dev";
    const DESCRIPTION: &'static str =
        "Spawn a Sonnet dev leaf in its OWN worktree + branch with a self-contained spec. PREFER \
         DELEGATING OVER DOING WORK YOURSELF — a Sonnet leaf costs far less than your own tokens; \
         every line you implement yourself is wasted budget. Use the structured fields \
         (steps, verify, boundary, read_first) for precise specs — give it acceptance criteria \
         and file paths, not line-by-line code. It commits to that branch and calls \
         `submit_branch` when ready; a one-shot reviewer checks it, then you `merge` the branch \
         locally. No PRs, no remote — convergence is on-disk. After spawning, return immediately \
         — idle and wait for [READY], do not poll.";
    type Args = SpawnDevArgs;

    async fn run(ctx: &R, args: SpawnDevArgs) -> CapResult<ToolOutput> {
        // The tool fixes the (role, kind): a Sonnet dev leaf in its own worktree.
        let spec = build_spawn(
            ExoRole::Dev,
            ChildKind::Worktree,
            "dev",
            SpawnArgs {
                name: args.name,
                task: args.task,
                steps: args.steps,
                verify: args.verify,
                done_criteria: args.done_criteria,
                context: args.context,
                boundary: args.boundary,
                read_first: args.read_first,
                fork_session: false,
            },
        )?;
        let task = spec.task.clone();
        let spawned = ctx.spawn(spec).await?;
        // Persist the child's spec as its acceptance bar (relocated out of the runtime birth).
        write_acceptance(ctx, &spawned, &task).await;
        Ok(ToolOutput::with_data(
            format!("Spawned dev {}", spawned.as_str()),
            serde_json::json!({ "spawned": spawned.as_str() }),
        ))
    }
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ForkChildArgs {
    pub name: Option<String>,
    pub task: String,
    #[serde(default)]
    pub steps: Vec<String>,
    #[serde(default)]
    pub verify: Vec<String>,
    #[serde(default)]
    pub done_criteria: Vec<String>,
    #[serde(default)]
    pub context: Option<String>,
    #[serde(default)]
    pub boundary: Vec<String>,
    #[serde(default)]
    pub read_first: Vec<String>,
    /// Opt-in (default false): inherit this TL session's context by launching the child
    /// Claude with `--resume --fork-session <this-session-uuid>`. Default false — the
    /// scaffold commit + spec is the primary context channel, and forking a stale/compacted
    /// parent context often hurts.
    #[serde(default)]
    pub fork_session: bool,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ForkWaveArgs {
    pub children: Vec<ForkChildArgs>,
}

pub struct ForkWave;

#[async_trait::async_trait]
impl<R: Spawner + Fs + Send + Sync> Tool<R> for ForkWave {
    const NAME: &'static str = "fork_wave";
    const DESCRIPTION: &'static str =
        "Fork a wave of parallel Claude TL children, each in its own worktree + branch. Each runs \
         scaffold-fork-converge on its subtree and calls `submit_branch` when its branch is \
         ready; you then `merge` it locally — no PRs, no remote, convergence is on-disk. \
         Decompose and delegate aggressively: every token you spend on work a child could do is \
         wasted. Requires a clean worktree. After spawning, return immediately — idle and wait, \
         do not poll; children's messages arrive between your turns.";
    type Args = ForkWaveArgs;

    async fn run(ctx: &R, args: ForkWaveArgs) -> CapResult<ToolOutput> {
        let mut specs = Vec::with_capacity(args.children.len());
        // Keep the rendered tasks parallel to `specs` so we can persist each spawned child's
        // acceptance bar after the wave returns (the results are positional).
        let mut tasks = Vec::with_capacity(args.children.len());
        for child in args.children {
            // The tool fixes the (role, kind): a Claude TL child in its own worktree.
            let spec = build_spawn(
                ExoRole::Tl,
                ChildKind::Worktree,
                "tl",
                SpawnArgs {
                    name: child.name,
                    task: child.task,
                    steps: child.steps,
                    verify: child.verify,
                    done_criteria: child.done_criteria,
                    context: child.context,
                    boundary: child.boundary,
                    read_first: child.read_first,
                    fork_session: child.fork_session,
                },
            )?;
            tasks.push(spec.task.clone());
            specs.push(spec);
        }
        let results = ctx.fork_wave(specs).await;

        let mut spawned = Vec::new();
        let mut errors = Vec::new();
        for (res, task) in results.into_iter().zip(tasks.iter()) {
            match res {
                Ok(name) => {
                    write_acceptance(ctx, &name, task).await;
                    spawned.push(name.as_str().to_string());
                }
                Err(e) => errors.push(e.to_string()),
            }
        }

        let total = spawned.len() + errors.len();
        let text = format!(
            "Forked {} children ({} succeeded, {} failed)",
            total,
            spawned.len(),
            errors.len()
        );
        Ok(ToolOutput::with_data(
            text,
            serde_json::json!({
                "spawned": spawned,
                "errors": errors
            }),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_framework::Tool;

    #[tokio::test]
    async fn test_spawn_worker() {
        let mock = MockRuntime::default();
        let args = SpawnWorkerArgs {
            name: Some("worker-1".to_string()),
            task: "do something".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
        };
        let out = SpawnWorker::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Spawned worker"));
        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        match &calls[0] {
            Call::Spawn { role, task, .. } => {
                assert_eq!(role, "worker");
                assert!(task.contains("do something"));
            }
            _ => panic!("wrong call"),
        }
    }

    #[tokio::test]
    async fn test_spawn_worker_structured() {
        let mock = MockRuntime::default();
        let args = SpawnWorkerArgs {
            name: Some("worker-1".to_string()),
            task: "do something".to_string(),
            steps: vec!["step 1".into()],
            verify: vec!["verify 1".into()],
            done_criteria: vec![],
            context: None,
            boundary: vec!["boundary 1".into()],
            read_first: vec![],
        };
        let _ = SpawnWorker::run(&mock, args).await.unwrap();
        let calls = mock.calls_made();
        match &calls[0] {
            Call::Spawn { task, .. } => {
                // The structured fields are rendered into the single task body by the domain.
                assert!(task.contains("STEPS:\n1. step 1"));
                assert!(task.contains("BOUNDARY (DO NOT):\n- boundary 1"));
            }
            _ => panic!("wrong call"),
        }
    }

    #[tokio::test]
    async fn test_spawn_dev() {
        let mock = MockRuntime::default();
        let args = SpawnDevArgs {
            name: Some("dev-1".to_string()),
            task: "do something else".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
        };
        let out = SpawnDev::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Spawned dev"));
        let calls = mock.calls_made();
        // The spawn, then the acceptance.md write (relocated into the domain tool).
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::Spawn { role, task, .. }
            if role == "dev" && task.contains("do something else"))));
        assert!(calls.iter().any(|c| matches!(c, Call::FsWrite { path }
            if path.contains("dev-1") && path.ends_with(".exo/acceptance.md"))));
    }

    #[tokio::test]
    async fn test_fork_wave() {
        let mock = MockRuntime::default();
        let args = ForkWaveArgs {
            children: vec![
                ForkChildArgs {
                    name: Some("child-1".to_string()),
                    task: "task 1".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec![],
                    fork_session: false,
                },
                ForkChildArgs {
                    name: Some("child-2".to_string()),
                    task: "task 2".to_string(),
                    steps: vec![],
                    verify: vec![],
                    done_criteria: vec![],
                    context: None,
                    boundary: vec![],
                    read_first: vec![],
                    fork_session: false,
                },
            ],
        };
        let out = ForkWave::run(&mock, args).await.unwrap();
        assert!(out
            .text
            .contains("Forked 2 children (2 succeeded, 0 failed)"));
        let calls = mock.calls_made();
        // One fork_wave call recording the wave size...
        assert!(calls
            .iter()
            .any(|c| matches!(c, Call::ForkWave { n } if *n == 2)));
        // ...and an acceptance.md write per spawned child.
        let writes = calls
            .iter()
            .filter(|c| matches!(c, Call::FsWrite { path } if path.ends_with(".exo/acceptance.md")))
            .count();
        assert_eq!(writes, 2);
    }
}
