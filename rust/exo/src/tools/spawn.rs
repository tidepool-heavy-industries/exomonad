//! **P3 leaf.** The three **per-op** spawn tools over the [`Spawner`] cap: `spawn_worker`
//! (→ Inline/Worker/Gemini), `spawn_gemini` (→ Worktree/Dev/Gemini), `fork_wave`
//! (→ Worktree/Tl/Claude). Each is a thin wrapper type: an `Args` carrying ONLY task content
//! (the `(role, agent_type, kind)` triple is fixed by which op, never a caller field), a
//! generic-over-caps `run<C: Spawner>`, and a `Tool<R>` adapter. Ships mock-cap unit tests
//! (assert the right `Spawner` method recorded) in this file.

use crate::roles::ExoRole;
use crate::spawn::{render_spec_prompt, write_acceptance, ExoSpawn};
use exo_caps::{AgentName, CapResult, ChildKind, Fs, Spawner};
use exo_framework::{ok_json, parse, schema_json, Tool, ToolOutput};
use schemars::JsonSchema;
use serde::Deserialize;

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

impl SpawnWorker {
    async fn run<C: Spawner>(ctx: &C, args: SpawnWorkerArgs) -> CapResult<ToolOutput> {
        let name = match args.name {
            Some(n) => Some(AgentName::new(n)?),
            None => None,
        };
        // The tool fixes the (role, kind): an ephemeral inline Gemini worker.
        let spec = ExoSpawn {
            role: ExoRole::Worker,
            kind: ChildKind::Inline,
            name,
            name_prefix: "worker",
            task: render_spec_prompt(
                &args.task,
                &args.read_first,
                &args.steps,
                &args.verify,
                &args.boundary,
                args.context.as_ref(),
                &args.done_criteria,
            ),
            fork_session: false,
        };
        let spawned = ctx.spawn(spec).await?;
        Ok(ToolOutput::with_data(
            format!("Spawned worker {}", spawned.as_str()),
            serde_json::json!({ "spawned": spawned.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Spawner + Send + Sync> Tool<R> for SpawnWorker {
    fn name(&self) -> &str {
        "spawn_worker"
    }
    fn description(&self) -> &str {
        "Spawn an ephemeral Gemini worker in a pane inside YOUR worktree (no own branch, no \
         review). PREFER DELEGATING OVER DOING WORK YOURSELF — a Gemini leaf costs ~10-30x less \
         than your Opus tokens, so every line you implement yourself is wasted budget. Give it \
         acceptance criteria, key file paths, and anti-patterns — not line-by-line code. For \
         research or non-conflicting in-place edits; it reports back with `notify_parent`. There \
         is nothing to merge — for work that should land on its own branch, use `spawn_gemini`. \
         After spawning, return immediately — idle and wait, do not poll."
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(SpawnWorkerArgs))
    }
    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let parsed = parse(args)?;
        let out = Self::run(ctx, parsed).await?;
        ok_json(out)
    }
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SpawnGeminiArgs {
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

pub struct SpawnGemini;

impl SpawnGemini {
    async fn run<C: Spawner + Fs>(ctx: &C, args: SpawnGeminiArgs) -> CapResult<ToolOutput> {
        let name = match args.name {
            Some(n) => Some(AgentName::new(n)?),
            None => None,
        };
        let task = render_spec_prompt(
            &args.task,
            &args.read_first,
            &args.steps,
            &args.verify,
            &args.boundary,
            args.context.as_ref(),
            &args.done_criteria,
        );
        // The tool fixes the (role, kind): a Gemini dev leaf in its own worktree.
        let spec = ExoSpawn {
            role: ExoRole::Dev,
            kind: ChildKind::Worktree,
            name,
            name_prefix: "dev",
            task: task.clone(),
            fork_session: false,
        };
        let spawned = ctx.spawn(spec).await?;
        // Persist the child's spec as its acceptance bar (relocated out of the runtime birth).
        write_acceptance(ctx, &spawned, &task).await;
        Ok(ToolOutput::with_data(
            format!("Spawned dev {}", spawned.as_str()),
            serde_json::json!({ "spawned": spawned.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Spawner + Fs + Send + Sync> Tool<R> for SpawnGemini {
    fn name(&self) -> &str {
        "spawn_gemini"
    }
    fn description(&self) -> &str {
        "Spawn a Gemini dev leaf in its OWN worktree + branch with a self-contained spec. PREFER \
         DELEGATING OVER DOING WORK YOURSELF — a Gemini leaf costs ~10-30x less than your Opus \
         tokens; every line you implement yourself is wasted budget. Use the structured fields \
         (steps, verify, boundary, read_first) for precise specs — give it acceptance criteria \
         and file paths, not line-by-line code. It commits to that branch and calls \
         `submit_branch` when ready; a one-shot reviewer checks it, then you `merge` the branch \
         locally. No PRs, no remote — convergence is on-disk. After spawning, return immediately \
         — idle and wait for [READY], do not poll."
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(SpawnGeminiArgs))
    }
    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let parsed = parse(args)?;
        let out = Self::run(ctx, parsed).await?;
        ok_json(out)
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

impl ForkWave {
    async fn run<C: Spawner + Fs + Sync>(ctx: &C, args: ForkWaveArgs) -> CapResult<ToolOutput> {
        let mut specs = Vec::with_capacity(args.children.len());
        // Keep the rendered tasks parallel to `specs` so we can persist each spawned child's
        // acceptance bar after the wave returns (the results are positional).
        let mut tasks = Vec::with_capacity(args.children.len());
        for child in args.children {
            let name = match child.name {
                Some(n) => Some(AgentName::new(n)?),
                None => None,
            };
            let task = render_spec_prompt(
                &child.task,
                &child.read_first,
                &child.steps,
                &child.verify,
                &child.boundary,
                child.context.as_ref(),
                &child.done_criteria,
            );
            tasks.push(task.clone());
            // The tool fixes the (role, kind): a Claude TL child in its own worktree.
            specs.push(ExoSpawn {
                role: ExoRole::Tl,
                kind: ChildKind::Worktree,
                name,
                name_prefix: "tl",
                task,
                fork_session: child.fork_session,
            });
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

#[async_trait::async_trait]
impl<R: Spawner + Fs + Send + Sync> Tool<R> for ForkWave {
    fn name(&self) -> &str {
        "fork_wave"
    }
    fn description(&self) -> &str {
        "Fork a wave of parallel Claude TL children, each in its own worktree + branch. Each runs \
         scaffold-fork-converge on its subtree and calls `submit_branch` when its branch is \
         ready; you then `merge` it locally — no PRs, no remote, convergence is on-disk. \
         Decompose and delegate aggressively: every token you spend on work a child could do is \
         wasted. Create a team (TeamCreate) BEFORE calling so children's messages reach you. \
         Requires a clean worktree. After spawning, return immediately — idle and wait, do not \
         poll."
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(ForkWaveArgs))
    }
    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let parsed = parse(args)?;
        let out = Self::run(ctx, parsed).await?;
        ok_json(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};

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
    async fn test_spawn_gemini() {
        let mock = MockRuntime::default();
        let args = SpawnGeminiArgs {
            name: Some("gemini-1".to_string()),
            task: "do something else".to_string(),
            steps: vec![],
            verify: vec![],
            done_criteria: vec![],
            context: None,
            boundary: vec![],
            read_first: vec![],
        };
        let out = SpawnGemini::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Spawned dev"));
        let calls = mock.calls_made();
        // The spawn, then the acceptance.md write (relocated into the domain tool).
        assert!(calls.iter().any(|c| matches!(c, Call::Spawn { role, task, .. }
            if role == "dev" && task.contains("do something else"))));
        assert!(calls.iter().any(|c| matches!(c, Call::FsWrite { path }
            if path.contains("gemini-1") && path.ends_with(".exo/acceptance.md"))));
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
