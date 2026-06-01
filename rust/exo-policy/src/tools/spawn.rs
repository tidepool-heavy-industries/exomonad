//! **P3 leaf.** The three **per-op** spawn tools over the [`Spawner`] cap: `spawn_worker`
//! (→ Inline/Worker/Gemini), `spawn_gemini` (→ Worktree/Dev/Gemini), `fork_wave`
//! (→ Worktree/Tl/Claude). Each is a thin wrapper type: an `Args` carrying ONLY task content
//! (the `(role, agent_type, kind)` triple is fixed by which op, never a caller field), a
//! generic-over-caps `run<C: Spawner>`, and a `Tool<R>` adapter. Ships mock-cap unit tests
//! (assert the right `Spawner` method recorded) in this file. See `docs/design/swarm/03-capabilities.md`.

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::{AgentName, CapResult, ForkSpec, GeminiSpec, Spawner, WorkerSpec};
use schemars::JsonSchema;
use serde::Deserialize;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SpawnWorkerArgs {
    pub name: Option<String>,
    pub task: String,
}

pub struct SpawnWorker;

impl SpawnWorker {
    async fn run<C: Spawner>(ctx: &C, args: SpawnWorkerArgs) -> CapResult<ToolOutput> {
        let name = match args.name {
            Some(n) => Some(AgentName::new(n)?),
            None => None,
        };
        let spec = WorkerSpec {
            name,
            task: args.task,
        };
        let spawned = ctx.spawn_worker(spec).await?;
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
}

pub struct SpawnGemini;

impl SpawnGemini {
    async fn run<C: Spawner>(ctx: &C, args: SpawnGeminiArgs) -> CapResult<ToolOutput> {
        let name = match args.name {
            Some(n) => Some(AgentName::new(n)?),
            None => None,
        };
        let spec = GeminiSpec {
            name,
            task: args.task,
        };
        let spawned = ctx.spawn_gemini(spec).await?;
        Ok(ToolOutput::with_data(
            format!("Spawned dev {}", spawned.as_str()),
            serde_json::json!({ "spawned": spawned.as_str() }),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Spawner + Send + Sync> Tool<R> for SpawnGemini {
    fn name(&self) -> &str {
        "spawn_gemini"
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
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ForkWaveArgs {
    pub children: Vec<ForkChildArgs>,
}

pub struct ForkWave;

impl ForkWave {
    async fn run<C: Spawner>(ctx: &C, args: ForkWaveArgs) -> CapResult<ToolOutput> {
        let mut specs = Vec::with_capacity(args.children.len());
        for child in args.children {
            let name = match child.name {
                Some(n) => Some(AgentName::new(n)?),
                None => None,
            };
            specs.push(ForkSpec {
                name,
                task: child.task,
            });
        }
        let results = ctx.fork_wave(specs).await;
        
        let mut spawned = Vec::new();
        let mut errors = Vec::new();
        for res in results {
            match res {
                Ok(name) => spawned.push(name.as_str().to_string()),
                Err(e) => errors.push(e.to_string()),
            }
        }
        
        let total = spawned.len() + errors.len();
        let text = format!("Forked {} children ({} succeeded, {} failed)", total, spawned.len(), errors.len());
        Ok(ToolOutput::with_data(text, serde_json::json!({
            "spawned": spawned,
            "errors": errors
        })))
    }
}

#[async_trait::async_trait]
impl<R: Spawner + Send + Sync> Tool<R> for ForkWave {
    fn name(&self) -> &str {
        "fork_wave"
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
        };
        let out = SpawnWorker::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Spawned worker"));
        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        match &calls[0] {
            Call::SpawnWorker { spec_task } => assert_eq!(spec_task, "do something"),
            _ => panic!("wrong call"),
        }
    }

    #[tokio::test]
    async fn test_spawn_gemini() {
        let mock = MockRuntime::default();
        let args = SpawnGeminiArgs {
            name: Some("gemini-1".to_string()),
            task: "do something else".to_string(),
        };
        let out = SpawnGemini::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Spawned dev"));
        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        match &calls[0] {
            Call::SpawnGemini { spec_task } => assert_eq!(spec_task, "do something else"),
            _ => panic!("wrong call"),
        }
    }

    #[tokio::test]
    async fn test_fork_wave() {
        let mock = MockRuntime::default();
        let args = ForkWaveArgs {
            children: vec![
                ForkChildArgs {
                    name: Some("child-1".to_string()),
                    task: "task 1".to_string(),
                },
                ForkChildArgs {
                    name: Some("child-2".to_string()),
                    task: "task 2".to_string(),
                },
            ],
        };
        let out = ForkWave::run(&mock, args).await.unwrap();
        assert!(out.text.contains("Forked 2 children (2 succeeded, 0 failed)"));
        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        match &calls[0] {
            Call::ForkWave { n } => assert_eq!(*n, 2),
            _ => panic!("wrong call"),
        }
    }
}
