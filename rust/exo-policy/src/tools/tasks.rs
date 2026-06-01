//! **P2 leaf.** `task_list` / `task_get` / `task_update` — the shared-task-list tools. Each
//! is a type with an `Args`, a generic-over-caps `run` (the task store is reached via a cap —
//! likely `Kv` or `Fs`; pick the narrowest that fits and document it), and a `Tool<R>`
//! adapter. Ships mock-cap unit tests in this file. See `docs/design/swarm/04-policy.md`.

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::{CapResult, Fs, FsError};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::io;
use std::path::Path;

/// Path to the shared task list JSON file.
/// Assumption: The shared task list is stored at `.exo/tasks.json` relative to the worktree root.
/// The Haskell twin (Tasks.hs) uses a Tasks effect which might resolve this differently
/// depending on team name, but here we assume a single shared list for the current context.
const TASKS_PATH: &str = ".exo/tasks.json";

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema, PartialEq)]
pub struct Task {
    pub id: String,
    pub content: String,
    pub status: String,
    pub owner: Option<String>,
    pub active_form: Option<String>,
}

fn is_not_found(e: &FsError) -> bool {
    match e {
        FsError::At { source, .. } => source.kind() == io::ErrorKind::NotFound,
        FsError::Io(source) => source.kind() == io::ErrorKind::NotFound,
    }
}

pub struct TaskList;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct TaskListArgs {}

impl TaskList {
    pub async fn run<C: Fs>(ctx: &C, _args: TaskListArgs) -> CapResult<ToolOutput> {
        let path = Path::new(TASKS_PATH);
        let bytes = match ctx.read(path).await {
            Ok(b) => b,
            Err(e) if is_not_found(&e) => {
                // If the file doesn't exist, we treat it as an empty list.
                return Ok(ToolOutput::with_data(
                    "No tasks found.",
                    serde_json::json!([]),
                ));
            }
            Err(e) => return Err(e.into()),
        };
        let tasks: Vec<Task> = serde_json::from_slice(&bytes).map_err(|e| exo_caps::CapError::Json {
            context: "parsing tasks.json".into(),
            source: e,
        })?;
        Ok(ToolOutput::with_data(
            format!("Listed {} tasks.", tasks.len()),
            serde_json::json!(tasks),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Fs + Send + Sync> Tool<R> for TaskList {
    fn name(&self) -> &str {
        "task_list"
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(TaskListArgs))
    }
    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let args = parse(args)?;
        ok_json(Self::run(ctx, args).await?)
    }
}

pub struct TaskGet;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct TaskGetArgs {
    pub id: String,
}

impl TaskGet {
    pub async fn run<C: Fs>(ctx: &C, args: TaskGetArgs) -> CapResult<ToolOutput> {
        let path = Path::new(TASKS_PATH);
        let bytes = match ctx.read(path).await {
            Ok(b) => b,
            Err(e) if is_not_found(&e) => {
                return Ok(ToolOutput::text(format!("Task {} not found.", args.id)));
            }
            Err(e) => return Err(e.into()),
        };
        let tasks: Vec<Task> = serde_json::from_slice(&bytes).map_err(|e| exo_caps::CapError::Json {
            context: "parsing tasks.json".into(),
            source: e,
        })?;

        for task in tasks {
            if task.id == args.id {
                return Ok(ToolOutput::with_data(
                    format!("Found task {}.", args.id),
                    serde_json::json!(task),
                ));
            }
        }

        Ok(ToolOutput::text(format!("Task {} not found.", args.id)))
    }
}

#[async_trait::async_trait]
impl<R: Fs + Send + Sync> Tool<R> for TaskGet {
    fn name(&self) -> &str {
        "task_get"
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(TaskGetArgs))
    }
    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let args = parse(args)?;
        ok_json(Self::run(ctx, args).await?)
    }
}

pub struct TaskUpdate;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct TaskUpdateArgs {
    pub id: String,
    pub status: Option<String>,
    pub owner: Option<String>,
    pub active_form: Option<String>,
}

impl TaskUpdate {
    pub async fn run<C: Fs>(ctx: &C, args: TaskUpdateArgs) -> CapResult<ToolOutput> {
        let path = Path::new(TASKS_PATH);
        let bytes = match ctx.read(path).await {
            Ok(b) => b,
            Err(e) if is_not_found(&e) => {
                return Ok(ToolOutput::text(format!("Task {} not found.", args.id)));
            }
            Err(e) => return Err(e.into()),
        };
        let mut tasks: Vec<Task> =
            serde_json::from_slice(&bytes).map_err(|e| exo_caps::CapError::Json {
                context: "parsing tasks.json".into(),
                source: e,
            })?;

        let mut updated_task = None;
        for task in &mut tasks {
            if task.id == args.id {
                if let Some(s) = args.status {
                    task.status = s;
                }
                if let Some(o) = args.owner {
                    task.owner = Some(o);
                }
                if let Some(af) = args.active_form {
                    task.active_form = Some(af);
                }
                updated_task = Some(task.clone());
                break;
            }
        }

        let Some(task) = updated_task else {
            return Ok(ToolOutput::text(format!("Task {} not found.", args.id)));
        };

        let new_bytes = serde_json::to_vec(&tasks).map_err(|e| exo_caps::CapError::Json {
            context: "serializing tasks.json".into(),
            source: e,
        })?;
        ctx.write_atomic(path, &new_bytes).await?;

        Ok(ToolOutput::with_data(
            format!("Updated task {}.", args.id),
            serde_json::json!(task),
        ))
    }
}

#[async_trait::async_trait]
impl<R: Fs + Send + Sync> Tool<R> for TaskUpdate {
    fn name(&self) -> &str {
        "task_update"
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(TaskUpdateArgs))
    }
    async fn call(&self, ctx: &R, args: serde_json::Value) -> CapResult<serde_json::Value> {
        let args = parse(args)?;
        ok_json(Self::run(ctx, args).await?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};

    #[tokio::test]
    async fn test_task_list_empty() {
        let mock = MockRuntime::default();
        let out = TaskList::run(&mock, TaskListArgs {}).await.unwrap();
        assert_eq!(out.text, "No tasks found.");
        assert_eq!(out.data.unwrap(), serde_json::json!([]));
        assert_eq!(
            mock.calls_made(),
            vec![Call::FsRead {
                path: TASKS_PATH.into()
            }]
        );
    }

    #[tokio::test]
    async fn test_task_lifecycle() {
        let mock = MockRuntime::default();
        let task = Task {
            id: "T1".into(),
            content: "Fix bug".into(),
            status: "pending".into(),
            owner: None,
            active_form: None,
        };
        let tasks = vec![task.clone()];
        mock.files
            .lock()
            .unwrap()
            .insert(TASKS_PATH.into(), serde_json::to_vec(&tasks).unwrap());

        // List
        let out = TaskList::run(&mock, TaskListArgs {}).await.unwrap();
        assert_eq!(out.data.unwrap(), serde_json::json!([task]));

        // Get
        let out = TaskGet::run(&mock, TaskGetArgs { id: "T1".into() })
            .await
            .unwrap();
        assert_eq!(out.data.unwrap(), serde_json::json!(task));

        // Update
        let out = TaskUpdate::run(
            &mock,
            TaskUpdateArgs {
                id: "T1".into(),
                status: Some("in_progress".into()),
                owner: Some("alice".into()),
                active_form: None,
            },
        )
        .await
        .unwrap();

        let updated = Task {
            id: "T1".into(),
            content: "Fix bug".into(),
            status: "in_progress".into(),
            owner: Some("alice".into()),
            active_form: None,
        };
        assert_eq!(out.data.unwrap(), serde_json::json!(updated));

        // Verify write
        assert!(mock
            .calls_made()
            .contains(&Call::FsWrite {
                path: TASKS_PATH.into()
            }));

        // Get again to verify persistent change in mock
        let out = TaskGet::run(&mock, TaskGetArgs { id: "T1".into() })
            .await
            .unwrap();
        assert_eq!(out.data.unwrap(), serde_json::json!(updated));
    }
}
