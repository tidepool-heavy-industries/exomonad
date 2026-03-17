use std::path::PathBuf;
use std::sync::Arc;
use std::fs;
use std::io::Write;
use async_trait::async_trait;
use tracing::info;
use claude_teams_bridge::TeamRegistry;
use exomonad_proto::effects::tasks::*;
use crate::effects::{dispatch_tasks_effect, EffectResult, TasksEffects, ResultExt};
use tempfile::NamedTempFile;

pub struct TasksHandler {
    tasks_dir: PathBuf,
    team_registry: Option<Arc<TeamRegistry>>,
}

impl TasksHandler {
    pub fn new(tasks_dir: PathBuf, team_registry: Option<Arc<TeamRegistry>>) -> Self {
        Self {
            tasks_dir,
            team_registry,
        }
    }

    async fn resolve_team(&self, ctx: &crate::effects::EffectContext, requested_team: &str) -> Option<String> {
        if !requested_team.is_empty() {
            return Some(requested_team.to_string());
        }

        if let Some(ref registry) = self.team_registry {
            // Try agent_name
            let key = ctx.agent_name.to_string();
            if let Some(info) = registry.get(&key).await {
                return Some(info.team_name);
            }

            // Try birth_branch
            let bb = ctx.birth_branch.to_string();
            if let Some(info) = registry.get(&bb).await {
                return Some(info.team_name);
            }

            // Try parent birth_branch
            if let Some(parent) = ctx.birth_branch.parent() {
                let p_bb = parent.to_string();
                if let Some(info) = registry.get(&p_bb).await {
                    return Some(info.team_name);
                }
            }
        }
        None
    }
}

crate::impl_pass_through_handler!(TasksHandler, "tasks", dispatch_tasks_effect);

#[async_trait]
impl TasksEffects for TasksHandler {
    async fn list_tasks(
        &self,
        req: ListTasksRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ListTasksResponse> {
        let team_name = self.resolve_team(ctx, &req.team_name).await
            .ok_or_else(|| crate::effects::EffectError::invalid_input("Could not resolve team name"))?;

        let team_dir = self.tasks_dir.join(&team_name);
        if !team_dir.exists() {
            return Ok(ListTasksResponse { tasks: vec![] });
        }

        let mut tasks = vec![];
        let entries = fs::read_dir(&team_dir).effect_err("tasks")?;

        for entry in entries {
            let entry = entry.effect_err("tasks")?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                let content = fs::read_to_string(&path).effect_err("tasks")?;
                let task_val: serde_json::Value = serde_json::from_str(&content).effect_err("tasks")?;
                
                let task = Task {
                    id: task_val["id"].as_str().unwrap_or_default().to_string(),
                    subject: task_val["subject"].as_str().unwrap_or_default().to_string(),
                    description: task_val["description"].as_str().unwrap_or_default().to_string(),
                    active_form: task_val["activeForm"].as_str().unwrap_or_default().to_string(),
                    status: task_val["status"].as_str().unwrap_or_default().to_string(),
                    blocks: task_val["blocks"].as_array().map(|a| a.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect()).unwrap_or_default(),
                    blocked_by: task_val["blockedBy"].as_array().map(|a| a.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect()).unwrap_or_default(),
                    owner: task_val["owner"].as_str().unwrap_or_default().to_string(),
                };

                if req.status_filter.is_empty() || task.status == req.status_filter {
                    tasks.push(task);
                }
            }
        }

        // Sort by numeric ID
        tasks.sort_by(|a, b| {
            let aid: u64 = a.id.parse().unwrap_or(0);
            let bid: u64 = b.id.parse().unwrap_or(0);
            aid.cmp(&bid)
        });

        Ok(ListTasksResponse { tasks })
    }

    async fn get_task(
        &self,
        req: GetTaskRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GetTaskResponse> {
        let team_name = self.resolve_team(ctx, &req.team_name).await
            .ok_or_else(|| crate::effects::EffectError::invalid_input("Could not resolve team name"))?;

        let task_path = self.tasks_dir.join(&team_name).join(format!("{}.json", req.task_id));
        if !task_path.exists() {
            return Ok(GetTaskResponse { task: None, found: false });
        }

        let content = fs::read_to_string(&task_path).effect_err("tasks")?;
        let task_val: serde_json::Value = serde_json::from_str(&content).effect_err("tasks")?;

        let task = Task {
            id: task_val["id"].as_str().unwrap_or_default().to_string(),
            subject: task_val["subject"].as_str().unwrap_or_default().to_string(),
            description: task_val["description"].as_str().unwrap_or_default().to_string(),
            active_form: task_val["activeForm"].as_str().unwrap_or_default().to_string(),
            status: task_val["status"].as_str().unwrap_or_default().to_string(),
            blocks: task_val["blocks"].as_array().map(|a| a.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect()).unwrap_or_default(),
            blocked_by: task_val["blockedBy"].as_array().map(|a| a.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect()).unwrap_or_default(),
            owner: task_val["owner"].as_str().unwrap_or_default().to_string(),
        };

        Ok(GetTaskResponse { task: Some(task), found: true })
    }

    async fn update_task(
        &self,
        req: UpdateTaskRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<UpdateTaskResponse> {
        let team_name = self.resolve_team(ctx, &req.team_name).await
            .ok_or_else(|| crate::effects::EffectError::invalid_input("Could not resolve team name"))?;

        let team_dir = self.tasks_dir.join(&team_name);
        let task_path = team_dir.join(format!("{}.json", req.task_id));
        if !task_path.exists() {
            return Ok(UpdateTaskResponse { success: false, error: format!("Task {} not found in team {}", req.task_id, team_name) });
        }

        let content = fs::read_to_string(&task_path).effect_err("tasks")?;
        let mut task_val: serde_json::Value = serde_json::from_str(&content).effect_err("tasks")?;

        if !req.status.is_empty() {
            task_val["status"] = serde_json::Value::String(req.status.clone());
        }
        if !req.owner.is_empty() {
            task_val["owner"] = serde_json::Value::String(req.owner.clone());
        }
        if !req.active_form.is_empty() {
            task_val["activeForm"] = serde_json::Value::String(req.active_form.clone());
        }

        // Write atomically
        let mut tmp = NamedTempFile::new_in(&team_dir).effect_err("tasks")?;
        let updated_json = serde_json::to_string_pretty(&task_val).effect_err("tasks")?;
        tmp.write_all(updated_json.as_bytes()).effect_err("tasks")?;
        tmp.persist(&task_path).effect_err("tasks")?;

        info!(team_name = %team_name, task_id = %req.task_id, "Updated task");

        Ok(UpdateTaskResponse { success: true, error: String::new() })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::{EffectContext, TasksEffects};
    use tempfile::tempdir;

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test-agent"),
            birth_branch: BirthBranch::from("main"),
        }
    }

    #[tokio::test]
    async fn test_list_tasks_empty() {
        let tmp = tempdir().unwrap();
        let handler = TasksHandler::new(tmp.path().to_path_buf(), None);
        let ctx = test_ctx();

        let req = ListTasksRequest {
            team_name: "my-team".into(),
            status_filter: "".into(),
        };

        let resp = handler.list_tasks(req, &ctx).await.unwrap();
        assert_eq!(resp.tasks.len(), 0);
    }

    #[tokio::test]
    async fn test_tasks_lifecycle() {
        let tmp = tempdir().unwrap();
        let tasks_dir = tmp.path().to_path_buf();
        let team_dir = tasks_dir.join("my-team");
        fs::create_dir_all(&team_dir).unwrap();

        let task_json = r#"{
            "id": "1",
            "subject": "Test Task",
            "description": "Desc",
            "activeForm": "Doing it",
            "status": "todo",
            "blocks": [],
            "blockedBy": []
        }"#;
        fs::write(team_dir.join("1.json"), task_json).unwrap();

        let handler = TasksHandler::new(tasks_dir, None);
        let ctx = test_ctx();

        // List
        let req = ListTasksRequest {
            team_name: "my-team".into(),
            status_filter: "".into(),
        };
        let resp = handler.list_tasks(req, &ctx).await.unwrap();
        assert_eq!(resp.tasks.len(), 1);
        assert_eq!(resp.tasks[0].subject, "Test Task");

        // Get
        let get_req = GetTaskRequest {
            team_name: "my-team".into(),
            task_id: "1".into(),
        };
        let get_resp = handler.get_task(get_req, &ctx).await.unwrap();
        assert!(get_resp.found);
        assert_eq!(get_resp.task.unwrap().subject, "Test Task");

        // Update
        let update_req = UpdateTaskRequest {
            team_name: "my-team".into(),
            task_id: "1".into(),
            status: "completed".into(),
            owner: "me".into(),
            active_form: "Done".into(),
        };
        let update_resp = handler.update_task(update_req, &ctx).await.unwrap();
        assert!(update_resp.success);

        // Verify update
        let get_resp = handler.get_task(GetTaskRequest {
            team_name: "my-team".into(),
            task_id: "1".into(),
        }, &ctx).await.unwrap();
        let t = get_resp.task.unwrap();
        assert_eq!(t.status, "completed");
        assert_eq!(t.owner, "me");
        assert_eq!(t.active_form, "Done");
    }
}
