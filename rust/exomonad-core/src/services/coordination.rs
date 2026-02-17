use crate::domain::{AgentName, MessageFilter};
use chrono::Utc;
use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::sync::RwLock;

/// In-memory coordination state. Lives in Arc on the MCP server.
pub struct CoordinationService {
    tasks: RwLock<BTreeMap<String, Task>>,
    messages: RwLock<Vec<Message>>,
    next_task_id: AtomicU64,
}

#[derive(Debug, Clone)]
pub struct Task {
    pub id: String,
    pub subject: String,
    pub description: String,
    pub status: TaskStatus,
    pub owner: AgentName,
    pub blocked_by: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TaskStatus {
    Pending,
    InProgress,
    Completed,
}

#[derive(Debug, Clone)]
pub struct Message {
    pub from: AgentName,
    pub text: String,
    pub summary: String,
    pub timestamp: String,
    pub read: bool,
}

impl CoordinationService {
    pub fn new() -> Self {
        Self {
            tasks: RwLock::new(BTreeMap::new()),
            messages: RwLock::new(Vec::new()),
            next_task_id: AtomicU64::new(1),
        }
    }

    pub async fn create_task(
        &self,
        subject: String,
        description: String,
        owner: AgentName,
        blocked_by: Vec<String>,
    ) -> String {
        let id_num = self.next_task_id.fetch_add(1, Ordering::SeqCst);
        let task_id = format!("task-{}", id_num);

        let task = Task {
            id: task_id.clone(),
            subject,
            description,
            status: TaskStatus::Pending,
            owner,
            blocked_by,
        };

        let mut tasks = self.tasks.write().await;
        tasks.insert(task_id.clone(), task);
        task_id
    }

    pub async fn update_task(
        &self,
        task_id: String,
        status: Option<TaskStatus>,
        owner: Option<AgentName>,
        description: Option<String>,
        subject: Option<String>,
    ) -> bool {
        let mut tasks = self.tasks.write().await;
        if let Some(task) = tasks.get_mut(&task_id) {
            if let Some(s) = status {
                task.status = s;
            }
            if let Some(o) = owner {
                task.owner = o;
            }
            if let Some(d) = description {
                task.description = d;
            }
            if let Some(sub) = subject {
                task.subject = sub;
            }
            true
        } else {
            false
        }
    }

    pub async fn list_tasks(&self, filter_status: Option<TaskStatus>) -> Vec<Task> {
        let tasks = self.tasks.read().await;
        tasks
            .values()
            .filter(|t| filter_status.is_none_or(|fs| t.status == fs))
            .cloned()
            .collect()
    }

    pub async fn get_task(&self, task_id: &str) -> Option<Task> {
        let tasks = self.tasks.read().await;
        tasks.get(task_id).cloned()
    }

    pub async fn send_message(&self, from: AgentName, text: String, summary: String) {
        let timestamp = Utc::now().to_rfc3339();
        let message = Message {
            from,
            text,
            summary,
            timestamp,
            read: false,
        };

        let mut messages = self.messages.write().await;
        messages.push(message);
    }

    pub async fn get_messages(&self, filter: MessageFilter) -> Vec<Message> {
        let mut messages = self.messages.write().await;
        let mut result = Vec::new();

        for msg in messages.iter_mut() {
            let include = match filter {
                MessageFilter::All => true,
                MessageFilter::UnreadOnly => !msg.read,
            };
            if include {
                result.push(msg.clone());
                msg.read = true;
            }
        }

        result
    }
}

impl Default for CoordinationService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_and_get_task() {
        let service = CoordinationService::new();
        let task_id = service
            .create_task(
                "Test Subject".to_string(),
                "Test Description".to_string(),
                AgentName::from("owner1"),
                vec!["blocker1".to_string()],
            )
            .await;

        assert_eq!(task_id, "task-1");

        let task = service.get_task(&task_id).await.unwrap();
        assert_eq!(task.subject, "Test Subject");
        assert_eq!(task.description, "Test Description");
        assert_eq!(task.owner, AgentName::from("owner1"));
        assert_eq!(task.blocked_by, vec!["blocker1".to_string()]);
        assert_eq!(task.status, TaskStatus::Pending);
    }

    #[tokio::test]
    async fn test_update_task_status() {
        let service = CoordinationService::new();
        let task_id = service
            .create_task(
                "Test".to_string(),
                "Desc".to_string(),
                AgentName::from("owner"),
                vec![],
            )
            .await;

        let success = service
            .update_task(
                task_id.clone(),
                Some(TaskStatus::InProgress),
                None,
                None,
                None,
            )
            .await;

        assert!(success);

        let task = service.get_task(&task_id).await.unwrap();
        assert_eq!(task.status, TaskStatus::InProgress);
    }

    #[tokio::test]
    async fn test_list_tasks_with_filter() {
        let service = CoordinationService::new();
        service
            .create_task("T1".to_string(), "D1".to_string(), AgentName::from("O1"), vec![])
            .await;
        let t2_id = service
            .create_task("T2".to_string(), "D2".to_string(), AgentName::from("O2"), vec![])
            .await;
        service
            .update_task(t2_id, Some(TaskStatus::Completed), None, None, None)
            .await;

        let all_tasks = service.list_tasks(None).await;
        assert_eq!(all_tasks.len(), 2);

        let pending_tasks = service.list_tasks(Some(TaskStatus::Pending)).await;
        assert_eq!(pending_tasks.len(), 1);
        assert_eq!(pending_tasks[0].subject, "T1");

        let completed_tasks = service.list_tasks(Some(TaskStatus::Completed)).await;
        assert_eq!(completed_tasks.len(), 1);
        assert_eq!(completed_tasks[0].subject, "T2");
    }

    #[tokio::test]
    async fn test_send_and_get_messages() {
        let service = CoordinationService::new();
        service
            .send_message(
                AgentName::from("alice"),
                "hello".to_string(),
                "greeting".to_string(),
            )
            .await;
        service
            .send_message(AgentName::from("bob"), "hi".to_string(), "response".to_string())
            .await;

        let messages = service.get_messages(MessageFilter::All).await;
        assert_eq!(messages.len(), 2);
        assert_eq!(messages[0].from, AgentName::from("alice"));
        assert_eq!(messages[1].from, AgentName::from("bob"));
    }

    #[tokio::test]
    async fn test_messages_marked_read() {
        let service = CoordinationService::new();
        service
            .send_message(
                AgentName::from("alice"),
                "hello".to_string(),
                "greeting".to_string(),
            )
            .await;

        let messages = service.get_messages(MessageFilter::UnreadOnly).await;
        assert_eq!(messages.len(), 1);

        let messages_again = service.get_messages(MessageFilter::UnreadOnly).await;
        assert_eq!(messages_again.len(), 0);

        let messages_all = service.get_messages(MessageFilter::All).await;
        assert_eq!(messages_all.len(), 1);
        assert!(messages_all[0].read);
    }
}
