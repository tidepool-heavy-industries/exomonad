use crate::domain::AgentName;
use crate::effects::{
    dispatch_coordination_effect, CoordinationEffects, EffectHandler, EffectResult,
};
use crate::services::coordination::{CoordinationService, TaskStatus as ServiceTaskStatus};
use async_trait::async_trait;
use exomonad_proto::effects::coordination::*;
use std::sync::Arc;

pub struct CoordinationHandler {
    service: Arc<CoordinationService>,
}

impl CoordinationHandler {
    pub fn new(service: Arc<CoordinationService>) -> Self {
        Self { service }
    }
}

#[async_trait]
impl EffectHandler for CoordinationHandler {
    fn namespace(&self) -> &str {
        "coordination"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_coordination_effect(self, effect_type, payload).await
    }
}

fn to_service_status(status: TaskStatus) -> Option<ServiceTaskStatus> {
    match status {
        TaskStatus::Pending => Some(ServiceTaskStatus::Pending),
        TaskStatus::InProgress => Some(ServiceTaskStatus::InProgress),
        TaskStatus::Completed => Some(ServiceTaskStatus::Completed),
        TaskStatus::Unspecified => None,
    }
}

fn from_service_status(status: ServiceTaskStatus) -> TaskStatus {
    match status {
        ServiceTaskStatus::Pending => TaskStatus::Pending,
        ServiceTaskStatus::InProgress => TaskStatus::InProgress,
        ServiceTaskStatus::Completed => TaskStatus::Completed,
    }
}

fn to_proto_task(task: crate::services::coordination::Task) -> Task {
    Task {
        id: task.id,
        subject: task.subject,
        description: task.description,
        status: from_service_status(task.status) as i32,
        owner: task.owner.to_string(),
        blocked_by: task.blocked_by,
    }
}

#[async_trait]
impl CoordinationEffects for CoordinationHandler {
    async fn create_task(&self, req: CreateTaskRequest) -> EffectResult<CreateTaskResponse> {
        let task_id = self
            .service
            .create_task(
                req.subject,
                req.description,
                AgentName::from(req.owner.as_str()),
                req.blocked_by,
            )
            .await;

        Ok(CreateTaskResponse { task_id })
    }

    async fn update_task(&self, req: UpdateTaskRequest) -> EffectResult<UpdateTaskResponse> {
        let status = to_service_status(req.status());
        let owner = if req.owner.is_empty() {
            None
        } else {
            Some(AgentName::from(req.owner.as_str()))
        };
        let description = if req.description.is_empty() {
            None
        } else {
            Some(req.description)
        };
        let subject = if req.subject.is_empty() {
            None
        } else {
            Some(req.subject)
        };

        let success = self
            .service
            .update_task(req.task_id, status, owner, description, subject)
            .await;

        Ok(UpdateTaskResponse { success })
    }

    async fn list_tasks(&self, req: ListTasksRequest) -> EffectResult<ListTasksResponse> {
        let filter_status = to_service_status(req.filter_status());
        let tasks = self.service.list_tasks(filter_status).await;

        Ok(ListTasksResponse {
            tasks: tasks.into_iter().map(to_proto_task).collect(),
        })
    }

    async fn get_task(&self, req: GetTaskRequest) -> EffectResult<GetTaskResponse> {
        let task = self.service.get_task(&req.task_id).await;

        Ok(GetTaskResponse {
            task: task.map(to_proto_task),
        })
    }

    async fn send_message(&self, req: SendMessageRequest) -> EffectResult<SendMessageResponse> {
        self.service
            .send_message(AgentName::from(req.from.as_str()), req.text, req.summary)
            .await;
        Ok(SendMessageResponse { success: true })
    }

    async fn get_messages(&self, req: GetMessagesRequest) -> EffectResult<GetMessagesResponse> {
        let filter = if req.unread_only {
            crate::domain::MessageFilter::UnreadOnly
        } else {
            crate::domain::MessageFilter::All
        };
        let messages = self.service.get_messages(filter).await;

        Ok(GetMessagesResponse {
            messages: messages
                .into_iter()
                .map(|m| AgentMessage {
                    from: m.from.to_string(),
                    text: m.text,
                    summary: m.summary,
                    timestamp: m.timestamp,
                    read: m.read,
                })
                .collect(),
        })
    }
}
