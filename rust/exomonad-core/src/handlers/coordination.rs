use crate::domain::{AgentName, TaskId};
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

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_coordination_effect(self, effect_type, payload, ctx).await
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
        id: task.id.to_string(),
        subject: task.subject,
        description: task.description,
        status: from_service_status(task.status) as i32,
        owner: task.owner.to_string(),
        blocked_by: task.blocked_by.into_iter().map(|t| t.to_string()).collect(),
    }
}

#[async_trait]
impl CoordinationEffects for CoordinationHandler {
    async fn create_task(
        &self,
        req: CreateTaskRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<CreateTaskResponse> {
        tracing::info!(subject = %req.subject, "[Coordination] create_task starting");
        let blocked_by: Vec<TaskId> = req.blocked_by.iter().map(|s| TaskId::from(s.as_str())).collect();
        let task_id = self
            .service
            .create_task(
                req.subject,
                req.description,
                AgentName::from(req.owner.as_str()),
                blocked_by,
            )
            .await;

        tracing::info!(task_id = %task_id, "[Coordination] create_task complete");
        Ok(CreateTaskResponse { task_id: task_id.to_string() })
    }

    async fn update_task(
        &self,
        req: UpdateTaskRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<UpdateTaskResponse> {
        let task_id = TaskId::from(req.task_id.as_str());
        tracing::info!(task_id = %task_id, "[Coordination] update_task starting");
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
            .update_task(&task_id, status, owner, description, subject)
            .await;

        tracing::info!(success, "[Coordination] update_task complete");
        Ok(UpdateTaskResponse { success })
    }

    async fn list_tasks(
        &self,
        req: ListTasksRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ListTasksResponse> {
        tracing::info!("[Coordination] list_tasks starting");
        let filter_status = to_service_status(req.filter_status());
        let tasks = self.service.list_tasks(filter_status).await;

        tracing::info!(count = tasks.len(), "[Coordination] list_tasks complete");
        Ok(ListTasksResponse {
            tasks: tasks.into_iter().map(to_proto_task).collect(),
        })
    }

    async fn get_task(
        &self,
        req: GetTaskRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GetTaskResponse> {
        let task_id = TaskId::from(req.task_id.as_str());
        tracing::info!(task_id = %task_id, "[Coordination] get_task starting");
        let task = self.service.get_task(&task_id).await;

        tracing::info!(found = task.is_some(), "[Coordination] get_task complete");
        Ok(GetTaskResponse {
            task: task.map(to_proto_task),
        })
    }

    async fn send_message(
        &self,
        req: SendMessageRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SendMessageResponse> {
        tracing::info!(from = %req.from, "[Coordination] send_message starting");
        self.service
            .send_message(AgentName::from(req.from.as_str()), req.text, req.summary)
            .await;
        tracing::info!("[Coordination] send_message complete");
        Ok(SendMessageResponse { success: true })
    }

    async fn get_messages(
        &self,
        req: GetMessagesRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GetMessagesResponse> {
        tracing::info!(unread_only = req.unread_only, "[Coordination] get_messages starting");
        let filter = if req.unread_only {
            crate::domain::MessageFilter::UnreadOnly
        } else {
            crate::domain::MessageFilter::All
        };
        let messages = self.service.get_messages(filter).await;

        tracing::info!(count = messages.len(), "[Coordination] get_messages complete");
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
