//! Messaging effect handler for the `messaging.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::messaging`.
//! Inbox operations delegate to `services::inbox` (single code path).
//!
//! Handles both directions:
//! - **Agent→TL**: `send_note`, `send_question` (written by agents)
//! - **TL→Agent**: `get_agent_messages`, `answer_question` (called by TL)

use crate::effects::{
    dispatch_messaging_effect, EffectError, EffectHandler, EffectResult, MessagingEffects,
};
use crate::services::inbox;
use crate::services::questions::QuestionRegistry;
use async_trait::async_trait;
use exomonad_proto::effects::messaging::*;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tracing::info;

/// Messaging effect handler.
///
/// Handles all `messaging.*` effects by reading/writing JSON inbox files
/// in `.exomonad/messages/`. The `QuestionRegistry` bridges `answer_question`
/// (TL-side) with `send_question` (agent-side) for immediate unblocking.
pub struct MessagingHandler {
    question_registry: Arc<QuestionRegistry>,
    project_dir: PathBuf,
}

impl MessagingHandler {
    pub fn new(question_registry: Arc<QuestionRegistry>, project_dir: PathBuf) -> Self {
        Self {
            question_registry,
            project_dir,
        }
    }
}

#[async_trait]
impl EffectHandler for MessagingHandler {
    fn namespace(&self) -> &str {
        "messaging"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_messaging_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl MessagingEffects for MessagingHandler {
    async fn send_note(&self, req: SendNoteRequest) -> EffectResult<SendNoteResponse> {
        let agent_id = get_agent_id();
        let tl_inbox = inbox::inbox_path(&self.project_dir, "team-lead");

        let msg = inbox::create_message(
            agent_id,
            req.content.clone(),
            Some(req.content.chars().take(50).collect()),
        );

        let tl_inbox_clone = tl_inbox.clone();
        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox_clone, &msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        Ok(SendNoteResponse { ack: true })
    }

    async fn send_question(&self, req: SendQuestionRequest) -> EffectResult<SendQuestionResponse> {
        let agent_id = get_agent_id();

        info!(agent = %agent_id, "Sending question to TL");

        // Register in QuestionRegistry for oneshot wakeup
        let (question_id, rx) = self.question_registry.register();

        // Send question to TL inbox with question_id for correlation
        let tl_inbox = inbox::inbox_path(&self.project_dir, "team-lead");
        let text = format!("[QUESTION q_id={}] {}", question_id, req.question);
        let msg = inbox::create_message(
            agent_id.clone(),
            text,
            Some(req.question.chars().take(50).collect()),
        );

        let tl_inbox_clone = tl_inbox.clone();
        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox_clone, &msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        // Await answer with 5 minute timeout (no polling)
        let timeout_dur = Duration::from_secs(300);
        match tokio::time::timeout(timeout_dur, rx).await {
            Ok(Ok(answer)) => {
                info!(
                    agent = %agent_id,
                    question_id = %question_id,
                    "Answer received via MessagingHandler"
                );
                Ok(SendQuestionResponse { answer })
            }
            Ok(Err(_)) => {
                self.question_registry.cancel(&question_id);
                Err(EffectError::custom(
                    "messaging_error",
                    "Question channel closed unexpectedly",
                ))
            }
            Err(_) => {
                self.question_registry.cancel(&question_id);
                info!(
                    agent = %agent_id,
                    question_id = %question_id,
                    "Question timed out after 300s"
                );
                Err(EffectError::timeout(format!(
                    "send_question({}) timed out after 300s",
                    question_id
                )))
            }
        }
    }

    async fn get_agent_messages(
        &self,
        req: GetAgentMessagesRequest,
    ) -> EffectResult<GetAgentMessagesResponse> {
        info!(agent_id = %req.agent_id, "Getting agent messages");

        if !req.agent_id.is_empty() {
            // Read specific agent's messages from TL inbox (messages FROM this agent)
            let tl_inbox = inbox::inbox_path(&self.project_dir, "team-lead");
            let all_messages = tokio::task::spawn_blocking(move || inbox::read_unread(&tl_inbox))
                .await
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

            // Filter to messages from the requested agent
            let agent_messages: Vec<_> = all_messages
                .into_iter()
                .filter(|m| m.from == req.agent_id)
                .collect();

            info!(agent = %req.agent_id, count = agent_messages.len(), "Read agent messages");

            let agent_msgs = AgentMessages {
                agent_id: req.agent_id,
                messages: agent_messages
                    .into_iter()
                    .map(|m| AgentMessage {
                        from: m.from,
                        text: m.text,
                        summary: m.summary.unwrap_or_default(),
                        timestamp: m.timestamp,
                        read: m.read,
                    })
                    .collect(),
            };

            Ok(GetAgentMessagesResponse {
                agents: vec![agent_msgs],
                warning: String::new(),
            })
        } else {
            // Read all messages from TL inbox, grouped by sender
            let tl_inbox = inbox::inbox_path(&self.project_dir, "team-lead");
            let all_messages = tokio::task::spawn_blocking(move || inbox::read_unread(&tl_inbox))
                .await
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

            // Group messages by sender
            let mut grouped: std::collections::HashMap<String, Vec<inbox::InboxMessage>> =
                std::collections::HashMap::new();
            for msg in all_messages {
                grouped.entry(msg.from.clone()).or_default().push(msg);
            }

            let agents: Vec<AgentMessages> = grouped
                .into_iter()
                .map(|(id, msgs)| AgentMessages {
                    agent_id: id,
                    messages: msgs
                        .into_iter()
                        .map(|m| AgentMessage {
                            from: m.from,
                            text: m.text,
                            summary: m.summary.unwrap_or_default(),
                            timestamp: m.timestamp,
                            read: m.read,
                        })
                        .collect(),
                })
                .collect();

            info!(agent_count = agents.len(), "Scanned all agent messages");

            Ok(GetAgentMessagesResponse {
                agents,
                warning: String::new(),
            })
        }
    }

    async fn answer_question(
        &self,
        req: AnswerQuestionRequest,
    ) -> EffectResult<AnswerQuestionResponse> {
        info!(
            agent = %req.agent_id,
            question_id = %req.question_id,
            "Answering question"
        );

        // Write answer to agent's inbox
        let agent_inbox = inbox::inbox_path(&self.project_dir, &req.agent_id);
        let msg = inbox::create_message(
            "team-lead".to_string(),
            req.answer.clone(),
            Some(format!("Answer to {}", req.question_id)),
        );

        tokio::task::spawn_blocking(move || inbox::append_message(&agent_inbox, &msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        // Resolve the oneshot channel so send_question unblocks immediately.
        let resolved = self
            .question_registry
            .resolve(&req.question_id, req.answer.clone());
        info!(
            agent = %req.agent_id,
            question_id = %req.question_id,
            resolved,
            "Resolved question via QuestionRegistry"
        );

        Ok(AnswerQuestionResponse {
            status: "answered".to_string(),
            agent_id: req.agent_id,
            question_id: req.question_id,
        })
    }
}

fn get_agent_id() -> String {
    crate::mcp::agent_identity::get_agent_id()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;
    use tempfile::TempDir;

    #[test]
    fn test_inbox_path_uses_service() {
        let path = inbox::inbox_path(Path::new("/project"), "myagent");
        assert_eq!(
            path,
            PathBuf::from("/project/.exomonad/messages/myagent.json")
        );
    }

    #[test]
    fn test_create_and_read_message() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("inbox.json");

        let msg = inbox::create_message(
            "agent".to_string(),
            "hello".to_string(),
            Some("hello".to_string()),
        );
        inbox::append_message(&path, &msg).unwrap();

        let messages = inbox::read_inbox(&path).unwrap();
        assert_eq!(messages.len(), 1);
        assert_eq!(messages[0].text, "hello");
        assert_eq!(messages[0].from, "agent");
        assert!(!messages[0].read);
    }
}
