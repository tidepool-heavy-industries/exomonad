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
use crate::services::messaging;
use crate::services::questions::QuestionRegistry;
use async_trait::async_trait;
use exomonad_proto::effects::messaging::*;
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;
use tracing::info;

/// Messaging effect handler.
///
/// Handles all `messaging.*` effects by reading/writing JSON inbox files
/// in the Teams directory. The `QuestionRegistry` bridges `answer_question`
/// (TL-side) with `send_question` (agent-side) for immediate unblocking.
pub struct MessagingHandler {
    question_registry: Option<Arc<QuestionRegistry>>,
}

impl MessagingHandler {
    pub fn new() -> Self {
        Self {
            question_registry: None,
        }
    }

    pub fn with_question_registry(mut self, registry: Arc<QuestionRegistry>) -> Self {
        self.question_registry = Some(registry);
        self
    }
}

impl Default for MessagingHandler {
    fn default() -> Self {
        Self::new()
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
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();
        let tl_inbox = inbox::inbox_path(&team_name, "team-lead");

        let msg = inbox::create_message(
            agent_id,
            req.content.clone(),
            Some(req.content.chars().take(50).collect()),
        );

        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox, &msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        Ok(SendNoteResponse { ack: true })
    }

    async fn send_question(&self, req: SendQuestionRequest) -> EffectResult<SendQuestionResponse> {
        let team_name = get_team_name()?;
        let agent_id = get_agent_id();
        let tl_inbox = inbox::inbox_path(&team_name, "team-lead");
        let my_inbox = inbox::inbox_path(&team_name, &agent_id);

        let text = format!("[QUESTION] {}", req.question);
        let msg = inbox::create_message(
            agent_id,
            text,
            Some(req.question.chars().take(50).collect()),
        );

        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox, &msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        // Poll for answer
        let timeout = Duration::from_secs(300);
        let interval = Duration::from_secs(5);
        let start = std::time::Instant::now();

        loop {
            if start.elapsed() >= timeout {
                return Err(EffectError::custom(
                    "messaging_error",
                    format!("Question timed out after {}s", timeout.as_secs()),
                ));
            }

            let my_inbox_clone = my_inbox.clone();
            let unread = tokio::task::spawn_blocking(move || inbox::read_unread(&my_inbox_clone))
                .await
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

            if let Some(answer) = unread.into_iter().next() {
                // Mark as read
                let my_inbox_mark = inbox::inbox_path(&team_name, &get_agent_id());
                let _ =
                    tokio::task::spawn_blocking(move || inbox::mark_all_read(&my_inbox_mark)).await;

                return Ok(SendQuestionResponse {
                    answer: answer.text,
                });
            }

            sleep(interval).await;
        }
    }

    async fn get_agent_messages(
        &self,
        req: GetAgentMessagesRequest,
    ) -> EffectResult<GetAgentMessagesResponse> {
        let team_name = if req.team_name.is_empty() {
            get_team_name()?
        } else {
            req.team_name.clone()
        };

        info!(agent_id = %req.agent_id, team = %team_name, "Getting agent messages");

        if !req.agent_id.is_empty() {
            // Read specific agent's inbox
            let messages = messaging::read_agent_inbox(&team_name, &req.agent_id)
                .await
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

            info!(agent = %req.agent_id, count = messages.len(), "Read agent messages from Teams inbox");

            let agent_msgs = AgentMessages {
                agent_id: req.agent_id,
                messages: messages
                    .into_iter()
                    .map(|m| AgentMessage {
                        from: m.from,
                        text: m.text,
                        summary: m.summary,
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
            // Read all agent messages from TL inbox
            let results = messaging::scan_all_agent_messages_teams(&team_name)
                .await
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

            let agents: Vec<AgentMessages> = results
                .into_iter()
                .map(|(id, msgs)| AgentMessages {
                    agent_id: id,
                    messages: msgs
                        .into_iter()
                        .map(|m| AgentMessage {
                            from: m.from,
                            text: m.text,
                            summary: m.summary,
                            timestamp: m.timestamp,
                            read: m.read,
                        })
                        .collect(),
                })
                .collect();

            info!(
                agent_count = agents.len(),
                "Scanned all agent messages from Teams inboxes"
            );

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
        let team_name = if req.team_name.is_empty() {
            get_team_name()?
        } else {
            req.team_name.clone()
        };

        info!(
            agent = %req.agent_id,
            question_id = %req.question_id,
            team = %team_name,
            "Answering question via Teams inbox"
        );

        messaging::write_to_agent_inbox(&team_name, &req.agent_id, &req.answer)
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        // Resolve the oneshot channel so send_question unblocks immediately.
        if let Some(ref registry) = self.question_registry {
            let resolved = registry.resolve(&req.question_id, req.answer.clone());
            info!(
                agent = %req.agent_id,
                question_id = %req.question_id,
                resolved,
                "Resolved question via QuestionRegistry"
            );
        }

        info!(agent = %req.agent_id, question_id = %req.question_id, "Answer written to Teams inbox");

        Ok(AnswerQuestionResponse {
            status: "answered".to_string(),
            agent_id: req.agent_id,
            question_id: req.question_id,
        })
    }
}

fn get_team_name() -> Result<String, EffectError> {
    if let Ok(name) = std::env::var("EXOMONAD_TEAM_NAME") {
        return Ok(name);
    }
    if let Ok(name) = std::env::var("CLAUDE_TEAM_NAME") {
        return Ok(name);
    }

    let config_path = std::env::current_dir()
        .unwrap_or_default()
        .join(".exomonad")
        .join("config.toml");

    if config_path.exists() {
        if let Ok(content) = std::fs::read_to_string(&config_path) {
            for line in content.lines() {
                let line = line.trim();
                if line.starts_with("team_name") {
                    if let Some(val) = line.split('=').nth(1) {
                        return Ok(val.trim().trim_matches('"').trim_matches('\'').to_string());
                    }
                }
            }
        }
    }

    Err(EffectError::custom(
        "messaging_error",
        "EXOMONAD_TEAM_NAME or CLAUDE_TEAM_NAME not set and no team_name in .exomonad/config.toml",
    ))
}

fn get_agent_id() -> String {
    crate::mcp::agent_identity::get_agent_id()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_inbox_path_uses_service() {
        let path = inbox::inbox_path("myteam", "myagent");
        assert!(path.to_string_lossy().contains("myteam"));
        assert!(path.to_string_lossy().contains("myagent.json"));
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
