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
use crate::services::zellij_events;
use async_trait::async_trait;
use exomonad_proto::effects::messaging::*;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tracing::info;

/// Messaging effect handler.
///
/// Handles all `messaging.*` effects by reading/writing JSON inbox files
/// in `.exo/messages/`. The `QuestionRegistry` bridges `answer_question`
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
            agent_id.clone(),
            req.content.clone(),
            Some(req.content.chars().take(50).collect()),
        );

        let tl_inbox_clone = tl_inbox.clone();
        tokio::task::spawn_blocking(move || inbox::append_message(&tl_inbox_clone, &msg))
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        // Best-effort: push into parent's pane via Zellij plugin (inbox is source of truth)
        let tab_name = resolve_parent_tab_name();
        let formatted = format!("[note from {}] {}", agent_id, req.content);
        zellij_events::inject_input(&tab_name, &formatted);

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

        // Best-effort: push into parent's pane via Zellij plugin (inbox is source of truth)
        let tab_name = resolve_parent_tab_name();
        let formatted = format!(
            "[question from {}, q_id={}] {}",
            agent_id, question_id, req.question
        );
        zellij_events::inject_input(&tab_name, &formatted);

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
        let timeout_secs = req.timeout_secs;
        info!(agent_id = %req.agent_id, timeout_secs, "Getting agent messages");

        let tl_inbox = inbox::inbox_path(&self.project_dir, "team-lead");

        // If timeout > 0, use long-polling
        let all_messages = if timeout_secs > 0 {
            let timeout = Duration::from_secs(timeout_secs as u64);
            let interval = Duration::from_secs(2); // Poll every 2 seconds

            info!("Starting long-poll for {}s", timeout_secs);
            let start = std::time::Instant::now();

            let tl_inbox_clone = tl_inbox.clone();
            let messages = tokio::task::spawn_blocking(move || {
                inbox::poll_unread(&tl_inbox_clone, timeout, interval)
            })
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

            info!(
                "Long-poll returned {} messages after {:.1}s",
                messages.len(),
                start.elapsed().as_secs_f64()
            );
            messages
        } else {
            // Immediate return (existing behavior)
            tokio::task::spawn_blocking(move || inbox::read_unread(&tl_inbox))
                .await
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
                .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?
        };

        if !req.agent_id.is_empty() {
            // Filter to messages from the requested agent
            let agent_messages: Vec<_> = all_messages
                .into_iter()
                .filter(|m| m.from == req.agent_id)
                .collect();

            info!(
                agent = %req.agent_id,
                count = agent_messages.len(),
                "Read agent messages"
            );

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
    crate::mcp::agent_identity::get_agent_id_string()
}

/// Resolve the Zellij tab name of the parent agent.
///
/// Uses the same identity model as `events.rs:notify_parent`:
/// - Workers (agent_id ends with `-gemini`): parent is derived from EXOMONAD_SESSION_ID
/// - Subtree agents: parent is one dot-level up in branch hierarchy
/// - Root agents (no dots): parent is the TL tab
fn resolve_parent_tab_name() -> String {
    let agent_id = get_agent_id();
    let session_id = std::env::var("EXOMONAD_SESSION_ID").unwrap_or_default();

    if agent_id.ends_with("-gemini") {
        // Worker: session_id is parent's session ID
        if session_id.contains('.') {
            // Parent is a subtree agent — extract slug from last dot segment
            let slug = session_id
                .rsplit_once('.')
                .map(|(_, s)| s)
                .unwrap_or(&session_id);
            format!("\u{1F916} {}", slug)
        } else {
            "TL".to_string()
        }
    } else {
        // Subtree agent: parent is one level up
        if let Some((parent, _)) = session_id.rsplit_once('.') {
            if parent.contains('.') {
                // Grandparent exists — parent is also a subtree
                let slug = parent.rsplit_once('.').map(|(_, s)| s).unwrap_or(parent);
                format!("\u{1F916} {}", slug)
            } else {
                "TL".to_string()
            }
        } else {
            "TL".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;
    use tempfile::TempDir;

    #[test]
    fn test_inbox_path_uses_service() {
        let path = inbox::inbox_path(Path::new("/project"), "myagent");
        assert_eq!(path, PathBuf::from("/project/.exo/messages/myagent.json"));
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
