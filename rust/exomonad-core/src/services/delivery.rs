use crate::services::acp_registry::AcpRegistry;
use crate::services::event_log::EventLog;
use crate::services::event_queue::EventQueue;
use claude_teams_bridge::TeamRegistry;
use claude_teams_bridge as teams_mailbox;
use crate::services::tmux_events;
use agent_client_protocol::{Agent, PromptRequest};
use exomonad_proto::effects::events::{event, AgentMessage, Event};
use tracing::{debug, info, warn};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeliveryResult {
    Teams,
    Acp,
    Uds,
    Tmux,
    Failed,
}

/// Format a parent-facing notification message.
/// "failure" → `[FAILED: {id}] {msg}`, otherwise → `[from: {id}] {msg}`.
pub fn format_parent_notification(agent_id: &str, status: &str, message: &str) -> String {
    match status {
        "failure" => format!(
            "[FAILED: {}] {}",
            agent_id,
            if message.is_empty() {
                "Task failed."
            } else {
                message
            }
        ),
        _ => format!(
            "[from: {}] {}",
            agent_id,
            if message.is_empty() {
                "Status update."
            } else {
                message
            }
        ),
    }
}

/// Notify a parent agent. Single codepath for all parent notifications.
///
/// Pipeline: event log → EventQueue → format `[from: id]`/`[FAILED: id]` → deliver_to_agent.
/// Used by both `EventHandler::notify_parent` (agent-initiated) and the poller's
/// `NotifyParentAction` (system-initiated via event handlers).
///
/// All messages are prefixed with `[from: id]` (or `[FAILED: id]` for failures).
/// Event handler messages include their own structural tags (e.g. `[PR READY]`)
/// inside the message body, so the TL sees: `[from: leaf-id] [PR READY] PR #5...`
///
/// For peer-to-peer messaging, use `deliver_to_agent()` directly instead.
pub async fn notify_parent_delivery(
    team_registry: Option<&TeamRegistry>,
    acp_registry: Option<&AcpRegistry>,
    event_log: Option<&EventLog>,
    event_queue: &EventQueue,
    project_dir: &std::path::Path,
    agent_id: &str,
    parent_session_id: &str,
    parent_tab_name: &str,
    status: &str,
    message: &str,
    summary: Option<&str>,
) -> DeliveryResult {
    // 1. Log to event log
    if let Some(log) = event_log {
        if let Err(e) = log.append(
            "agent.notify_parent",
            agent_id,
            &serde_json::json!({
                "parent": parent_session_id,
                "status": status,
                "message": message,
            }),
        ) {
            tracing::warn!(error = %e, "Failed to write notify_parent event");
        }
    }

    // 2. Publish to event queue
    let event = Event {
        event_id: 0,
        event_type: Some(event::EventType::AgentMessage(AgentMessage {
            agent_id: agent_id.to_string(),
            status: status.to_string(),
            message: message.to_string(),
            changes: Vec::new(),
        })),
    };
    event_queue.notify_event(parent_session_id, event).await;

    // 3. Format and deliver
    let notification = format_parent_notification(agent_id, status, message);
    let default_summary = format!("Agent update: {}", agent_id);
    let summary = summary.unwrap_or(&default_summary);

    let delivery_result = deliver_to_agent(
        team_registry,
        acp_registry,
        project_dir,
        parent_session_id,
        parent_tab_name,
        agent_id,
        &notification,
        summary,
    )
    .await;

    // 4. Log delivery result
    if let Some(log) = event_log {
        let delivery_method = match delivery_result {
            DeliveryResult::Teams => "teams_inbox",
            DeliveryResult::Acp => "acp",
            DeliveryResult::Uds => "unix_socket",
            DeliveryResult::Tmux => "tmux_stdin",
            DeliveryResult::Failed => "failed",
        };
        if let Err(e) = log.append(
            "agent.message_delivered",
            agent_id,
            &serde_json::json!({
                "parent": parent_session_id,
                "method": delivery_method,
            }),
        ) {
            tracing::warn!(error = %e, "Failed to write message_delivered event");
        }
    }

    delivery_result
}

/// Deliver a notification via HTTP POST over a Unix domain socket.
/// Fire-and-forget with 5s timeout.
async fn deliver_via_uds(
    socket_path: &std::path::Path,
    from: &str,
    message: &str,
    summary: &str,
) -> Result<(), String> {
    use std::time::Duration;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    use tokio::net::UnixStream;

    let body = serde_json::json!({
        "from": from,
        "message": message,
        "summary": summary,
    });
    let body_bytes = serde_json::to_vec(&body).map_err(|e| e.to_string())?;

    let request = format!(
        "POST /notify HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body_bytes.len()
    );

    let result = tokio::time::timeout(Duration::from_secs(5), async {
        let mut stream = UnixStream::connect(socket_path)
            .await
            .map_err(|e| e.to_string())?;
        stream
            .write_all(request.as_bytes())
            .await
            .map_err(|e| e.to_string())?;
        stream
            .write_all(&body_bytes)
            .await
            .map_err(|e| e.to_string())?;
        stream.flush().await.map_err(|e| e.to_string())?;

        let mut buf = [0u8; 128];
        let n = stream.read(&mut buf).await.map_err(|e| e.to_string())?;
        let response = String::from_utf8_lossy(&buf[..n]);
        if response.contains("200") || response.contains("204") || response.contains("202") {
            Ok(())
        } else {
            Err(format!(
                "UDS server responded: {}",
                response.lines().next().unwrap_or("empty")
            ))
        }
    })
    .await;

    match result {
        Ok(inner) => inner,
        Err(_) => Err("UDS delivery timed out after 5s".to_string()),
    }
}

/// Deliver a message to an agent.
///
/// Tries Teams inbox delivery if a registry and agent key are provided.
/// Attempts ACP prompt delivery if a registry is provided and agent is registered.
/// Attempts HTTP-over-UDS delivery for custom binary agents (e.g., shoal-agent).
/// Falls back to tmux input injection if other delivery methods fail or are not available.
pub async fn deliver_to_agent(
    team_registry: Option<&TeamRegistry>,
    acp_registry: Option<&super::acp_registry::AcpRegistry>,
    project_dir: &std::path::Path,
    agent_key: &str,
    tmux_target: &str,
    from: &str,
    message: &str,
    summary: &str,
) -> DeliveryResult {
    if let Some(registry) = team_registry {
        if let Some(team_info) = registry.get(agent_key).await {
            match teams_mailbox::write_to_inbox(
                &team_info.team_name,
                &team_info.inbox_name,
                from,
                message,
                summary,
            ) {
                Ok(timestamp) => {
                    info!(
                        agent = %agent_key,
                        team = %team_info.team_name,
                        inbox = %team_info.inbox_name,
                        timestamp = %timestamp,
                        "Wrote message to Teams inbox, spawning delivery verifier (30s)"
                    );
                    // Spawn background task to verify CC's InboxPoller read the message.
                    // If not read within 30s, fall back to tmux STDIN injection.
                    let team_name = team_info.team_name.clone();
                    let inbox_name = team_info.inbox_name.clone();
                    let agent = agent_key.to_string();
                    let target = tmux_target.to_string();
                    let msg = message.to_string();
                    tokio::spawn(async move {
                        for attempt in 1..=3 {
                            tokio::time::sleep(std::time::Duration::from_secs(10)).await;
                            let is_read =
                                teams_mailbox::is_message_read(&team_name, &inbox_name, &timestamp);
                            info!(
                                agent = %agent,
                                team = %team_name,
                                inbox = %inbox_name,
                                timestamp = %timestamp,
                                attempt,
                                is_read,
                                "Delivery verifier poll"
                            );
                            if is_read {
                                return;
                            }
                        }
                        warn!(
                            agent = %agent,
                            team = %team_name,
                            target = %target,
                            "Teams inbox message not read after 30s, falling back to tmux injection"
                        );
                        if let Err(e) = tmux_events::inject_input(&target, &msg).await {
                            warn!(target = %target, error = %e, "tmux inject_input failed (Teams fallback)");
                        }
                    });
                    return DeliveryResult::Teams;
                }
                Err(e) => {
                    warn!(
                        agent = %agent_key,
                        error = %e,
                        "Teams inbox write failed, falling back to ACP/tmux"
                    );
                }
            }
        }
    }

    if let Some(registry) = acp_registry {
        if let Some(conn) = registry.get(agent_key).await {
            match conn
                .conn
                .prompt(PromptRequest::new(
                    conn.session_id.clone(),
                    // ACP prompt content can be multiple messages, but we deliver one-at-a-time here.
                    vec![message.into()],
                ))
                .await
            {
                Ok(_) => {
                    info!(agent = %agent_key, "Delivered message via ACP prompt");
                    return DeliveryResult::Acp;
                }
                Err(e) => {
                    warn!(
                        agent = %agent_key,
                        error = ?e,
                        "ACP prompt failed, falling back to tmux"
                    );
                }
            }
        }
    }

    // Try HTTP-over-UDS delivery (for custom binary agents like shoal-agent)
    let socket_path = project_dir.join(format!(".exo/agents/{}/notify.sock", agent_key));
    if socket_path.exists() {
        match deliver_via_uds(&socket_path, from, message, summary).await {
            Ok(()) => {
                info!(agent = %agent_key, socket = %socket_path.display(), "Delivered message via Unix socket");
                return DeliveryResult::Uds;
            }
            Err(e) => {
                warn!(agent = %agent_key, error = %e, "UDS delivery failed, falling back to tmux");
            }
        }
    }

    // For worker agents (panes in a shared tab), routing.json records the parent tab
    // and pane display name written at spawn time. Use it if present — the computed
    // tmux_target is wrong for workers (no such tab exists).
    let routing_path = project_dir
        .join(".exo")
        .join("agents")
        .join(format!("{}-gemini", agent_key))
        .join("routing.json");
    if routing_path.exists() {
        if let Ok(content) = std::fs::read_to_string(&routing_path) {
            if let Ok(routing) = serde_json::from_str::<serde_json::Value>(&content) {
                // Use pane_id (%N) for direct tmux targeting
                let target = routing["pane_id"]
                    .as_str()
                    .or_else(|| routing["parent_tab"].as_str())
                    .unwrap_or("TL");
                info!(
                    agent = %agent_key,
                    target,
                    chars = message.len(),
                    "Injecting message into worker pane via routing.json"
                );
                if let Err(e) = tmux_events::inject_input(target, message).await {
                    warn!(target = %target, error = %e, "tmux inject_input failed (routing.json)");
                }
                return DeliveryResult::Tmux;
            }
        }
    }

    debug!(
        target = %tmux_target,
        agent = %agent_key,
        chars = message.len(),
        "Injecting message into agent pane via tmux"
    );
    if let Err(e) = tmux_events::inject_input(tmux_target, message).await {
        warn!(target = %tmux_target, error = %e, "tmux inject_input failed (fallback)");
    }
    DeliveryResult::Tmux
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_parent_notification_success() {
        let msg = format_parent_notification("agent-1", "success", "All done");
        assert_eq!(msg, "[from: agent-1] All done");
    }

    #[test]
    fn test_format_parent_notification_success_empty() {
        let msg = format_parent_notification("agent-1", "success", "");
        assert_eq!(msg, "[from: agent-1] Status update.");
    }

    #[test]
    fn test_format_parent_notification_failure() {
        let msg = format_parent_notification("agent-2", "failure", "Something went wrong");
        assert_eq!(msg, "[FAILED: agent-2] Something went wrong");
    }

    #[test]
    fn test_format_parent_notification_failure_empty() {
        let msg = format_parent_notification("agent-2", "failure", "");
        assert_eq!(msg, "[FAILED: agent-2] Task failed.");
    }

    #[test]
    fn test_format_parent_notification_other_status() {
        let msg = format_parent_notification("agent-3", "running", "Working...");
        assert_eq!(msg, "[from: agent-3] Working...");
    }

    #[test]
    fn test_delivery_result_variants_distinct() {
        assert_ne!(DeliveryResult::Teams, DeliveryResult::Tmux);
        assert_ne!(DeliveryResult::Teams, DeliveryResult::Failed);
        assert_ne!(DeliveryResult::Tmux, DeliveryResult::Failed);
    }

    #[tokio::test]
    async fn test_deliver_no_registry_returns_tmux() {
        let result = deliver_to_agent(
            None,
            None,
            std::path::Path::new("/tmp/nonexistent"),
            "agent-1",
            "tab-1",
            "test",
            "hello",
            "summary",
        )
        .await;
        assert_eq!(result, DeliveryResult::Tmux);
    }
}
