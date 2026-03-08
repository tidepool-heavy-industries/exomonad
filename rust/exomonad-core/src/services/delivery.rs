use crate::services::team_registry::TeamRegistry;
use crate::services::teams_mailbox;
use crate::services::zellij_events;
use agent_client_protocol::{Agent, PromptRequest};
use tracing::{debug, info, warn};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeliveryResult {
    Teams,
    Acp,
    Uds,
    Zellij,
    Failed,
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
/// Falls back to Zellij input injection if other delivery methods fail or are not available.
pub async fn deliver_to_agent(
    team_registry: Option<&TeamRegistry>,
    acp_registry: Option<&super::acp_registry::AcpRegistry>,
    project_dir: &std::path::Path,
    agent_key: &str,
    zellij_tab_name: &str,
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
                Ok(()) => {
                    info!(
                        agent = %agent_key,
                        team = %team_info.team_name,
                        "Delivered message via Teams inbox"
                    );
                    return DeliveryResult::Teams;
                }
                Err(e) => {
                    warn!(
                        agent = %agent_key,
                        error = %e,
                        "Teams inbox write failed, falling back to ACP/Zellij"
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
                        "ACP prompt failed, falling back to Zellij"
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
                warn!(agent = %agent_key, error = %e, "UDS delivery failed, falling back to Zellij");
            }
        }
    }

    debug!(
        tab = %zellij_tab_name,
        chars = message.len(),
        "Injecting message into agent pane via Zellij"
    );
    zellij_events::inject_input(zellij_tab_name, message);
    DeliveryResult::Zellij
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delivery_result_variants_distinct() {
        assert_ne!(DeliveryResult::Teams, DeliveryResult::Zellij);
        assert_ne!(DeliveryResult::Teams, DeliveryResult::Failed);
        assert_ne!(DeliveryResult::Zellij, DeliveryResult::Failed);
    }

    #[tokio::test]
    async fn test_deliver_no_registry_returns_zellij() {
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
        assert_eq!(result, DeliveryResult::Zellij);
    }
}
