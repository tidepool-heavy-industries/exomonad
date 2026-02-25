use crate::services::team_registry::TeamRegistry;
use crate::services::teams_mailbox;
use crate::services::zellij_events;
use agent_client_protocol::{Agent, PromptRequest};
use tracing::{debug, info, warn};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeliveryResult {
    Teams,
    Acp,
    Zellij,
    Failed,
}

/// Deliver a message to an agent.
///
/// Tries Teams inbox delivery if a registry and agent key are provided.
/// Attempts ACP prompt delivery if a registry is provided and agent is registered.
/// Falls back to Zellij input injection if other delivery methods fail or are not available.
pub async fn deliver_to_agent(
    team_registry: Option<&TeamRegistry>,
    acp_registry: Option<&super::acp_registry::AcpRegistry>,
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
        let result = deliver_to_agent(None, None, "agent-1", "tab-1", "test", "hello", "summary").await;
        assert_eq!(result, DeliveryResult::Zellij);
    }
}
