//! Integration tests for agent/getAuthenticatedExtendedCard endpoint (v0.3.0)

#![cfg(all(feature = "http-client", feature = "http-server"))]

mod common;

use a2a_rs::{
    adapter::{
        DefaultRequestProcessor, HttpClient, HttpServer, InMemoryTaskStorage, SimpleAgentInfo,
    },
    application::json_rpc,
    domain::A2AError,
    services::AsyncA2AClient,
};
use common::TestBusinessHandler;
use std::time::Duration;
use tokio::sync::oneshot;

async fn setup_server(port: u16, supports_authenticated_card: bool) -> oneshot::Sender<()> {
    let storage = InMemoryTaskStorage::new();
    let handler = TestBusinessHandler::with_storage(storage);

    // Create a single agent info instance to use for both processor and server
    let mut agent_info = SimpleAgentInfo::new(
        "Authenticated Card Test Agent".to_string(),
        format!("http://localhost:{}", port),
    )
    .with_description("Agent for testing authenticated extended card".to_string());

    if supports_authenticated_card {
        agent_info = agent_info.with_authenticated_extended_card();
    }

    // Clone the agent info for the processor
    let processor = DefaultRequestProcessor::with_handler(handler, agent_info.clone());

    let server = HttpServer::new(processor, agent_info, format!("127.0.0.1:{}", port));

    let (shutdown_tx, shutdown_rx) = oneshot::channel::<()>();

    tokio::spawn(async move {
        tokio::select! {
            _ = server.start() => {},
            _ = shutdown_rx => {}
        }
    });

    tokio::time::sleep(Duration::from_millis(100)).await;

    shutdown_tx
}

#[tokio::test]
async fn test_get_authenticated_extended_card_not_configured() {
    let port = 9100;
    let shutdown_tx = setup_server(port, false).await;

    let client = HttpClient::new(format!("http://localhost:{}", port));

    // Create request for authenticated extended card
    let request = json_rpc::GetAuthenticatedExtendedCardRequest::new();
    let response = client
        .send_request(&json_rpc::A2ARequest::GetAuthenticatedExtendedCard(request))
        .await
        .expect("Failed to send request");

    // Should return error -32007
    assert!(response.error.is_some(), "Should have error response");
    let error = response.error.unwrap();
    assert_eq!(
        error.code, -32007,
        "Should return error code -32007 (AUTHENTICATED_EXTENDED_CARD_NOT_CONFIGURED)"
    );
    assert!(
        error.message.contains("not configured")
            || error.message.contains("not supported")
            || error.message.contains("not available"),
        "Error message should indicate card not configured: {}",
        error.message
    );

    shutdown_tx.send(()).ok();
}

#[tokio::test]
async fn test_get_authenticated_extended_card_success() {
    let port = 9101;
    let shutdown_tx = setup_server(port, true).await;

    let client = HttpClient::new(format!("http://localhost:{}", port));

    // Create request for authenticated extended card
    let request = json_rpc::GetAuthenticatedExtendedCardRequest::new();
    let response = client
        .send_request(&json_rpc::A2ARequest::GetAuthenticatedExtendedCard(request))
        .await
        .expect("Failed to send request");

    // Should return successful response with AgentCard
    if let Some(ref err) = response.error {
        eprintln!(
            "Unexpected error: code={}, message={}",
            err.code, err.message
        );
    }
    assert!(response.error.is_none(), "Should not have error response");
    assert!(response.result.is_some(), "Should have result");

    let card_value = response.result.unwrap();
    let card: a2a_rs::domain::AgentCard =
        serde_json::from_value(card_value).expect("Failed to parse AgentCard");

    // Verify it's a valid agent card
    assert_eq!(card.name, "Authenticated Card Test Agent");
    assert!(!card.description.is_empty());
    assert_eq!(card.protocol_version, "0.3.0");

    // Verify this is the authenticated version (may have additional info)
    // The authenticated card should support this capability
    assert!(
        card.supports_authenticated_extended_card.unwrap_or(false),
        "Authenticated card should indicate support"
    );

    shutdown_tx.send(()).ok();
}

#[tokio::test]
async fn test_authenticated_card_vs_regular_card() {
    let port = 9102;
    let shutdown_tx = setup_server(port, true).await;

    let client = HttpClient::new(format!("http://localhost:{}", port));

    // Get regular card via HTTP endpoint
    let http_client = reqwest::Client::new();
    let regular_card_response = http_client
        .get(format!("http://localhost:{}/agent-card", port))
        .send()
        .await
        .expect("Failed to fetch regular agent card");

    let regular_card: a2a_rs::domain::AgentCard = regular_card_response
        .json()
        .await
        .expect("Failed to parse regular agent card");

    // Get authenticated extended card
    let request = json_rpc::GetAuthenticatedExtendedCardRequest::new();
    let response = client
        .send_request(&json_rpc::A2ARequest::GetAuthenticatedExtendedCard(request))
        .await
        .expect("Failed to send request");

    let auth_card: a2a_rs::domain::AgentCard = serde_json::from_value(response.result.unwrap())
        .expect("Failed to parse authenticated card");

    // Both should have same basic info
    assert_eq!(regular_card.name, auth_card.name);
    assert_eq!(regular_card.url, auth_card.url);
    assert_eq!(regular_card.protocol_version, auth_card.protocol_version);

    // Both should indicate support for authenticated extended card
    assert!(
        regular_card
            .supports_authenticated_extended_card
            .unwrap_or(false)
    );
    assert!(
        auth_card
            .supports_authenticated_extended_card
            .unwrap_or(false)
    );

    shutdown_tx.send(()).ok();
}

#[tokio::test]
async fn test_authenticated_card_error_structure() {
    let port = 9103;
    let shutdown_tx = setup_server(port, false).await;

    let client = HttpClient::new(format!("http://localhost:{}", port));

    // Try to get authenticated card when not configured
    let request = json_rpc::GetAuthenticatedExtendedCardRequest::new();
    let response = client
        .send_request(&json_rpc::A2ARequest::GetAuthenticatedExtendedCard(request))
        .await
        .expect("Failed to send request");

    assert!(response.error.is_some());
    let error = response.error.unwrap();

    // Verify error structure matches JSON-RPC spec
    assert_eq!(error.code, -32007);
    assert!(!error.message.is_empty());

    // Result should be None when there's an error
    assert!(response.result.is_none());

    // Response should have proper JSON-RPC version
    assert_eq!(response.jsonrpc, "2.0");

    shutdown_tx.send(()).ok();
}

#[test]
fn test_authenticated_extended_card_error_code() {
    // Test that the error enum produces correct error code
    let error = A2AError::AuthenticatedExtendedCardNotConfigured;
    let jsonrpc_error = error.to_jsonrpc_error();

    assert_eq!(jsonrpc_error["code"], -32007);
    assert_eq!(
        jsonrpc_error["message"],
        "Authenticated Extended Card is not configured"
    );
}

#[tokio::test]
async fn test_authenticated_card_with_extensions() {
    let port = 9104;
    let shutdown_tx = setup_server(port, true).await;

    let client = HttpClient::new(format!("http://localhost:{}", port));

    // Get authenticated card
    let request = json_rpc::GetAuthenticatedExtendedCardRequest::new();
    let response = client
        .send_request(&json_rpc::A2ARequest::GetAuthenticatedExtendedCard(request))
        .await
        .expect("Failed to send request");

    let card: a2a_rs::domain::AgentCard =
        serde_json::from_value(response.result.unwrap()).expect("Failed to parse card");

    // Card should have v0.3.0 fields
    assert_eq!(card.protocol_version, "0.3.0");
    assert_eq!(card.preferred_transport, "JSONRPC");

    // Should be able to have extensions (even if empty)
    // This verifies the authenticated card includes all v0.3.0 fields
    let capabilities = &card.capabilities;
    // Extensions field should be present in capabilities
    // (may be None or Some(vec![]))
    let _ = &capabilities.extensions;

    shutdown_tx.send(()).ok();
}
