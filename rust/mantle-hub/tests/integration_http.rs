//! HTTP API integration tests for mantle-hub.
//!
//! These tests verify all HTTP endpoints work correctly with the new
//! two-entity model (sessions + nodes).
//!
//! They spawn a real hub server but don't require Docker or Claude.
//!
//! Tests are marked #[serial(hub)] because they spawn TestHub instances that
//! can interfere with each other when run in parallel.

mod helpers;

use helpers::{
    create_child_node, create_session, make_test_node_result, make_test_register, TestHub,
};
use serial_test::serial;

// ============================================================================
// Session Lifecycle Tests
// ============================================================================

#[tokio::test]
#[serial(hub)]
async fn test_create_session() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let req = make_test_register();

    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200, "Session creation should succeed");

    let body: serde_json::Value = resp.json().await.unwrap();

    // Verify response has session and root_node
    assert!(
        body["session"]["id"]
            .as_str()
            .is_some_and(|s| !s.is_empty()),
        "Session ID should be non-empty"
    );
    assert!(
        body["root_node"]["id"]
            .as_str()
            .is_some_and(|s| !s.is_empty()),
        "Root node ID should be non-empty"
    );

    // Verify session state is derived from node state (running)
    assert_eq!(body["session"]["state"], "running");
    assert_eq!(body["session"]["name"], "test-branch");
    assert_eq!(body["session"]["node_count"], 1);

    // Verify root node
    assert_eq!(body["root_node"]["state"], "running");
    assert_eq!(body["root_node"]["branch"], "test-branch");
    assert_eq!(body["root_node"]["prompt"], "Test prompt");
    assert!(body["root_node"]["parent_node_id"].is_null());
}

#[tokio::test]
#[serial(hub)]
async fn test_list_sessions_empty() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let resp = client
        .get(format!("{}/api/sessions", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let sessions: Vec<serde_json::Value> = resp.json().await.unwrap();
    assert!(sessions.is_empty(), "New hub should have no sessions");
}

#[tokio::test]
#[serial(hub)]
async fn test_list_sessions_with_data() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create two sessions
    for _ in 0..2 {
        let req = make_test_register();
        client
            .post(format!("{}/api/sessions", hub.http_url))
            .json(&req)
            .send()
            .await
            .unwrap();
    }

    let resp = client
        .get(format!("{}/api/sessions", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let sessions: Vec<serde_json::Value> = resp.json().await.unwrap();
    assert_eq!(sessions.len(), 2, "Should have 2 sessions");
}

#[tokio::test]
#[serial(hub)]
async fn test_get_session_with_nodes() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;

    // Get session with nodes
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let body: serde_json::Value = resp.json().await.unwrap();

    // Verify session
    assert_eq!(body["session"]["id"], session_id.as_str());
    assert_eq!(body["session"]["node_count"], 1);

    // Verify nodes array
    let nodes = body["nodes"].as_array().unwrap();
    assert_eq!(nodes.len(), 1, "Should have 1 node (the root)");
    assert_eq!(nodes[0]["prompt"], "Test prompt");
}

#[tokio::test]
#[serial(hub)]
async fn test_get_session_not_found() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let resp = client
        .get(format!("{}/api/sessions/nonexistent", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 404, "Should return 404 for missing session");
}

#[tokio::test]
#[serial(hub)]
async fn test_delete_session() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;

    // Delete session
    let resp = client
        .delete(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 204, "Delete should return 204 No Content");

    // Verify it's gone
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 404, "Session should no longer exist");
}

// ============================================================================
// Node Operations Tests
// ============================================================================

#[tokio::test]
#[serial(hub)]
async fn test_create_child_node() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let root_node_id = &created.root_node.id;

    // Create child node
    let child: serde_json::Value =
        create_child_node(&client, &hub.http_url, session_id, root_node_id)
            .await
            .unwrap();

    assert!(
        child["node"]["id"]
            .as_str()
            .map(|s| !s.is_empty())
            .unwrap_or(false),
        "Child node ID should be non-empty"
    );
    assert_eq!(child["node"]["parent_node_id"], root_node_id.as_str());
    assert_eq!(child["node"]["branch"], "test-child-branch");

    // Verify session now has 2 nodes
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let body: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(body["session"]["node_count"], 2);
}

#[tokio::test]
#[serial(hub)]
async fn test_get_node() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Get node
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let node: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(node["id"], node_id.as_str());
    assert_eq!(node["session_id"], session_id.as_str());
}

#[tokio::test]
#[serial(hub)]
async fn test_get_node_wrong_session() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create two sessions
    let created1 = create_session(&client, &hub.http_url).await.unwrap();
    let created2 = create_session(&client, &hub.http_url).await.unwrap();

    // Try to get node from session1 using session2's path
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, created2.session.id, created1.root_node.id
        ))
        .send()
        .await
        .unwrap();

    assert_eq!(
        resp.status(),
        400,
        "Should return 400 when node doesn't belong to session"
    );
}

#[tokio::test]
#[serial(hub)]
async fn test_submit_node_result() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Submit result via HTTP
    let result = make_test_node_result(node_id);
    let resp = client
        .post(format!(
            "{}/api/sessions/{}/nodes/{}/result",
            hub.http_url, session_id, node_id
        ))
        .json(&result)
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200, "Result submission should succeed");

    // Verify node state changed
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    let node: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(node["state"], "completed");
    assert!(node["result"].is_object(), "Should have result attached");
    assert_eq!(node["result"]["exit_code"], 0);

    // Verify session state also changed (derived from nodes)
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let body: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(body["session"]["state"], "completed");
}

#[tokio::test]
#[serial(hub)]
async fn test_get_node_events_empty() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Get events (should be empty initially)
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}/events",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let events: Vec<serde_json::Value> = resp.json().await.unwrap();
    assert!(events.is_empty(), "Should have no events initially");
}

// ============================================================================
// Graph Data Tests
// ============================================================================

#[tokio::test]
#[serial(hub)]
async fn test_graph_data_single_node() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    let resp = client
        .get(format!(
            "{}/api/sessions/{}/graph",
            hub.http_url, session_id
        ))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let graph: serde_json::Value = resp.json().await.unwrap();
    let nodes = graph["nodes"].as_array().unwrap();
    let edges = graph["edges"].as_array().unwrap();

    assert_eq!(nodes.len(), 1);
    assert_eq!(nodes[0]["id"], node_id.as_str());
    assert!(edges.is_empty(), "Root node has no edges");
}

#[tokio::test]
#[serial(hub)]
async fn test_graph_data_with_children() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session with root -> child1 -> child2
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let root_id = &created.root_node.id;

    let child1: serde_json::Value = create_child_node(&client, &hub.http_url, session_id, root_id)
        .await
        .unwrap();
    let child1_id = child1["node"]["id"].as_str().unwrap();

    let _child2: serde_json::Value =
        create_child_node(&client, &hub.http_url, session_id, child1_id)
            .await
            .unwrap();

    // Get graph
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/graph",
            hub.http_url, session_id
        ))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let graph: serde_json::Value = resp.json().await.unwrap();
    let nodes = graph["nodes"].as_array().unwrap();
    let edges = graph["edges"].as_array().unwrap();

    assert_eq!(nodes.len(), 3, "Should have 3 nodes");
    assert_eq!(
        edges.len(),
        2,
        "Should have 2 edges (parent -> child links)"
    );

    // Verify edge structure
    let edge_targets: Vec<&str> = edges
        .iter()
        .map(|e| e["target"].as_str().unwrap())
        .collect();
    assert!(edge_targets.contains(&child1_id));
}

#[tokio::test]
#[serial(hub)]
async fn test_graph_not_found() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let resp = client
        .get(format!("{}/api/sessions/nonexistent/graph", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 404);
}

// ============================================================================
// Session State Derivation Tests
// ============================================================================

#[tokio::test]
#[serial(hub)]
async fn test_session_state_running() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session (has running node)
    let created = create_session(&client, &hub.http_url).await.unwrap();

    // Session should be running
    assert_eq!(created.session.state.to_string(), "running");
}

#[tokio::test]
#[serial(hub)]
async fn test_session_state_completed() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session and complete its node
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    let result = make_test_node_result(node_id);
    client
        .post(format!(
            "{}/api/sessions/{}/nodes/{}/result",
            hub.http_url, session_id, node_id
        ))
        .json(&result)
        .send()
        .await
        .unwrap();

    // Verify session is completed
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let body: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(body["session"]["state"], "completed");
}

#[tokio::test]
#[serial(hub)]
async fn test_session_state_failed() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session and fail its node
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    let mut result = make_test_node_result(node_id);
    result.exit_code = 1;
    result.is_error = true;

    client
        .post(format!(
            "{}/api/sessions/{}/nodes/{}/result",
            hub.http_url, session_id, node_id
        ))
        .json(&result)
        .send()
        .await
        .unwrap();

    // Verify session is failed
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let body: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(body["session"]["state"], "failed");
}