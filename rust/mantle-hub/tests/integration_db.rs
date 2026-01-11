//! Database edge case integration tests for mantle-hub.
//!
//! These tests verify database behavior including node relationships,
//! graph data generation, and edge cases.

mod helpers;

use helpers::{
    create_child_node, create_session, make_test_node_result, submit_via_socket, TestHub,
};
use std::collections::HashMap;

#[tokio::test]
async fn test_graph_with_forks() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session with branching nodes: root -> (child1, child2)
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let root_id = &created.root_node.id;

    // Create two child nodes from the same parent (fork)
    let child1: serde_json::Value =
        create_child_node(&client, &hub.http_url, session_id, root_id)
            .await
            .unwrap();
    let child1_id = child1["node"]["id"].as_str().unwrap();

    let child2: serde_json::Value =
        create_child_node(&client, &hub.http_url, session_id, root_id)
            .await
            .unwrap();
    let child2_id = child2["node"]["id"].as_str().unwrap();

    // Get graph data
    let resp = client
        .get(format!("{}/api/sessions/{}/graph", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let graph: serde_json::Value = resp.json().await.unwrap();
    let nodes = graph["nodes"].as_array().unwrap();
    let edges = graph["edges"].as_array().unwrap();

    assert_eq!(nodes.len(), 3, "Should have 3 nodes (root + 2 children)");
    assert_eq!(
        edges.len(),
        2,
        "Should have 2 edges (root→child1, root→child2)"
    );

    // Verify edges point from parent to children
    for edge in edges {
        assert_eq!(
            edge["source"], root_id.as_str(),
            "All edges should have root as source"
        );
    }

    // Verify children are correct
    let targets: Vec<&str> = edges.iter().map(|e| e["target"].as_str().unwrap()).collect();
    assert!(targets.contains(&child1_id));
    assert!(targets.contains(&child2_id));
}

#[tokio::test]
async fn test_graph_node_states() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create sessions with different node states
    let running_session = create_session(&client, &hub.http_url).await.unwrap();
    let completed_session = create_session(&client, &hub.http_url).await.unwrap();
    let failed_session = create_session(&client, &hub.http_url).await.unwrap();

    // Submit success result for completed session's root node
    let success_result = make_test_node_result(&completed_session.root_node.id);
    submit_via_socket(hub.socket_path(), &success_result)
        .await
        .unwrap();

    // Submit error result for failed session's root node
    let error_result = mantle_shared::hub::types::NodeResult {
        node_id: failed_session.root_node.id.clone(),
        exit_code: 1,
        is_error: true,
        result_text: Some("Failed".into()),
        structured_output: None,
        total_cost_usd: 0.01,
        num_turns: 1,
        cc_session_id: format!("cc-{}", failed_session.root_node.id),
        duration_secs: 1.0,
        model_usage: HashMap::new(),
    };
    submit_via_socket(hub.socket_path(), &error_result)
        .await
        .unwrap();

    // Check running session's graph
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/graph",
            hub.http_url, running_session.session.id
        ))
        .send()
        .await
        .unwrap();
    let graph: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(graph["nodes"][0]["state"], "running");

    // Check completed session's graph
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/graph",
            hub.http_url, completed_session.session.id
        ))
        .send()
        .await
        .unwrap();
    let graph: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(graph["nodes"][0]["state"], "completed");

    // Check failed session's graph
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/graph",
            hub.http_url, failed_session.session.id
        ))
        .send()
        .await
        .unwrap();
    let graph: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(graph["nodes"][0]["state"], "failed");
}

#[tokio::test]
async fn test_delete_session_with_nodes() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session with child nodes and submit result
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Add a child node
    let _child = create_child_node(&client, &hub.http_url, session_id, node_id)
        .await
        .unwrap();

    // Submit result for root node
    let result = make_test_node_result(node_id);
    submit_via_socket(hub.socket_path(), &result)
        .await
        .unwrap();

    // Delete session
    let resp = client
        .delete(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 204, "Delete should succeed");

    // Verify session is gone
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 404, "Session should be deleted");

    // All nodes should be gone too (cascade delete)
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    // Should fail to find session first
    assert!(resp.status().is_client_error());
}

#[tokio::test]
async fn test_session_timestamps() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Get initial timestamps
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let body1: serde_json::Value = resp.json().await.unwrap();
    let created_at = body1["session"]["created_at"].as_str().unwrap().to_string();
    let updated_at1 = body1["session"]["updated_at"].as_str().unwrap().to_string();

    // Wait a bit and submit result
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    let result = make_test_node_result(node_id);
    submit_via_socket(hub.socket_path(), &result)
        .await
        .unwrap();

    // Get updated timestamps
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let body2: serde_json::Value = resp.json().await.unwrap();
    let updated_at2 = body2["session"]["updated_at"].as_str().unwrap().to_string();

    // created_at should be unchanged
    assert_eq!(body2["session"]["created_at"], created_at);

    // updated_at should be later
    assert_ne!(
        updated_at1, updated_at2,
        "updated_at should change after result submission"
    );
}

#[tokio::test]
async fn test_multiple_sessions_unique_ids() {
    // Test that each session generates unique IDs
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let mut session_ids = std::collections::HashSet::new();
    let mut node_ids = std::collections::HashSet::new();

    // Create 10 sessions
    for _ in 0..10 {
        let created = create_session(&client, &hub.http_url).await.unwrap();
        assert!(
            session_ids.insert(created.session.id.clone()),
            "Session IDs should be unique"
        );
        assert!(
            node_ids.insert(created.root_node.id.clone()),
            "Node IDs should be unique"
        );
    }

    assert_eq!(session_ids.len(), 10, "Should have 10 unique session IDs");
    assert_eq!(node_ids.len(), 10, "Should have 10 unique node IDs");
}

#[tokio::test]
async fn test_graph_includes_cost_and_duration() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create and complete session
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    let result = mantle_shared::hub::types::NodeResult {
        node_id: node_id.clone(),
        exit_code: 0,
        is_error: false,
        result_text: Some("Done".into()),
        structured_output: None,
        total_cost_usd: 0.0523,
        num_turns: 5,
        cc_session_id: format!("cc-{}", node_id),
        duration_secs: 42.5,
        model_usage: HashMap::new(),
    };

    submit_via_socket(hub.socket_path(), &result)
        .await
        .unwrap();

    // Get graph data
    let resp = client
        .get(format!("{}/api/sessions/{}/graph", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let graph: serde_json::Value = resp.json().await.unwrap();
    let node = &graph["nodes"].as_array().unwrap()[0];

    // Graph nodes should include metrics
    assert_eq!(node["total_cost_usd"], 0.0523);
    assert_eq!(node["duration_secs"], 42.5);
}

#[tokio::test]
async fn test_deep_node_chain() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session with deep chain: root -> n1 -> n2 -> n3
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let mut parent_id = created.root_node.id.clone();

    // Create 3 child nodes in chain
    for _ in 0..3 {
        let child: serde_json::Value =
            create_child_node(&client, &hub.http_url, session_id, &parent_id)
                .await
                .unwrap();
        parent_id = child["node"]["id"].as_str().unwrap().to_string();
    }

    // Get graph data
    let resp = client
        .get(format!("{}/api/sessions/{}/graph", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let graph: serde_json::Value = resp.json().await.unwrap();
    let nodes = graph["nodes"].as_array().unwrap();
    let edges = graph["edges"].as_array().unwrap();

    assert_eq!(nodes.len(), 4, "Should have 4 nodes (root + 3 children)");
    assert_eq!(edges.len(), 3, "Should have 3 edges");

    // Verify session node count
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let body: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(body["session"]["node_count"], 4);
}
