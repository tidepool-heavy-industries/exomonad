//! Database edge case integration tests for mantle-hub.
//!
//! These tests verify database behavior including parent/child relationships,
//! graph data generation, and edge cases.

mod helpers;

use helpers::{
    make_test_result, make_test_register_with_parent, register_session, submit_via_socket,
    TestHub,
};

#[tokio::test]
async fn test_graph_with_forks() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create parent session
    let parent_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let parent_id = &parent_info.id;

    // Create child sessions
    let child1_req = make_test_register_with_parent(parent_id);
    let child1_resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&child1_req)
        .send()
        .await
        .unwrap();
    let child1: serde_json::Value = child1_resp.json().await.unwrap();

    let child2_req = make_test_register_with_parent(parent_id);
    let child2_resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&child2_req)
        .send()
        .await
        .unwrap();
    let child2: serde_json::Value = child2_resp.json().await.unwrap();

    // Get graph data
    let resp = client
        .get(format!("{}/api/graph", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let graph: serde_json::Value = resp.json().await.unwrap();
    let nodes = graph["nodes"].as_array().unwrap();
    let edges = graph["edges"].as_array().unwrap();

    assert_eq!(nodes.len(), 3, "Should have 3 nodes (parent + 2 children)");
    assert_eq!(edges.len(), 2, "Should have 2 edges (parent→child1, parent→child2)");

    // Verify edges point from parent to children
    for edge in edges {
        assert_eq!(
            edge["source"], parent_id.as_str(),
            "All edges should have parent as source"
        );
    }

    // Verify children are correct
    let targets: Vec<&str> = edges.iter().map(|e| e["target"].as_str().unwrap()).collect();
    assert!(targets.contains(&child1["id"].as_str().unwrap()));
    assert!(targets.contains(&child2["id"].as_str().unwrap()));
}

#[tokio::test]
async fn test_graph_node_states() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create sessions with different states
    let running_info = register_session(&client, &hub.http_url).await.unwrap();
    let completed_info = register_session(&client, &hub.http_url).await.unwrap();
    let failed_info = register_session(&client, &hub.http_url).await.unwrap();

    // Submit success result
    let success_result = make_test_result(&completed_info.id);
    submit_via_socket(hub.socket_path(), &success_result)
        .await
        .unwrap();

    // Submit error result
    let error_result = mantle_shared::hub::types::SessionResult {
        session_id: failed_info.id.clone(),
        exit_code: 1,
        is_error: true,
        result_text: Some("Failed".into()),
        structured_output: None,
        total_cost_usd: 0.01,
        num_turns: 1,
        cc_session_id: format!("cc-{}", failed_info.id),
        duration_secs: 1.0,
        model_usage: std::collections::HashMap::new(),
    };
    submit_via_socket(hub.socket_path(), &error_result)
        .await
        .unwrap();

    // Get graph data
    let resp = client
        .get(format!("{}/api/graph", hub.http_url))
        .send()
        .await
        .unwrap();

    let graph: serde_json::Value = resp.json().await.unwrap();
    let nodes = graph["nodes"].as_array().unwrap();

    // Find nodes by ID and check states
    let find_node = |id: &str| nodes.iter().find(|n| n["id"] == id).unwrap();

    assert_eq!(find_node(&running_info.id)["state"], "running");
    assert_eq!(find_node(&completed_info.id)["state"], "completed");
    assert_eq!(find_node(&failed_info.id)["state"], "failed");
}

#[tokio::test]
async fn test_delete_session_with_result() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register and submit result
    let session_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let session_id = &session_info.id;

    let result = make_test_result(session_id);
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

    // Verify both session and result are gone
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 404, "Session should be deleted");
}

#[tokio::test]
async fn test_session_timestamps() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register session
    let session_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let session_id = &session_info.id;

    // Get initial timestamps
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session1: serde_json::Value = resp.json().await.unwrap();
    let created_at = session1["created_at"].as_str().unwrap().to_string();
    let updated_at1 = session1["updated_at"].as_str().unwrap().to_string();

    // Wait a bit and submit result
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    let result = make_test_result(session_id);
    submit_via_socket(hub.socket_path(), &result)
        .await
        .unwrap();

    // Get updated timestamps
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session2: serde_json::Value = resp.json().await.unwrap();
    let updated_at2 = session2["updated_at"].as_str().unwrap().to_string();

    // created_at should be unchanged
    assert_eq!(session2["created_at"], created_at);

    // updated_at should be later
    assert_ne!(
        updated_at1, updated_at2,
        "updated_at should change after result submission"
    );
}

#[tokio::test]
async fn test_multiple_sessions_unique_ids() {
    // Test that each registration generates a unique ID
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let mut ids = std::collections::HashSet::new();

    // Register 10 sessions
    for _ in 0..10 {
        let session_info = register_session(&client, &hub.http_url).await.unwrap();
        assert!(
            ids.insert(session_info.id.clone()),
            "Session IDs should be unique"
        );
    }

    assert_eq!(ids.len(), 10, "Should have 10 unique session IDs");
}

#[tokio::test]
async fn test_graph_includes_cost_and_duration() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register and complete session
    let session_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let session_id = &session_info.id;

    let result = mantle_shared::hub::types::SessionResult {
        session_id: session_id.clone(),
        exit_code: 0,
        is_error: false,
        result_text: Some("Done".into()),
        structured_output: None,
        total_cost_usd: 0.0523,
        num_turns: 5,
        cc_session_id: format!("cc-{}", session_id),
        duration_secs: 42.5,
        model_usage: std::collections::HashMap::new(),
    };

    submit_via_socket(hub.socket_path(), &result)
        .await
        .unwrap();

    // Get graph data
    let resp = client
        .get(format!("{}/api/graph", hub.http_url))
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
async fn test_parent_child_relationship() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create parent session
    let parent_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let parent_id = &parent_info.id;

    // Create child with parent reference
    let child_req = make_test_register_with_parent(parent_id);
    let child_resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&child_req)
        .send()
        .await
        .unwrap();

    assert_eq!(child_resp.status(), 200);

    let child: serde_json::Value = child_resp.json().await.unwrap();
    assert_eq!(child["parent_id"], parent_id.as_str());
}
