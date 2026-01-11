//! End-to-end integration tests with real Docker containers and Claude Code.
//!
//! These tests are marked #[ignore] because they require:
//! 1. Docker daemon running
//! 2. ANTHROPIC_API_KEY environment variable set
//! 3. mantle-agent Docker image built
//!
//! Run with: cargo test -p mantle-hub --test integration_e2e -- --ignored --test-threads=1

mod helpers;

use bollard::models::{ContainerCreateBody, HostConfig, Mount, MountTypeEnum};
use bollard::query_parameters::{
    CreateContainerOptions, ListImagesOptions, RemoveContainerOptions, StartContainerOptions,
    WaitContainerOptions,
};
use bollard::Docker;
use futures_util::StreamExt;
use helpers::{create_session, TestHub};
use std::env::temp_dir;
use std::time::Duration;

/// Check if we have the prerequisites for E2E tests.
fn check_prerequisites() -> Result<(), String> {
    // Check for API key
    if std::env::var("ANTHROPIC_API_KEY").is_err() {
        return Err("ANTHROPIC_API_KEY environment variable not set".to_string());
    }

    Ok(())
}

/// Check if Docker is available and the mantle-agent image exists.
async fn check_docker() -> Result<Docker, String> {
    let docker = Docker::connect_with_local_defaults()
        .map_err(|e| format!("Failed to connect to Docker: {}", e))?;

    // Check if mantle-agent image exists
    let options = ListImagesOptions::default();
    let images = docker
        .list_images(Some(options))
        .await
        .map_err(|e| format!("Failed to list images: {}", e))?;

    let has_agent_image = images
        .iter()
        .any(|img| img.repo_tags.iter().any(|t| t.contains("mantle-agent")));

    if !has_agent_image {
        return Err(
            "mantle-agent Docker image not found. Build with: docker build -t mantle-agent:latest -f rust/mantle-agent/Dockerfile .".to_string()
        );
    }

    Ok(docker)
}

#[tokio::test]
#[ignore]
async fn test_e2e_multistep_task_with_structured_output() {
    // Check prerequisites
    if let Err(msg) = check_prerequisites() {
        eprintln!("Skipping E2E test: {}", msg);
        return;
    }

    let docker = match check_docker().await {
        Ok(d) => d,
        Err(msg) => {
            eprintln!("Skipping E2E test: {}", msg);
            return;
        }
    };

    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // 1. Setup task files in temp directory
    let test_dir_name = format!("e2e-test-{}", helpers::unique_id());
    let task_dir = temp_dir().join(&test_dir_name);
    std::fs::create_dir_all(&task_dir).expect("Failed to create task directory");

    // file_a.txt contains path to file_b.txt
    let file_b_path = task_dir.join("file_b.txt");
    std::fs::write(
        task_dir.join("file_a.txt"),
        file_b_path.display().to_string(),
    )
    .expect("Failed to write file_a.txt");

    // file_b.txt contains the final answer
    std::fs::write(&file_b_path, "The answer is: 42").expect("Failed to write file_b.txt");

    // 2. Create session via HTTP (returns session + root_node)
    let prompt = r#"Read the file at /workspace/file_a.txt. It contains a path to another file. Read that file too. Then output JSON with this exact format: {"file_a_content": "<contents of file_a>", "file_b_content": "<contents of the second file>"}"#;

    let req = mantle_shared::hub::types::SessionRegister {
        branch: "test-e2e".into(),
        worktree: task_dir.clone(),
        prompt: prompt.to_string(),
        model: "sonnet".into(),
    };

    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .expect("Failed to create session");

    assert_eq!(resp.status(), 200, "Session creation should succeed");

    let created: serde_json::Value = resp.json().await.unwrap();
    let session_id = created["session"]["id"].as_str().unwrap().to_string();
    let node_id = created["root_node"]["id"].as_str().unwrap().to_string();

    // 3. Create and start Docker container
    let container_name = format!("mantle-test-e2e-{}", session_id);

    let config = ContainerCreateBody {
        image: Some("mantle-agent:latest".to_string()),
        cmd: Some(vec![
            "wrap".to_string(),
            "--hub-socket".to_string(),
            "/tmp/mantle.sock".to_string(),
            "--node-id".to_string(),
            node_id.clone(),
            "--cwd".to_string(),
            "/workspace".to_string(),
            "--".to_string(),
            "-p".to_string(),
            prompt.to_string(),
            "--output-format".to_string(),
            "json".to_string(),
        ]),
        env: Some(vec![format!(
            "ANTHROPIC_API_KEY={}",
            std::env::var("ANTHROPIC_API_KEY").unwrap()
        )]),
        host_config: Some(HostConfig {
            mounts: Some(vec![
                // Mount hub socket
                Mount {
                    target: Some("/tmp/mantle.sock".to_string()),
                    source: Some(hub.socket_path().display().to_string()),
                    typ: Some(MountTypeEnum::BIND),
                    ..Default::default()
                },
                // Mount task directory
                Mount {
                    target: Some("/workspace".to_string()),
                    source: Some(task_dir.display().to_string()),
                    typ: Some(MountTypeEnum::BIND),
                    read_only: Some(true),
                    ..Default::default()
                },
            ]),
            ..Default::default()
        }),
        ..Default::default()
    };

    let create_options = CreateContainerOptions {
        name: Some(container_name.clone()),
        platform: String::new(),
    };

    let container = docker
        .create_container(Some(create_options), config)
        .await
        .expect("Failed to create container");

    let start_options = StartContainerOptions::default();
    docker
        .start_container(&container.id, Some(start_options))
        .await
        .expect("Failed to start container");

    // 4. Wait for container to finish (with timeout)
    let wait_options = WaitContainerOptions::default();
    let mut wait_stream = docker.wait_container(&container.id, Some(wait_options));

    let wait_result = tokio::time::timeout(Duration::from_secs(180), wait_stream.next()).await;

    // 5. Check container exit status
    match wait_result {
        Ok(Some(Ok(exit))) => {
            if exit.status_code != 0 {
                // Get container logs for debugging
                let log_options = bollard::query_parameters::LogsOptions {
                    stdout: true,
                    stderr: true,
                    ..Default::default()
                };
                let logs = docker
                    .logs(&container.id, Some(log_options))
                    .collect::<Vec<_>>()
                    .await;

                eprintln!("Container logs:");
                for log in logs {
                    if let Ok(log) = log {
                        eprintln!("{:?}", log);
                    }
                }

                panic!(
                    "Container exited with non-zero status: {}",
                    exit.status_code
                );
            }
        }
        Ok(Some(Err(e))) => panic!("Container wait error: {}", e),
        Ok(None) => panic!("Container wait stream ended unexpectedly"),
        Err(_) => panic!("Container timed out after 3 minutes"),
    }

    // 6. Get node result (should be available now)
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .expect("Failed to get node");

    assert_eq!(resp.status(), 200);
    let node: serde_json::Value = resp.json().await.unwrap();

    assert_eq!(
        node["state"], "completed",
        "Node should be completed. Full node: {:?}",
        node
    );

    let result = &node["result"];
    assert!(result.is_object(), "Result should exist");
    assert_eq!(result["exit_code"], 0, "Claude should succeed");
    assert_eq!(result["is_error"], false, "Should not be error");

    // 7. Verify structured output (if present)
    if let Some(structured) = result["structured_output"].as_object() {
        eprintln!("Structured output: {:?}", structured);
    }

    // 8. Verify cost metrics
    assert!(
        result["total_cost_usd"].as_f64().unwrap_or(0.0) > 0.0,
        "Should have non-zero cost"
    );
    assert!(
        result["num_turns"].as_i64().unwrap_or(0) >= 1,
        "Should have at least 1 turn"
    );

    // 9. Verify session state is also completed
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(session["session"]["state"], "completed");

    // 10. Cleanup
    let remove_options = RemoveContainerOptions {
        force: true,
        ..Default::default()
    };
    docker
        .remove_container(&container.id, Some(remove_options))
        .await
        .ok();

    std::fs::remove_dir_all(&task_dir).ok();
}

#[tokio::test]
#[ignore]
async fn test_e2e_simple_echo_task() {
    // Simpler E2E test that just has Claude echo a value
    if let Err(msg) = check_prerequisites() {
        eprintln!("Skipping E2E test: {}", msg);
        return;
    }

    let docker = match check_docker().await {
        Ok(d) => d,
        Err(msg) => {
            eprintln!("Skipping E2E test: {}", msg);
            return;
        }
    };

    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session (returns session + root_node)
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Create container
    let container_name = format!("mantle-test-echo-{}", session_id);
    let prompt = "Output exactly this JSON and nothing else: {\"test\": \"success\"}";

    let config = ContainerCreateBody {
        image: Some("mantle-agent:latest".to_string()),
        cmd: Some(vec![
            "wrap".to_string(),
            "--hub-socket".to_string(),
            "/tmp/mantle.sock".to_string(),
            "--node-id".to_string(),
            node_id.clone(),
            "--".to_string(),
            "-p".to_string(),
            prompt.to_string(),
            "--output-format".to_string(),
            "json".to_string(),
        ]),
        env: Some(vec![format!(
            "ANTHROPIC_API_KEY={}",
            std::env::var("ANTHROPIC_API_KEY").unwrap()
        )]),
        host_config: Some(HostConfig {
            mounts: Some(vec![Mount {
                target: Some("/tmp/mantle.sock".to_string()),
                source: Some(hub.socket_path().display().to_string()),
                typ: Some(MountTypeEnum::BIND),
                ..Default::default()
            }]),
            ..Default::default()
        }),
        ..Default::default()
    };

    let create_options = CreateContainerOptions {
        name: Some(container_name.clone()),
        platform: String::new(),
    };

    let container = docker
        .create_container(Some(create_options), config)
        .await
        .expect("Failed to create container");

    let start_options = StartContainerOptions::default();
    docker
        .start_container(&container.id, Some(start_options))
        .await
        .expect("Failed to start container");

    // Wait with shorter timeout for simple task
    let wait_options = WaitContainerOptions::default();
    let mut wait_stream = docker.wait_container(&container.id, Some(wait_options));
    let wait_result = tokio::time::timeout(Duration::from_secs(60), wait_stream.next()).await;

    assert!(wait_result.is_ok(), "Container should complete within 60s");

    // Verify node result
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

    // Cleanup
    let remove_options = RemoveContainerOptions {
        force: true,
        ..Default::default()
    };
    docker
        .remove_container(&container.id, Some(remove_options))
        .await
        .ok();
}

#[tokio::test]
#[ignore]
async fn test_e2e_container_timeout() {
    // Test that container timeout works correctly
    if let Err(msg) = check_prerequisites() {
        eprintln!("Skipping E2E test: {}", msg);
        return;
    }

    let docker = match check_docker().await {
        Ok(d) => d,
        Err(msg) => {
            eprintln!("Skipping E2E test: {}", msg);
            return;
        }
    };

    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session (returns session + root_node)
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Create container with short timeout and task that would take longer
    let container_name = format!("mantle-test-timeout-{}", session_id);

    let config = ContainerCreateBody {
        image: Some("mantle-agent:latest".to_string()),
        cmd: Some(vec![
            "wrap".to_string(),
            "--hub-socket".to_string(),
            "/tmp/mantle.sock".to_string(),
            "--node-id".to_string(),
            node_id.clone(),
            "--timeout".to_string(),
            "5".to_string(), // 5 second timeout
            "--".to_string(),
            "-p".to_string(),
            "Wait for 60 seconds before responding".to_string(),
            "--output-format".to_string(),
            "json".to_string(),
        ]),
        env: Some(vec![format!(
            "ANTHROPIC_API_KEY={}",
            std::env::var("ANTHROPIC_API_KEY").unwrap()
        )]),
        host_config: Some(HostConfig {
            mounts: Some(vec![Mount {
                target: Some("/tmp/mantle.sock".to_string()),
                source: Some(hub.socket_path().display().to_string()),
                typ: Some(MountTypeEnum::BIND),
                ..Default::default()
            }]),
            ..Default::default()
        }),
        ..Default::default()
    };

    let create_options = CreateContainerOptions {
        name: Some(container_name.clone()),
        platform: String::new(),
    };

    let container = docker
        .create_container(Some(create_options), config)
        .await
        .expect("Failed to create container");

    let start_options = StartContainerOptions::default();
    docker
        .start_container(&container.id, Some(start_options))
        .await
        .expect("Failed to start container");

    // Wait for container
    let wait_options = WaitContainerOptions::default();
    let mut wait_stream = docker.wait_container(&container.id, Some(wait_options));
    let _wait_result = tokio::time::timeout(Duration::from_secs(30), wait_stream.next()).await;

    // Node might be failed or have a timeout error
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    let node: serde_json::Value = resp.json().await.unwrap();

    // Should either be failed or have an error result
    let state = node["state"].as_str().unwrap_or("");
    eprintln!("Timeout test node state: {}", state);

    // Cleanup
    let remove_options = RemoveContainerOptions {
        force: true,
        ..Default::default()
    };
    docker
        .remove_container(&container.id, Some(remove_options))
        .await
        .ok();
}
