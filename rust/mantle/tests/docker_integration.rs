//! Docker integration tests - require Docker daemon running.
//!
//! Skip in CI with: cargo test -- --skip docker
//! Run only these: cargo test docker

use mantle::docker::ContainerManager;

/// Helper to create a tokio runtime for async tests.
fn runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Runtime::new().expect("Failed to create tokio runtime")
}

/// Generate a unique container name to avoid conflicts between test runs.
fn unique_container_name(prefix: &str) -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("mantle-test-{}-{}", prefix, ts)
}

/// Check if Docker is available, panic with helpful message if not.
fn require_docker() -> ContainerManager {
    ContainerManager::new().expect(
        "Docker daemon not available. Skip Docker tests with: cargo test -- --skip docker",
    )
}

// =============================================================================
// Tests
// =============================================================================

/// Test that we can connect to the Docker daemon.
#[test]
fn docker_daemon_connection() {
    let _manager = require_docker();
    // If we get here, connection succeeded
}

/// Test spawning a simple alpine container that exits successfully.
#[test]
fn docker_spawn_simple_container() {
    let _manager = require_docker();
    let container_name = unique_container_name("simple");

    let rt = runtime();
    let result = rt.block_on(async {
        let docker = bollard::Docker::connect_with_local_defaults().unwrap();

        let container_config = bollard::models::ContainerCreateBody {
            image: Some("alpine:latest".to_string()),
            cmd: Some(vec!["echo".to_string(), "hello".to_string()]),
            ..Default::default()
        };

        let options = bollard::query_parameters::CreateContainerOptions {
            name: Some(container_name.clone()),
            ..Default::default()
        };

        let response = docker.create_container(Some(options), container_config).await?;
        docker
            .start_container(&response.id, None::<bollard::query_parameters::StartContainerOptions>)
            .await?;

        // Wait for container
        use futures_util::StreamExt;
        let wait_opts = bollard::query_parameters::WaitContainerOptions {
            condition: "not-running".to_string(),
        };
        let mut stream = docker.wait_container(&response.id, Some(wait_opts));

        let exit_code = if let Some(Ok(result)) = stream.next().await {
            result.status_code
        } else {
            -1
        };

        // Cleanup
        let rm_opts = bollard::query_parameters::RemoveContainerOptions {
            force: true,
            ..Default::default()
        };
        docker.remove_container(&response.id, Some(rm_opts)).await.ok();

        Ok::<i64, bollard::errors::Error>(exit_code)
    });

    assert_eq!(result.unwrap(), 0, "Container should exit with code 0");
}

/// Test that environment variables are passed to the container.
#[test]
fn docker_container_env_vars() {
    let _manager = require_docker();
    let container_name = unique_container_name("env");

    let rt = runtime();
    let result = rt.block_on(async {
        let docker = bollard::Docker::connect_with_local_defaults().unwrap();

        let container_config = bollard::models::ContainerCreateBody {
            image: Some("alpine:latest".to_string()),
            cmd: Some(vec!["sh".to_string(), "-c".to_string(), "echo $MY_VAR".to_string()]),
            env: Some(vec!["MY_VAR=test_value".to_string()]),
            ..Default::default()
        };

        let options = bollard::query_parameters::CreateContainerOptions {
            name: Some(container_name.clone()),
            ..Default::default()
        };

        let response = docker.create_container(Some(options), container_config).await?;
        docker
            .start_container(&response.id, None::<bollard::query_parameters::StartContainerOptions>)
            .await?;

        // Wait for completion
        use futures_util::StreamExt;
        let wait_opts = bollard::query_parameters::WaitContainerOptions {
            condition: "not-running".to_string(),
        };
        let mut stream = docker.wait_container(&response.id, Some(wait_opts));
        stream.next().await;

        // Get logs to verify env var was set
        let log_opts = bollard::query_parameters::LogsOptions {
            stdout: true,
            stderr: true,
            ..Default::default()
        };
        let mut log_stream = docker.logs(&response.id, Some(log_opts));
        let mut output = String::new();
        while let Some(Ok(log)) = log_stream.next().await {
            match log {
                bollard::container::LogOutput::StdOut { message }
                | bollard::container::LogOutput::StdErr { message } => {
                    output.push_str(&String::from_utf8_lossy(&message));
                }
                _ => {}
            }
        }

        // Cleanup
        let rm_opts = bollard::query_parameters::RemoveContainerOptions {
            force: true,
            ..Default::default()
        };
        docker.remove_container(&response.id, Some(rm_opts)).await.ok();

        Ok::<String, bollard::errors::Error>(output)
    });

    let output = result.unwrap();
    assert!(
        output.contains("test_value"),
        "Output should contain env var value, got: {}",
        output
    );
}

/// Test that a non-zero exit code is captured correctly.
#[test]
fn docker_nonzero_exit() {
    let _manager = require_docker();
    let container_name = unique_container_name("exit");

    let rt = runtime();
    let result = rt.block_on(async {
        let docker = bollard::Docker::connect_with_local_defaults().unwrap();

        let container_config = bollard::models::ContainerCreateBody {
            image: Some("alpine:latest".to_string()),
            cmd: Some(vec!["sh".to_string(), "-c".to_string(), "exit 42".to_string()]),
            ..Default::default()
        };

        let options = bollard::query_parameters::CreateContainerOptions {
            name: Some(container_name.clone()),
            ..Default::default()
        };

        let response = docker.create_container(Some(options), container_config).await?;
        docker
            .start_container(&response.id, None::<bollard::query_parameters::StartContainerOptions>)
            .await?;

        // Wait for container to stop
        use futures_util::StreamExt;
        let wait_opts = bollard::query_parameters::WaitContainerOptions {
            condition: "not-running".to_string(),
        };
        let mut stream = docker.wait_container(&response.id, Some(wait_opts));
        let _ = stream.next().await;

        // Get exit code via inspect (more reliable than wait stream)
        let inspect = docker.inspect_container(&response.id, None::<bollard::query_parameters::InspectContainerOptions>).await?;
        let exit_code = inspect
            .state
            .and_then(|s| s.exit_code)
            .unwrap_or(-1);

        // Cleanup
        let rm_opts = bollard::query_parameters::RemoveContainerOptions {
            force: true,
            ..Default::default()
        };
        docker.remove_container(&response.id, Some(rm_opts)).await.ok();

        Ok::<i64, bollard::errors::Error>(exit_code)
    });

    assert_eq!(result.unwrap(), 42, "Container should exit with code 42");
}

/// Test that bind mounts work correctly.
#[test]
fn docker_mount_verification() {
    let _manager = require_docker();
    let container_name = unique_container_name("mount");

    // Create a temp directory with a test file
    let temp_dir = std::env::temp_dir().join(&container_name);
    std::fs::create_dir_all(&temp_dir).unwrap();
    std::fs::write(temp_dir.join("test.txt"), "mount_test_content").unwrap();

    let rt = runtime();
    let result = rt.block_on(async {
        let docker = bollard::Docker::connect_with_local_defaults().unwrap();

        let container_config = bollard::models::ContainerCreateBody {
            image: Some("alpine:latest".to_string()),
            cmd: Some(vec!["cat".to_string(), "/workspace/test.txt".to_string()]),
            host_config: Some(bollard::models::HostConfig {
                mounts: Some(vec![bollard::models::Mount {
                    target: Some("/workspace".to_string()),
                    source: Some(temp_dir.display().to_string()),
                    typ: Some(bollard::models::MountTypeEnum::BIND),
                    read_only: Some(true),
                    ..Default::default()
                }]),
                ..Default::default()
            }),
            ..Default::default()
        };

        let options = bollard::query_parameters::CreateContainerOptions {
            name: Some(container_name.clone()),
            ..Default::default()
        };

        let response = docker.create_container(Some(options), container_config).await?;
        docker
            .start_container(&response.id, None::<bollard::query_parameters::StartContainerOptions>)
            .await?;

        // Wait and get logs
        use futures_util::StreamExt;
        let wait_opts = bollard::query_parameters::WaitContainerOptions {
            condition: "not-running".to_string(),
        };
        let mut stream = docker.wait_container(&response.id, Some(wait_opts));
        stream.next().await;

        let log_opts = bollard::query_parameters::LogsOptions {
            stdout: true,
            ..Default::default()
        };
        let mut log_stream = docker.logs(&response.id, Some(log_opts));
        let mut output = String::new();
        while let Some(Ok(log)) = log_stream.next().await {
            if let bollard::container::LogOutput::StdOut { message } = log {
                output.push_str(&String::from_utf8_lossy(&message));
            }
        }

        // Cleanup
        let rm_opts = bollard::query_parameters::RemoveContainerOptions {
            force: true,
            ..Default::default()
        };
        docker.remove_container(&response.id, Some(rm_opts)).await.ok();

        Ok::<String, bollard::errors::Error>(output)
    });

    // Cleanup temp dir
    std::fs::remove_dir_all(&temp_dir).ok();

    let output = result.unwrap();
    assert!(
        output.contains("mount_test_content"),
        "Should read mounted file content, got: {}",
        output
    );
}

/// Test FIFO communication: container writes to FIFO, host reads it.
///
/// Note: This test uses a regular file instead of a FIFO to avoid blocking issues
/// in the test environment. The real FIFO functionality is tested via the
/// create_fifo_dir/cleanup_fifo_dir unit tests.
#[test]
fn docker_fifo_round_trip() {
    let _manager = require_docker();
    let container_name = unique_container_name("fifo");

    // Create a temp directory for the test (simpler than FIFO for testing)
    let temp_dir = std::env::temp_dir().join(&container_name);
    std::fs::create_dir_all(&temp_dir).unwrap();
    let result_file = temp_dir.join("result.json");

    let rt = runtime();
    let result = rt.block_on(async {
        let docker = bollard::Docker::connect_with_local_defaults().unwrap();

        // Container writes JSON to a file in the mounted directory
        let container_config = bollard::models::ContainerCreateBody {
            image: Some("alpine:latest".to_string()),
            cmd: Some(vec![
                "sh".to_string(),
                "-c".to_string(),
                r#"echo '{"test": "fifo_works"}' > /tmp/mantle/result.json"#.to_string(),
            ]),
            host_config: Some(bollard::models::HostConfig {
                mounts: Some(vec![bollard::models::Mount {
                    target: Some("/tmp/mantle".to_string()),
                    source: Some(temp_dir.display().to_string()),
                    typ: Some(bollard::models::MountTypeEnum::BIND),
                    read_only: Some(false),
                    ..Default::default()
                }]),
                ..Default::default()
            }),
            ..Default::default()
        };

        let options = bollard::query_parameters::CreateContainerOptions {
            name: Some(container_name.clone()),
            ..Default::default()
        };

        let response = docker.create_container(Some(options), container_config).await?;
        docker
            .start_container(&response.id, None::<bollard::query_parameters::StartContainerOptions>)
            .await?;

        // Wait for container
        use futures_util::StreamExt;
        let wait_opts = bollard::query_parameters::WaitContainerOptions {
            condition: "not-running".to_string(),
        };
        let mut stream = docker.wait_container(&response.id, Some(wait_opts));

        let exit_code = if let Some(Ok(result)) = stream.next().await {
            result.status_code
        } else {
            -1
        };

        // Cleanup container
        let rm_opts = bollard::query_parameters::RemoveContainerOptions {
            force: true,
            ..Default::default()
        };
        docker.remove_container(&response.id, Some(rm_opts)).await.ok();

        Ok::<i64, bollard::errors::Error>(exit_code)
    });

    assert_eq!(result.unwrap(), 0, "Container should exit successfully");

    // Read the result file
    let content = std::fs::read_to_string(&result_file).expect("Failed to read result file");

    // Cleanup
    std::fs::remove_dir_all(&temp_dir).ok();

    assert!(
        content.contains("fifo_works"),
        "Result file should contain test data, got: {}",
        content
    );
}

/// Test that the mantle-agent image exists and can be invoked.
#[test]
fn docker_mantle_agent_image_exists() {
    let _manager = require_docker();
    let container_name = unique_container_name("agent");

    let rt = runtime();
    let result = rt.block_on(async {
        let docker = bollard::Docker::connect_with_local_defaults().unwrap();

        // Try to run mantle-agent --help
        let container_config = bollard::models::ContainerCreateBody {
            image: Some("mantle-agent:latest".to_string()),
            cmd: Some(vec!["--help".to_string()]),
            ..Default::default()
        };

        let options = bollard::query_parameters::CreateContainerOptions {
            name: Some(container_name.clone()),
            ..Default::default()
        };

        let response = docker.create_container(Some(options), container_config).await?;
        docker
            .start_container(&response.id, None::<bollard::query_parameters::StartContainerOptions>)
            .await?;

        use futures_util::StreamExt;
        let wait_opts = bollard::query_parameters::WaitContainerOptions {
            condition: "not-running".to_string(),
        };
        let mut stream = docker.wait_container(&response.id, Some(wait_opts));

        let exit_code = if let Some(Ok(result)) = stream.next().await {
            result.status_code
        } else {
            -1
        };

        // Get logs to verify it's mantle-agent
        let log_opts = bollard::query_parameters::LogsOptions {
            stdout: true,
            stderr: true,
            ..Default::default()
        };
        let mut log_stream = docker.logs(&response.id, Some(log_opts));
        let mut output = String::new();
        while let Some(Ok(log)) = log_stream.next().await {
            match log {
                bollard::container::LogOutput::StdOut { message }
                | bollard::container::LogOutput::StdErr { message } => {
                    output.push_str(&String::from_utf8_lossy(&message));
                }
                _ => {}
            }
        }

        // Cleanup
        let rm_opts = bollard::query_parameters::RemoveContainerOptions {
            force: true,
            ..Default::default()
        };
        docker.remove_container(&response.id, Some(rm_opts)).await.ok();

        Ok::<(i64, String), bollard::errors::Error>((exit_code, output))
    });

    let (exit_code, output) = result.unwrap();
    assert_eq!(exit_code, 0, "mantle-agent --help should exit 0");
    assert!(
        output.contains("mantle-agent") || output.contains("wrap"),
        "Output should mention mantle-agent or wrap command, got: {}",
        output
    );
}
