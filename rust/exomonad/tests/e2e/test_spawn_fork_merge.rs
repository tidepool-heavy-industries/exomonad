use std::process::{Command, Stdio};
use std::time::{Duration, Instant};
use anyhow::{Result, anyhow};
use crate::e2e::harness::TestHarness;

#[test]
#[ignore] // Requires WASM to be built and present in .exo/wasm/
fn test_spawn_pipeline() -> Result<()> {
    let harness = TestHarness::build()?;
    let project_dir = &harness.project_dir;
    let socket_path = project_dir.join(".exo/server.sock");
    
    // 1. Start 'exomonad serve' as a subprocess
    // Use env!("CARGO_BIN_EXE_exomonad") which cargo sets for integration tests
    let bin_path = env!("CARGO_BIN_EXE_exomonad");
    
    let mut server = Command::new(bin_path)
        .arg("serve")
        .current_dir(project_dir)
        .env("GITHUB_API_BASE_URL", format!("http://127.0.0.1:{}", harness.mock_github_port))
        .env("PATH", format!("{}:{}", harness.mock_gh_path.parent().unwrap().to_str().unwrap(), std::env::var("PATH").unwrap()))
        .env("GITHUB_TOKEN", "test-token")
        .env("RUST_LOG", "warn")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    // 2. Wait for server socket to appear
    let start = Instant::now();
    let timeout = Duration::from_secs(10);
    while !socket_path.exists() {
        if start.elapsed() > timeout {
            let _ = server.kill();
            return Err(anyhow!("Timeout waiting for server socket at {:?}", socket_path));
        }
        std::thread::sleep(Duration::from_millis(100));
        
        // Check if server exited early
        if let Some(status) = server.try_wait()? {
            return Err(anyhow!("Server exited early with status: {}", status));
        }
    }

    // 3. Verify /health endpoint
    let health_output = Command::new("curl")
        .args([
            "--unix-socket", socket_path.to_str().unwrap(),
            "http://localhost/health",
            "-s"
        ])
        .output()?;
    
    if !health_output.status.success() {
        let _ = server.kill();
        return Err(anyhow!("Health check failed: {}", String::from_utf8_lossy(&health_output.stderr)));
    }
    
    let health: serde_json::Value = serde_json::from_slice(&health_output.stdout)?;
    assert_eq!(health["status"], "ok");

    // 4. Verify tools listing
    let tools_output = Command::new("curl")
        .args([
            "--unix-socket", socket_path.to_str().unwrap(),
            "http://localhost/agents/root/root/tools",
            "-s"
        ])
        .output()?;

    if !tools_output.status.success() {
        let _ = server.kill();
        return Err(anyhow!("Tools listing failed: {}", String::from_utf8_lossy(&tools_output.stderr)));
    }

    let tools_body = String::from_utf8_lossy(&tools_output.stdout);
    let tools: serde_json::Value = serde_json::from_str(&tools_body)
        .map_err(|e| anyhow!("Failed to parse tools JSON: {}. Body: {}", e, tools_body))?;
    
    let tool_list = tools["tools"].as_array().ok_or_else(|| anyhow!("Tools response does not have 'tools' array. Body: {}", tools_body))?;
    
    let tool_names: Vec<&str> = tool_list.iter()
        .filter_map(|t| t["name"].as_str())
        .collect();
    
    assert!(tool_names.contains(&"fork_wave"), "Tools should contain fork_wave. Found: {:?}", tool_names);
    assert!(tool_names.contains(&"spawn_gemini"), "Tools should contain spawn_gemini. Found: {:?}", tool_names);
    assert!(tool_names.contains(&"spawn_worker"), "Tools should contain spawn_worker. Found: {:?}", tool_names);

    // 5. Cleanup
    let _ = server.kill();
    let _ = server.wait();
    
    Ok(())
}
