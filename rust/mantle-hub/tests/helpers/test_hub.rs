//! Test helper for spawning and managing hub server instances.

use std::env::temp_dir;
use std::io::{BufRead, BufReader};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

/// A test hub server instance with automatic cleanup.
pub struct TestHub {
    child: Child,
    pub http_url: String,
    pub socket_path: PathBuf,
    db_path: PathBuf,
}

impl TestHub {
    /// Spawn a new hub server asynchronously.
    ///
    /// Uses async/await throughout to avoid reqwest::blocking runtime conflicts.
    /// Blocking I/O (stdout reading) is isolated via spawn_blocking.
    pub async fn spawn() -> Self {
        let id = unique_id();
        let db_path = temp_dir().join(format!("hub-test-{}.db", id));
        let socket_path = temp_dir().join(format!("hub-test-{}.sock", id));
        let port = find_free_port();

        let hub_binary = get_hub_binary();

        let mut child = Command::new(&hub_binary)
            .args([
                "serve",
                "--db",
                &db_path.display().to_string(),
                "--socket",
                &socket_path.display().to_string(),
                "--port",
                &port.to_string(),
            ])
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("Failed to spawn hub server");

        // Read startup message in blocking task to avoid runtime conflict
        let stdout = child.stdout.take().unwrap();
        tokio::task::spawn_blocking(move || {
            let mut reader = BufReader::new(stdout);
            let mut line = String::new();
            let start = Instant::now();

            while start.elapsed() < Duration::from_secs(10) {
                line.clear();
                if reader.read_line(&mut line).unwrap_or(0) > 0 {
                    if line.contains("Starting mantle-hub") || line.contains("listening") {
                        return;
                    }
                }
                std::thread::sleep(Duration::from_millis(50));
            }
            panic!("Hub server failed to start within 10 seconds");
        })
        .await
        .expect("spawn_blocking failed");

        let http_url = format!("http://127.0.0.1:{}", port);

        // Async HTTP health check
        wait_for_http_ready(&http_url, Duration::from_secs(5)).await;

        TestHub {
            child,
            http_url,
            socket_path,
            db_path,
        }
    }

    /// Get an HTTP client for making requests to this hub.
    pub fn http_client(&self) -> reqwest::Client {
        reqwest::Client::new()
    }

    /// Get the socket path for this hub.
    pub fn socket_path(&self) -> &Path {
        &self.socket_path
    }
}

impl Drop for TestHub {
    fn drop(&mut self) {
        // Only kill process - no blocking wait() in Drop
        self.child.kill().ok();

        // Best-effort file cleanup (non-blocking)
        std::fs::remove_file(&self.db_path).ok();
        std::fs::remove_file(&self.socket_path).ok();

        // WAL/SHM cleanup
        let db_str = self.db_path.display().to_string();
        std::fs::remove_file(format!("{}-wal", db_str)).ok();
        std::fs::remove_file(format!("{}-shm", db_str)).ok();

        // Note: Process may still be running briefly after Drop.
        // OS will clean up zombie process and temp files.
        // This is acceptable for test infrastructure.
    }
}

/// Generate a unique ID based on timestamp.
pub fn unique_id() -> String {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos()
        .to_string()
}

/// Find a free port by binding to port 0.
fn find_free_port() -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind to port 0");
    listener.local_addr().unwrap().port()
}

/// Get the path to the hub binary (built in target/debug or target/release).
fn get_hub_binary() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir.parent().unwrap();

    let debug_path = workspace_root.join("target/debug/mantle-hub");
    if debug_path.exists() {
        return debug_path;
    }

    let release_path = workspace_root.join("target/release/mantle-hub");
    if release_path.exists() {
        return release_path;
    }

    panic!(
        "mantle-hub binary not found. Run `cargo build -p mantle-hub` first.\n\
         Searched: {:?} and {:?}",
        debug_path, release_path
    );
}

/// Async HTTP health check using async reqwest.
async fn wait_for_http_ready(url: &str, timeout: Duration) {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_millis(500))
        .build()
        .unwrap();

    let start = Instant::now();
    loop {
        if client
            .get(format!("{}/api/sessions", url))
            .send()
            .await
            .is_ok()
        {
            return;
        }

        if start.elapsed() > timeout {
            panic!(
                "Hub server at {} failed to become ready within {:?}",
                url, timeout
            );
        }

        tokio::time::sleep(Duration::from_millis(100)).await;
    }
}
