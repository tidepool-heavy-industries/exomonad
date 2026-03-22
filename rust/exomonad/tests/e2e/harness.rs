use anyhow::{anyhow, Result};
use std::fs;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use tempfile::TempDir;

pub struct TestHarness {
    pub tmpdir: TempDir,
    pub project_dir: PathBuf,
    pub mock_github_process: Child,
    pub mock_github_port: u16,
    pub mock_gh_path: PathBuf,
    pub tmux_session: String,
}

impl TestHarness {
    pub fn build() -> Result<Self> {
        let tmpdir = TempDir::new()?;
        let tmpdir_path = tmpdir.path().to_path_buf();

        // 1. Create bare git remote
        let remote_dir = tmpdir_path.join("remote.git");
        fs::create_dir_all(&remote_dir)?;
        let status = Command::new("git")
            .arg("init")
            .arg("--bare")
            .arg("-q")
            .current_dir(&remote_dir)
            .status()?;
        if !status.success() {
            return Err(anyhow!("Failed to init bare remote git repo"));
        }

        // 2. Create project repo
        let project_dir = tmpdir_path.join("project");
        fs::create_dir_all(&project_dir)?;
        let status = Command::new("git")
            .arg("init")
            .arg("-q")
            .current_dir(&project_dir)
            .status()?;
        if !status.success() {
            return Err(anyhow!("Failed to init project git repo"));
        }

        let status = Command::new("git")
            .args(["remote", "add", "origin", "../remote.git"])
            .current_dir(&project_dir)
            .status()?;
        if !status.success() {
            return Err(anyhow!("Failed to add origin remote to project git repo"));
        }

        // Initial commit
        fs::write(project_dir.join("README.md"), "# Test Project")?;
        let status = Command::new("git")
            .args(["add", "README.md"])
            .current_dir(&project_dir)
            .status()?;
        if !status.success() {
            return Err(anyhow!("Failed to add README.md to git index"));
        }
        let status = Command::new("git")
            .args([
                "-c",
                "user.name=Test",
                "-c",
                "user.email=test@test.com",
                "commit",
                "-m",
                "initial commit",
                "-q",
            ])
            .current_dir(&project_dir)
            .status()?;
        if !status.success() {
            return Err(anyhow!("Failed to create initial commit"));
        }

        // 3. Copy WASM
        let mut workspace_root = std::env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("."));

        let start_dir = workspace_root.clone();
        loop {
            if workspace_root.join("justfile").exists() {
                break;
            }
            match workspace_root.parent() {
                Some(parent) => {
                    workspace_root = parent.to_path_buf();
                }
                None => {
                    return Err(anyhow!(
                        "Could not locate justfile; searched upwards from {}",
                        start_dir.display()
                    ));
                }
            }
        }

        let wasm_src = workspace_root.join(".exo/wasm");
        let wasm_dest = project_dir.join(".exo/wasm");
        if !wasm_src.exists() {
            return Err(anyhow!(
                "WASM directory not found at {:?}. Run `just wasm-all` first.",
                wasm_src
            ));
        }
        fs::create_dir_all(&wasm_dest)?;
        for entry in fs::read_dir(&wasm_src)? {
            let entry = entry?;
            if entry.file_type()?.is_file() {
                fs::copy(entry.path(), wasm_dest.join(entry.file_name()))?;
            }
        }

        // 4. Write minimal config.toml
        let tmux_session = format!("exo-test-{}", std::process::id());
        let config_path = project_dir.join(".exo/config.toml");
        fs::create_dir_all(project_dir.join(".exo"))?;
        fs::write(
            config_path,
            format!(
                "default_role = \"tl\"\nproject_dir = \".\"\ntmux_session = \"{}\"\n",
                tmux_session
            ),
        )?;

        // 5. Copy mock_gh
        let mock_gh_src = workspace_root.join("tests/e2e/mock_gh");
        let mock_gh_dest = tmpdir_path.join("mock_gh");
        fs::copy(&mock_gh_src, &mock_gh_dest)?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&mock_gh_dest)?.permissions();
            perms.set_mode(0o755);
            fs::set_permissions(&mock_gh_dest, perms)?;
        }

        // 6. Start mock_github.py on a random port
        let port = {
            let listener = std::net::TcpListener::bind("127.0.0.1:0")?;
            listener.local_addr()?.port()
        };
        let mock_github_src = workspace_root.join("tests/e2e/mock_github.py");

        let mock_github_process = Command::new("python3")
            .arg(&mock_github_src)
            .arg("--port")
            .arg(port.to_string())
            .stderr(Stdio::null())
            .spawn()?;

        Ok(TestHarness {
            tmpdir,
            project_dir,
            mock_github_process,
            mock_github_port: port,
            mock_gh_path: mock_gh_dest,
            tmux_session,
        })
    }

    pub fn mock_github_port(&self) -> u16 {
        self.mock_github_port
    }

    pub fn tmux_list_windows(&self) -> Vec<String> {
        let output = Command::new("tmux")
            .args(["list-windows", "-t", &self.tmux_session])
            .output();

        match output {
            Ok(out) if out.status.success() => String::from_utf8_lossy(&out.stdout)
                .lines()
                .map(|s| s.to_string())
                .collect(),
            _ => Vec::new(),
        }
    }

    pub fn git_branches(&self) -> Vec<String> {
        let output = Command::new("git")
            .arg("branch")
            .current_dir(&self.project_dir)
            .output();

        match output {
            Ok(out) if out.status.success() => String::from_utf8_lossy(&out.stdout)
                .lines()
                .map(|s| s.trim().trim_start_matches('*').trim().to_string())
                .collect(),
            _ => Vec::new(),
        }
    }

    pub fn file_exists(&self, path: &str) -> bool {
        self.project_dir.join(path).exists()
    }
}

impl Drop for TestHarness {
    fn drop(&mut self) {
        let _ = Command::new("tmux")
            .args(["kill-session", "-t", &self.tmux_session])
            .status();

        let _ = self.mock_github_process.kill();
        let _ = self.mock_github_process.wait();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::TcpStream;
    use std::time::Duration;

    #[tokio::test]
    async fn test_harness_setup() -> Result<(), Box<dyn std::error::Error>> {
        let harness = TestHarness::build()?;

        assert!(harness.project_dir.exists());
        assert!(harness.project_dir.join(".git").exists());
        assert!(harness.project_dir.join(".exo/config.toml").exists());

        // Wait for mock_github to start
        tokio::time::sleep(Duration::from_millis(500)).await;

        // Verify mock_github responds (port is open)
        let addr = format!("127.0.0.1:{}", harness.mock_github_port);
        let stream = TcpStream::connect(addr)?;
        drop(stream);

        assert!(harness.mock_gh_path.exists());

        Ok(())
    }
}
