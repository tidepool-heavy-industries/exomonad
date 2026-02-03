use anyhow::{Context, Result};
use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use std::fs;
use std::path::Path;
use tracing::{info, warn};

/// Enforces that only one instance of the sidecar is running for this project.
///
/// 1. Checks if PID file exists.
/// 2. If yes, checks if process is still running.
/// 3. If running, kills it (SIGTERM, then SIGKILL if it persists).
/// 4. Writes new PID to the file.
pub fn enforce_singleton(pid_file: &Path) -> Result<()> {
    if let Some(parent) = pid_file.parent() {
        fs::create_dir_all(parent).context("Failed to create directory for PID file")?;
    }

    if pid_file.exists() {
        let content = fs::read_to_string(pid_file).context("Failed to read PID file")?;
        if let Ok(old_pid) = content.trim().parse::<i32>() {
            let pid = Pid::from_raw(old_pid);

            // Check if process exists by sending signal 0
            if signal::kill(pid, None).is_ok() {
                info!(pid = old_pid, "Found existing sidecar process, terminating");

                // Try SIGTERM
                if let Err(e) = signal::kill(pid, Signal::SIGTERM) {
                    warn!(pid = old_pid, error = %e, "Failed to send SIGTERM to existing process");
                } else {
                    // Wait a moment for it to exit
                    std::thread::sleep(std::time::Duration::from_millis(500));
                }

                // If still alive, try SIGKILL
                if signal::kill(pid, None).is_ok() {
                    warn!(pid = old_pid, "Process still alive after SIGTERM, sending SIGKILL");
                    if let Err(e) = signal::kill(pid, Signal::SIGKILL) {
                        warn!(pid = old_pid, error = %e, "Failed to send SIGKILL to existing process");
                    }
                }
            }
        }
    }

    // Write current PID
    let current_pid = std::process::id();
    fs::write(pid_file, current_pid.to_string()).context("Failed to write PID file")?;
    info!(pid = current_pid, path = %pid_file.display(), "PID file written");

    Ok(())
}

/// RAII guard for PID file.
pub struct PidGuard {
    path: std::path::PathBuf,
}

impl PidGuard {
    pub fn new(path: &Path) -> Result<Self> {
        enforce_singleton(path)?;
        Ok(Self {
            path: path.to_path_buf(),
        })
    }
}

impl Drop for PidGuard {
    fn drop(&mut self) {
        cleanup_pid_file(&self.path);
    }
}

/// Removes the PID file.
pub fn cleanup_pid_file(pid_file: &Path) {
    if pid_file.exists() {
        if let Err(e) = fs::remove_file(pid_file) {
            warn!(path = %pid_file.display(), error = %e, "Failed to remove PID file on exit");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_pid_file_creation_and_cleanup() {
        let dir = tempdir().unwrap();
        let pid_file = dir.path().join("sidecar.pid");

        // Initial creation
        enforce_singleton(&pid_file).unwrap();
        assert!(pid_file.exists());

        let content = fs::read_to_string(&pid_file).unwrap();
        let pid: u32 = content.parse().unwrap();
        assert_eq!(pid, std::process::id());

        // Cleanup
        cleanup_pid_file(&pid_file);
        assert!(!pid_file.exists());
    }

    #[test]
    fn test_pid_guard() {
        let dir = tempdir().unwrap();
        let pid_file = dir.path().join("sidecar.pid");

        {
            let _guard = PidGuard::new(&pid_file).unwrap();
            assert!(pid_file.exists());
        }

        assert!(!pid_file.exists());
    }

    #[test]
    fn test_enforce_singleton_replaces_dead_pid() {
        let dir = tempdir().unwrap();
        let pid_file = dir.path().join("sidecar.pid");

        // Write a "dead" PID
        fs::write(&pid_file, "999999").unwrap();

        enforce_singleton(&pid_file).unwrap();
        let content = fs::read_to_string(&pid_file).unwrap();
        let pid: u32 = content.parse().unwrap();
        assert_eq!(pid, std::process::id());
    }
}
