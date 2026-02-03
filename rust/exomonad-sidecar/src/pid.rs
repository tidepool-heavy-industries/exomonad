use anyhow::{Context, Result};
use std::fs;
use std::path::Path;
use tracing::{info, warn};

#[cfg(unix)]
use nix::sys::signal::{self, Signal};
#[cfg(unix)]
use nix::unistd::Pid;

/// Enforces that only one instance of the sidecar is running for this project.
///
/// 1. Checks if PID file exists.
/// 2. If yes, checks if process is still running.
/// 3. If running, kills it (SIGTERM, then SIGKILL if it persists).
/// 4. Writes new PID to the file.
pub fn enforce_singleton(pid_file: &Path) -> Result<()> {
    #[cfg(unix)]
    return unix_enforce_singleton(pid_file);

    #[cfg(not(unix))]
    {
        warn!("PID enforcement is only supported on Unix systems");
        Ok(())
    }
}

#[cfg(unix)]
fn unix_enforce_singleton(pid_file: &Path) -> Result<()> {
    if let Some(parent) = pid_file.parent() {
        fs::create_dir_all(parent).context("Failed to create directory for PID file")?;
    }

    if pid_file.exists() {
        let content = fs::read_to_string(pid_file).context("Failed to read PID file")?;
        let trimmed = content.trim();
        match trimmed.parse::<i32>() {
            Ok(old_pid) => {
                let pid = Pid::from_raw(old_pid);

                // Check if process exists by sending signal 0
                // Handle ESRCH (No such process) by treating it as success (process already gone)
                match signal::kill(pid, None) {
                    Ok(_) => {
                        info!(pid = old_pid, "Found existing sidecar process, terminating");

                        // Try SIGTERM
                        if let Err(e) = signal::kill(pid, Signal::SIGTERM) {
                            // If process vanished between check and kill, that's fine
                            if e != nix::errno::Errno::ESRCH {
                                warn!(pid = old_pid, error = %e, "Failed to send SIGTERM to existing process");
                            }
                        } else {
                            // Poll for exit
                            let timeout = std::time::Duration::from_secs(5);
                            let interval = std::time::Duration::from_millis(100);
                            let start = std::time::Instant::now();
                            let mut exited = false;

                            while start.elapsed() < timeout {
                                if signal::kill(pid, None).is_err() {
                                    exited = true;
                                    break;
                                }
                                std::thread::sleep(interval);
                            }

                            if !exited {
                                warn!(pid = old_pid, "Process still alive after SIGTERM timeout, sending SIGKILL");
                                if let Err(e) = signal::kill(pid, Signal::SIGKILL) {
                                    if e != nix::errno::Errno::ESRCH {
                                        warn!(pid = old_pid, error = %e, "Failed to send SIGKILL to existing process");
                                    }
                                }
                                // Brief wait for SIGKILL to take effect
                                std::thread::sleep(std::time::Duration::from_millis(100));
                                if signal::kill(pid, None).is_ok() {
                                    warn!(pid = old_pid, "Process still appears to be alive after SIGKILL");
                                } else {
                                    info!(pid = old_pid, "Process terminated after SIGKILL");
                                }
                            } else {
                                info!(pid = old_pid, "Process exited after SIGTERM");
                            }
                        }
                    }
                    Err(nix::errno::Errno::ESRCH) => {
                        // Process doesn't exist, we can overwrite
                    }
                    Err(e) => {
                        warn!(pid = old_pid, error = %e, "Failed to check if process exists");
                    }
                }
            }
            Err(e) => {
                warn!(error = %e, raw = trimmed, "PID file contains invalid data, replacing it");
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

    #[cfg(unix)]
    #[test]
    fn test_enforce_singleton_replaces_dead_pid() {
        let dir = tempdir().unwrap();
        let pid_file = dir.path().join("sidecar.pid");

        // Use a PID that is unlikely to exist (max PID on 64-bit systems is often 32768 or 4194304)
        // 99999999 is safe enough for a test.
        fs::write(&pid_file, "99999999").unwrap();

        enforce_singleton(&pid_file).unwrap();
        let content = fs::read_to_string(&pid_file).unwrap();
        let pid: u32 = content.parse().unwrap();
        assert_eq!(pid, std::process::id());
    }

    #[cfg(unix)]
    #[test]
    fn test_enforce_singleton_handles_invalid_content() {
        let dir = tempdir().unwrap();
        let pid_file = dir.path().join("sidecar.pid");

        fs::write(&pid_file, "not-a-number").unwrap();

        enforce_singleton(&pid_file).unwrap();
        let content = fs::read_to_string(&pid_file).unwrap();
        let pid: u32 = content.parse().unwrap();
        assert_eq!(pid, std::process::id());
    }
}