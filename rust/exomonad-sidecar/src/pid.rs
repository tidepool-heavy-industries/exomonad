use anyhow::{Context, Result};
use std::fs::{self, File, OpenOptions};
use std::io::{Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use tracing::{info, warn};

#[cfg(unix)]
use nix::sys::signal::{self, Signal};
#[cfg(unix)]
use nix::unistd::Pid;
#[cfg(unix)]
use std::os::unix::io::AsRawFd;

/// Enforces that only one instance of the sidecar is running for this project.
///
/// Uses file locking (flock) to prevent race conditions.
/// 1. Opens the PID file.
/// 2. Tries to acquire an exclusive lock.
/// 3. If locked by another process:
///    a. Reads the PID.
///    b. Verifies the process identity (to avoid killing unrelated processes on PID reuse).
///    c. Kills the process.
///    d. Retries locking.
/// 4. Once locked, writes the current PID.
/// 5. Returns a guard that holds the lock (and file handle) until dropped.
pub fn enforce_singleton(pid_file: &Path) -> Result<PidGuard> {
    #[cfg(unix)]
    return unix_enforce_singleton(pid_file);

    #[cfg(not(unix))]
    {
        warn!("PID enforcement is only supported on Unix systems");
        Ok(PidGuard {
            path: pid_file.to_path_buf(),
            file: None,
        })
    }
}

#[cfg(unix)]
fn unix_enforce_singleton(pid_file: &Path) -> Result<PidGuard> {
    if let Some(parent) = pid_file.parent() {
        fs::create_dir_all(parent).context("Failed to create directory for PID file")?;
    }

    // Open with read/write/create. We keep this file open in the guard.
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(pid_file)
        .context("Failed to open PID file")?;

    let timeout = std::time::Duration::from_secs(10);
    let start = std::time::Instant::now();

    loop {
        // Try to acquire exclusive, non-blocking lock
        #[allow(deprecated)]
        match nix::fcntl::flock(
            file.as_raw_fd(),
            nix::fcntl::FlockArg::LockExclusiveNonblock,
        ) {
            Ok(_) => {
                // Lock acquired! We are the singleton.
                break;
            }
            Err(nix::errno::Errno::EWOULDBLOCK) => {
                // Locked by another process.
                if start.elapsed() > timeout {
                    anyhow::bail!("Timed out waiting for previous sidecar instance to exit");
                }

                // Read PID from file to kill it
                let content = fs::read_to_string(pid_file).unwrap_or_default();
                let trimmed = content.trim();

                if let Ok(old_pid_u32) = trimmed.parse::<u32>() {
                    // Safe cast to i32 (PID type in nix)
                    if let Ok(old_pid) = i32::try_from(old_pid_u32) {
                        let pid = Pid::from_raw(old_pid);

                        // Verify process identity if possible (rudimentary check via ps)
                        if is_sidecar_process(old_pid_u32) {
                            kill_process(pid, old_pid_u32)?;
                        } else {
                            warn!(pid = old_pid, "Process holding lock does not appear to be exomonad-sidecar (or cannot be verified). Waiting...");
                            // We can't kill it safely if we don't know what it is.
                            // But if it holds the lock on our PID file, it's likely relevant or a zombie.
                            // However, flock locks are released on process termination.
                            // If it's still holding the lock, it's alive.
                            // If check fails, maybe it's a false negative?
                            // Let's retry kill logic but be careful.
                            // Actually, if we can't verify it, we probably shouldn't kill random PIDs.
                            // But checking 'ps' is flaky.
                            // Let's stick to the Copilot requirement: verify identity.
                        }
                    }
                }

                // Wait a bit before retrying lock
                std::thread::sleep(std::time::Duration::from_millis(200));
            }
            Err(e) => {
                return Err(e).context("Failed to acquire lock on PID file");
            }
        }
    }

    // We have the lock. Truncate and write our PID.
    file.set_len(0).context("Failed to truncate PID file")?;
    file.seek(SeekFrom::Start(0))
        .context("Failed to seek PID file")?;

    let current_pid = std::process::id();
    file.write_all(current_pid.to_string().as_bytes())
        .context("Failed to write to PID file")?;

    info!(pid = current_pid, path = %pid_file.display(), "PID file locked and written");

    Ok(PidGuard {
        path: pid_file.to_path_buf(),
        file: Some(file),
    })
}

#[cfg(unix)]
fn is_sidecar_process(pid: u32) -> bool {
    // Simple check using `ps -p <pid> -o command=`
    // This is portable enough for standard Unixes (Linux, macOS).
    let output = std::process::Command::new("ps")
        .arg("-p")
        .arg(pid.to_string())
        .arg("-o")
        .arg("command=")
        .output();

    match output {
        Ok(out) => {
            let stdout = String::from_utf8_lossy(&out.stdout);
            // Check for binary name.
            // process might be "target/debug/exomonad-sidecar" or just "exomonad-sidecar"
            stdout.contains("exomonad-sidecar")
        }
        Err(_) => false, // Cannot verify
    }
}

#[cfg(unix)]
fn kill_process(pid: Pid, old_pid: u32) -> Result<()> {
    info!(
        pid = old_pid,
        "Attempting to terminate existing sidecar process"
    );

    // Try SIGTERM
    match signal::kill(pid, Signal::SIGTERM) {
        Ok(_) => {
            // Poll for exit
            let timeout = std::time::Duration::from_secs(5);
            let start = std::time::Instant::now();

            while start.elapsed() < timeout {
                if signal::kill(pid, None).is_err() {
                    info!(pid = old_pid, "Process terminated after SIGTERM");
                    return Ok(());
                }
                std::thread::sleep(std::time::Duration::from_millis(100));
            }

            warn!(
                pid = old_pid,
                "Process still alive after SIGTERM timeout, sending SIGKILL"
            );
        }
        Err(nix::errno::Errno::ESRCH) => return Ok(()), // Already gone
        Err(nix::errno::Errno::EPERM) => {
            warn!(
                pid = old_pid,
                "Permission denied when trying to kill process"
            );
            return Ok(()); // We can't do anything, loop will likely timeout on lock acquire
        }
        Err(e) => warn!(pid = old_pid, error = %e, "Failed to send SIGTERM"),
    }

    // Try SIGKILL
    match signal::kill(pid, Signal::SIGKILL) {
        Ok(_) => {
            // Brief wait
            std::thread::sleep(std::time::Duration::from_millis(100));
            if signal::kill(pid, None).is_ok() {
                anyhow::bail!(
                    "Failed to kill process {} with SIGKILL (still running)",
                    old_pid
                );
            }
            info!(pid = old_pid, "Process terminated after SIGKILL");
            Ok(())
        }
        Err(nix::errno::Errno::ESRCH) => Ok(()),
        Err(nix::errno::Errno::EPERM) => {
            warn!(
                pid = old_pid,
                "Permission denied when trying to kill process (SIGKILL)"
            );
            Ok(())
        }
        Err(e) => {
            warn!(pid = old_pid, error = %e, "Failed to send SIGKILL");
            Ok(()) // Loop will handle retry/timeout
        }
    }
}

/// RAII guard for PID file.
pub struct PidGuard {
    path: PathBuf,
    #[cfg(unix)]
    #[allow(dead_code)]
    file: Option<File>, // Holds the lock (fd)
    #[cfg(not(unix))]
    file: Option<()>,
}

impl PidGuard {
    pub fn new(path: &Path) -> Result<Self> {
        enforce_singleton(path)
    }
}

impl Drop for PidGuard {
    fn drop(&mut self) {
        // Close file (releases lock) and remove it
        // Note: verify we are still the owner?
        // Since we held the lock exclusive, no one else should have written to it.
        // Unless they force deleted it.
        // We can just remove it.

        // On Unix, we need to explicitly unlock? No, closing fd unlocks.
        // Drop of File closes fd.

        // Remove file
        if self.path.exists() {
            let _ = fs::remove_file(&self.path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_pid_guard_lifecycle() {
        let dir = tempdir().unwrap();
        let pid_file = dir.path().join("sidecar.pid");

        {
            let _guard = PidGuard::new(&pid_file).unwrap();
            assert!(pid_file.exists());
            let content = fs::read_to_string(&pid_file).unwrap();
            assert_eq!(content.trim(), std::process::id().to_string());
        }

        assert!(!pid_file.exists());
    }

    #[cfg(unix)]
    #[test]
    fn test_lock_contention() {
        // Test requires spawning a child to hold the lock
        // This is complex to test in unit test without external binary.
        // We can simulate by flocking in the same process on a different fd?
        // flock is per-file-handle in Linux usually, checking nix behavior.

        let dir = tempdir().unwrap();
        let pid_file = dir.path().join("contention.pid");

        let file1 = OpenOptions::new()
            .create(true)
            .write(true)
            .open(&pid_file)
            .unwrap();
        #[allow(deprecated)]
        nix::fcntl::flock(file1.as_raw_fd(), nix::fcntl::FlockArg::LockExclusive).unwrap();

        // Try to acquire in background thread? flock is process-associated on some systems (BSD/macOS)?
        // On Linux flock is associated with the file description (fd).
        // Let's try separate thread with separate opening.

        let path_clone = pid_file.clone();
        let handle = std::thread::spawn(move || {
            // Should block or fail
            match enforce_singleton(&path_clone) {
                Ok(_) => true,
                Err(_) => false,
            }
        });

        // The other thread will try to lock. It will block or loop.
        // Since we hold the lock and are "sidecar-like" (same process name), it might try to kill us?
        // Wait, current process IS "exomonad-sidecar" (or test binary).
        // `is_sidecar_process` checks "exomonad-sidecar".
        // The test runner name is usually different.
        // So `is_sidecar_process` will return false.
        // So it should just loop waiting.

        std::thread::sleep(std::time::Duration::from_millis(500));

        // Release lock
        drop(file1);

        // Now thread should succeed
        assert!(handle.join().unwrap());
    }
}
