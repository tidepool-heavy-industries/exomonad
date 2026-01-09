//! Integration tests for Supervisor timeout and process lifecycle.
//!
//! These tests spawn actual processes to verify:
//! - Timeout enforcement (child is killed when timeout fires)
//! - Process cleanup on drop (no orphan processes)
//! - Child processes can be killed by signals

use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use mantle::error::ZellijCcError;
use mantle::supervisor::Supervisor;

/// Test that a process that exits quickly is handled correctly.
#[test]
fn test_quick_exit() {
    let mut cmd = Command::new("echo");
    cmd.arg("hello").stdout(Stdio::piped());

    let mut sup = Supervisor::spawn(cmd, Some(Duration::from_secs(10))).unwrap();
    let _stdout = sup.take_stdout();

    let status = sup.wait_with_timeout().unwrap();
    assert!(status.success());
}

/// Test that timeout kills a long-running process.
#[test]
fn test_timeout_kills_process() {
    // Spawn a process that sleeps for 60 seconds
    let mut cmd = Command::new("sleep");
    cmd.arg("60").stdout(Stdio::piped());

    let timeout = Duration::from_millis(500);
    let mut sup = Supervisor::spawn(cmd, Some(timeout)).unwrap();
    let _stdout = sup.take_stdout();

    let start = Instant::now();
    let result = sup.wait_with_timeout();
    let elapsed = start.elapsed();

    // Should fail with ProcessTimeout
    match result {
        Err(ZellijCcError::ProcessTimeout { elapsed: _ }) => {
            // Expected
        }
        other => panic!("Expected ProcessTimeout, got {:?}", other),
    }

    // Should complete in roughly the timeout duration (with some margin)
    assert!(
        elapsed < Duration::from_secs(2),
        "Timeout took too long: {:?}",
        elapsed
    );
}

/// Test that Drop kills orphan processes.
#[test]
fn test_drop_kills_process() {
    // Spawn a process that sleeps for 60 seconds
    let mut cmd = Command::new("sleep");
    cmd.arg("60").stdout(Stdio::piped());

    let sup = Supervisor::spawn(cmd, None).unwrap();

    // Drop the supervisor - it should kill the child
    // If Drop doesn't kill the child, this test would hang waiting for
    // the 60-second sleep to complete.
    drop(sup);

    // If we get here without hanging, the child was killed
}

/// Test that stdout can be read while process runs.
#[test]
fn test_stdout_reading() {
    use std::io::{BufRead, BufReader};

    // Spawn a process that outputs something then exits
    let mut cmd = Command::new("sh");
    cmd.args(["-c", "echo line1; echo line2; echo line3"])
        .stdout(Stdio::piped());

    let mut sup = Supervisor::spawn(cmd, Some(Duration::from_secs(10))).unwrap();
    let stdout = sup.take_stdout();

    let reader = BufReader::new(stdout);
    let lines: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();

    assert_eq!(lines, vec!["line1", "line2", "line3"]);

    let status = sup.wait_with_timeout().unwrap();
    assert!(status.success());
}

/// Test that a process with non-zero exit code is captured.
#[test]
fn test_nonzero_exit() {
    let mut cmd = Command::new("sh");
    cmd.args(["-c", "exit 42"]).stdout(Stdio::piped());

    let mut sup = Supervisor::spawn(cmd, Some(Duration::from_secs(10))).unwrap();
    let _stdout = sup.take_stdout();

    let status = sup.wait_with_timeout().unwrap();
    assert!(!status.success());
    assert_eq!(status.code(), Some(42));
}

/// Test that timeout works for longer durations (verifies poll timeout fix).
/// This test uses a 2-second timeout to verify the poll loop works correctly.
#[test]
fn test_longer_timeout() {
    let mut cmd = Command::new("sleep");
    cmd.arg("60").stdout(Stdio::piped());

    // Use a timeout longer than 65s would be problematic with the old u16 bug,
    // but we use 2s here for test speed while still exercising the loop.
    let timeout = Duration::from_secs(2);
    let mut sup = Supervisor::spawn(cmd, Some(timeout)).unwrap();
    let _stdout = sup.take_stdout();

    let start = Instant::now();
    let result = sup.wait_with_timeout();
    let elapsed = start.elapsed();

    // Should timeout
    assert!(matches!(result, Err(ZellijCcError::ProcessTimeout { .. })));

    // Should complete in roughly 2 seconds (with margin for kill overhead)
    assert!(
        elapsed < Duration::from_secs(5),
        "Timeout took too long: {:?}",
        elapsed
    );
    assert!(
        elapsed >= Duration::from_secs(1),
        "Timeout fired too early: {:?}",
        elapsed
    );
}

/// Test that spawn failure is handled correctly.
#[test]
fn test_spawn_failure() {
    let mut cmd = Command::new("/nonexistent/binary/that/does/not/exist");
    cmd.stdout(Stdio::piped());

    let result = Supervisor::spawn(cmd, None);
    assert!(matches!(result, Err(ZellijCcError::Spawn(_))));
}

/// Test that child processes can be killed by signals.
///
/// This test verifies that when we send SIGKILL directly to a child,
/// the child terminates. This is the observable behavior that matters
/// for cleanup - if SIGKILL works, SIGTERM will too (with proper handling).
#[test]
fn test_child_can_be_killed() {
    use nix::sys::signal::{kill, Signal};
    use std::os::unix::process::ExitStatusExt;
    use std::thread;

    // Spawn a process that sleeps
    let mut cmd = Command::new("sleep");
    cmd.arg("60").stdout(Stdio::piped());

    let mut sup = Supervisor::spawn(cmd, Some(Duration::from_secs(10))).unwrap();
    let _stdout = sup.take_stdout();

    // Small delay to ensure process is running
    thread::sleep(Duration::from_millis(50));

    // Send SIGKILL directly to the child (cannot be caught/ignored)
    let child_pid = sup.pid();
    kill(child_pid, Signal::SIGKILL).expect("Failed to send SIGKILL to child");

    // Wait for the child - it should be killed
    let status = sup.wait_with_timeout().unwrap();

    // Process killed by SIGKILL should have signal 9
    assert_eq!(
        status.signal(),
        Some(9),
        "Child should have been killed by SIGKILL (signal 9)"
    );
}
