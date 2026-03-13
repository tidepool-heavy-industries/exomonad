use crate::inbox::is_message_read;
use std::time::Duration;

/// Poll until CC's InboxPoller reads a message, or timeout.
pub async fn wait_for_read(
    team: &str,
    recipient: &str,
    timestamp: &str,
    timeout: Duration,
) -> bool {
    let interval = Duration::from_secs(2);
    let start = tokio::time::Instant::now();

    loop {
        if is_message_read(team, recipient, timestamp) {
            return true;
        }
        if start.elapsed() >= timeout {
            return false;
        }
        tokio::time::sleep(interval).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_timeout_returns_false() {
        let result = wait_for_read(
            "nonexistent-team",
            "nobody",
            "2024-01-01T00:00:00.000Z",
            Duration::from_millis(100),
        ).await;
        assert!(!result);
    }
}
