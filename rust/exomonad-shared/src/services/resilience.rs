//! Composable retry and health tracking primitives.
//!
//! Two orthogonal concerns:
//! - **`RetryPolicy` + `retry()`** — stateless, per-call, bounded attempts with backoff
//! - **`HealthTracker`** — stateful, cross-call, consecutive-failure counter with recovery

use std::sync::atomic::{AtomicU32, Ordering};
use std::time::Duration;

/// Backoff strategy between retry attempts.
#[derive(Debug, Clone)]
pub enum Backoff {
    /// Fixed delay between attempts.
    Fixed(Duration),
    /// Linearly increasing delay: `initial * (attempt + 1)`.
    Linear { initial: Duration },
    /// Exponential delay with cap: `min(initial * 2^attempt, max)`.
    Exponential { initial: Duration, max: Duration },
}

impl Backoff {
    /// Compute the delay for a 0-indexed attempt number.
    pub fn delay(&self, attempt: u32) -> Duration {
        match self {
            Backoff::Fixed(d) => *d,
            Backoff::Linear { initial } => *initial * (attempt + 1),
            Backoff::Exponential { initial, max } => {
                let d = initial.saturating_mul(1u32.checked_shl(attempt).unwrap_or(u32::MAX));
                d.min(*max)
            }
        }
    }
}

/// Stateless retry configuration for a single call.
pub struct RetryPolicy {
    pub max_attempts: u32,
    pub backoff: Backoff,
    pub should_retry: fn(&anyhow::Error) -> bool,
}

fn always_retry(_: &anyhow::Error) -> bool {
    true
}

impl RetryPolicy {
    /// Retry all errors with the given backoff.
    pub fn new(max_attempts: u32, backoff: Backoff) -> Self {
        Self {
            max_attempts,
            backoff,
            should_retry: always_retry,
        }
    }

    /// Retry only errors matching the predicate.
    pub fn filtered(
        max_attempts: u32,
        backoff: Backoff,
        should_retry: fn(&anyhow::Error) -> bool,
    ) -> Self {
        Self {
            max_attempts,
            backoff,
            should_retry,
        }
    }
}

/// Execute an async operation with retry according to the policy.
pub async fn retry<T, F, Fut>(policy: &RetryPolicy, mut f: F) -> anyhow::Result<T>
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = anyhow::Result<T>>,
{
    let mut attempt = 0u32;
    loop {
        match f().await {
            Ok(v) => return Ok(v),
            Err(e) if attempt + 1 < policy.max_attempts && (policy.should_retry)(&e) => {
                let delay = policy.backoff.delay(attempt);
                tracing::warn!(
                    attempt,
                    delay_ms = delay.as_millis() as u64,
                    error = %e,
                    "Retrying"
                );
                tokio::time::sleep(delay).await;
                attempt += 1;
            }
            Err(e) => return Err(e),
        }
    }
}

/// Cross-call health tracker with consecutive-failure counting and sync recovery.
///
/// For async recovery (e.g., `GitHubClient` rebuild), use domain-specific logic.
/// `HealthTracker` is for lightweight sync recovery (setting flags, clearing caches).
pub struct HealthTracker<F: Fn() + Send + Sync> {
    consecutive_failures: AtomicU32,
    threshold: u32,
    recover: F,
}

impl<F: Fn() + Send + Sync> HealthTracker<F> {
    pub fn new(threshold: u32, recover: F) -> Self {
        Self {
            consecutive_failures: AtomicU32::new(0),
            threshold,
            recover,
        }
    }

    /// Reset failure counter on success.
    pub fn report_success(&self) {
        self.consecutive_failures.store(0, Ordering::Relaxed);
    }

    /// Increment failure counter. Calls `recover()` if threshold is reached.
    pub fn report_failure(&self) {
        let prev = self.consecutive_failures.fetch_add(1, Ordering::Relaxed);
        if prev + 1 >= self.threshold {
            (self.recover)();
            self.consecutive_failures.store(0, Ordering::Relaxed);
        }
    }

    /// Current consecutive failure count.
    pub fn failure_count(&self) -> u32 {
        self.consecutive_failures.load(Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU32;
    use std::sync::Arc;

    #[test]
    fn test_backoff_fixed() {
        let b = Backoff::Fixed(Duration::from_millis(100));
        assert_eq!(b.delay(0), Duration::from_millis(100));
        assert_eq!(b.delay(1), Duration::from_millis(100));
        assert_eq!(b.delay(5), Duration::from_millis(100));
    }

    #[test]
    fn test_backoff_linear() {
        let b = Backoff::Linear {
            initial: Duration::from_millis(200),
        };
        assert_eq!(b.delay(0), Duration::from_millis(200));
        assert_eq!(b.delay(1), Duration::from_millis(400));
        assert_eq!(b.delay(2), Duration::from_millis(600));
    }

    #[test]
    fn test_backoff_exponential() {
        let b = Backoff::Exponential {
            initial: Duration::from_millis(500),
            max: Duration::from_secs(2),
        };
        assert_eq!(b.delay(0), Duration::from_millis(500));
        assert_eq!(b.delay(1), Duration::from_millis(1000));
        assert_eq!(b.delay(2), Duration::from_secs(2)); // capped
        assert_eq!(b.delay(3), Duration::from_secs(2)); // capped
    }

    #[test]
    fn test_backoff_exponential_overflow() {
        let b = Backoff::Exponential {
            initial: Duration::from_millis(500),
            max: Duration::from_secs(10),
        };
        // Very large attempt number should not panic
        assert!(b.delay(31) <= Duration::from_secs(10));
    }

    #[tokio::test]
    async fn test_retry_succeeds_first_attempt() {
        let policy = RetryPolicy::new(3, Backoff::Fixed(Duration::from_millis(1)));
        let result = retry(&policy, || async { Ok::<_, anyhow::Error>(42) }).await;
        assert_eq!(result.unwrap(), 42);
    }

    #[tokio::test]
    async fn test_retry_succeeds_after_failures() {
        let attempt = Arc::new(AtomicU32::new(0));
        let policy = RetryPolicy::new(3, Backoff::Fixed(Duration::from_millis(1)));
        let a = attempt.clone();
        let result = retry(&policy, || {
            let a = a.clone();
            async move {
                let n = a.fetch_add(1, Ordering::SeqCst);
                if n < 2 {
                    anyhow::bail!("not yet")
                }
                Ok(99)
            }
        })
        .await;
        assert_eq!(result.unwrap(), 99);
        assert_eq!(attempt.load(Ordering::SeqCst), 3);
    }

    #[tokio::test]
    async fn test_retry_exhausted() {
        let policy = RetryPolicy::new(2, Backoff::Fixed(Duration::from_millis(1)));
        let result: anyhow::Result<()> =
            retry(&policy, || async { anyhow::bail!("always fails") }).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_retry_filtered_skips_non_matching() {
        let attempt = Arc::new(AtomicU32::new(0));
        let policy = RetryPolicy::filtered(3, Backoff::Fixed(Duration::from_millis(1)), |_| false);
        let a = attempt.clone();
        let result: anyhow::Result<()> = retry(&policy, || {
            let a = a.clone();
            async move {
                a.fetch_add(1, Ordering::SeqCst);
                anyhow::bail!("not retryable")
            }
        })
        .await;
        assert!(result.is_err());
        // Only 1 attempt because predicate returns false
        assert_eq!(attempt.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_health_tracker_success_resets() {
        let tracker = HealthTracker::new(3, || {});
        tracker.report_failure();
        tracker.report_failure();
        assert_eq!(tracker.failure_count(), 2);
        tracker.report_success();
        assert_eq!(tracker.failure_count(), 0);
    }

    #[test]
    fn test_health_tracker_triggers_recovery() {
        let recovered = Arc::new(AtomicU32::new(0));
        let r = recovered.clone();
        let tracker = HealthTracker::new(3, move || {
            r.fetch_add(1, Ordering::SeqCst);
        });
        tracker.report_failure();
        tracker.report_failure();
        assert_eq!(recovered.load(Ordering::SeqCst), 0);
        tracker.report_failure(); // hits threshold
        assert_eq!(recovered.load(Ordering::SeqCst), 1);
        assert_eq!(tracker.failure_count(), 0); // reset after recovery
    }

    #[test]
    fn test_health_tracker_multiple_recovery_cycles() {
        let recovered = Arc::new(AtomicU32::new(0));
        let r = recovered.clone();
        let tracker = HealthTracker::new(2, move || {
            r.fetch_add(1, Ordering::SeqCst);
        });
        // First cycle
        tracker.report_failure();
        tracker.report_failure();
        assert_eq!(recovered.load(Ordering::SeqCst), 1);
        // Second cycle
        tracker.report_failure();
        tracker.report_failure();
        assert_eq!(recovered.load(Ordering::SeqCst), 2);
    }

    #[tokio::test]
    async fn test_retry_single_attempt_fails_immediately() {
        let attempt = Arc::new(AtomicU32::new(0));
        let a = attempt.clone();
        let policy = RetryPolicy::new(1, Backoff::Fixed(Duration::from_millis(1)));
        let result: anyhow::Result<()> = retry(&policy, || {
            let a = a.clone();
            async move {
                a.fetch_add(1, Ordering::SeqCst);
                anyhow::bail!("fail")
            }
        })
        .await;
        assert!(result.is_err());
        assert_eq!(attempt.load(Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn test_retry_filtered_retries_matching_errors() {
        let attempt = Arc::new(AtomicU32::new(0));
        let a = attempt.clone();
        // Only retry errors containing "transient"
        let policy = RetryPolicy::filtered(3, Backoff::Fixed(Duration::from_millis(1)), |e| {
            e.to_string().contains("transient")
        });
        let result = retry(&policy, || {
            let a = a.clone();
            async move {
                let n = a.fetch_add(1, Ordering::SeqCst);
                if n < 2 {
                    anyhow::bail!("transient error")
                }
                Ok(42)
            }
        })
        .await;
        assert_eq!(result.unwrap(), 42);
        assert_eq!(attempt.load(Ordering::SeqCst), 3);
    }

    #[tokio::test]
    async fn test_retry_filtered_stops_on_non_matching_error() {
        let attempt = Arc::new(AtomicU32::new(0));
        let a = attempt.clone();
        let policy = RetryPolicy::filtered(5, Backoff::Fixed(Duration::from_millis(1)), |e| {
            e.to_string().contains("transient")
        });
        let result: anyhow::Result<()> = retry(&policy, || {
            let a = a.clone();
            async move {
                let n = a.fetch_add(1, Ordering::SeqCst);
                if n == 0 {
                    anyhow::bail!("transient error") // retryable
                }
                anyhow::bail!("permanent error") // not retryable
            }
        })
        .await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("permanent"));
        assert_eq!(attempt.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_backoff_linear_zero_initial() {
        let b = Backoff::Linear {
            initial: Duration::ZERO,
        };
        assert_eq!(b.delay(0), Duration::ZERO);
        assert_eq!(b.delay(100), Duration::ZERO);
    }

    #[test]
    fn test_health_tracker_interleaved_success_failure() {
        let recovered = Arc::new(AtomicU32::new(0));
        let r = recovered.clone();
        let tracker = HealthTracker::new(3, move || {
            r.fetch_add(1, Ordering::SeqCst);
        });
        tracker.report_failure();
        tracker.report_failure();
        tracker.report_success(); // resets counter
        tracker.report_failure();
        tracker.report_failure();
        // Only 2 consecutive failures, not 3 — should not trigger
        assert_eq!(recovered.load(Ordering::SeqCst), 0);
    }
}
