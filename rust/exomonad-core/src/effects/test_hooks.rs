//! Test instrumentation for effect dispatch.
//!
//! Provides `WithTestHooks<D>` — a wrapper around any `EffectDispatch` that emits
//! `DebugEvent`s via `tokio::sync::broadcast` and supports barriers for synchronous
//! probing at effect boundaries.

use std::path::Path;
use std::sync::Arc;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncWriteExt, BufWriter};
use tokio::sync::{broadcast, oneshot, Mutex, Notify};

use super::*;

/// Phase of an effect dispatch (before or after execution).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Phase {
    Before,
    After,
}

/// Event emitted by test hooks around effect dispatch.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DebugEvent {
    /// Timestamp of the event.
    pub ts: DateTime<Utc>,
    /// Effect type (e.g., "git.get_branch").
    pub effect_type: String,
    /// Agent name from the effect context.
    pub agent_name: String,
    /// Whether this is a before or after event.
    pub phase: Phase,
    /// Whether the effect succeeded (None for Before, Some for After).
    pub success: Option<bool>,
    /// Duration in milliseconds (None for Before, Some for After).
    pub duration_ms: Option<u64>,
}

/// Core test hooks state.
pub struct TestHooks {
    tx: broadcast::Sender<DebugEvent>,
    writer: Option<Arc<Mutex<BufWriter<tokio::fs::File>>>>,
    #[allow(clippy::type_complexity)]
    barriers: Arc<Mutex<Vec<(String, Arc<Notify>, Option<oneshot::Sender<DebugEvent>>)>>>,
}

impl TestHooks {
    /// Create new test hooks with a broadcast channel.
    pub fn new() -> (Self, broadcast::Receiver<DebugEvent>) {
        let (tx, rx) = broadcast::channel(1024);
        (
            Self {
                tx,
                writer: None,
                barriers: Arc::new(Mutex::new(Vec::new())),
            },
            rx,
        )
    }

    /// Create new test hooks that also log to a JSONL file.
    pub async fn with_jsonl(
        path: &Path,
    ) -> tokio::io::Result<(Self, broadcast::Receiver<DebugEvent>)> {
        let (tx, rx) = broadcast::channel(1024);
        let file = tokio::fs::File::create(path).await?;
        let writer = Arc::new(Mutex::new(BufWriter::new(file)));

        Ok((
            Self {
                tx,
                writer: Some(writer),
                barriers: Arc::new(Mutex::new(Vec::new())),
            },
            rx,
        ))
    }

    /// Set a barrier on an effect pattern. When an effect matching the pattern
    /// is dispatched, it will block until the handle is released.
    pub async fn set_barrier(&self, effect_pattern: &str) -> BarrierHandle {
        let notify = Arc::new(Notify::new());
        let (tx, rx) = oneshot::channel();
        self.barriers
            .lock()
            .await
            .push((effect_pattern.to_string(), notify.clone(), Some(tx)));

        BarrierHandle {
            notify,
            event_rx: Some(rx),
        }
    }

    async fn emit(&self, event: DebugEvent) {
        let _ = self.tx.send(event.clone());

        if let Some(writer) = &self.writer {
            if let Ok(line) = serde_json::to_string(&event) {
                let mut w = writer.lock().await;
                let _ = w.write_all(line.as_bytes()).await;
                let _ = w.write_all(b"\n").await;
                let _ = w.flush().await;
            }
        }
    }
}

/// Handle for a barrier that blocks an effect dispatch.
pub struct BarrierHandle {
    notify: Arc<Notify>,
    event_rx: Option<oneshot::Receiver<DebugEvent>>,
}

impl BarrierHandle {
    /// Wait for the effect to hit the barrier. Returns the `Before` event.
    pub async fn wait(&mut self) -> DebugEvent {
        self.event_rx
            .take()
            .expect("Wait called twice")
            .await
            .expect("Barrier dropped")
    }

    /// Allow the effect dispatch to proceed.
    pub fn release(self) {
        self.notify.notify_one();
    }
}

impl Drop for BarrierHandle {
    fn drop(&mut self) {
        self.notify.notify_one();
    }
}

/// Wrapper around `EffectDispatch` that integrates `TestHooks`.
pub struct WithTestHooks<D: EffectDispatch> {
    inner: D,
    hooks: Arc<TestHooks>,
}

impl<D: EffectDispatch> WithTestHooks<D> {
    /// Wrap an existing dispatcher with test hooks.
    pub fn new(inner: D, hooks: Arc<TestHooks>) -> Self {
        Self { inner, hooks }
    }
}

#[async_trait]
impl<D: EffectDispatch> EffectDispatch for WithTestHooks<D> {
    async fn dispatch(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &EffectContext,
    ) -> EffectResult<Vec<u8>> {
        let ts_before = Utc::now();
        let event_before = DebugEvent {
            ts: ts_before,
            effect_type: effect_type.to_string(),
            agent_name: ctx.agent_name.to_string(),
            phase: Phase::Before,
            success: None,
            duration_ms: None,
        };

        // Check barriers
        let mut barriers = self.hooks.barriers.lock().await;
        let mut matched_barrier = None;
        for (i, (pattern, _, _)) in barriers.iter().enumerate() {
            if effect_type.contains(pattern) {
                matched_barrier = Some(i);
                break;
            }
        }

        if let Some(i) = matched_barrier {
            let (_, notify, mut tx_opt) = barriers.swap_remove(i);
            let notify = notify.clone();
            if let Some(tx) = tx_opt.take() {
                let _ = tx.send(event_before.clone());
            }
            drop(barriers);
            notify.notified().await;
        } else {
            drop(barriers);
        }

        self.hooks.emit(event_before).await;

        let start = std::time::Instant::now();
        let result = self.inner.dispatch(effect_type, payload, ctx).await;
        let duration = start.elapsed();

        let event_after = DebugEvent {
            ts: Utc::now(),
            effect_type: effect_type.to_string(),
            agent_name: ctx.agent_name.to_string(),
            phase: Phase::After,
            success: Some(result.is_ok()),
            duration_ms: Some(duration.as_millis() as u64),
        };

        self.hooks.emit(event_after).await;

        result
    }

    fn namespaces(&self) -> Vec<&str> {
        self.inner.namespaces()
    }

    fn has_handler(&self, namespace: &str) -> bool {
        self.inner.has_handler(namespace)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    struct MockDispatch;

    #[async_trait]
    impl EffectDispatch for MockDispatch {
        async fn dispatch(
            &self,
            _effect_type: &str,
            _payload: &[u8],
            _ctx: &EffectContext,
        ) -> EffectResult<Vec<u8>> {
            Ok(vec![42])
        }
        fn namespaces(&self) -> Vec<&str> {
            vec!["mock"]
        }
        fn has_handler(&self, ns: &str) -> bool {
            ns == "mock"
        }
    }

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: crate::domain::AgentName::from("test"),
            birth_branch: crate::domain::BirthBranch::from("test-branch"),
            working_dir: std::path::PathBuf::from("."),
        }
    }

    #[tokio::test]
    async fn test_hooks_emit_events() {
        let (hooks, mut rx) = TestHooks::new();
        let dispatcher = WithTestHooks::new(MockDispatch, Arc::new(hooks));
        let ctx = test_ctx();

        let _ = dispatcher.dispatch("mock.test", &[], &ctx).await.unwrap();

        let event_before = rx.recv().await.unwrap();
        assert_eq!(event_before.phase, Phase::Before);
        assert_eq!(event_before.effect_type, "mock.test");

        let event_after = rx.recv().await.unwrap();
        assert_eq!(event_after.phase, Phase::After);
        assert_eq!(event_after.success, Some(true));
    }

    #[tokio::test]
    async fn test_barrier_blocks_execution() {
        let (hooks, _rx) = TestHooks::new();
        let hooks = Arc::new(hooks);
        let dispatcher = WithTestHooks::new(MockDispatch, hooks.clone());
        let ctx = test_ctx();

        let mut barrier = hooks.set_barrier("mock.blocked").await;

        let dispatch_handle = tokio::spawn(async move {
            dispatcher
                .dispatch("mock.blocked", &[], &ctx)
                .await
                .unwrap()
        });

        // Wait for it to hit the barrier
        let event = barrier.wait().await;
        assert_eq!(event.effect_type, "mock.blocked");

        // It should still be running/blocked
        tokio::task::yield_now().await;
        assert!(!dispatch_handle.is_finished());

        // Release it
        barrier.release();

        let result = dispatch_handle.await.unwrap();
        assert_eq!(result, vec![42]);
    }

    #[tokio::test]
    async fn test_barrier_removed_after_use() {
        let (hooks, _rx) = TestHooks::new();
        let hooks = Arc::new(hooks);
        let dispatcher = WithTestHooks::new(MockDispatch, hooks.clone());
        let ctx = test_ctx();

        let mut barrier = hooks.set_barrier("mock.once").await;

        // First dispatch should hit barrier
        let dispatch_handle1 = tokio::spawn({
            let dispatcher = WithTestHooks::new(MockDispatch, hooks.clone());
            let ctx = ctx.clone();
            async move { dispatcher.dispatch("mock.once", &[], &ctx).await.unwrap() }
        });

        barrier.wait().await;
        barrier.release();
        dispatch_handle1.await.unwrap();

        // Second dispatch should NOT hit barrier (it was removed)
        let dispatch_handle2 = tokio::spawn({
            let dispatcher = WithTestHooks::new(MockDispatch, hooks.clone());
            let ctx = ctx.clone();
            async move { dispatcher.dispatch("mock.once", &[], &ctx).await.unwrap() }
        });

        // This should complete immediately without blocking on a barrier
        let result = tokio::time::timeout(std::time::Duration::from_millis(100), dispatch_handle2)
            .await
            .expect("Second dispatch timed out - barrier was likely not removed")
            .unwrap();
        assert_eq!(result, vec![42]);
    }

    #[tokio::test]
    async fn test_jsonl_logging() {
        let temp = NamedTempFile::new().unwrap();
        let (hooks, _rx) = TestHooks::with_jsonl(temp.path()).await.unwrap();
        let dispatcher = WithTestHooks::new(MockDispatch, Arc::new(hooks));
        let ctx = test_ctx();

        let _ = dispatcher.dispatch("mock.jsonl", &[], &ctx).await.unwrap();

        // Check file content
        let content = std::fs::read_to_string(temp.path()).unwrap();
        let lines: Vec<&str> = content.lines().collect();
        assert_eq!(lines.len(), 2);

        let event_before: DebugEvent = serde_json::from_str(lines[0]).unwrap();
        assert_eq!(event_before.phase, Phase::Before);
        assert_eq!(event_before.effect_type, "mock.jsonl");

        let event_after: DebugEvent = serde_json::from_str(lines[1]).unwrap();
        assert_eq!(event_after.phase, Phase::After);
        assert_eq!(event_after.effect_type, "mock.jsonl");
    }
}
