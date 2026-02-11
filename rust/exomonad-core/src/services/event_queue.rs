use anyhow::{anyhow, Result};
use exomonad_proto::effects::events::{Event, event::EventType};
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{oneshot, Mutex};

type SessionId = String;

pub struct EventQueue {
    queues: Arc<Mutex<HashMap<SessionId, VecDeque<Event>>>>,
    wakers: Arc<Mutex<HashMap<SessionId, Vec<oneshot::Sender<Event>>>>>,
}

impl EventQueue {
    pub fn new() -> Self {
        Self {
            queues: Arc::new(Mutex::new(HashMap::new())),
            wakers: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub async fn wait_for_event(
        &self,
        session_id: &str,
        types: &[String],
        timeout: Duration,
    ) -> Result<Event> {
        // Check queue first (might already have event)
        {
            let mut queues = self.queues.lock().await;
            if let Some(queue) = queues.get_mut(session_id) {
                if let Some(event) = Self::find_matching(queue, types) {
                    return Ok(event);
                }
            }
        }

        // Register waker
        let (tx, rx) = oneshot::channel();
        {
            let mut wakers = self.wakers.lock().await;
            wakers.entry(session_id.to_string()).or_default().push(tx);
        }

        // Await with timeout
        match tokio::time::timeout(timeout, rx).await {
            Ok(Ok(event)) if Self::matches_type(&event, types) => Ok(event),
            Ok(Ok(_)) => Err(anyhow!("received non-matching event")),
            Err(_) => Ok(Self::timeout_event(timeout.as_secs() as i32)),
            Ok(Err(_)) => Err(anyhow!("waker channel closed")),
        }
    }

    pub async fn notify_event(&self, session_id: &str, event: Event) {
        // Append to queue
        {
            let mut queues = self.queues.lock().await;
            queues.entry(session_id.to_string()).or_default().push_back(event.clone());
        }

        // Wake ONE waiting call (if any)
        let mut wakers = self.wakers.lock().await;
        if let Some(waiters) = wakers.get_mut(session_id) {
            if let Some(tx) = waiters.pop() {
                let _ = tx.send(event);
            }
        }
    }

    fn find_matching(queue: &mut VecDeque<Event>, types: &[String]) -> Option<Event> {
        let pos = queue.iter().position(|e| Self::matches_type(e, types))?;
        queue.remove(pos)
    }

    fn matches_type(event: &Event, types: &[String]) -> bool {
        if types.is_empty() {
            return true;
        }
        match &event.event_type {
            Some(EventType::WorkerComplete(_)) => types.contains(&"worker_complete".to_string()),
            Some(EventType::Timeout(_)) => types.contains(&"timeout".to_string()),
            None => false,
        }
    }

    fn timeout_event(timeout_secs: i32) -> Event {
        Event {
            event_type: Some(EventType::Timeout(
                exomonad_proto::effects::events::Timeout { timeout_secs }
            )),
        }
    }
}

impl Default for EventQueue {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_notify_then_wait() {
        let queue = EventQueue::new();
        let event = Event {
            event_type: Some(EventType::WorkerComplete(
                exomonad_proto::effects::events::WorkerComplete {
                    worker_id: "test".to_string(),
                    status: "success".to_string(),
                    changes: vec![],
                    message: "done".to_string(),
                }
            )),
        };

        queue.notify_event("session1", event.clone()).await;
        let result = queue.wait_for_event("session1", &["worker_complete".to_string()], Duration::from_secs(1)).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_wait_then_notify() {
        let queue = Arc::new(EventQueue::new());
        let queue_clone = queue.clone();

        let handle = tokio::spawn(async move {
            queue_clone.wait_for_event("session2", &["worker_complete".to_string()], Duration::from_secs(5)).await
        });

        tokio::time::sleep(Duration::from_millis(100)).await;

        let event = Event {
            event_type: Some(EventType::WorkerComplete(
                exomonad_proto::effects::events::WorkerComplete {
                    worker_id: "test2".to_string(),
                    status: "success".to_string(),
                    changes: vec![],
                    message: "done".to_string(),
                }
            )),
        };
        queue.notify_event("session2", event).await;

        let result = handle.await.unwrap();
        assert!(result.is_ok());
    }
}
