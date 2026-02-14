use anyhow::{anyhow, Result};
use exomonad_proto::effects::events::{event::EventType, Event};
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{oneshot, Mutex};
use tracing::{debug, info};

type SessionId = String;

pub struct EventQueue {
    queues: Arc<Mutex<HashMap<SessionId, VecDeque<Event>>>>,
    wakers: Arc<Mutex<HashMap<SessionId, Vec<oneshot::Sender<Event>>>>>,
    next_id: Arc<Mutex<u64>>,
}

impl EventQueue {
    pub fn new() -> Self {
        Self {
            queues: Arc::new(Mutex::new(HashMap::new())),
            wakers: Arc::new(Mutex::new(HashMap::new())),
            next_id: Arc::new(Mutex::new(1)),
        }
    }

    pub async fn wait_for_event(
        &self,
        session_id: &str,
        types: &[String],
        timeout: Duration,
        after_event_id: u64,
    ) -> Result<Event> {
        // Check queue first (might already have event)
        {
            let mut queues = self.queues.lock().await;
            if let Some(queue) = queues.get_mut(session_id) {
                if let Some(event) = Self::find_matching(queue, types, after_event_id) {
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
            Ok(Ok(event))
                if (after_event_id == 0 || event.event_id > after_event_id)
                    && Self::matches_type(&event, types) =>
            {
                Ok(event)
            }
            Ok(Ok(_)) => Err(anyhow!("received non-matching event")),
            Err(_) => Ok(Self::timeout_event(timeout.as_secs() as i32)),
            Ok(Err(_)) => Err(anyhow!("waker channel closed")),
        }
    }

    pub async fn notify_event(&self, session_id: &str, mut event: Event) {
        // Assign monotonic event ID
        let id = {
            let mut next = self.next_id.lock().await;
            let id = *next;
            *next += 1;
            id
        };
        event.event_id = id;

        info!(session_id = %session_id, event_id = id, "EventQueue: notify_event appending to queue");
        // Append to queue
        {
            let mut queues = self.queues.lock().await;
            let queue = queues.entry(session_id.to_string()).or_default();
            queue.push_back(event.clone());
            info!(session_id = %session_id, queue_len = queue.len(), "EventQueue: event appended");
        }

        // Wake ONE waiting call (if any)
        let mut wakers = self.wakers.lock().await;
        if let Some(waiters) = wakers.get_mut(session_id) {
            if let Some(tx) = waiters.pop() {
                info!(session_id = %session_id, "EventQueue: waking a waiter");
                let _ = tx.send(event);
            } else {
                debug!(session_id = %session_id, "EventQueue: no waiters to wake");
            }
        } else {
            debug!(session_id = %session_id, "EventQueue: no waiter list for session");
        }
    }

    fn find_matching(
        queue: &mut VecDeque<Event>,
        types: &[String],
        after_event_id: u64,
    ) -> Option<Event> {
        let pos = queue.iter().position(|e| {
            (after_event_id == 0 || e.event_id > after_event_id) && Self::matches_type(e, types)
        })?;
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
            event_id: 0,
            event_type: Some(EventType::Timeout(
                exomonad_proto::effects::events::Timeout { timeout_secs },
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
            event_id: 0,
            event_type: Some(EventType::WorkerComplete(
                exomonad_proto::effects::events::WorkerComplete {
                    worker_id: "test".to_string(),
                    status: "success".to_string(),
                    changes: vec![],
                    message: "done".to_string(),
                },
            )),
        };

        queue.notify_event("session1", event.clone()).await;
        let result = queue
            .wait_for_event(
                "session1",
                &["worker_complete".to_string()],
                Duration::from_secs(1),
                0,
            )
            .await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_wait_then_notify() {
        let queue = Arc::new(EventQueue::new());
        let queue_clone = queue.clone();

        let handle = tokio::spawn(async move {
            queue_clone
                .wait_for_event(
                    "session2",
                    &["worker_complete".to_string()],
                    Duration::from_secs(5),
                    0,
                )
                .await
        });

        tokio::time::sleep(Duration::from_millis(100)).await;

        let event = Event {
            event_id: 0,
            event_type: Some(EventType::WorkerComplete(
                exomonad_proto::effects::events::WorkerComplete {
                    worker_id: "test2".to_string(),
                    status: "success".to_string(),
                    changes: vec![],
                    message: "done".to_string(),
                },
            )),
        };
        queue.notify_event("session2", event).await;

        let result = handle.await.unwrap();
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cursor_filtering() {
        let queue = EventQueue::new();

        let event1 = Event {
            event_id: 0, // Will be assigned by queue
            event_type: Some(EventType::WorkerComplete(
                exomonad_proto::effects::events::WorkerComplete {
                    worker_id: "worker-1".to_string(),
                    status: "success".to_string(),
                    changes: vec![],
                    message: "first".to_string(),
                },
            )),
        };
        let event2 = Event {
            event_id: 0,
            event_type: Some(EventType::WorkerComplete(
                exomonad_proto::effects::events::WorkerComplete {
                    worker_id: "worker-2".to_string(),
                    status: "success".to_string(),
                    changes: vec![],
                    message: "second".to_string(),
                },
            )),
        };

        queue.notify_event("s1", event1).await;
        queue.notify_event("s1", event2).await;

        // Without cursor: gets first event (id=1)
        let result = queue
            .wait_for_event(
                "s1",
                &["worker_complete".to_string()],
                Duration::from_secs(1),
                0,
            )
            .await
            .unwrap();
        assert_eq!(result.event_id, 1);

        // With cursor=1: skips first, gets second (id=2)
        let result = queue
            .wait_for_event(
                "s1",
                &["worker_complete".to_string()],
                Duration::from_secs(1),
                1,
            )
            .await
            .unwrap();
        assert_eq!(result.event_id, 2);
    }
}
