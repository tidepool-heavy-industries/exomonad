//! Event-driven question/answer registry using oneshot channels.
//!
//! Shared between effect handlers (ask_question) and MCP tools (answer_question).
//! No polling — the waiting effect handler is woken immediately when an answer arrives.

use std::collections::HashMap;
use std::sync::Mutex;
use tokio::sync::oneshot;
use uuid::Uuid;

/// Registry of pending questions awaiting answers.
///
/// Effect handlers register a question and await the receiver.
/// MCP tools resolve questions by sending through the stored sender.
pub struct QuestionRegistry {
    pending: Mutex<HashMap<String, oneshot::Sender<String>>>,
}

impl QuestionRegistry {
    pub fn new() -> Self {
        Self {
            pending: Mutex::new(HashMap::new()),
        }
    }

    /// Register a pending question. Returns (question_id, receiver).
    ///
    /// The caller awaits the receiver (with timeout). When `resolve` is called
    /// with the matching question_id, the receiver completes.
    pub fn register(&self) -> (String, oneshot::Receiver<String>) {
        let question_id = format!("q-{}", Uuid::new_v4());
        let (tx, rx) = oneshot::channel();

        let mut pending = self.pending.lock().expect("QuestionRegistry lock poisoned");
        pending.insert(question_id.clone(), tx);

        (question_id, rx)
    }

    /// Resolve a pending question with an answer.
    ///
    /// Returns `true` if the question was found and resolved.
    /// Returns `false` if the question_id was not found (already answered, timed out,
    /// or never registered).
    pub fn resolve(&self, question_id: &str, answer: String) -> bool {
        let mut pending = self.pending.lock().expect("QuestionRegistry lock poisoned");
        if let Some(tx) = pending.remove(question_id) {
            // Receiver may have been dropped (timeout), so ignore send errors.
            let _ = tx.send(answer);
            true
        } else {
            false
        }
    }

    /// Remove a question from the registry without answering it.
    ///
    /// Used when a question times out to clean up the sender.
    pub fn cancel(&self, question_id: &str) {
        let mut pending = self.pending.lock().expect("QuestionRegistry lock poisoned");
        pending.remove(question_id);
    }
}

impl Default for QuestionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_register_and_resolve() {
        let registry = QuestionRegistry::new();
        let (qid, rx) = registry.register();

        assert!(registry.resolve(&qid, "the answer".to_string()));

        let answer = tokio::time::timeout(Duration::from_secs(1), rx)
            .await
            .expect("should not timeout")
            .expect("channel should not be closed");

        assert_eq!(answer, "the answer");
    }

    #[tokio::test]
    async fn test_resolve_unknown_returns_false() {
        let registry = QuestionRegistry::new();
        assert!(!registry.resolve("q-nonexistent", "answer".to_string()));
    }

    #[tokio::test]
    async fn test_cancel_removes_question() {
        let registry = QuestionRegistry::new();
        let (qid, _rx) = registry.register();

        registry.cancel(&qid);
        assert!(!registry.resolve(&qid, "too late".to_string()));
    }

    #[tokio::test]
    async fn test_timeout_drops_receiver() {
        let registry = QuestionRegistry::new();
        let (qid, rx) = registry.register();

        // Drop the receiver (simulates timeout)
        drop(rx);

        // Resolve still succeeds (removes from map) even though receiver is gone
        assert!(registry.resolve(&qid, "answer".to_string()));
    }
}
