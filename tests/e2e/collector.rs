use std::time::Duration;
use tokio::sync::broadcast;
use tokio::time::timeout;
use exomonad_core::effects::test_hooks::{DebugEvent, Phase};
use std::path::Path;
use std::process::Command;

/// EventCollector wraps a broadcast receiver for DebugEvents and buffers them.
pub struct EventCollector {
    rx: broadcast::Receiver<DebugEvent>,
    buffer: Vec<DebugEvent>,
}

impl EventCollector {
    /// Create a new EventCollector from a broadcast receiver.
    pub fn new(rx: broadcast::Receiver<DebugEvent>) -> Self {
        Self {
            rx,
            buffer: Vec::new(),
        }
    }

    /// Collect events until the predicate returns true or timeout expires.
    pub async fn collect_until<F>(&mut self, predicate: F, timeout_duration: Duration) -> Vec<DebugEvent> 
    where F: Fn(&[DebugEvent]) -> bool 
    {
        let start = tokio::time::Instant::now();
        loop {
            if predicate(&self.buffer) {
                break;
            }
            
            let elapsed = start.elapsed();
            if elapsed >= timeout_duration {
                break;
            }
            
            let remaining = timeout_duration - elapsed;
            
            match timeout(remaining, self.rx.recv()).await {
                Ok(Ok(event)) => {
                    self.buffer.push(event);
                }
                Ok(Err(broadcast::error::RecvError::Closed)) => {
                    break;
                }
                Ok(Err(broadcast::error::RecvError::Lagged(_))) => {
                    // If we lagged, we continue and try to receive the next available event
                    continue;
                }
                Err(_) => {
                    // Timeout
                    break;
                }
            }
        }
        self.buffer.clone()
    }

    /// Access the collected events.
    pub fn events(&self) -> &[DebugEvent] {
        &self.buffer
    }

    /// Checks if any collected event matches the given effect type.
    pub fn has(&self, effect_type: &str) -> bool {
        self.buffer.iter().any(|e| e.effect_type == effect_type)
    }

    /// Counts how many collected events match the given effect type.
    pub fn count(&self, effect_type: &str) -> usize {
        self.buffer.iter().filter(|e| e.effect_type == effect_type).count()
    }

    /// Checks if events of the given types appear in order (not necessarily contiguous).
    pub fn has_sequence(&self, types: &[&str]) -> bool {
        let mut type_idx = 0;
        if type_idx == types.len() {
            return true;
        }
        for event in &self.buffer {
            if event.effect_type == types[type_idx] {
                type_idx += 1;
                if type_idx == types.len() {
                    return true;
                }
            }
        }
        false
    }
}

/// ProbeContext provides helpers for inspecting the external state (tmux, git, filesystem).
pub struct ProbeContext;

impl ProbeContext {
    /// List window names in a tmux session.
    pub fn tmux_list_windows(session: &str) -> Vec<String> {
        let output = Command::new("tmux")
            .args(["list-windows", "-t", session, "-F", "#{window_name}"])
            .output();
        
        match output {
            Ok(o) if o.status.success() => {
                String::from_utf8_lossy(&o.stdout)
                    .lines()
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    /// List local branch names in a git repository.
    pub fn git_branches(dir: &Path) -> Vec<String> {
        let output = Command::new("git")
            .arg("-C")
            .arg(dir)
            .args(["branch", "--format=%(refname:short)"])
            .output();
        
        match output {
            Ok(o) if o.status.success() => {
                String::from_utf8_lossy(&o.stdout)
                    .lines()
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    /// Checks if a file exists in a directory.
    pub fn file_exists(dir: &Path, name: &str) -> bool {
        dir.join(name).exists()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    fn mock_event(effect_type: &str) -> DebugEvent {
        DebugEvent {
            ts: Utc::now(),
            effect_type: effect_type.to_string(),
            agent_name: "test-agent".to_string(),
            phase: Phase::Before,
            success: None,
            duration_ms: None,
        }
    }

    #[tokio::test]
    async fn test_collect_until() {
        let (tx, rx) = broadcast::channel(10);
        let mut collector = EventCollector::new(rx);
        
        let tx_clone = tx.clone();
        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_millis(10)).await;
            tx_clone.send(mock_event("git.clone")).unwrap();
            tokio::time::sleep(Duration::from_millis(10)).await;
            tx_clone.send(mock_event("git.checkout")).unwrap();
        });
        
        let events = collector.collect_until(|evs| evs.len() >= 2, Duration::from_millis(100)).await;
        assert_eq!(events.len(), 2);
        assert_eq!(events[0].effect_type, "git.clone");
        assert_eq!(events[1].effect_type, "git.checkout");
    }

    #[test]
    fn test_assertion_helpers() {
        let (_tx, rx) = broadcast::channel(1);
        let mut collector = EventCollector::new(rx);
        collector.buffer.push(mock_event("git.clone"));
        collector.buffer.push(mock_event("git.checkout"));
        collector.buffer.push(mock_event("git.push"));
        
        assert!(collector.has("git.clone"));
        assert!(!collector.has("git.commit"));
        assert_eq!(collector.count("git.clone"), 1);
        assert!(collector.has_sequence(&["git.clone", "git.push"]));
        assert!(!collector.has_sequence(&["git.push", "git.clone"]));
        assert!(collector.has_sequence(&["git.clone", "git.checkout", "git.push"]));
        assert!(collector.has_sequence(&[]));
    }
}
