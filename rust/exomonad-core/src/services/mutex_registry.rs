use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{oneshot, Mutex};
use tracing::{info, warn};
use uuid::Uuid;
use exomonad_proto::effects::coordination::AcquireMutexResponse;

#[derive(Debug, Clone)]
pub struct LockState {
    pub lock_id: String,
    pub holder_agent: String,
    pub intent: String,
    pub acquired_at: Instant,
    pub expires_at: Instant,
}

#[derive(Debug)]
pub struct Waiter {
    pub agent: String,
    pub intent: String,
    pub estimated_time: Duration,
    pub tx: oneshot::Sender<AcquireMutexResponse>,
}

pub struct MutexRegistry {
    locks: Arc<Mutex<HashMap<String, LockState>>>,
    waiters: Arc<Mutex<HashMap<String, VecDeque<Waiter>>>>,
}

impl MutexRegistry {
    pub fn new() -> Self {
        Self {
            locks: Arc::new(Mutex::new(HashMap::new())),
            waiters: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub async fn acquire(
        &self,
        resource: String,
        agent: String,
        intent: String,
        estimated_time: Duration,
        timeout: Duration,
    ) -> AcquireMutexResponse {
        // Fast path: check if already held by this agent (idempotent) or if available
        {
            let mut locks = self.locks.lock().await;
            if let Some(lock) = locks.get(&resource) {
                if lock.holder_agent == agent {
                    return AcquireMutexResponse {
                        acquired: true,
                        lock_id: lock.lock_id.clone(),
                        holder_intent: lock.intent.clone(),
                    };
                }
            } else {
                // Lock is available
                let lock_id = Uuid::new_v4().to_string();
                let now = Instant::now();
                let lock = LockState {
                    lock_id: lock_id.clone(),
                    holder_agent: agent.clone(),
                    intent: intent.clone(),
                    acquired_at: now,
                    expires_at: now + estimated_time,
                };
                locks.insert(resource.clone(), lock);
                info!(resource = %resource, agent = %agent, lock_id = %lock_id, "Mutex acquired");
                return AcquireMutexResponse {
                    acquired: true,
                    lock_id,
                    holder_intent: intent,
                };
            }
        }

        // Capture current holder intent for timeout diagnostics
        let current_holder_intent = {
            let locks = self.locks.lock().await;
            locks.get(&resource).map(|l| l.intent.clone()).unwrap_or_default()
        };

        // Slow path: register waiter
        let (tx, rx) = oneshot::channel();
        {
            let mut waiters = self.waiters.lock().await;
            let queue = waiters.entry(resource.clone()).or_default();
            queue.push_back(Waiter {
                agent: agent.clone(),
                intent: intent.clone(),
                estimated_time,
                tx,
            });
        }

        match tokio::time::timeout(timeout, rx).await {
            Ok(Ok(resp)) => resp,
            Ok(Err(_)) => {
                warn!(resource = %resource, agent = %agent, "Waiter channel closed");
                AcquireMutexResponse {
                    acquired: false,
                    lock_id: String::new(),
                    holder_intent: current_holder_intent,
                }
            }
            Err(_) => {
                info!(resource = %resource, agent = %agent, "Timeout waiting for mutex");
                AcquireMutexResponse {
                    acquired: false,
                    lock_id: String::new(),
                    holder_intent: current_holder_intent,
                }
            }
        }
    }

    pub async fn release(&self, resource: String, lock_id: String) -> bool {
        let mut locks = self.locks.lock().await;
        if let Some(lock) = locks.get(&resource) {
            if lock.lock_id == lock_id {
                locks.remove(&resource);
                info!(resource = %resource, lock_id = %lock_id, "Mutex released");

                // Drop locks guard before granting next waiter to avoid deadlock
                drop(locks);
                self.grant_next_waiter(resource).await;
                return true;
            }
        }
        false
    }

    async fn grant_next_waiter(&self, resource: String) {
        let mut waiters = self.waiters.lock().await;
        while let Some(queue) = waiters.get_mut(&resource) {
            if let Some(waiter) = queue.pop_front() {
                // Attempt to acquire lock for this waiter
                let mut locks = self.locks.lock().await;
                // Re-verify resource is still available
                if !locks.contains_key(&resource) {
                    let lock_id = Uuid::new_v4().to_string();
                    let now = Instant::now();
                    let lock = LockState {
                        lock_id: lock_id.clone(),
                        holder_agent: waiter.agent.clone(),
                        intent: waiter.intent.clone(),
                        acquired_at: now,
                        expires_at: now + waiter.estimated_time,
                    };
                    locks.insert(resource.clone(), lock);

                    let resp = AcquireMutexResponse {
                        acquired: true,
                        lock_id: lock_id.clone(),
                        holder_intent: waiter.intent.clone(),
                    };

                    if waiter.tx.send(resp).is_ok() {
                        info!(resource = %resource, agent = %waiter.agent, lock_id = %lock_id, "Mutex granted to waiter");
                        return; // Successfully granted
                    } else {
                        // Waiter dropped (timed out), release lock and try next
                        locks.remove(&resource);
                        warn!(resource = %resource, agent = %waiter.agent, "Waiter dropped channel, trying next");
                        continue;
                    }
                }
            } else {
                break;
            }
        }
    }

    pub fn spawn_expiry_task(self: &Arc<Self>) {
        let registry = self.clone();
        tokio::spawn(async move {
            let mut interval = tokio::time::interval(Duration::from_secs(1));
            loop {
                interval.tick().await;
                let mut expired_resources = Vec::new();
                {
                    let locks = registry.locks.lock().await;
                    let now = Instant::now();
                    for (resource, lock) in locks.iter() {
                        if now >= lock.expires_at {
                            expired_resources.push((resource.clone(), lock.lock_id.clone()));
                        }
                    }
                }

                for (resource, lock_id) in expired_resources {
                    warn!(resource = %resource, lock_id = %lock_id, "Mutex expired, auto-releasing");
                    registry.release(resource, lock_id).await;
                }
            }
        });
    }
}

impl Default for MutexRegistry {
    fn default() -> Self {
        Self::new()
    }
}
