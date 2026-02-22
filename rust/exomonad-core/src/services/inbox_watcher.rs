use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;
use tracing::{info, warn};
use super::teams_mailbox::{TeamsMessage, inbox_path};
use super::zellij_events::inject_input;

/// State for a single tracked inbox.
pub struct WatchState {
    pub last_count: usize,
    pub zellij_tab_name: String,
    pub is_running: Arc<AtomicBool>,
}

/// Service that watches synthetic member inbox files and routes messages to Zellij panes.
pub struct InboxWatcher {
    watches: Arc<Mutex<HashMap<String, WatchState>>>,
}

impl InboxWatcher {
    pub fn new() -> Self {
        Self {
            watches: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Start watching a specific member's inbox.
    ///
    /// Spawns a tokio task that polls the inbox file every 500ms.
    pub async fn watch_inbox(&self, team_name: String, member_name: String, zellij_tab_name: String) {
        let mut watches = self.watches.lock().await;
        if watches.contains_key(&member_name) {
            return;
        }

        let is_running = Arc::new(AtomicBool::new(true));
        
        // Initial count to avoid re-injecting old messages if the file already exists
        let last_count = if let Some(path) = inbox_path(&team_name, &member_name) {
            if path.exists() {
                std::fs::read_to_string(&path)
                    .ok()
                    .and_then(|c| serde_json::from_str::<Vec<TeamsMessage>>(&c).ok())
                    .map(|msgs| msgs.len())
                    .unwrap_or(0)
            } else {
                0
            }
        } else {
            0
        };

        watches.insert(member_name.clone(), WatchState {
            last_count,
            zellij_tab_name: zellij_tab_name.clone(),
            is_running: is_running.clone(),
        });

        let watches_clone = self.watches.clone();
        let member_name_clone = member_name.clone();
        let team_name_clone = team_name.clone();

        tokio::spawn(async move {
            info!(
                "[InboxWatcher] Starting watch for {} (team: {}) -> tab: {}",
                member_name_clone, team_name_clone, zellij_tab_name
            );

            while is_running.load(Ordering::Relaxed) {
                tokio::time::sleep(Duration::from_millis(500)).await;

                if !is_running.load(Ordering::Relaxed) {
                    break;
                }

                if let Some(path) = inbox_path(&team_name_clone, &member_name_clone) {
                    if !path.exists() {
                        continue;
                    }

                    let content = match std::fs::read_to_string(&path) {
                        Ok(c) => c,
                        Err(e) => {
                            warn!("[InboxWatcher] Failed to read inbox {}: {}", path.display(), e);
                            continue;
                        }
                    };

                    let msgs: Vec<TeamsMessage> = match serde_json::from_str(&content) {
                        Ok(m) => m,
                        Err(e) => {
                            warn!("[InboxWatcher] Failed to parse inbox {}: {}", path.display(), e);
                            continue;
                        }
                    };

                    let mut watches = watches_clone.lock().await;
                    if let Some(state) = watches.get_mut(&member_name_clone) {
                        if msgs.len() > state.last_count {
                            info!(
                                "[InboxWatcher] Detected {} new messages for {}",
                                msgs.len() - state.last_count,
                                member_name_clone
                            );
                            for msg in &msgs[state.last_count..] {
                                inject_input(&state.zellij_tab_name, &msg.text);
                            }
                            state.last_count = msgs.len();
                        }
                    } else {
                        // Watch state removed from map, stop polling
                        break;
                    }
                }
            }
            info!("[InboxWatcher] Stopped watch for {}", member_name_clone);
        });
    }

    /// Stop watching a member's inbox.
    pub async fn stop_watching(&self, member_name: &str) {
        let mut watches = self.watches.lock().await;
        if let Some(state) = watches.remove(member_name) {
            state.is_running.store(false, Ordering::Relaxed);
        }
    }
}

impl Default for InboxWatcher {
    fn default() -> Self {
        Self::new()
    }
}
