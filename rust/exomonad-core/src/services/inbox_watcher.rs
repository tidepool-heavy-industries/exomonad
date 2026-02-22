//! Claude Teams inbox watcher for synthetic members.
//!
//! Polls inbox files for Gemini workers registered as synthetic team members.
//! When new messages appear, injects their text into the Gemini agent's
//! Zellij pane via `inject_input`.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;
use tracing::{info, warn};

use super::teams_mailbox::{inbox_path, TeamsMessage};
use super::zellij_events::inject_input;

/// Per-member watch state.
struct WatchState {
    last_count: usize,
    zellij_tab_name: String,
    is_running: Arc<AtomicBool>,
}

/// Watches synthetic member inbox files and routes messages to Zellij panes.
pub struct InboxWatcher {
    watches: Arc<Mutex<HashMap<String, WatchState>>>,
}

impl Default for InboxWatcher {
    fn default() -> Self {
        Self::new()
    }
}

impl InboxWatcher {
    pub fn new() -> Self {
        Self {
            watches: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Start watching a member's inbox. Spawns a tokio task polling every 500ms.
    pub async fn watch_inbox(
        &self,
        team_name: String,
        member_name: String,
        zellij_tab_name: String,
    ) {
        let mut watches = self.watches.lock().await;
        if watches.contains_key(&member_name) {
            return;
        }

        let is_running = Arc::new(AtomicBool::new(true));

        // Snapshot current message count to avoid re-injecting old messages.
        let last_count = inbox_path(&team_name, &member_name)
            .filter(|p| p.exists())
            .and_then(|p| std::fs::read_to_string(&p).ok())
            .and_then(|c| serde_json::from_str::<Vec<TeamsMessage>>(&c).ok())
            .map(|msgs| msgs.len())
            .unwrap_or(0);

        watches.insert(
            member_name.clone(),
            WatchState {
                last_count,
                zellij_tab_name: zellij_tab_name.clone(),
                is_running: is_running.clone(),
            },
        );

        let watches_ref = self.watches.clone();
        let member = member_name.clone();
        let team = team_name.clone();

        tokio::spawn(async move {
            info!(
                member = %member,
                team = %team,
                tab = %zellij_tab_name,
                "InboxWatcher: starting poll loop"
            );

            while is_running.load(Ordering::Relaxed) {
                tokio::time::sleep(Duration::from_millis(500)).await;

                if !is_running.load(Ordering::Relaxed) {
                    break;
                }

                let path = match inbox_path(&team, &member) {
                    Some(p) if p.exists() => p,
                    _ => continue,
                };

                let content = match std::fs::read_to_string(&path) {
                    Ok(c) => c,
                    Err(e) => {
                        warn!(member = %member, error = %e, "InboxWatcher: failed to read inbox");
                        continue;
                    }
                };

                let msgs: Vec<TeamsMessage> = match serde_json::from_str(&content) {
                    Ok(m) => m,
                    Err(e) => {
                        warn!(member = %member, error = %e, "InboxWatcher: failed to parse inbox");
                        continue;
                    }
                };

                let mut watches = watches_ref.lock().await;
                if let Some(state) = watches.get_mut(&member) {
                    if msgs.len() > state.last_count {
                        let new_count = msgs.len() - state.last_count;
                        info!(member = %member, new_messages = new_count, "InboxWatcher: routing to Zellij");
                        for msg in &msgs[state.last_count..] {
                            inject_input(&state.zellij_tab_name, &msg.text);
                        }
                        state.last_count = msgs.len();
                    }
                } else {
                    break;
                }
            }

            info!(member = %member, "InboxWatcher: stopped");
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
