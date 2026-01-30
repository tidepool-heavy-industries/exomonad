use crate::api::ApiClient;
use crate::app::UserEvent;
use crate::state::DashboardState;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use tokio_util::sync::CancellationToken;
use tuirealm::Event;
use tuirealm::listener::{ListenerResult, Poll};

pub struct BackendPort {
    #[allow(dead_code)]
    client: Arc<ApiClient>,
    #[allow(dead_code)]
    state: Arc<RwLock<DashboardState>>,
    shutdown: CancellationToken,
}

impl BackendPort {
    pub fn new(client: Arc<ApiClient>, state: Arc<RwLock<DashboardState>>) -> Self {
        let client_clone = client.clone();
        let state_clone = state.clone();
        let shutdown = CancellationToken::new();
        let shutdown_clone = shutdown.clone();

        // Spawn the polling task
        tokio::spawn(async move {
            loop {
                tokio::select! {
                    _ = shutdown_clone.cancelled() => {
                        return;
                    }
                    _ = tokio::time::sleep(Duration::from_secs(2)) => {}
                }

                // 1. Fetch Agents
                if let Ok(agents) = client_clone.get_agents().await {
                    if let Ok(mut s) = state_clone.write() {
                        s.agents = agents;
                        s.connected = true;
                        s.last_updated = Some(chrono::Local::now().to_rfc3339());
                    }
                } else {
                    if let Ok(mut s) = state_clone.write() {
                        s.connected = false;
                    }
                }

                // 2. Fetch Logs for selected
                let selected_id = {
                    if let Ok(s) = state_clone.read() {
                        if s.agents.is_empty() {
                            None
                        } else {
                            Some(s.agents[s.selected_index].id.clone())
                        }
                    } else {
                        None
                    }
                };

                if let Some(id) = selected_id {
                    if let Ok(logs) = client_clone.get_logs(&id).await {
                        if let Ok(mut s) = state_clone.write() {
                            s.logs_cache.insert(id, logs);
                        }
                    }
                }
            }
        });

        Self {
            client,
            state,
            shutdown,
        }
    }
}

impl Drop for BackendPort {
    fn drop(&mut self) {
        self.shutdown.cancel();
    }
}

impl Poll<UserEvent> for BackendPort {
    fn poll(&mut self) -> ListenerResult<Option<Event<UserEvent>>> {
        // We just return a Tick to tell the UI to redraw if needed
        // Since we update state directly via Arc<RwLock>, we just need a redraw trigger.
        Ok(Some(Event::User(UserEvent::Tick)))
    }
}
