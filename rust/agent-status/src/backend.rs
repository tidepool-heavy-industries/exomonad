use crate::api::ApiClient;
use crate::app::UserEvent;
use crate::state::DashboardState;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use tuirealm::Event;
use tuirealm::listener::{ListenerResult, Poll};

pub struct BackendPort {
    client: Arc<ApiClient>,
    state: Arc<RwLock<DashboardState>>,
}

impl BackendPort {
    pub fn new(client: Arc<ApiClient>, state: Arc<RwLock<DashboardState>>) -> Self {
        let client_clone = client.clone();
        let state_clone = state.clone();

        // Spawn the polling task
        tokio::spawn(async move {
            loop {
                // 1. Fetch Agents
                if let Ok(agents) = client_clone.get_agents().await {
                    let mut s = state_clone.write().unwrap();
                    s.agents = agents;
                    s.connected = true;
                    s.last_updated = Some(chrono::Local::now().to_rfc3339());
                } else {
                    let mut s = state_clone.write().unwrap();
                    s.connected = false;
                }

                // 2. Fetch Logs for selected
                let (selected_id, needs_logs) = {
                    let s = state_clone.read().unwrap();
                    if s.agents.is_empty() {
                        (None, false)
                    } else {
                        let id = s.agents[s.selected_index].id.clone();
                        (Some(id), true)
                    }
                };

                if let Some(id) = selected_id {
                    if let Ok(logs) = client_clone.get_logs(&id).await {
                        let mut s = state_clone.write().unwrap();
                        s.logs_cache.insert(id, logs);
                    }
                }

                tokio::time::sleep(Duration::from_secs(2)).await;
            }
        });

        Self { client, state }
    }
}

impl Poll<UserEvent> for BackendPort {
    fn poll(&mut self) -> ListenerResult<Option<Event<UserEvent>>> {
        // We just return a Tick to tell the UI to redraw if needed
        // Since we update state directly via Arc<RwLock>, we just need a redraw trigger.
        Ok(Some(Event::User(UserEvent::Tick)))
    }
}
