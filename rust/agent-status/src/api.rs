use anyhow::Result;
use reqwest::Client;
use crate::state::AgentStatus;
use serde::Deserialize;

#[derive(Deserialize)]
struct AgentsResponse {
    agents: Vec<AgentStatus>,
}

pub struct ApiClient {
    client: Client,
    base_url: String,
}

impl ApiClient {
    pub fn new(base_url: String) -> Self {
        Self {
            client: Client::new(),
            base_url,
        }
    }

    pub async fn get_agents(&self) -> Result<Vec<AgentStatus>> {
        let url = format!("{}/api/agents", self.base_url);
        let resp = self.client.get(&url).send().await?.json::<AgentsResponse>().await?;
        Ok(resp.agents)
    }

    pub async fn get_logs(&self, agent_id: &str) -> Result<String> {
        let url = format!("{}/api/agents/{}/logs", self.base_url, agent_id);
        let resp = self.client.get(&url).send().await?.text().await?;
        Ok(resp)
    }

    pub async fn stop_agent(&self, agent_id: &str) -> Result<()> {
        let url = format!("{}/api/agents/{}/stop", self.base_url, agent_id);
        let _ = self.client.post(&url).send().await?.error_for_status()?;
        Ok(())
    }
}