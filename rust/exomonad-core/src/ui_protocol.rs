//! UI protocol and types for ExoMonad TUI integration.
//!
//! This crate defines the communication protocol between the ExoMonad host/plugin
//! and the TUI sidebar/coordinator. It includes message types for:
//!
//! - **Telemetry**: Real-time progress and logs
//! - **Control**: Commands for agent lifecycle management

use crate::domain::PRNumber;
use serde::{Deserialize, Serialize};
use std::fmt;

// ============================================================================
// Agent Identifiers
// ============================================================================

/// Agent identifier.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct AgentId(String);

impl TryFrom<String> for AgentId {
    type Error = String;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        if s.is_empty() {
            return Err("Agent ID cannot be empty".to_string());
        }
        Ok(Self(s))
    }
}

impl From<AgentId> for String {
    fn from(id: AgentId) -> String {
        id.0
    }
}

impl fmt::Display for AgentId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Events broadcast by agent sidecars.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type")]
pub enum AgentEvent {
    #[serde(rename = "agent:started")]
    AgentStarted {
        agent_id: AgentId,
        timestamp: String,
    },
    #[serde(rename = "agent:stopped")]
    AgentStopped {
        agent_id: AgentId,
        timestamp: String,
    },
    #[serde(rename = "stop_hook:blocked")]
    StopHookBlocked {
        agent_id: AgentId,
        reason: String,
        timestamp: String,
    },
    #[serde(rename = "hook:received")]
    HookReceived {
        agent_id: AgentId,
        hook_type: String,
        timestamp: String,
    },
    #[serde(rename = "pr:filed")]
    PrFiled {
        agent_id: AgentId,
        pr_number: PRNumber,
        timestamp: String,
    },
    #[serde(rename = "copilot:reviewed")]
    CopilotReviewed {
        agent_id: AgentId,
        comment_count: u32,
        timestamp: String,
    },
    #[serde(rename = "agent:stuck")]
    AgentStuck {
        agent_id: AgentId,
        failed_stop_count: u32,
        timestamp: String,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_id_valid() {
        let id: Result<AgentId, _> = "agent-123".to_string().try_into();
        assert!(id.is_ok());
        assert_eq!(id.unwrap().to_string(), "agent-123");
    }

    #[test]
    fn test_agent_id_empty_rejected() {
        let id: Result<AgentId, _> = "".to_string().try_into();
        assert!(id.is_err());
        assert!(id.unwrap_err().contains("empty"));
    }

    #[test]
    fn test_agent_id_serialization_roundtrip() {
        let id: AgentId = "test-agent".to_string().try_into().unwrap();
        let json = serde_json::to_string(&id).unwrap();
        assert_eq!(json, "\"test-agent\"");

        let deserialized: AgentId = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.to_string(), "test-agent");
    }

    #[test]
    fn test_agent_event_serialization() {
        let event = AgentEvent::AgentStarted {
            agent_id: "agent-1".to_string().try_into().unwrap(),
            timestamp: "2024-01-01T00:00:00Z".to_string(),
        };
        let json = serde_json::to_string(&event).unwrap();
        assert!(json.contains("\"type\":\"agent:started\""));
        assert!(json.contains("\"agent_id\":\"agent-1\""));
    }
}
