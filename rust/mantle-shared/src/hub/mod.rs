//! Hub communication module.
//!
//! This module provides the client and types for communicating with mantle-hub,
//! the session visualization and coordination daemon.

pub mod client;
pub mod config;
pub mod socket_client;
pub mod types;

pub use client::HubClient;
pub use config::HubConfig;
pub use socket_client::{run_result_to_session_result, write_result_to_socket};
pub use types::{
    GraphData, GraphEdge, GraphNode, HubEvent, ModelUsage, SessionInfo, SessionRegister,
    SessionResult, SessionState,
};
