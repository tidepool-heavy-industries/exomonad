//! Hub communication module.
//!
//! This module provides the client and types for communicating with mantle-hub,
//! the session visualization and coordination daemon.
//!
//! ## Entity Model
//!
//! - **Session**: An orchestration run (tree of nodes)
//! - **Node**: An individual Claude Code execution within a session

pub mod client;
pub mod config;
pub mod socket_client;
pub mod types;

pub use client::{HubClient, SyncEventStream};
pub use config::HubConfig;
pub use socket_client::{run_result_to_node_result, write_result_to_socket};
pub use types::{
    GraphData, GraphEdge, GraphNode, HubEvent, ModelUsage, NodeCreateResponse, NodeEvent,
    NodeInfo, NodeRegister, NodeResult, NodeState, SessionCreateResponse, SessionInfo,
    SessionRegister, SessionState, SessionWithNodes,
};
