//! Read/write messages through Claude Code's Teams filesystem.
//!
//! This crate provides typed access to Claude Code's on-disk mailbox format —
//! the `~/.claude/teams/` directory that Claude Code's InboxPoller watches.

mod config;
mod discovery;
mod inbox;
mod paths;
mod registry;
mod verifier;

pub use config::{read_team_config, write_team_config, TeamConfig, TeamMember};
pub use discovery::{list_inboxes, list_teams};
pub use inbox::{is_message_read, read_inbox, unread_messages, write_to_inbox, TeamsMessage};
pub use paths::{config_path, inbox_path, teams_base_dir};
pub use registry::{TeamInfo, TeamRegistry};
pub use verifier::wait_for_read;
