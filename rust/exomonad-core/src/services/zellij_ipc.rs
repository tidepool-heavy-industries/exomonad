//! Direct Zellij IPC via Unix Domain Socket.
//!
//! Replaces `Command::new("zellij")` subprocess calls with direct socket writes
//! using the `zellij-utils` IPC types. Zero subprocess overhead, type-safe,
//! no session targeting env vars needed (socket path includes session name).
//!
//! ```text
//! Before: MCP server → fork/exec 19MB zellij binary → IPC socket → daemon
//! After:  MCP server → UnixStream::connect(socket_path) → rmp_serde → daemon
//! ```

use anyhow::{Context, Result};
use interprocess::local_socket::LocalSocketStream;
use std::path::PathBuf;
use tracing::{debug, info, warn};
use zellij_utils::consts::ZELLIJ_SOCK_DIR;
use zellij_utils::input::actions::Action;
use zellij_utils::input::command::RunCommandAction;
use zellij_utils::input::layout::Layout;
use zellij_utils::ipc::{ClientToServerMsg, IpcSenderWithContext, ServerToClientMsg};

/// Direct IPC client for a specific Zellij session.
///
/// Each instance targets one session (socket path is fixed at construction).
/// Methods open a short-lived connection per call — no persistent state across
/// async boundaries.
#[derive(Debug, Clone)]
pub struct ZellijIpc {
    session_name: String,
    socket_path: PathBuf,
}

impl ZellijIpc {
    /// Create a new IPC client for the given Zellij session.
    ///
    /// Resolves the socket path from `ZELLIJ_SOCK_DIR/{session_name}`.
    /// Does NOT verify the socket exists — that happens on first send.
    pub fn new(session_name: &str) -> Self {
        let socket_path = ZELLIJ_SOCK_DIR.join(session_name);
        info!(
            session = %session_name,
            socket = %socket_path.display(),
            "[ZellijIpc] Created client"
        );
        Self {
            session_name: session_name.to_string(),
            socket_path,
        }
    }

    pub fn session_name(&self) -> &str {
        &self.session_name
    }

    /// Send an Action to the zellij daemon.
    ///
    /// Opens a connection, sends, closes. Short-lived connections avoid
    /// holding state across async boundaries.
    fn send_action(&self, action: Action) -> Result<()> {
        debug!(
            session = %self.session_name,
            "[ZellijIpc] Sending action"
        );

        let stream = LocalSocketStream::connect(self.socket_path.as_path()).with_context(|| {
            format!(
                "Failed to connect to Zellij socket at {}",
                self.socket_path.display()
            )
        })?;

        // ClientToServerMsg::Action(Action, Option<u32>, Option<ClientId>)
        let msg = ClientToServerMsg::Action(action, None, None);
        let mut sender = IpcSenderWithContext::new(stream);
        sender
            .send(msg)
            .context("Failed to send action to Zellij daemon")?;

        Ok(())
    }

    /// Send an action and read responses until `UnblockInputThread` or `Exit`.
    fn send_action_with_response(&self, action: Action) -> Result<Vec<ServerToClientMsg>> {
        let stream = LocalSocketStream::connect(self.socket_path.as_path()).with_context(|| {
            format!(
                "Failed to connect to Zellij socket at {}",
                self.socket_path.display()
            )
        })?;

        let msg = ClientToServerMsg::Action(action, None, None);
        let mut sender = IpcSenderWithContext::new(stream);
        sender
            .send(msg)
            .context("Failed to send action to Zellij daemon")?;

        let mut receiver = sender.get_receiver::<ServerToClientMsg>();
        let mut responses = Vec::new();

        loop {
            match receiver.recv() {
                Some((ServerToClientMsg::UnblockInputThread, _ctx)) => break,
                Some((ServerToClientMsg::Exit(_), _ctx)) => break,
                Some((msg, _ctx)) => responses.push(msg),
                None => {
                    warn!("[ZellijIpc] Connection closed while reading response");
                    break;
                }
            }
        }

        Ok(responses)
    }

    /// Send a pipe message to a plugin.
    ///
    /// Replaces `zellij pipe --plugin <plugin> --name <name> -- <payload>`.
    pub fn pipe_to_plugin(&self, plugin: &str, name: &str, payload: &str) -> Result<()> {
        debug!(
            plugin = %plugin,
            name = %name,
            payload_len = payload.len(),
            "[ZellijIpc] pipe_to_plugin"
        );

        self.send_action(Action::CliPipe {
            pipe_id: uuid::Uuid::new_v4().to_string(),
            name: Some(name.to_string()),
            payload: Some(payload.to_string()),
            plugin: Some(plugin.to_string()),
            args: None,
            configuration: None,
            launch_new: false,
            skip_cache: false,
            floating: None,
            in_place: None,
            cwd: None,
            pane_title: None,
        })
    }

    /// Create a new tab from a KDL layout string.
    ///
    /// Parses the KDL into zellij's internal layout types, then sends
    /// `NewTab` action directly — no temp file needed.
    pub fn new_tab_with_layout(&self, layout_kdl: &str, tab_name: Option<&str>) -> Result<()> {
        debug!(
            kdl_len = layout_kdl.len(),
            "[ZellijIpc] new_tab_with_layout"
        );

        let layout = Layout::from_kdl(layout_kdl, None, None, None)
            .map_err(|e| anyhow::anyhow!("Failed to parse KDL layout: {:?}", e))?;

        // If layout has explicit tabs, send one NewTab per tab.
        // Otherwise, use the template.
        let tabs = layout.tabs();
        if !tabs.is_empty() {
            let swap_tiled = Some(layout.swap_tiled_layouts.clone());
            let swap_floating = Some(layout.swap_floating_layouts.clone());
            for (i, (name, tiled, floating)) in tabs.into_iter().enumerate() {
                let name = name.or_else(|| tab_name.map(String::from));
                self.send_action(Action::NewTab(
                    Some(tiled),
                    floating,
                    swap_tiled.clone(),
                    swap_floating.clone(),
                    name,
                    i == 0, // focus first tab
                    None,
                ))?;
            }
        } else {
            let swap_tiled = Some(layout.swap_tiled_layouts.clone());
            let swap_floating = Some(layout.swap_floating_layouts.clone());
            let (tiled, floating) = layout.new_tab();
            self.send_action(Action::NewTab(
                Some(tiled),
                floating,
                swap_tiled,
                swap_floating,
                tab_name.map(String::from),
                true,
                None,
            ))?;
        }

        Ok(())
    }

    /// Create a new pane in the current tab.
    ///
    /// Replaces `zellij action new-pane --name <name> --cwd <cwd> -- <shell> -l -c <cmd>`.
    pub fn new_pane(
        &self,
        name: &str,
        cwd: &std::path::Path,
        shell: &str,
        command: &str,
    ) -> Result<()> {
        debug!(
            name = %name,
            cwd = %cwd.display(),
            "[ZellijIpc] new_pane"
        );

        // NewTiledPane(Option<Direction>, Option<RunCommandAction>, Option<String>)
        let run_command = RunCommandAction {
            command: PathBuf::from(shell),
            args: vec!["-l".to_string(), "-c".to_string(), command.to_string()],
            cwd: Some(cwd.to_path_buf()),
            direction: None,
            hold_on_close: true,
            hold_on_start: false,
            originating_plugin: None,
            use_terminal_title: false,
        };

        self.send_action(Action::NewTiledPane(
            None,
            Some(run_command),
            Some(name.to_string()),
        ))
    }

    /// Switch to a tab by name.
    ///
    /// Replaces `zellij action go-to-tab-name <name>`.
    pub fn go_to_tab_name(&self, name: &str) -> Result<()> {
        debug!(name = %name, "[ZellijIpc] go_to_tab_name");
        // GoToTabName(String, bool) — bool is create_if_not_exists
        self.send_action(Action::GoToTabName(name.to_string(), false))
    }

    /// Rename the currently focused pane.
    ///
    /// Replaces `zellij action rename-pane <name>`.
    /// Zellij CLI sends UndoRenamePane first, then PaneNameInput(bytes).
    pub fn rename_pane(&self, name: &str) -> Result<()> {
        debug!(name = %name, "[ZellijIpc] rename_pane");
        self.send_action(Action::UndoRenamePane)?;
        self.send_action(Action::PaneNameInput(name.as_bytes().to_vec()))
    }

    /// Query tab names from the Zellij session.
    ///
    /// Sends `QueryTabNames` and reads `Log(Vec<String>)` responses.
    pub fn query_tab_names(&self) -> Result<Vec<String>> {
        debug!("[ZellijIpc] query_tab_names");

        let responses = self.send_action_with_response(Action::QueryTabNames)?;

        let mut tab_names = Vec::new();
        for msg in responses {
            if let ServerToClientMsg::Log(lines) = msg {
                tab_names.extend(lines);
            }
        }

        debug!(
            count = tab_names.len(),
            "[ZellijIpc] query_tab_names result"
        );
        Ok(tab_names)
    }

    /// Close a tab by name.
    ///
    /// Switches to the tab first, then closes the current tab.
    /// Replaces `zellij action close-tab --tab-name <name>`.
    pub fn close_tab(&self, name: &str) -> Result<()> {
        debug!(name = %name, "[ZellijIpc] close_tab");
        self.go_to_tab_name(name)?;
        self.send_action(Action::CloseTab)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_socket_path_resolution() {
        let ipc = ZellijIpc::new("test-session");
        assert!(ipc.socket_path.to_string_lossy().contains("test-session"));
    }

    #[test]
    fn test_session_name() {
        let ipc = ZellijIpc::new("my-session");
        assert_eq!(ipc.session_name(), "my-session");
    }
}
