//! `teams-mcp` — a standalone stdio MCP server: a Swiss-army-knife for Claude
//! Teams.
//!
//! Spawned by a Claude Code session (as a child process), it self-identifies its
//! active team via the [`exo_scry`] library — no registration, resolved live on
//! every call — and exposes read-only Teams introspection as MCP tools. It
//! depends on nothing from exomonad: identity comes from `exo-scry` and it reads
//! the `~/.claude` substrate directly.

use exo_scry::identity::ActiveTeam;
use exo_scry::ScryError;
use rmcp::handler::server::router::tool::ToolRouter;
use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::{CallToolResult, Content, ServerCapabilities, ServerInfo};
use rmcp::transport::stdio;
use rmcp::{
    schemars, tool, tool_handler, tool_router, ErrorData as McpError, ServerHandler, ServiceExt,
};
use serde::Deserialize;

/// The Teams MCP server. Stateless: identity is resolved per call, never cached
/// (the process may have started before any team existed).
#[derive(Clone)]
pub struct TeamsServer {
    tool_router: ToolRouter<Self>,
}

impl TeamsServer {
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }
}

impl Default for TeamsServer {
    fn default() -> Self {
        Self::new()
    }
}

fn to_mcp(e: ScryError) -> McpError {
    McpError::internal_error(e.to_string(), None)
}

/// Resolve this sidecar's active team, or `None` if its session isn't in one.
///
/// tmux pane first: the sidecar inherits `$TMUX_PANE` from its Claude parent, and
/// matching it against members' `tmuxPaneId` is durable (survives session-id
/// churn) and unambiguous (unique per session, so it disambiguates multiple
/// sessions in one cwd). This resolves *teammates*. The human lead isn't
/// pane-indexed, so fall back to the live watch for it.
fn active() -> Result<Option<ActiveTeam>, McpError> {
    if let Ok(pane) = std::env::var("TMUX_PANE") {
        if let Some(team) = exo_scry::resolve_by_pane(&pane).map_err(to_mcp)? {
            return Ok(Some(team));
        }
    }
    exo_scry::resolve_self().map_err(to_mcp)
}

fn not_in_team() -> CallToolResult {
    CallToolResult::success(vec![Content::text(
        "This session is not currently in a team.",
    )])
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
struct SendMessageArgs {
    /// Teammate to send to, by member name.
    to: String,
    /// The message body.
    text: String,
    /// Optional short preview/summary.
    #[serde(default)]
    summary: Option<String>,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
struct ReadInboxArgs {
    /// Inbox to read, by member name. Defaults to the team lead's inbox.
    #[serde(default)]
    member: Option<String>,
    /// Return only unread messages.
    #[serde(default)]
    unread_only: Option<bool>,
}

#[tool_router(router = tool_router)]
impl TeamsServer {
    /// Resolve the caller's active team from live OS state.
    #[tool(
        description = "Resolve this session's active Claude Teams team from live OS state (zero registration): team name, lead inbox, resolved session id, and member count."
    )]
    async fn team_status(&self) -> Result<CallToolResult, McpError> {
        let Some(t) = active()? else {
            return Ok(not_in_team());
        };
        let members = exo_scry::teams::load_team(&t.team.0)
            .map(|tm| tm.members.len())
            .unwrap_or(0);
        let me = t.me.as_ref().map(|m| {
            serde_json::json!({"name": m.name, "agent_type": m.agent_type, "pane": m.tmux_pane_id})
        });
        let body = serde_json::json!({
            "team": t.team.0,
            "me": me,
            "role": if t.me.is_some() { "member" } else { "lead" },
            "lead_inbox": t.lead_inbox,
            "lead_session_id": t.lead_session_id,
            "claude_pid": t.claude_pid.map(|p| p.0),
            "tasks_dir": t.tasks_dir,
            "member_count": members,
        });
        Ok(CallToolResult::success(vec![Content::json(body)?]))
    }

    /// Enumerate teammates with a best-effort liveness flag.
    #[tool(
        description = "List teammates in this session's team: name, type, model, tmux pane, `live` (ground truth — pane exists and runs a Claude process; null for the human lead), and `is_active` (Claude Code's own flag, which can go stale after an unclean exit)."
    )]
    async fn list_teammates(&self) -> Result<CallToolResult, McpError> {
        let Some(t) = active()? else {
            return Ok(not_in_team());
        };
        let team = exo_scry::teams::load_team(&t.team.0).map_err(to_mcp)?;
        let members: Vec<_> = team
            .members
            .iter()
            .map(|m| {
                // Ground-truth liveness: pane exists AND runs a Claude process.
                // Only meaningful for tmux-backed members (the human lead has no
                // pane). `is_active` is CC's own flag, reported raw — it can be a
                // stale `true` after an unclean exit, so `live` is authoritative.
                let live = (!m.tmux_pane_id.is_empty())
                    .then(|| exo_scry::proc::pane_has_live_claude(&m.tmux_pane_id));
                serde_json::json!({
                    "name": m.name,
                    "agent_type": m.agent_type,
                    "model": m.model,
                    "pane": m.tmux_pane_id,
                    "backend": m.backend_type,
                    "live": live,
                    "is_active": m.is_active,
                })
            })
            .collect();
        Ok(CallToolResult::success(vec![Content::json(members)?]))
    }

    /// Send a message to a teammate's inbox.
    #[tool(
        description = "Send a message to a teammate in this session's team. It lands in their inbox and Claude Code delivers it to them as a <teammate-message>. `from` is set automatically to your own identity (your member name, or the lead)."
    )]
    async fn send_message(
        &self,
        Parameters(args): Parameters<SendMessageArgs>,
    ) -> Result<CallToolResult, McpError> {
        let Some(t) = active()? else {
            return Ok(not_in_team());
        };
        // Validate the recipient is a real member — don't create orphan inboxes.
        let team = exo_scry::teams::load_team(&t.team.0).map_err(to_mcp)?;
        if team.member(&args.to).is_none() {
            let names: Vec<&str> = team.members.iter().map(|m| m.name.as_str()).collect();
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "No teammate named {:?} in team {:?}. Members: {:?}",
                args.to, t.team.0, names
            ))]));
        }
        // `from` = my own identity: member name if pane-resolved, else the lead.
        let from = t
            .me
            .as_ref()
            .map(|m| m.name.clone())
            .or_else(|| t.lead_inbox.clone())
            .unwrap_or_else(|| "unknown".to_string());
        let summary = args.summary.unwrap_or_default();
        let msg = exo_scry::inbox::send_message(&t.team.0, &args.to, &from, &args.text, &summary)
            .map_err(to_mcp)?;
        let body = serde_json::json!({
            "delivered_to": args.to,
            "from": from,
            "timestamp": msg.timestamp,
        });
        Ok(CallToolResult::success(vec![Content::json(body)?]))
    }

    /// Read messages from an inbox in the caller's team.
    #[tool(
        description = "Read messages from an inbox in this session's team. Defaults to the team lead's inbox; pass `member` to read another's. Set `unread_only` to filter."
    )]
    async fn read_inbox(
        &self,
        Parameters(args): Parameters<ReadInboxArgs>,
    ) -> Result<CallToolResult, McpError> {
        let Some(t) = active()? else {
            return Ok(not_in_team());
        };
        let Some(member) = args.member.or_else(|| t.lead_inbox.clone()) else {
            return Ok(CallToolResult::success(vec![Content::text(
                "No inbox to read: the team has no resolvable lead.",
            )]));
        };
        let mut msgs = exo_scry::inbox::read_inbox(&t.team.0, &member).map_err(to_mcp)?;
        if args.unread_only.unwrap_or(false) {
            msgs.retain(|m| !m.read);
        }
        let body = serde_json::json!({
            "inbox": member,
            "count": msgs.len(),
            "messages": msgs,
        });
        Ok(CallToolResult::success(vec![Content::json(body)?]))
    }
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for TeamsServer {
    fn get_info(&self) -> ServerInfo {
        let mut info = ServerInfo::new(ServerCapabilities::builder().enable_tools().build());
        info.server_info.name = "teams-mcp".into();
        info.server_info.version = env!("CARGO_PKG_VERSION").into();
        info.instructions = Some(
            "Swiss-army-knife for Claude Teams. Self-identifies this session's active \
             team from live OS state (no registration). Tools: team_status, \
             list_teammates, read_inbox, send_message."
                .into(),
        );
        info
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let server = TeamsServer::new().serve(stdio()).await?;
    server.waiting().await?;
    Ok(())
}
