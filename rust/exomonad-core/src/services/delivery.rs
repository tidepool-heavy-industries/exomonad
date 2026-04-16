use crate::domain::Address;
use crate::services::tmux_events;
use agent_client_protocol::{Agent, PromptRequest};
use claude_teams_bridge as teams_mailbox;
use claude_teams_bridge::TeamRegistry;
use exomonad_proto::effects::events::{event, AgentMessage, Event};
use tracing::{debug, info, instrument, warn};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeliveryResult {
    Teams,
    Acp,
    Uds,
    Tmux,
    StaleRouting,
    Failed,
}

impl DeliveryResult {
    pub fn is_failure(self) -> bool {
        matches!(self, DeliveryResult::StaleRouting | DeliveryResult::Failed)
    }
}

/// Notification status for parent-facing messages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NotifyStatus {
    Success,
    Failure,
}

impl NotifyStatus {
    /// Parse from proto/wire string ("failure" → Failure, anything else → Success).
    pub fn parse(s: &str) -> Self {
        match s {
            "failure" => NotifyStatus::Failure,
            _ => NotifyStatus::Success,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            NotifyStatus::Success => "success",
            NotifyStatus::Failure => "failure",
        }
    }
}

impl std::fmt::Display for NotifyStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Format a parent-facing notification message.
/// Failure → `[FAILED: {id}] {msg}`, otherwise → `[from: {id}] {msg}`.
pub fn format_parent_notification(
    agent_id: &crate::domain::AgentName,
    status: NotifyStatus,
    message: &str,
) -> String {
    let default_msg = match status {
        NotifyStatus::Failure => "Task failed.",
        NotifyStatus::Success => "Status update.",
    };
    let msg = if message.is_empty() {
        default_msg
    } else {
        message
    };
    match status {
        NotifyStatus::Failure => format!("[FAILED: {}] {}", agent_id, msg),
        NotifyStatus::Success => format!("[from: {}] {}", agent_id, msg),
    }
}

/// Delivery method used for message routing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeliveryMethod {
    TeamsInbox,
    Acp,
    Uds,
    Tmux,
}

/// Outcome of a routed message delivery.
#[derive(Debug)]
pub enum DeliveryOutcome {
    /// Successfully delivered to the resolved recipient.
    Delivered {
        method: DeliveryMethod,
        recipient: crate::domain::AgentName,
    },
    /// Original target could not be resolved; fell back to team lead.
    FallbackToLead {
        method: DeliveryMethod,
        original: String,
        lead: crate::domain::AgentName,
    },
    /// Delivery failed entirely.
    Failed { original: String, reason: String },
}

impl DeliveryOutcome {
    fn from_result(result: DeliveryResult, recipient: &str) -> Self {
        let agent = crate::domain::AgentName::from(recipient);
        match result {
            DeliveryResult::Failed => DeliveryOutcome::Failed {
                original: recipient.to_string(),
                reason: "all delivery methods failed".to_string(),
            },
            DeliveryResult::StaleRouting => DeliveryOutcome::Failed {
                original: recipient.to_string(),
                reason: "stale routing.json (dead pane/window)".to_string(),
            },
            DeliveryResult::Teams => DeliveryOutcome::Delivered {
                method: DeliveryMethod::TeamsInbox,
                recipient: agent,
            },
            DeliveryResult::Acp => DeliveryOutcome::Delivered {
                method: DeliveryMethod::Acp,
                recipient: agent,
            },
            DeliveryResult::Uds => DeliveryOutcome::Delivered {
                method: DeliveryMethod::Uds,
                recipient: agent,
            },
            DeliveryResult::Tmux => DeliveryOutcome::Delivered {
                method: DeliveryMethod::Tmux,
                recipient: agent,
            },
        }
    }

    /// Whether delivery succeeded (including fallback).
    pub fn is_success(&self) -> bool {
        matches!(
            self,
            DeliveryOutcome::Delivered { .. } | DeliveryOutcome::FallbackToLead { .. }
        )
    }

    /// The delivery method string for proto response.
    pub fn method_string(&self) -> &str {
        match self {
            DeliveryOutcome::Delivered { method, .. }
            | DeliveryOutcome::FallbackToLead { method, .. } => match method {
                DeliveryMethod::TeamsInbox => "teams_inbox",
                DeliveryMethod::Acp => "acp",
                DeliveryMethod::Uds => "unix_socket",
                DeliveryMethod::Tmux => "tmux_stdin",
            },
            DeliveryOutcome::Failed { .. } => "failed",
        }
    }
}

/// Route a message to a typed Address.
///
/// Resolves the Address to a concrete agent key and tab name, then delegates
/// to `deliver_to_agent()`. For `Address::Team` with no member, resolves the
/// team lead from the TeamRegistry.
#[instrument(skip_all, fields(address = %address, from = %from))]
pub async fn route_message(
    ctx: &(impl super::HasTeamRegistry
          + super::HasAcpRegistry
          + super::HasAgentResolver
          + super::HasProjectDir
          + super::HasTmuxIpc),
    address: &Address,
    from: &crate::domain::AgentName,
    content: &str,
    summary: &str,
) -> DeliveryOutcome {
    match address {
        Address::Agent(name) => {
            let tab_name = resolve_tab_name_for_agent(name, Some(ctx.agent_resolver()));
            let agent_key = name.as_str();
            let result = deliver_to_agent(ctx, agent_key, &tab_name, from, content, summary).await;
            DeliveryOutcome::from_result(result, agent_key)
        }
        Address::Team { team, member } => {
            if let Some(member_name) = member {
                // Direct team member delivery
                let tab_name = resolve_tab_name_for_agent(member_name, Some(ctx.agent_resolver()));
                let agent_key = member_name.as_str();
                let result =
                    deliver_to_agent(ctx, agent_key, &tab_name, from, content, summary).await;
                DeliveryOutcome::from_result(result, agent_key)
            } else {
                // Team lead resolution: find who owns this team
                resolve_and_deliver_to_lead(ctx, team.as_str(), from, content, summary).await
            }
        }
        Address::Supervisor => {
            // Supervisor resolves to "root" by default (the root TL)
            let result = deliver_to_agent(ctx, "root", "TL", from, content, summary).await;
            DeliveryOutcome::from_result(result, "root")
        }
    }
}

/// Resolve team lead and deliver. Uses `config.json`'s `leadAgentId` to find
/// the lead, falls back to first in-memory entry, then to "root".
async fn resolve_and_deliver_to_lead(
    ctx: &(impl super::HasTeamRegistry
          + super::HasAcpRegistry
          + super::HasAgentResolver
          + super::HasProjectDir
          + super::HasTmuxIpc),
    team_name: &str,
    from: &crate::domain::AgentName,
    content: &str,
    summary: &str,
) -> DeliveryOutcome {
    let original = format!("team:{}:lead", team_name);

    // Resolve lead: config.json leadAgentId → in-memory first entry → "root"
    let lead_key = ctx
        .team_registry()
        .resolve_lead(team_name)
        .await
        .unwrap_or_else(|| "root".to_string());

    info!(
        team = %team_name,
        lead = %lead_key,
        "Resolved team lead for delivery"
    );

    let lead_agent = crate::domain::AgentName::from(lead_key.as_str());
    let tab_name = resolve_tab_name_for_agent(&lead_agent, Some(ctx.agent_resolver()));
    let result = deliver_to_agent(ctx, &lead_key, &tab_name, from, content, summary).await;

    if result.is_failure() {
        DeliveryOutcome::Failed {
            original,
            reason: format!("delivery to resolved lead '{}' failed ({:?})", lead_key, result),
        }
    } else {
        DeliveryOutcome::FallbackToLead {
            method: delivery_method_from_result(result),
            original,
            lead: crate::domain::AgentName::from(lead_key.as_str()),
        }
    }
}

fn delivery_method_from_result(result: DeliveryResult) -> DeliveryMethod {
    match result {
        DeliveryResult::Teams => DeliveryMethod::TeamsInbox,
        DeliveryResult::Acp => DeliveryMethod::Acp,
        DeliveryResult::Uds => DeliveryMethod::Uds,
        DeliveryResult::Tmux | DeliveryResult::Failed | DeliveryResult::StaleRouting => {
            DeliveryMethod::Tmux
        }
    }
}

/// Resolve the tmux window/display name for an agent.
///
/// Primary path: `AgentResolver` lookup (pre-computed `display_name`).
/// Derivation fallback: for agents not in the resolver (CC-native teammates
/// that were never spawned via exomonad and thus never registered).
pub fn resolve_tab_name_for_agent(
    agent_key: &crate::domain::AgentName,
    resolver: Option<&super::agent_resolver::AgentResolver>,
) -> String {
    if agent_key.as_str() == "root" {
        return "TL".to_string();
    }

    if let Some(resolver) = resolver {
        if let Ok(records) = resolver.records_ref().try_read() {
            if let Some(record) = records.get(agent_key) {
                return record.display_name.clone();
            }
        }
    }

    // Unregistered agent (e.g., CC-native teammate) — derive from name
    let identity =
        crate::services::agent_control::AgentIdentity::from_internal_name(agent_key.as_str());
    identity.display_name()
}

/// Notify a parent agent. Single codepath for all parent notifications.
///
/// Pipeline: event log → EventQueue → format `[from: id]`/`[FAILED: id]` → deliver_to_agent.
/// Used by both `EventHandler::notify_parent` (agent-initiated) and the poller's
/// `NotifyParentAction` (system-initiated via event handlers).
///
/// All messages are prefixed with `[from: id]` (or `[FAILED: id]` for failures).
/// Event handler messages include their own structural tags (e.g. `[PR READY]`)
/// inside the message body, so the TL sees: `[from: leaf-id] [PR READY] PR #5...`
///
/// For peer-to-peer messaging, use `deliver_to_agent()` directly instead.
#[allow(clippy::too_many_arguments)]
#[instrument(skip_all, fields(agent_id = %agent_id, parent_session_id = %parent_session_id, status = %status))]
pub async fn notify_parent_delivery(
    ctx: &(impl super::HasTeamRegistry
          + super::HasAcpRegistry
          + super::HasEventLog
          + super::HasEventQueue
          + super::HasProjectDir
          + super::HasTmuxIpc),
    agent_id: &crate::domain::AgentName,
    parent_session_id: &str,
    parent_tab_name: &str,
    status: NotifyStatus,
    message: &str,
    summary: Option<&str>,
    source: &str,
) -> DeliveryResult {
    // 1. Log OTel event + JSONL
    tracing::info!(
        otel.name = "agent.notify_parent",
        parent = %parent_session_id,
        status = %status,
        source = %source,
        "[event] agent.notify_parent"
    );
    if let Some(log) = ctx.event_log() {
        let _ = log.append(
            "agent.notify_parent",
            agent_id.as_str(),
            &serde_json::json!({
                "parent": parent_session_id,
                "status": status.as_str(),
                "message": message,
                "source": source,
            }),
        );
    }

    // 2. Publish to event queue
    let event = Event {
        event_id: 0,
        event_type: Some(event::EventType::AgentMessage(AgentMessage {
            agent_id: agent_id.to_string(),
            status: status.to_string(),
            message: message.to_string(),
            changes: Vec::new(),
        })),
    };
    ctx.event_queue()
        .notify_event(parent_session_id, event)
        .await;

    // 3. Format and deliver
    let notification = format_parent_notification(agent_id, status, message);
    let default_summary = format!("Agent update: {}", agent_id);
    let summary = summary.unwrap_or(&default_summary);

    let delivery_result = deliver_to_agent(
        ctx,
        parent_session_id,
        parent_tab_name,
        agent_id,
        &notification,
        summary,
    )
    .await;

    delivery_result
}

/// Deliver a notification via HTTP POST over a Unix domain socket.
/// Fire-and-forget with 5s timeout.
async fn deliver_via_uds(
    socket_path: &std::path::Path,
    from: &str,
    message: &str,
    summary: &str,
) -> Result<(), String> {
    use http_body_util::{BodyExt, Full};
    use hyper::Request;
    use hyper_util::rt::TokioIo;
    use std::time::Duration;
    use tokio::net::UnixStream;

    let body = serde_json::json!({
        "from": from,
        "message": message,
        "summary": summary,
    });
    let body_bytes = serde_json::to_vec(&body).map_err(|e| e.to_string())?;

    let result = tokio::time::timeout(Duration::from_secs(5), async {
        let stream = UnixStream::connect(socket_path)
            .await
            .map_err(|e| e.to_string())?;
        let io = TokioIo::new(stream);

        let (mut sender, conn) = hyper::client::conn::http1::handshake(io)
            .await
            .map_err(|e| e.to_string())?;

        tokio::spawn(async move {
            let _ = conn.await;
        });

        let req = Request::post("/notify")
            .header("host", "localhost")
            .header("content-type", "application/json")
            .body(Full::new(hyper::body::Bytes::from(body_bytes)))
            .map_err(|e| e.to_string())?;

        let resp = sender.send_request(req).await.map_err(|e| e.to_string())?;

        let status = resp.status();
        if status.is_success() {
            Ok(())
        } else {
            let body_bytes = resp
                .into_body()
                .collect()
                .await
                .map_err(|e| e.to_string())?
                .to_bytes();
            Err(format!(
                "UDS server responded: {} - {}",
                status,
                String::from_utf8_lossy(&body_bytes)
                    .lines()
                    .next()
                    .unwrap_or("empty")
            ))
        }
    })
    .await;

    match result {
        Ok(inner) => inner,
        Err(_) => Err("UDS delivery timed out after 5s".to_string()),
    }
}

/// Deliver a message to an agent.
///
/// Tries Teams inbox delivery if a registry and agent key are provided.
/// Attempts ACP prompt delivery if a registry is provided and agent is registered.
/// Attempts HTTP-over-UDS delivery for custom binary agents (e.g., shoal-agent).
/// Falls back to tmux input injection if other delivery methods fail or are not available.
#[instrument(skip_all, fields(agent_key = %agent_key, from = %from, delivery_method = tracing::field::Empty))]
pub async fn deliver_to_agent(
    ctx: &(impl super::HasTeamRegistry
          + super::HasAcpRegistry
          + super::HasProjectDir
          + super::HasTmuxIpc),
    agent_key: &str,
    tmux_target: &str,
    from: &crate::domain::AgentName,
    message: &str,
    summary: &str,
) -> DeliveryResult {
    let team_registry = ctx.team_registry();
    let acp_registry = ctx.acp_registry();
    let project_dir = ctx.project_dir();
    let tmux_ipc = ctx.tmux_ipc();

    // Batch lookup: sender's team (for Tier 2 scoping) + recipient in-memory check.
    // Single lock acquisition instead of two separate get() calls.
    let (sender_info, recipient_info) = team_registry.get_pair(from.as_str(), agent_key).await;
    let sender_team = sender_info.map(|info| info.team_name);
    // Track whether this is a Tier 1 (in-memory) resolution — CC-native agents
    // (Tier 2, config.json) don't have worktrees or routing.json, so the
    // verifier's tmux fallback should be skipped for them.
    let is_in_memory = recipient_info.is_some();
    // Use in-memory result directly, or fall back to Tier 2 (config.json scan)
    let resolved = recipient_info.or_else(|| {
        sender_team
            .as_deref()
            .and_then(|team| TeamRegistry::resolve_from_config(team, agent_key))
    });
    if let Some(team_info) = resolved {
        // Retry inbox writes up to 3 times before falling back
        let team_name_ref = &team_info.team_name;
        let inbox_name_ref = &team_info.inbox_name;
        let inbox_policy = super::resilience::RetryPolicy::new(
            3,
            super::resilience::Backoff::Fixed(std::time::Duration::from_millis(100)),
        );
        let teams_result = super::resilience::retry(&inbox_policy, || async {
            teams_mailbox::write_to_inbox(
                team_name_ref,
                inbox_name_ref,
                from.as_str(),
                message,
                summary,
            )
            .map_err(|e| anyhow::anyhow!("{}", e))
        })
        .await;
        let teams_result = match teams_result {
            Ok(timestamp) => Some(timestamp),
            Err(e) => {
                warn!(
                    agent = %agent_key,
                    error = %e,
                    "Teams inbox write failed after 3 attempts, falling back to ACP/tmux"
                );
                tracing::info!(
                    otel.name = "message.delivery",
                    agent_id = %from,
                    recipient = %agent_key,
                    method = "teams_inbox",
                    outcome = "failed",
                    detail = %e,
                    "[event] message.delivery"
                );
                None
            }
        };

        if let Some(timestamp) = teams_result {
            tracing::Span::current().record("delivery_method", "teams");
            info!(
                agent = %agent_key,
                team = %team_info.team_name,
                inbox = %team_info.inbox_name,
                timestamp = %timestamp,
                "Wrote message to Teams inbox, spawning delivery verifier (30s)"
            );

            tracing::info!(
                otel.name = "message.delivery",
                agent_id = %from,
                recipient = %agent_key,
                method = "teams_inbox",
                outcome = "success",
                detail = format!("{}/{}", team_info.team_name, team_info.inbox_name),
                "[event] message.delivery"
            );

            // Spawn background task to verify CC's InboxPoller read the message.
            // If not read within 30s, fall back to tmux STDIN injection.
            // For Tier 2 (CC-native) recipients, skip tmux fallback — they don't
            // have exomonad worktrees or routing.json. CC's InboxPoller owns delivery.
            let team_name = team_info.team_name.clone();
            let inbox_name = team_info.inbox_name.clone();
            let agent = agent_key.to_string();
            let target = tmux_target.to_string();
            let msg = message.to_string();
            let has_tmux_fallback = is_in_memory;
            let worktree = if agent_key.contains('.') {
                crate::services::resolve_working_dir(agent_key)
            } else if tmux_target == "TL" {
                std::path::PathBuf::from(".")
            } else {
                crate::services::resolve_worktree_from_tab(tmux_target)
            };
            let pd = project_dir.join(worktree);
            let tmux_ipc = tmux_ipc.clone();
            tokio::spawn(async move {
                let verify_policy = crate::services::resilience::RetryPolicy::new(
                    3,
                    crate::services::resilience::Backoff::Fixed(std::time::Duration::from_secs(10)),
                );
                let verified = crate::services::resilience::retry(&verify_policy, || {
                    let is_read =
                        teams_mailbox::is_message_read(&team_name, &inbox_name, &timestamp);
                    info!(
                        agent = %agent,
                        team = %team_name,
                        inbox = %inbox_name,
                        timestamp = %timestamp,
                        is_read,
                        "Delivery verifier poll"
                    );
                    async move {
                        if is_read {
                            Ok(())
                        } else {
                            anyhow::bail!("message not yet read")
                        }
                    }
                })
                .await;
                if verified.is_ok() {
                    return;
                }
                if !has_tmux_fallback {
                    warn!(
                        agent = %agent,
                        team = %team_name,
                        "Teams inbox message not read after 30s (Tier 2 recipient, no tmux fallback)"
                    );
                    return;
                }
                warn!(
                    agent = %agent,
                    team = %team_name,
                    target = %target,
                    "Teams inbox message not read after 30s, falling back to tmux injection"
                );
                if let Err(e) = tmux_events::inject_input(&tmux_ipc, &target, &msg, &pd).await {
                    warn!(target = %target, error = %e, "tmux inject_input failed (Teams fallback)");
                }
            });
            return DeliveryResult::Teams;
        }
    }

    if let Some(conn) = acp_registry.get(agent_key).await {
        match conn
            .conn
            .prompt(PromptRequest::new(
                conn.session_id.clone(),
                // ACP prompt content can be multiple messages, but we deliver one-at-a-time here.
                vec![message.into()],
            ))
            .await
        {
            Ok(_) => {
                tracing::Span::current().record("delivery_method", "acp");
                info!(agent = %agent_key, "Delivered message via ACP prompt");
                tracing::info!(
                    otel.name = "message.delivery",
                    agent_id = %from,
                    recipient = %agent_key,
                    method = "acp",
                    outcome = "success",
                    detail = %conn.session_id,
                    "[event] message.delivery"
                );
                return DeliveryResult::Acp;
            }
            Err(e) => {
                warn!(
                    agent = %agent_key,
                    error = ?e,
                    "ACP prompt failed, falling back to tmux"
                );
                // Purge on connection-level errors. ACP's Error doesn't expose ErrorKind
                // directly, but we can check if it's an internal error from the server
                // shutting down or a broken transport.
                if is_acp_connection_error(&e) {
                    acp_registry.remove(agent_key).await;
                }
                tracing::info!(
                    otel.name = "message.delivery",
                    agent_id = %from,
                    recipient = %agent_key,
                    method = "acp",
                    outcome = "failed",
                    detail = ?e,
                    "[event] message.delivery"
                );
            }
        }
    }

    // Try HTTP-over-UDS delivery (for custom binary agents like shoal-agent)
    let socket_path = project_dir.join(format!(".exo/agents/{}/notify.sock", agent_key));
    if socket_path.exists() {
        match deliver_via_uds(&socket_path, from.as_str(), message, summary).await {
            Ok(()) => {
                tracing::Span::current().record("delivery_method", "uds");
                info!(agent = %agent_key, socket = %socket_path.display(), "Delivered message via Unix socket");
                tracing::info!(
                    otel.name = "message.delivery",
                    agent_id = %from,
                    recipient = %agent_key,
                    method = "unix_socket",
                    outcome = "success",
                    detail = %socket_path.to_string_lossy(),
                    "[event] message.delivery"
                );
                return DeliveryResult::Uds;
            }
            Err(e) => {
                warn!(agent = %agent_key, error = %e, "UDS delivery failed, falling back to tmux");
                tracing::info!(
                    otel.name = "message.delivery",
                    agent_id = %from,
                    recipient = %agent_key,
                    method = "unix_socket",
                    outcome = "failed",
                    detail = %e,
                    "[event] message.delivery"
                );
            }
        }
    }

    // routing.json records tmux identifiers at spawn time: pane_id (%N) for
    // workers, window_id (@N) for subtrees/leaves. Use slug (last dot-segment)
    // since agent_control writes routing under the slug, not the full branch name.
    // Try direct agent_key path first (for peer messaging where key is already
    // the directory name), then slug with all agent type suffixes.
    let slug = agent_key
        .rsplit_once('.')
        .map(|(_, s)| s)
        .unwrap_or(agent_key);
    let agents_dir = project_dir.join(".exo/agents");
    let routing_candidates = std::iter::once(agent_key.to_string()).chain(
        ["gemini", "claude", "shoal"].iter().flat_map(|suffix| {
            [
                format!("{}-{}", slug, suffix),
                format!("{}-{}", agent_key, suffix),
            ]
        }),
    );

    let mut routing_target = None;
    let mut routing_parent_tab = None;
    let mut matched_dir_name = None;
    let mut stale_candidates = Vec::new();

    for dir_name in routing_candidates {
        let path = agents_dir.join(&dir_name).join("routing.json");
        debug!(candidate = %dir_name, path = %path.display(), "Checking routing candidate");
        if let Ok(content) = tokio::fs::read_to_string(&path).await {
            debug!(path = %path.display(), "Found routing.json");
            if let Ok(routing) = serde_json::from_str::<serde_json::Value>(&content) {
                // Prefer pane_id (workers), then window_id (subtrees/leaves), then parent_tab
                let target = routing["pane_id"]
                    .as_str()
                    .or_else(|| routing["window_id"].as_str())
                    .or_else(|| routing["parent_tab"].as_str())
                    .map(|s| s.to_string());

                if let Some(t) = target {
                    // Liveness check: verify tmux target still exists before attempting injection
                    if tmux_ipc.target_alive(&t).await {
                        debug!(target = %t, "Routing target is alive");
                        routing_target = Some(t);
                        routing_parent_tab = routing["parent_tab"].as_str().map(|s| s.to_string());
                        matched_dir_name = Some(dir_name.clone());
                        break;
                    } else {
                        debug!(target = %t, "Routing target is dead");
                        warn!(
                            agent = %agent_key,
                            target = %t,
                            dir = %dir_name,
                            "Routing target is dead, skipping candidate"
                        );
                        stale_candidates.push(dir_name.clone());
                    }
                }
            }
        }
    }

    if let Some(target) = routing_target {
        tracing::Span::current().record("delivery_method", "tmux");
        info!(
            agent = %agent_key,
            target = %target,
            chars = message.len(),
            "Injecting message via routing.json"
        );
        let worktree = if let Some(ref parent_tab) = routing_parent_tab {
            crate::services::resolve_worktree_from_tab(parent_tab)
        } else if let Some(ref dir_name) = matched_dir_name {
            // Check if a worktree exists with this name (subtree agent)
            let wt_path = project_dir.join(".exo/worktrees").join(dir_name);
            if wt_path.exists() {
                std::path::PathBuf::from(format!(".exo/worktrees/{}/", dir_name))
            } else {
                crate::services::resolve_working_dir(agent_key)
            }
        } else {
            crate::services::resolve_working_dir(agent_key)
        };
        let effective_pd = project_dir.join(worktree);
        let outcome =
            match tmux_events::inject_input(tmux_ipc, &target, message, &effective_pd).await {
                Ok(()) => "success",
                Err(e) => {
                    warn!(target = %target, error = %e, "tmux inject_input failed (routing.json)");
                    "failed"
                }
            };
        tracing::info!(
            otel.name = "message.delivery",
            agent_id = %from,
            recipient = %agent_key,
            method = "tmux_routing",
            outcome = outcome,
            detail = %target,
            "[event] message.delivery"
        );
        return DeliveryResult::Tmux;
    }

    // All routing candidates proved dead — prune the stale JSON files
    if !stale_candidates.is_empty() {
        for dir_name in stale_candidates {
            prune_stale_routing(project_dir, &dir_name).await;
        }
        return DeliveryResult::StaleRouting;
    }

    tracing::Span::current().record("delivery_method", "tmux");
    debug!(
        target = %tmux_target,
        agent = %agent_key,
        chars = message.len(),
        "Injecting message into agent pane via tmux"
    );
    let worktree = if tmux_target == "TL" {
        std::path::PathBuf::from(".")
    } else {
        crate::services::resolve_worktree_from_tab(tmux_target)
    };
    let effective_pd = project_dir.join(worktree);
    let outcome =
        match tmux_events::inject_input(tmux_ipc, tmux_target, message, &effective_pd).await {
            Ok(()) => "success",
            Err(e) => {
                warn!(target = %tmux_target, error = %e, "tmux inject_input failed (fallback)");
                "failed"
            }
        };
    tracing::info!(
        otel.name = "message.delivery",
        agent_id = %from,
        recipient = %agent_key,
        method = "tmux_fallback",
        outcome = outcome,
        detail = %tmux_target,
        "[event] message.delivery"
    );
    DeliveryResult::Tmux
}

/// Prune a stale routing.json file for an agent.
async fn prune_stale_routing(project_dir: &std::path::Path, agent_dir_name: &str) {
    let path = project_dir
        .join(".exo/agents")
        .join(agent_dir_name)
        .join("routing.json");
    if path.exists() {
        if let Err(e) = tokio::fs::remove_file(&path).await {
            warn!(
                path = %path.display(),
                error = %e,
                "Failed to prune stale routing.json"
            );
        } else {
            warn!(path = %path.display(), "Pruned stale routing.json");
        }
    }
}

/// Helper to detect if an ACP error indicates a broken connection that should
/// be purged from the registry.
fn is_acp_connection_error(e: &agent_client_protocol::Error) -> bool {
    // ACP's Error uses JSON-RPC codes. InternalError is -32603.
    // The RPC layer specifically uses "server shut down unexpectedly" for oneshot
    // receiver failures (broken pipes/task crashes).
    if matches!(e.code, agent_client_protocol::ErrorCode::InternalError) {
        if let Some(data) = &e.data {
            let s = data.to_string();
            return s.contains("server shut down unexpectedly")
                || s.contains("BrokenPipe")
                || s.contains("ConnectionClosed");
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::AgentName;
    use crate::services::HasEventQueue;
    use serial_test::serial;
    use std::sync::Arc;

    #[test]
    fn test_format_parent_notification_success() {
        let id = crate::domain::AgentName::from("agent-1");
        let msg = format_parent_notification(&id, NotifyStatus::Success, "All done");
        assert_eq!(msg, "[from: agent-1] All done");
    }

    #[test]
    fn test_format_parent_notification_success_empty() {
        let id = crate::domain::AgentName::from("agent-1");
        let msg = format_parent_notification(&id, NotifyStatus::Success, "");
        assert_eq!(msg, "[from: agent-1] Status update.");
    }

    #[test]
    fn test_format_parent_notification_failure() {
        let id = crate::domain::AgentName::from("agent-2");
        let msg = format_parent_notification(&id, NotifyStatus::Failure, "Something went wrong");
        assert_eq!(msg, "[FAILED: agent-2] Something went wrong");
    }

    #[test]
    fn test_format_parent_notification_failure_empty() {
        let id = crate::domain::AgentName::from("agent-2");
        let msg = format_parent_notification(&id, NotifyStatus::Failure, "");
        assert_eq!(msg, "[FAILED: agent-2] Task failed.");
    }

    #[test]
    fn test_format_parent_notification_other_status() {
        let id = crate::domain::AgentName::from("agent-3");
        let msg = format_parent_notification(&id, NotifyStatus::parse("running"), "Working...");
        assert_eq!(msg, "[from: agent-3] Working...");
    }

    #[test]
    fn test_delivery_result_variants_distinct() {
        assert_ne!(DeliveryResult::Teams, DeliveryResult::Tmux);
        assert_ne!(DeliveryResult::Teams, DeliveryResult::Failed);
        assert_ne!(DeliveryResult::Tmux, DeliveryResult::Failed);
    }

    #[tokio::test]
    async fn test_deliver_no_registry_returns_tmux() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!("skipping test_deliver_no_registry_returns_tmux: tmux not available");
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let result = deliver_to_agent(
            &services,
            "agent-1",
            "tab-1",
            &AgentName::from("test"),
            "hello",
            "summary",
        )
        .await;
        // In isolated tmux, tab-1 doesn't exist, so it should still return Tmux
        // (the delivery code returns Tmux regardless of inject_input success, it just warns on Err)
        assert_eq!(result, DeliveryResult::Tmux);
    }

    #[tokio::test]
    async fn test_route_message_to_agent_address_unknown() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!("skipping test_route_message_to_agent_address_unknown: tmux not available");
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let from = AgentName::from("sender");
        let address = Address::Agent(AgentName::from("unknown"));
        let outcome = route_message(&services, &address, &from, "content", "summary").await;

        // Currently deliver_to_agent falls back to Tmux if everything else fails.
        // The test verifies the branch runs without panic.
        assert!(outcome.is_success());
    }

    #[tokio::test]
    async fn test_route_message_to_team_with_explicit_member() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!(
                "skipping test_route_message_to_team_with_explicit_member: tmux not available"
            );
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let from = AgentName::from("sender");
        let address = Address::Team {
            team: "team-a".into(),
            member: Some(AgentName::from("member-1")),
        };
        let outcome = route_message(&services, &address, &from, "content", "summary").await;
        assert!(outcome.is_success());
    }

    #[tokio::test]
    async fn test_route_message_to_team_lead_fallback_no_config() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!(
                "skipping test_route_message_to_team_lead_fallback_no_config: tmux not available"
            );
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let from = AgentName::from("sender");
        let address = Address::Team {
            team: "team-a".into(),
            member: None,
        };
        let outcome = route_message(&services, &address, &from, "content", "summary").await;

        // Should resolve to "root" by default and return FallbackToLead or Delivered(Tmux)
        match outcome {
            DeliveryOutcome::FallbackToLead { lead, .. } => {
                assert_eq!(lead.as_str(), "root");
            }
            DeliveryOutcome::Delivered { recipient, .. } => {
                assert_eq!(recipient.as_str(), "root");
            }
            _ => panic!("Expected fallback to root, got {:?}", outcome),
        }
    }

    #[tokio::test]
    async fn test_deliver_to_agent_no_routing() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!("skipping test_deliver_to_agent_no_routing: tmux not available");
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let result = deliver_to_agent(
            &services,
            "agent-no-routing",
            "target",
            &AgentName::from("sender"),
            "msg",
            "sum",
        )
        .await;
        // Falls back to Tmux
        assert_eq!(result, DeliveryResult::Tmux);
    }

    #[tokio::test]
    async fn test_deliver_via_uds_missing_socket() {
        let tmp = tempfile::tempdir().unwrap();
        let socket_path = tmp.path().join("non-existent.sock");
        let result = deliver_via_uds(&socket_path, "sender", "msg", "sum").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_notify_parent_delivery_publishes_event() {
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let agent_id = AgentName::from("agent-1");

        notify_parent_delivery(
            &services,
            &agent_id,
            "parent-1",
            "TL",
            NotifyStatus::Success,
            "test message",
            None,
            "source",
        )
        .await;

        // Verify event published to event queue
        let len = services.event_queue().queue_len("parent-1").await;
        assert_eq!(len, 1);
    }

    #[tokio::test]
    async fn test_resolve_lead_root_fallback() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!("skipping test_resolve_lead_root_fallback: tmux not available");
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let from = AgentName::from("sender");
        // No config.json and empty TeamRegistry should fallback to root
        let outcome =
            resolve_and_deliver_to_lead(&services, "unknown-team", &from, "content", "summary")
                .await;

        match outcome {
            DeliveryOutcome::FallbackToLead { lead, .. } => {
                assert_eq!(lead.as_str(), "root");
            }
            DeliveryOutcome::Delivered { recipient, .. } => {
                assert_eq!(recipient.as_str(), "root");
            }
            _ => panic!("Expected fallback to root, got {:?}", outcome),
        }
    }

    /// RAII guard to safely override HOME for tests.
    struct ScopedHome {
        old_home: Option<String>,
    }

    impl ScopedHome {
        fn new(new_home: &std::path::Path) -> Self {
            let old_home = std::env::var("HOME").ok();
            unsafe { std::env::set_var("HOME", new_home) };
            Self { old_home }
        }
    }

    impl Drop for ScopedHome {
        fn drop(&mut self) {
            if let Some(ref old) = self.old_home {
                unsafe { std::env::set_var("HOME", old) };
            } else {
                unsafe { std::env::remove_var("HOME") };
            }
        }
    }

    #[tokio::test]
    #[serial]
    async fn test_resolve_lead_from_config_json() {
        let tmp = tempfile::tempdir().unwrap();

        // Use RAII guard to safely override HOME
        let _home = ScopedHome::new(tmp.path());

        let team_name = "test-team-lead";
        let config_dir = tmp.path().join(".claude/teams").join(team_name);
        std::fs::create_dir_all(&config_dir).unwrap();
        let config_file = config_dir.join("config.json");

        let config = serde_json::json!({
            "name": team_name,
            "description": "test",
            "createdAt": 1700000000,
            "leadAgentId": "lead-agent",
            "leadSessionId": "session-1",
            "members": [
                {
                    "agentId": "lead-agent",
                    "name": "resolved-lead",
                    "agentType": "claude",
                    "model": "opus",
                    "joinedAt": 1700000001,
                    "cwd": "/tmp"
                }
            ]
        });
        std::fs::write(&config_file, serde_json::to_string(&config).unwrap()).unwrap();

        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        let from = AgentName::from("sender");
        let outcome =
            resolve_and_deliver_to_lead(&services, team_name, &from, "content", "summary").await;

        match outcome {
            DeliveryOutcome::FallbackToLead { lead, .. } => {
                assert_eq!(lead.as_str(), "resolved-lead");
            }
            DeliveryOutcome::Delivered { recipient, .. } => {
                assert_eq!(recipient.as_str(), "resolved-lead");
            }
            _ => panic!(
                "Expected FallbackToLead or Delivered to resolved-lead, got {:?}",
                outcome
            ),
        }
    }

    #[tokio::test]
    async fn test_routing_live_target_delivers() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let tmp = tempfile::tempdir().unwrap();
        let services = crate::services::ServicesBuilder::new(
            tmp.path().to_path_buf(),
            tmp.path().join(".claude/tasks"),
            Arc::new(crate::services::GitWorktreeService::new(
                tmp.path().to_path_buf(),
            )),
            Arc::new(isolated.ipc.clone()),
        )
        .build();

        let agent_key = "test-agent-live";
        let agent_dir = tmp.path().join(".exo/agents").join(agent_key);
        std::fs::create_dir_all(&agent_dir).unwrap();

        // Create a real window in tmux to be "live"
        let window_id = isolated
            .ipc
            .new_window("test-window", tmp.path(), "/bin/sh", "sleep 100")
            .await
            .unwrap();

        let routing = serde_json::json!({
            "window_id": window_id.as_str()
        });
        std::fs::write(
            agent_dir.join("routing.json"),
            serde_json::to_string(&routing).unwrap(),
        )
        .unwrap();

        let result = deliver_to_agent(
            &services,
            agent_key,
            "fallback",
            &AgentName::from("sender"),
            "msg",
            "sum",
        )
        .await;

        // Should use routing and return Tmux
        assert_eq!(result, DeliveryResult::Tmux);
        // routing.json should still exist
        assert!(agent_dir.join("routing.json").exists());
    }

    #[tokio::test]
    async fn test_routing_dead_target_skipped_and_pruned() {
        let _ = tracing_subscriber::fmt::try_init();
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let tmp = tempfile::tempdir().unwrap();
        let services = crate::services::ServicesBuilder::new(
            tmp.path().to_path_buf(),
            tmp.path().join(".claude/tasks"),
            Arc::new(crate::services::GitWorktreeService::new(
                tmp.path().to_path_buf(),
            )),
            Arc::new(isolated.ipc.clone()),
        )
        .build();

        let agent_key = "test-agent-dead";
        let agent_dir = tmp.path().join(".exo/agents").join(agent_key);
        std::fs::create_dir_all(&agent_dir).unwrap();

        let routing = serde_json::json!({
            "window_id": "@9999" // Non-existent window
        });
        std::fs::write(
            agent_dir.join("routing.json"),
            serde_json::to_string(&routing).unwrap(),
        )
        .unwrap();

        let result = deliver_to_agent(
            &services,
            agent_key,
            "fallback",
            &AgentName::from("sender"),
            "msg",
            "sum",
        )
        .await;

        // Should return StaleRouting and prune the file
        assert_eq!(result, DeliveryResult::StaleRouting);
        assert!(!agent_dir.join("routing.json").exists());
    }

    #[tokio::test]
    async fn test_routing_live_pane_target_delivers() {
        let _ = tracing_subscriber::fmt::try_init();
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let tmp = tempfile::tempdir().unwrap();
        let services = crate::services::ServicesBuilder::new(
            tmp.path().to_path_buf(),
            tmp.path().join(".claude/tasks"),
            Arc::new(crate::services::GitWorktreeService::new(
                tmp.path().to_path_buf(),
            )),
            Arc::new(isolated.ipc.clone()),
        )
        .build();

        // Create a new window which also creates a pane
        let window_id = isolated
            .ipc
            .new_window("test-pane-window", tmp.path(), "/bin/sh", "sleep 100")
            .await
            .unwrap();

        // Find the pane_id for this window
        let panes = isolated
            .ipc
            .run_tmux_command(&["list-panes", "-t", window_id.as_str(), "-F", "#{pane_id}"])
            .await
            .expect("failed to list panes");
        let pane_id = panes
            .lines()
            .map(str::trim)
            .find(|line| line.starts_with('%'))
            .expect("expected a pane_id")
            .to_string();

        let agent_key = "test-agent-live-pane";
        let agent_dir = tmp.path().join(".exo/agents").join(agent_key);
        std::fs::create_dir_all(&agent_dir).unwrap();

        let routing = serde_json::json!({
            "pane_id": pane_id
        });
        std::fs::write(
            agent_dir.join("routing.json"),
            serde_json::to_string(&routing).unwrap(),
        )
        .unwrap();

        let result = deliver_to_agent(
            &services,
            agent_key,
            "fallback",
            &AgentName::from("sender"),
            "msg",
            "sum",
        )
        .await;

        // Should use routing and return Tmux
        assert_eq!(result, DeliveryResult::Tmux);
        // routing.json should still exist
        assert!(agent_dir.join("routing.json").exists());
    }
}
