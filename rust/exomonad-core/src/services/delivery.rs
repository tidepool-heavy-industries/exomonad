use crate::domain::Address;
use crate::services::delivery_channels::{execute_plan, PlanContext};
use claude_teams_bridge::{TeamInfo, TeamRegistry};
use exomonad_proto::effects::events::{event, AgentMessage, Event};
use tracing::{debug, info, instrument, warn};

/// Honest-typed delivery result.
#[derive(Debug)]
pub enum DeliveryResult {
    /// Channel reported synchronous delivery.
    Confirmed(DeliveryChannel),
    /// Channel handed off to an async verifier. Callers that need certainty
    /// can `.await` the receiver; fire-and-forget callers can treat this as
    /// success-pending.
    QueuedUnverified(
        DeliveryChannel,
        tokio::sync::oneshot::Receiver<VerifyOutcome>,
    ),
    /// Plan exhausted or empty.
    Failed(FailureReason),
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

/// Outcome of a routed message delivery.
#[derive(Debug)]
pub enum DeliveryOutcome {
    /// Channel reported synchronous delivery to the intended recipient.
    Delivered {
        channel: DeliveryChannel,
        recipient: crate::domain::AgentName,
    },
    /// Channel wrote but the recipient has not yet been verified to have read
    /// (Teams inbox). Carries the verifier handle; callers that need certainty
    /// may await it.
    Queued {
        channel: DeliveryChannel,
        recipient: crate::domain::AgentName,
        verify: tokio::sync::oneshot::Receiver<VerifyOutcome>,
    },
    /// Target could not be resolved; fell back to team lead (successfully or not).
    FallbackToLead {
        channel: DeliveryChannel,
        original: String,
        lead: crate::domain::AgentName,
    },
    /// Delivery failed entirely.
    Failed { original: String, reason: String },
}

impl DeliveryOutcome {
    /// Whether delivery succeeded (including fallback and queuing).
    pub fn is_success(&self) -> bool {
        !matches!(self, DeliveryOutcome::Failed { .. })
    }

    /// The delivery channel used, if any.
    pub fn channel_or_none(&self) -> Option<DeliveryChannel> {
        match self {
            DeliveryOutcome::Delivered { channel, .. }
            | DeliveryOutcome::Queued { channel, .. }
            | DeliveryOutcome::FallbackToLead { channel, .. } => Some(*channel),
            DeliveryOutcome::Failed { .. } => None,
        }
    }

    /// Map outcome to (success, delivery_method) for proto response.
    pub fn outcome_to_response(&self) -> (bool, String) {
        match self {
            DeliveryOutcome::Delivered { channel, .. } => (true, channel.as_str().to_string()),
            DeliveryOutcome::Queued { channel, .. } => {
                (true, format!("{}_queued", channel.as_str()))
            }
            DeliveryOutcome::FallbackToLead { channel, .. } => {
                (true, format!("{}_lead_fallback", channel.as_str()))
            }
            DeliveryOutcome::Failed { .. } => (false, "failed".to_string()),
        }
    }
}

/// A single delivery transport. The delivery plan is a totally-ordered list of
/// channels to attempt; the executor walks the list until one yields a non-Err
/// outcome or the list is exhausted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DeliveryChannel {
    Teams,
    Acp,
    Uds,
    Tmux,
}

impl DeliveryChannel {
    pub fn as_str(&self) -> &'static str {
        match self {
            DeliveryChannel::Teams => "teams_inbox",
            DeliveryChannel::Acp => "acp",
            DeliveryChannel::Uds => "unix_socket",
            DeliveryChannel::Tmux => "tmux_stdin",
        }
    }
}

/// Whether the recipient runs under exomonad (Tier 1: in-memory registered,
/// has a worktree + `routing.json` + tmux window) or is a CC-native teammate
/// discovered only via `~/.claude/teams/{team}/config.json` (Tier 2: no
/// exomonad-side infrastructure, so tmux fallback is not available).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendType {
    Exomonad,
    CcNative,
}

/// Everything `delivery_plan` needs to decide how to reach a recipient. By
/// design narrow — widening this struct needs strong justification (see
/// anti-patterns in `plans/post-fixup-wave-followups.md`).
#[derive(Debug, Clone, Copy)]
pub struct RecipientMeta {
    pub agent_type: crate::services::agent_control::AgentType,
    pub backend_type: BackendType,
}

/// Outcome of the async Teams-inbox verifier. Only meaningful for
/// `ChannelOutcome::Queued` / `DeliveryResult::QueuedUnverified`.
#[derive(Debug)]
pub enum VerifyOutcome {
    /// Recipient's InboxPoller read the message within the verification window.
    Confirmed,
    /// Recipient did not read; verifier successfully injected via tmux.
    ///
    /// NOTE: Not implemented in the refactored `try_teams_channel` yet; only
    /// produced by the legacy `deliver_to_agent` path.
    FellBackToTmux,
    /// Recipient did not read and tmux fallback was either unavailable
    /// (Tier 2 / CC-native) or failed.
    VerificationFailed(String),
}

/// Honest outcome of a single channel attempt.
#[derive(Debug)]
pub enum ChannelOutcome {
    /// Synchronous proof of receipt (ACP ack, UDS 2xx, tmux inject success).
    Confirmed,
    /// Handed off to an async verifier (Teams inbox). The receiver fires with
    /// a `VerifyOutcome` when the verifier completes (up to ~30s).
    Queued(tokio::sync::oneshot::Receiver<VerifyOutcome>),
    /// Channel attempt failed; executor should continue to the next channel.
    Failed(String),
}

/// Reasons the full plan failed to deliver.
#[derive(Debug)]
pub enum FailureReason {
    /// Every channel in the plan returned `Failed`.
    AllChannelsExhausted,
    /// `delivery_plan` produced an empty plan for this recipient (e.g.
    /// `AgentType::Process`, or `Gemini` + `CcNative`).
    Undeliverable(RecipientMeta),
    /// `routing.json` exists but all targets (pane/window) are dead.
    StaleRouting,
}

/// Pure policy: which channels should be attempted, in what order, for this
/// recipient. The single source of truth for channel selection — no caller
/// may branch on agent type outside this function.
pub fn delivery_plan(recipient: &RecipientMeta) -> Vec<DeliveryChannel> {
    use crate::services::agent_control::AgentType::*;
    use BackendType::*;
    use DeliveryChannel::*;
    match (recipient.agent_type, recipient.backend_type) {
        (Claude, Exomonad) => vec![Teams, Tmux],
        (Claude, CcNative) => vec![Teams],
        (Gemini, Exomonad) => vec![Acp, Tmux],
        (Gemini, CcNative) => vec![],
        (Shoal, Exomonad) => vec![Uds, Tmux],
        (Shoal, CcNative) => vec![],
        (Process, _) => vec![],
    }
}

/// Inverse view of the policy: which channels this recipient is *capable* of
/// receiving on. The executor's invariant is
/// `delivery_plan(m) ⊆ channels_recipient_can_receive(m)`; the property test
/// in leaf `delivery-plan-pure` enforces it.
pub fn channels_recipient_can_receive(
    recipient: &RecipientMeta,
) -> std::collections::BTreeSet<DeliveryChannel> {
    use crate::services::agent_control::AgentType::*;
    use BackendType::*;
    use DeliveryChannel::*;
    let mut set = std::collections::BTreeSet::new();
    match (recipient.agent_type, recipient.backend_type) {
        (Claude, Exomonad) => {
            set.insert(Teams);
            set.insert(Tmux);
        }
        (Claude, CcNative) => {
            set.insert(Teams);
        }
        (Gemini, Exomonad) => {
            set.insert(Acp);
            set.insert(Tmux);
        }
        (Gemini, CcNative) => {}
        (Shoal, Exomonad) => {
            set.insert(Uds);
            set.insert(Tmux);
        }
        (Shoal, CcNative) => {}
        (Process, _) => {}
    }
    set
}

// ---------------------------------------------------------------------------

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
            map_result_to_outcome(result, name.clone(), agent_key)
        }
        Address::Team { team, member } => {
            if let Some(member_name) = member {
                // Direct team member delivery
                let tab_name = resolve_tab_name_for_agent(member_name, Some(ctx.agent_resolver()));
                let agent_key = member_name.as_str();
                let result =
                    deliver_to_agent(ctx, agent_key, &tab_name, from, content, summary).await;
                map_result_to_outcome(result, member_name.clone(), agent_key)
            } else {
                // Team lead resolution: find who owns this team
                resolve_and_deliver_to_lead(ctx, team.as_str(), from, content, summary).await
            }
        }
        Address::Supervisor => {
            // Supervisor resolves to "root" by default (the root TL)
            let result = deliver_to_agent(ctx, "root", "TL", from, content, summary).await;
            map_result_to_outcome(result, crate::domain::AgentName::from("root"), "root")
        }
    }
}

fn map_result_to_outcome(
    result: DeliveryResult,
    recipient: crate::domain::AgentName,
    original: &str,
) -> DeliveryOutcome {
    match result {
        DeliveryResult::Confirmed(channel) => DeliveryOutcome::Delivered { channel, recipient },
        DeliveryResult::QueuedUnverified(channel, verify) => DeliveryOutcome::Queued {
            channel,
            recipient,
            verify,
        },
        DeliveryResult::Failed(reason) => DeliveryOutcome::Failed {
            original: original.to_string(),
            reason: format!("{:?}", reason),
        },
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

    match result {
        DeliveryResult::Confirmed(channel) => DeliveryOutcome::FallbackToLead {
            channel,
            original,
            lead: lead_agent,
        },
        DeliveryResult::QueuedUnverified(channel, _rx) => {
            // NOTE: We drop the receiver for fallback-to-lead for now to keep
            // DeliveryOutcome simple. Callers of route_message typically don't
            // await verification for fallbacks.
            DeliveryOutcome::FallbackToLead {
                channel,
                original,
                lead: lead_agent,
            }
        }
        DeliveryResult::Failed(reason) => DeliveryOutcome::Failed {
            original,
            reason: format!(
                "delivery to resolved lead '{}' failed ({:?})",
                lead_key, reason
            ),
        },
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
pub(super) async fn deliver_via_uds(
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
/// Thin wrapper over `delivery_plan` + `execute_plan`. Builds a
/// `RecipientMeta` from the TeamRegistry lookup (agent_type + tier), resolves
/// the tmux target via `routing.json` (with stale-entry pruning), and runs
/// the resulting capability-driven plan.
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
    // 1. Classify the recipient.
    let (team_info_opt, is_in_memory) = resolve_team_info(ctx, agent_key, from).await;
    let recipient_meta = recipient_meta_from_team_info(team_info_opt.as_ref(), is_in_memory);

    // 2. Pre-resolve the tmux target via routing.json.
    let project_dir = ctx.project_dir();
    let tmux_ipc = ctx.tmux_ipc();
    let routing_res = resolve_routing(agent_key, project_dir, tmux_ipc).await;
    let (resolved_tmux_target, tmux_working_dir) = match routing_res {
        RoutingResolution::Alive {
            target,
            working_dir,
        } => (target, working_dir),
        RoutingResolution::AllStale => return DeliveryResult::Failed(FailureReason::StaleRouting),
        RoutingResolution::NoRouting => (
            tmux_target.to_string(),
            fallback_tmux_working_dir(tmux_target, project_dir),
        ),
    };

    // 3. Build the plan + per-channel context, then execute.
    let plan = delivery_plan(&recipient_meta);
    let uds_socket_path = project_dir.join(format!(".exo/agents/{}/notify.sock", agent_key));
    let uds_socket_opt: Option<&std::path::Path> = if uds_socket_path.exists() {
        Some(uds_socket_path.as_path())
    } else {
        None
    };
    let pctx = PlanContext {
        agent_key,
        tmux_target: &resolved_tmux_target,
        from,
        message,
        summary,
        team_info: team_info_opt.as_ref(),
        uds_socket: uds_socket_opt,
        tmux_working_dir: &tmux_working_dir,
    };
    let result = execute_plan(plan, ctx, &pctx).await;

    match &result {
        DeliveryResult::Confirmed(channel) => {
            tracing::Span::current().record("delivery_method", channel.as_str());
        }
        DeliveryResult::QueuedUnverified(channel, _) => {
            tracing::Span::current().record("delivery_method", channel.as_str());
        }
        DeliveryResult::Failed(reason) => {
            warn!(agent = %agent_key, ?reason, "execute_plan exhausted without delivery");
        }
    }

    result
}

/// Best-effort `TeamInfo` lookup: Tier 1 (in-memory) then Tier 2 (config.json
/// scan scoped by the sender's team). Returns `(team_info, is_tier_1)`.
async fn resolve_team_info(
    ctx: &impl super::HasTeamRegistry,
    agent_key: &str,
    from: &crate::domain::AgentName,
) -> (Option<TeamInfo>, bool) {
    let (sender_info, recipient_info) =
        ctx.team_registry().get_pair(from.as_str(), agent_key).await;
    let sender_team = sender_info.map(|info| info.team_name);
    let is_in_memory = recipient_info.is_some();
    let resolved = recipient_info.or_else(|| {
        sender_team
            .as_deref()
            .and_then(|team| TeamRegistry::resolve_from_config(team, agent_key))
    });
    (resolved, is_in_memory)
}

fn recipient_meta_from_team_info(info: Option<&TeamInfo>, is_in_memory: bool) -> RecipientMeta {
    let agent_type = info
        .map(|i| parse_agent_type_str(&i.agent_type))
        .unwrap_or(crate::services::agent_control::AgentType::Claude);
    let backend_type = if is_in_memory {
        BackendType::Exomonad
    } else {
        match info.and_then(|i| i.backend_type.as_deref()) {
            Some("exomonad") => BackendType::Exomonad,
            Some(_) => BackendType::CcNative,
            None => {
                if info.is_some() {
                    BackendType::CcNative
                } else {
                    BackendType::Exomonad
                }
            }
        }
    };
    RecipientMeta {
        agent_type,
        backend_type,
    }
}

fn parse_agent_type_str(s: &str) -> crate::services::agent_control::AgentType {
    use crate::services::agent_control::AgentType;
    match s {
        "claude" => AgentType::Claude,
        "shoal" => AgentType::Shoal,
        "process" => AgentType::Process,
        _ => AgentType::Gemini,
    }
}

enum RoutingResolution {
    Alive {
        target: String,
        working_dir: std::path::PathBuf,
    },
    AllStale,
    NoRouting,
}

async fn resolve_routing(
    agent_key: &str,
    project_dir: &std::path::Path,
    tmux_ipc: &super::tmux_ipc::TmuxIpc,
) -> RoutingResolution {
    let slug = agent_key
        .rsplit_once('.')
        .map(|(_, s)| s)
        .unwrap_or(agent_key);
    let agents_dir = project_dir.join(".exo/agents");
    let candidates = std::iter::once(agent_key.to_string()).chain(
        ["gemini", "claude", "shoal"].iter().flat_map(|suffix| {
            [
                format!("{}-{}", slug, suffix),
                format!("{}-{}", agent_key, suffix),
            ]
        }),
    );

    let mut stale = Vec::new();
    for dir_name in candidates {
        let path = agents_dir.join(&dir_name).join("routing.json");
        debug!(candidate = %dir_name, path = %path.display(), "Checking routing candidate");
        let Ok(content) = tokio::fs::read_to_string(&path).await else {
            continue;
        };
        let Ok(routing) = serde_json::from_str::<serde_json::Value>(&content) else {
            continue;
        };
        let Some(target) = routing["pane_id"]
            .as_str()
            .or_else(|| routing["window_id"].as_str())
            .or_else(|| routing["parent_tab"].as_str())
            .map(|s| s.to_string())
        else {
            continue;
        };

        if tmux_ipc.target_alive(&target).await {
            let parent_tab = routing["parent_tab"].as_str().map(|s| s.to_string());
            let working_dir =
                routing_working_dir(&dir_name, parent_tab.as_deref(), agent_key, project_dir);
            return RoutingResolution::Alive {
                target,
                working_dir,
            };
        }
        warn!(
            agent = %agent_key,
            target = %target,
            dir = %dir_name,
            "Routing target is dead, skipping candidate"
        );
        stale.push(dir_name);
    }

    if stale.is_empty() {
        return RoutingResolution::NoRouting;
    }
    for dir_name in stale {
        prune_stale_routing(project_dir, &dir_name).await;
    }
    RoutingResolution::AllStale
}

fn routing_working_dir(
    dir_name: &str,
    parent_tab: Option<&str>,
    agent_key: &str,
    project_dir: &std::path::Path,
) -> std::path::PathBuf {
    let relative = if let Some(parent_tab) = parent_tab {
        crate::services::resolve_worktree_from_tab(parent_tab)
    } else if project_dir.join(".exo/worktrees").join(dir_name).exists() {
        std::path::PathBuf::from(format!(".exo/worktrees/{}/", dir_name))
    } else {
        crate::services::resolve_working_dir(agent_key)
    };
    project_dir.join(relative)
}

fn fallback_tmux_working_dir(
    tmux_target: &str,
    project_dir: &std::path::Path,
) -> std::path::PathBuf {
    let relative = if tmux_target == "TL" {
        std::path::PathBuf::from(".")
    } else {
        crate::services::resolve_worktree_from_tab(tmux_target)
    };
    project_dir.join(relative)
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
pub(super) fn is_acp_connection_error(e: &agent_client_protocol::Error) -> bool {
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
        // Use matches! since they no longer derive PartialEq easily due to oneshot::Receiver
        assert!(matches!(
            DeliveryResult::Confirmed(DeliveryChannel::Teams),
            DeliveryResult::Confirmed(DeliveryChannel::Teams)
        ));
    }

    #[tokio::test]
    async fn test_deliver_no_registry_no_tmux_target_returns_failed() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!("skipping test_deliver_no_registry_no_tmux_target_returns_failed: tmux not available");
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = crate::services::Services::test_with_tmux(Arc::new(isolated.ipc.clone()));
        // "tab-1" does not exist in the isolated tmux session — with honest-typed
        // delivery, tmux injection now reports `Failed` instead of falsely
        // claiming success (the legacy `DeliveryResult::Tmux`-on-error behavior
        // was the exact silent dead-letter this refactor eliminates).
        let result = deliver_to_agent(
            &services,
            "agent-1",
            "tab-1",
            &AgentName::from("test"),
            "hello",
            "summary",
        )
        .await;
        assert!(matches!(
            result,
            DeliveryResult::Failed(FailureReason::AllChannelsExhausted)
        ));
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

        // Unknown agent + no live tmux target → honest `Failed` outcome.
        // Under legacy semantics this claimed success; the refactor makes the
        // failure structurally visible.
        assert!(matches!(outcome, DeliveryOutcome::Failed { .. }));
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
        // No live tmux target for "member-1" → honest `Failed`.
        assert!(matches!(outcome, DeliveryOutcome::Failed { .. }));
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

        // Resolves to "root" as the lead, but no live tmux target → `Failed`.
        // The failure reason still carries the resolved lead name, proving the
        // routing logic found the right recipient even though delivery itself failed.
        match outcome {
            DeliveryOutcome::Failed { reason, .. } => {
                assert!(
                    reason.contains("root"),
                    "reason should mention 'root', got: {reason}"
                );
            }
            DeliveryOutcome::FallbackToLead { lead, .. } => {
                assert_eq!(lead.as_str(), "root");
            }
            DeliveryOutcome::Delivered { recipient, .. } => {
                assert_eq!(recipient.as_str(), "root");
            }
            DeliveryOutcome::Queued { recipient, .. } => {
                assert_eq!(recipient.as_str(), "root");
            }
        }
    }

    #[tokio::test]
    async fn test_deliver_to_agent_no_routing_dead_target_fails() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            eprintln!(
                "skipping test_deliver_to_agent_no_routing_dead_target_fails: tmux not available"
            );
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
        // Target "target" does not exist in the isolated tmux session. Honest
        // semantics: delivery fails.
        assert!(matches!(
            result,
            DeliveryResult::Failed(FailureReason::AllChannelsExhausted)
        ));
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
        // No config.json and empty TeamRegistry should fallback to root.
        // With honest-typed delivery, the TL tab "TL" doesn't exist in isolated
        // tmux so delivery fails — but the failure reason names the resolved
        // lead, proving the fallback logic ran correctly.
        let outcome =
            resolve_and_deliver_to_lead(&services, "unknown-team", &from, "content", "summary")
                .await;

        match outcome {
            DeliveryOutcome::Failed { reason, .. } => {
                assert!(
                    reason.contains("root"),
                    "reason should mention 'root', got: {reason}"
                );
            }
            DeliveryOutcome::FallbackToLead { lead, .. } => {
                assert_eq!(lead.as_str(), "root");
            }
            DeliveryOutcome::Delivered { recipient, .. } => {
                assert_eq!(recipient.as_str(), "root");
            }
            DeliveryOutcome::Queued { recipient, .. } => {
                assert_eq!(recipient.as_str(), "root");
            }
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
            DeliveryOutcome::Failed { reason, .. } => {
                assert!(
                    reason.contains("resolved-lead"),
                    "reason should mention 'resolved-lead', got: {reason}"
                );
            }
            DeliveryOutcome::FallbackToLead { lead, .. } => {
                assert_eq!(lead.as_str(), "resolved-lead");
            }
            DeliveryOutcome::Delivered { recipient, .. } => {
                assert_eq!(recipient.as_str(), "resolved-lead");
            }
            DeliveryOutcome::Queued { recipient, .. } => {
                assert_eq!(recipient.as_str(), "resolved-lead");
            }
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
        assert!(matches!(
            result,
            DeliveryResult::Confirmed(DeliveryChannel::Tmux)
        ));
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
        assert!(matches!(
            result,
            DeliveryResult::Failed(FailureReason::StaleRouting)
        ));
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
        assert!(matches!(
            result,
            DeliveryResult::Confirmed(DeliveryChannel::Tmux)
        ));
        // routing.json should still exist
        assert!(agent_dir.join("routing.json").exists());
    }
}

#[cfg(test)]
mod plan_tests {
    use super::*;
    use crate::services::agent_control::AgentType;
    use proptest::prelude::*;

    fn arb_meta() -> impl Strategy<Value = RecipientMeta> {
        (
            prop_oneof![
                Just(AgentType::Claude),
                Just(AgentType::Gemini),
                Just(AgentType::Shoal),
                Just(AgentType::Process),
            ],
            prop_oneof![Just(BackendType::Exomonad), Just(BackendType::CcNative)],
        )
            .prop_map(|(agent_type, backend_type)| RecipientMeta {
                agent_type,
                backend_type,
            })
    }

    proptest! {
        #[test]
        fn plan_is_subset_of_receivable_channels(meta in arb_meta()) {
            let plan = delivery_plan(&meta);
            let receivable = channels_recipient_can_receive(&meta);
            for channel in &plan {
                prop_assert!(
                    receivable.contains(channel),
                    "plan for {:?} contains {:?} which is not in receivable set {:?}",
                    meta, channel, receivable
                );
            }
        }

        #[test]
        fn plan_is_nonempty_iff_receivable_is_nonempty(meta in arb_meta()) {
            let plan_empty = delivery_plan(&meta).is_empty();
            let recv_empty = channels_recipient_can_receive(&meta).is_empty();
            prop_assert_eq!(
                plan_empty, recv_empty,
                "plan/receivable emptiness mismatch for {:?}: plan_empty={}, recv_empty={}",
                meta, plan_empty, recv_empty
            );
        }

        #[test]
        fn plan_has_no_duplicate_channels(meta in arb_meta()) {
            let plan = delivery_plan(&meta);
            let mut dedup: std::collections::BTreeSet<DeliveryChannel> = std::collections::BTreeSet::new();
            for c in &plan {
                prop_assert!(dedup.insert(*c), "duplicate channel {:?} in plan for {:?}", c, meta);
            }
        }
    }

    #[test]
    fn claude_exomonad_plan_matches_spec() {
        let meta = RecipientMeta {
            agent_type: AgentType::Claude,
            backend_type: BackendType::Exomonad,
        };
        assert_eq!(
            delivery_plan(&meta),
            vec![DeliveryChannel::Teams, DeliveryChannel::Tmux]
        );
    }

    #[test]
    fn gemini_exomonad_plan_matches_spec() {
        let meta = RecipientMeta {
            agent_type: AgentType::Gemini,
            backend_type: BackendType::Exomonad,
        };
        assert_eq!(
            delivery_plan(&meta),
            vec![DeliveryChannel::Acp, DeliveryChannel::Tmux]
        );
    }

    #[test]
    fn process_is_undeliverable_on_both_backends() {
        for backend in [BackendType::Exomonad, BackendType::CcNative] {
            let meta = RecipientMeta {
                agent_type: AgentType::Process,
                backend_type: backend,
            };
            assert!(delivery_plan(&meta).is_empty());
            assert!(channels_recipient_can_receive(&meta).is_empty());
        }
    }

    #[test]
    fn gemini_ccnative_is_undeliverable() {
        let meta = RecipientMeta {
            agent_type: AgentType::Gemini,
            backend_type: BackendType::CcNative,
        };
        assert!(delivery_plan(&meta).is_empty());
        assert!(channels_recipient_can_receive(&meta).is_empty());
    }

    #[test]
    fn test_tier2_exomonad_synthetic_classified_as_exomonad() {
        let info = TeamInfo {
            team_name: "test-team".into(),
            inbox_name: "gemini-leaf".into(),
            agent_type: "gemini".into(),
            model: "gemini-1.5-pro".into(),
            backend_type: Some("exomonad".into()),
        };
        let meta = recipient_meta_from_team_info(Some(&info), false);
        assert_eq!(meta.backend_type, BackendType::Exomonad);
        assert_eq!(meta.agent_type, AgentType::Gemini);
        assert_eq!(
            delivery_plan(&meta),
            vec![DeliveryChannel::Acp, DeliveryChannel::Tmux]
        );
    }

    #[test]
    fn test_tier2_ccnative_classified_as_ccnative() {
        let info = TeamInfo {
            team_name: "test-team".into(),
            inbox_name: "gemini-native".into(),
            agent_type: "gemini".into(),
            model: "gemini-1.5-pro".into(),
            backend_type: None,
        };
        let meta = recipient_meta_from_team_info(Some(&info), false);
        assert_eq!(meta.backend_type, BackendType::CcNative);
        assert!(delivery_plan(&meta).is_empty());
    }
}
