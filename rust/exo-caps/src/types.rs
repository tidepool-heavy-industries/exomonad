//! Domain types: validated newtypes + the identity/message vocabulary.
//!
//! Newtypes validate at construction (`new` → `CapResult`); once built, always valid
//! (per `.claude/rules/rust.md`). **Serde deserializes *through* the constructor**
//! (`#[serde(try_from)]`), so a value read from disk (papers, a bus line) is validated
//! too — there is no "transparent" hole that lets an invalid value in. Serialization is
//! transparent (the inner value).

use crate::error::{CapError, CapResult};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

// ── identity newtypes ────────────────────────────────────────────────────────

/// Tree address; `name()` = last segment, `parent()` = prefix. A **list of
/// [`AgentName`]**, not a dot-string — branch names may contain `.`, so a joined form
/// can't round-trip. Segment-validity *is* the `AgentName` invariant (single source):
/// the only extra rule here is non-empty.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "Vec<AgentName>")]
pub struct NodePath(Vec<AgentName>);

impl NodePath {
    pub fn new(segments: Vec<AgentName>) -> CapResult<Self> {
        if segments.is_empty() {
            return Err(CapError::invalid(
                "NodePath",
                "must have at least one segment",
            ));
        }
        Ok(NodePath(segments))
    }
    pub fn segments(&self) -> &[AgentName] {
        &self.0
    }
    /// The node's own name = the last segment. Returns a real `AgentName` by
    /// construction (a `NodePath` is a non-empty `Vec<AgentName>` — no validation bypass).
    pub fn name(&self) -> AgentName {
        self.0
            .last()
            .cloned()
            .expect("NodePath is non-empty by construction")
    }
    /// The parent address = prefix (the tree is prefix-containment); `None` for the root.
    pub fn parent(&self) -> Option<NodePath> {
        (self.0.len() > 1).then(|| NodePath(self.0[..self.0.len() - 1].to_vec()))
    }
    /// Extend one level: `self ++ [name]`.
    pub fn child(&self, name: &AgentName) -> NodePath {
        let mut s = self.0.clone();
        s.push(name.clone());
        NodePath(s)
    }
}

impl TryFrom<Vec<AgentName>> for NodePath {
    type Error = CapError;
    fn try_from(v: Vec<AgentName>) -> CapResult<Self> {
        NodePath::new(v)
    }
}

/// Git branch — generated **safely** from a `NodePath` (decoupled, so a `.` in a
/// segment can't corrupt it).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct Branch(String);

impl Branch {
    pub fn new(s: String) -> CapResult<Self> {
        // A pragmatic subset of git ref-name rules — enough to reject the values that
        // actually bite (spaces, `..`, ref-meta chars, leading `-`).
        let bad_char = |c: char| c.is_whitespace() || "~^:?*[\\".contains(c) || c.is_control();
        if s.is_empty()
            || s.contains("..")
            || s.starts_with('-')
            || s.starts_with('/')
            || s.ends_with('/')
            || s.ends_with(".lock")
            || s.chars().any(bad_char)
        {
            return Err(CapError::invalid(
                "Branch",
                format!("not a valid git ref name: {s:?}"),
            ));
        }
        Ok(Branch(s))
    }
    /// Safe generation from a tree address: sanitize each segment to `[A-Za-z0-9_-]`,
    /// join with `.`. Never the raw dot-join (which a `.`-bearing segment would corrupt).
    pub fn from_path(path: &NodePath) -> Self {
        let safe = path
            .0
            .iter()
            .map(|seg| {
                seg.as_str()
                    .chars()
                    .map(|c| {
                        if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                            c
                        } else {
                            '-'
                        }
                    })
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join(".");
        Branch(safe)
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Branch {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        Branch::new(s)
    }
}

/// tmux pane id, `%N`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct PaneId(String);

impl PaneId {
    pub fn new(s: String) -> CapResult<Self> {
        if !s.starts_with('%') || s.len() < 2 || !s[1..].bytes().all(|b| b.is_ascii_digit()) {
            return Err(CapError::invalid(
                "PaneId",
                format!("not a %N pane id: {s:?}"),
            ));
        }
        Ok(PaneId(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for PaneId {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        PaneId::new(s)
    }
}

/// Absolute path to an ingestion inbox
/// (`~/.claude/exo/inboxes/{run-id}/pane-N.jsonl`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InboxPath(PathBuf);

impl InboxPath {
    pub fn new(p: PathBuf) -> Self {
        InboxPath(p)
    }
    pub fn as_path(&self) -> &Path {
        &self.0
    }
}

/// A node's name = the `NodePath` last segment; non-empty, no path separators.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct AgentName(String);

impl AgentName {
    pub fn new(s: String) -> CapResult<Self> {
        if s.is_empty() || s.contains('/') || s.contains('\\') {
            return Err(CapError::invalid(
                "AgentName",
                format!("empty or contains a path separator: {s:?}"),
            ));
        }
        Ok(AgentName(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for AgentName {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        AgentName::new(s)
    }
}

/// A non-node persona ("github", "ci"); non-empty.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct SyntheticName(String);

impl SyntheticName {
    pub fn new(s: String) -> CapResult<Self> {
        if s.is_empty() {
            return Err(CapError::invalid("SyntheticName", "must be non-empty"));
        }
        Ok(SyntheticName(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for SyntheticName {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        SyntheticName::new(s)
    }
}

/// Plain message body; bounded length, no C0 control chars except `\t \n \r`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct MessageBody(String);

impl MessageBody {
    /// 4 KiB — a message is ~a paragraph. The bus carries only small lines (no spill):
    /// it asserts the serialized line ≤ `PIPE_BUF` at append, so the effective body max
    /// is a touch under this once the envelope is added. **Bulk content is never inlined
    /// in a message** — the sender writes a file (worktree `.exo/` or `/tmp`) and sends a
    /// small message referencing the path; the receiver reads it with its file tools.
    ///
    pub const MAX_LEN: usize = 4 * 1024;

    pub fn new(s: String) -> CapResult<Self> {
        if s.len() > Self::MAX_LEN {
            return Err(CapError::invalid(
                "MessageBody",
                format!("{} bytes exceeds {} max", s.len(), Self::MAX_LEN),
            ));
        }
        if let Some(c) = s
            .chars()
            .find(|&c| c.is_control() && c != '\t' && c != '\n' && c != '\r')
        {
            return Err(CapError::invalid(
                "MessageBody",
                format!("contains control char {:?}", c),
            ));
        }
        Ok(MessageBody(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for MessageBody {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        MessageBody::new(s)
    }
}

/// A short one-line preview (rendered into panes / inbox UIs). Bounded, **no control
/// chars at all** (including newlines — it's a single line), unlike the multi-line body.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct Summary(String);

impl Summary {
    /// 256 bytes — it's a one-line preview, not a second body.
    pub const MAX_LEN: usize = 256;

    pub fn new(s: String) -> CapResult<Self> {
        if s.len() > Self::MAX_LEN {
            return Err(CapError::invalid(
                "Summary",
                format!("{} bytes exceeds {} max", s.len(), Self::MAX_LEN),
            ));
        }
        if let Some(c) = s.chars().find(|c| c.is_control()) {
            return Err(CapError::invalid(
                "Summary",
                format!("contains control char {c:?}"),
            ));
        }
        Ok(Summary(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Summary {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        Summary::new(s)
    }
}

/// A hook decision reason; non-empty, multi-line allowed (block reasons span multiple lines).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct Reason(String);

impl Reason {
    pub fn new(s: String) -> CapResult<Self> {
        if s.is_empty() {
            return Err(CapError::invalid("Reason", "must be non-empty"));
        }
        Ok(Reason(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Reason {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        Reason::new(s)
    }
}

/// A CC tool name; non-empty, no path separators.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct ToolName(String);

impl ToolName {
    pub fn new(s: String) -> CapResult<Self> {
        if s.is_empty() || s.contains('/') || s.contains('\\') {
            return Err(CapError::invalid(
                "ToolName",
                format!("empty or contains a path separator: {s:?}"),
            ));
        }
        Ok(ToolName(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for ToolName {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        ToolName::new(s)
    }
}

// ── archetype & runtime ──────────────────────────────────────────────────────

// The concrete role enum is **domain-owned** (`exo::ExoRole`), reached through the
// [`RoleKind`](crate::RoleKind) seam. The engine never names a role variant. (Before the trait
// refactor a closed `NodeKind` lived here — that was leak #1.)

/// Runtime — used by the **delivery last-hop only** (the Claude/Shoal switch).
/// The interactive harness behind a tree node. Shoal is a companion / external-rmcp participant,
/// not a per-op spawn archetype; Claude and Codex are the two spawnable tree backends.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum AgentType {
    #[default]
    Claude,
    /// Ordinary interactive OpenAI Codex TUI with an Exomonad MCP sidecar.
    Codex,
    Shoal,
}

/// How a child relates to its parent's worktree. **Set by the spawn op, never a free
/// caller field** — drives papers-location + teardown. Used by the [`Spawner`](crate::Spawner)
/// and recorded in [`ChildRecord`](crate::ChildRecord). `Standalone` (own fresh repo) is
/// a `Worktree` flavor; revisit if needed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ChildKind {
    Inline,
    Worktree,
}

// ── messaging vocabulary ─────────────────────────────────────────────────────

/// Who a message is "from" — not a raw `String` (no spoofing).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Persona {
    Agent(AgentName),
    Synthetic(SyntheticName),
}

/// What **policy** builds and hands to [`Bus::deliver`](crate::Bus) — plain-text body +
/// a short summary + a `kind` tag. **No `from`/`id`/`ts`:** the runtime stamps the
/// envelope at append (see [`IngestionEntry`]), so a tool *cannot* spoof its sender —
/// the anti-spoof guarantee is structural, not a convention.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Message {
    pub text: MessageBody,
    pub summary: Summary,
    pub kind: MessageKind,
    /// The [`IngestionEntry::id`] of the message this one answers, for threading a reply back to
    /// its question. Lives on the *policy* half (not the envelope) because a **tool** supplies it
    /// from its own args — so populating it later needs no change to `Bus::deliver`'s signature.
    /// The `#[serde(flatten)]` in [`IngestionEntry`] puts it at the same wire position either way.
    /// Nothing populates it yet; it is rendered by the last hop when set.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reply_to: Option<String>,
}

/// One line of an ingestion inbox — the **wire** form. The runtime stamps `from` (the
/// true sender; `Agent(me)` for a node send, `Synthetic(src)` for an event injection),
/// `ts`, `id`, and the schema version `v`; the [`Message`] is flattened in, so the line is
/// exactly `{v,ts,from,id,kind,summary,text}`. **Ordering is the append order** (line order).
/// `v` defaults and unknown fields are tolerated (no `deny_unknown_fields`) — a mixed-version
/// swarm won't crash.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct IngestionEntry {
    #[serde(default)]
    pub v: u32,
    pub ts: DateTime<Utc>,
    pub from: Persona,
    /// A per-message UUID v4, stamped by the runtime at append. **Reference only** — a handle an
    /// agent (or a log) can name a specific message by, and what [`Message::reply_to`] points at.
    ///
    /// **It is NEVER a dedup key.** The cursor protocol is deliberately at-least-once (the cursor
    /// advances only after a successful last-hop delivery), so a redelivered line arrives with the
    /// *same* id by design. Treating a repeated id as "already seen" would silently drop the retry
    /// the protocol depends on. Omitted from the wire when `None` (a pre-field line).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    /// **Claim-check pointer.** When `Some(path)`, THIS line is a small stand-in: the real
    /// (oversized) entry is the JSON in that side-file, and the reader loads + processes *that*
    /// instead of the inline `msg` (a stub here). The bus writes it when a serialized entry would
    /// exceed `PIPE_BUF`, so every inbox line stays ≤ `PIPE_BUF` (one atomic append) while a payload
    /// — e.g. a rich review verdict — can be arbitrarily large. `None` (the common case) is omitted
    /// from the wire, so an ordinary line is byte-identical to before.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub spill: Option<String>,
    #[serde(flatten)]
    pub msg: Message,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageKind {
    /// peer/agent message.
    Chat,
    /// a notification (e.g. an external signal). Delivered to the agent like a `Chat`, tagged
    /// `kind: event` in the last-hop header. Bare tag: the detail rides the plain-text body,
    /// not the enum (keeps the body CC-last-hop-friendly).
    Event,
    /// lifecycle (exomonad-internal) — see `ControlKind`.
    Control(ControlKind),
    /// An **engine-owned lifecycle signal** ([`Lifecycle`]) — the closed, typed set the sidecar
    /// acts on itself (`try_reap` / the shutdown matrix). Never rendered to the LLM except as the
    /// handler decides. Typed because the engine owns the variant set.
    Lifecycle(Lifecycle),
    /// A **domain-opaque inter-node payload** — a domain's [`DomainSystem`](crate::DomainSystem)
    /// erased to raw JSON, so a tool that emits one needs only `C: Bus` (least-privilege intact: a
    /// fully-typed System wire would force `C: Bus<D::System>` everywhere). Deserialized back to the
    /// concrete `D::System` at exactly one place — the inbound loop's Domain arm — before
    /// `D::handle_system`. Built via [`deliver_domain`](crate::domain::deliver_domain).
    Domain(DomainPayload),
}

/// A domain system payload erased to raw JSON on the bus (see [`MessageKind::Domain`]). A newtype
/// over `String` of the serialized JSON. A `String` is used (not `RawValue`) because `RawValue`
/// cannot be deserialized through `#[serde(flatten)]`'s buffered intermediate `Content` map,
/// which silently broke `Domain`-message parsing. Surrounding `MessageKind` still derives
/// `PartialEq`/`Eq` because `String` implements them.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct DomainPayload(pub String);

/// A directed control **message**. Lifecycle **records** (`ChildRecord::Spawned`)
/// live in the json record log, not here.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ControlKind {
    /// Ask a node to shut down. `grace_ms` is the pre-kill backstop. `force` distinguishes the two
    /// modes: a **cooperative** request (`force=false`) defers if the target has live children
    /// (the target bounces an "are you sure" back to the requester); a **forced** request
    /// (`force=true`) tears the whole subtree down — the sidecar cascades `Shutdown{force}` to every
    /// child. `force` defaults to `false` (the native CC `shutdown_request` has no force field, so
    /// the bridged form is always cooperative).
    Shutdown {
        grace_ms: u32,
        #[serde(default)]
        force: bool,
    },
}

/// The outcome a node reports for a `Control(Shutdown)` it received (see
/// [`Lifecycle::ShutdownResponse`]).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShutdownStatus {
    /// The node accepted the shutdown and is winding down (cooperative leaf reaping on the
    /// watchdog's next periodic tick, or a forced cascade tearing its subtree down). Not yet gone
    /// — its (advisory) `Lifecycle::Exiting` poke follows later.
    Accepted,
    /// The node refused for now: it has a live subtree that would be orphaned. The requester can
    /// re-send with `force: true` to cascade.
    Deferred,
}

/// Engine-owned **lifecycle** signals — the closed, typed set of node-to-node control signals the
/// sidecar acts on *itself* (reap on exit, render a shutdown reply), distinct from a domain's
/// [`DomainSystem`](crate::DomainSystem) payload. Carried by [`MessageKind::Lifecycle`]; the engine
/// matches it exhaustively, so a domain cannot add a lifecycle variant (the documented IoC —
/// lifecycle is the engine's concern). Serde-tagged on `type`, tolerant of unknown fields.
///
/// There used to be a `ChildIdle` variant here (sent when a node's Stop hook fired) — it was
/// removed along with the rest of the Stop-hook machinery (see `rust/exo/CLAUDE.md`): `Stop` fires
/// on every turn-end, including a node legitimately yielding to wait on a backgrounded async task,
/// so the busy-bit it fed was routinely wrong. Liveness is now read directly off pane existence
/// (`ChildLiveness`), not derived from this wire.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Lifecycle {
    /// ADVISORY ONLY. Sent by a child to its parent just before it kills its own pane — receipt
    /// does NOT prove the pane is gone (the kill can still fail, or race). The parent treats this
    /// as a poke to re-evaluate its own pending shutdown (`try_reap`), never as proof of exit;
    /// pane-liveness (`Topology`) is the sole authority for "this child is gone".
    Exiting { reason: String },
    /// A node's structured reply to a [`ControlKind::Shutdown`] it received. The requester's
    /// sidecar renders it into a chat line for its LLM.
    ShutdownResponse {
        status: ShutdownStatus,
        #[serde(default)]
        live_children: Vec<AgentName>,
        #[serde(default)]
        busy: bool,
        #[serde(default)]
        reason: String,
    },
    /// A child reports that `branch@sha` is committed and awaiting this node's merge. The parent's
    /// sidecar records it (`ChildRecord::Submitted`) so the pending-merge queue is durable, then
    /// still renders the `[READY]` prose into the parent's LLM.
    ///
    /// **Wire note:** an older node receiving this variant fails the whole-line `IngestionEntry`
    /// parse and warn-drops the line (its cursor advances past it). That is accepted — a swarm run
    /// is single-version by construction (one binary births the whole tree).
    Submitted {
        branch: Branch,
        sha: String,
        #[serde(default)]
        reviewed: bool,
    },
}

// ── node status ──────────────────────────────────────────────────────────────

/// Small periodic status snapshot for swarm visibility.
/// Written to `pane-N.status.json` periodically by the node's sidecar.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeStatus {
    pub node: NodePath,
    /// The node's role as its domain **`role_str`** (not a typed enum), so domain-agnostic readers
    /// (exo-scry / observability) parse the status without knowing the domain's role type.
    pub kind: String,
    pub branch: String,
    pub shutdown_pending: bool,
    /// Whether an `exo listen` wake-channel client is currently attached to this node's
    /// listen socket. `false` means inbound messages are queuing (cursor-pinned) until the
    /// agent arms/re-arms its Monitor — senders surface this as a ⚠ in their tool responses.
    #[serde(default)]
    pub listener_connected: bool,
    /// Direct children and their busy state.
    pub children: Vec<ChildStatus>,
    pub ts: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChildStatus {
    pub name: String,
    pub busy: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shutdown_response_serde_roundtrip() {
        let m = Lifecycle::ShutdownResponse {
            status: ShutdownStatus::Deferred,
            live_children: vec![
                AgentName::new("a".into()).unwrap(),
                AgentName::new("b".into()).unwrap(),
            ],
            busy: true,
            reason: String::new(),
        };
        let json = serde_json::to_string(&m).unwrap();
        assert!(json.contains("\"type\":\"shutdown_response\""));
        assert!(json.contains("\"status\":\"deferred\""));
        let back: Lifecycle = serde_json::from_str(&json).unwrap();
        assert_eq!(m, back);
    }

    #[test]
    fn domain_payload_round_trips_and_compares_by_raw() {
        #[derive(serde::Serialize, serde::Deserialize)]
        struct S {
            kind: String,
            n: u32,
        }
        let json = serde_json::to_string(&S {
            kind: "demo".into(),
            n: 7,
        })
        .unwrap();
        let kind = MessageKind::Domain(DomainPayload(json));
        let serialized = serde_json::to_string(&kind).unwrap();
        // The domain payload is now a JSON string literal (escaped) because String is used,
        // which survives flatten's buffering.
        let back: MessageKind = serde_json::from_str(&serialized).unwrap();
        assert_eq!(kind, back);
    }

    fn an(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }

    #[test]
    fn node_path_name_parent_child() {
        let p = NodePath::new(vec![an("dev"), an("auth-claude"), an("oauth-dev")]).unwrap();
        assert_eq!(p.name().as_str(), "oauth-dev");
        assert_eq!(p.parent().unwrap().name().as_str(), "auth-claude");
        let root = NodePath::new(vec![an("dev")]).unwrap();
        assert!(root.parent().is_none());
        let kid = root.child(&an("auth-claude"));
        assert_eq!(
            kid.segments()
                .iter()
                .map(|a| a.as_str())
                .collect::<Vec<_>>(),
            vec!["dev", "auth-claude"]
        );
    }

    #[test]
    fn newtypes_reject_bad_input() {
        assert!(NodePath::new(vec![]).is_err());
        // a bad segment is now rejected at AgentName construction — it can't even reach NodePath
        assert!(AgentName::new("".into()).is_err());
        assert!(AgentName::new("a/b".into()).is_err());
        assert!(PaneId::new("317".into()).is_err());
        assert!(PaneId::new("%31a".into()).is_err());
        assert!(PaneId::new("%317".into()).is_ok());
        assert!(Branch::new("has space".into()).is_err());
        assert!(Branch::new("a..b".into()).is_err());
        assert!(Branch::new("dev.auth-claude".into()).is_ok());
        assert!(MessageBody::new("ok\nwith newline".into()).is_ok());
        assert!(MessageBody::new("nul\0byte".into()).is_err());
    }

    #[test]
    fn branch_from_path_is_safe() {
        // A `.`-bearing segment must not corrupt the branch.
        let p = NodePath::new(vec![an("dev"), an("v1.2")]).unwrap();
        let b = Branch::from_path(&p);
        assert_eq!(b.as_str(), "dev.v1-2");
        // and the generated branch is itself a valid ref name
        assert!(Branch::new(b.as_str().to_string()).is_ok());
    }

    #[test]
    fn serde_deserializes_through_validation() {
        // valid round-trips
        let n: NodePath = serde_json::from_str(r#"["dev","leaf"]"#).unwrap();
        assert_eq!(n.name().as_str(), "leaf");
        assert_eq!(serde_json::to_string(&n).unwrap(), r#"["dev","leaf"]"#);
        // invalid is REJECTED at deserialize (the hole the review closed)
        assert!(serde_json::from_str::<AgentName>(r#""a/b""#).is_err());
        assert!(serde_json::from_str::<NodePath>(r#"[]"#).is_err());
    }

    #[test]
    fn ingestion_entry_round_trips_and_flattens() {
        let entry = IngestionEntry {
            v: 1,
            ts: DateTime::parse_from_rfc3339("2026-05-31T22:00:00Z")
                .unwrap()
                .with_timezone(&Utc),
            from: Persona::Synthetic(SyntheticName::new("github".into()).unwrap()),
            id: Some("11111111-2222-3333-4444-555555555555".into()),
            spill: None,
            msg: Message {
                text: MessageBody::new("PR #5 approved".into()).unwrap(),
                summary: Summary::new("approved".into()).unwrap(),
                kind: MessageKind::Event,
                reply_to: None,
            },
        };
        let json = serde_json::to_string(&entry).unwrap();
        // flattened: message fields sit at the top level, not nested under "msg"
        assert!(json.contains(r#""text":"PR #5 approved""#));
        assert!(!json.contains(r#""msg""#));
        let back: IngestionEntry = serde_json::from_str(&json).unwrap();
        assert_eq!(entry, back);
    }

    #[test]
    fn summary_rejects_newline() {
        assert!(Summary::new("ok".into()).is_ok());
        assert!(Summary::new("two\nlines".into()).is_err());
    }

    #[test]
    fn reason_validates() {
        assert!(Reason::new("non-empty reason".into()).is_ok());
        assert!(Reason::new("multi\nline\nreason".into()).is_ok());
        assert!(Reason::new("".into()).is_err());
    }

    #[test]
    fn tool_name_validates() {
        assert!(ToolName::new("Bash".into()).is_ok());
        assert!(ToolName::new("pre_tool_use".into()).is_ok());
        assert!(ToolName::new("".into()).is_err());
        assert!(ToolName::new("a/b".into()).is_err());
        assert!(ToolName::new("a\\b".into()).is_err());
    }
}
