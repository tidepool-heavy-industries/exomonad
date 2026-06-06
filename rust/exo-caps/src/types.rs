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

// ── archetype & runtime ──────────────────────────────────────────────────────

/// A node's archetype — the **one** stored identity enum. `role` (the `role_def` key)
/// is the variant; `agent_type` **derives**. Only the four real archetypes are
/// representable, so `(Root, Gemini)` / `(Worker, Claude)` are unnameable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NodeKind {
    Root,
    Tl,
    Dev,
    Worker,
    /// A short-lived Gemini spawned by a submitting node to review its branch. Works in its own
    /// worktree off the under-review code (full YOLO; its blast radius is its own branch) and
    /// emits a `verdict`. Not a tree-building archetype — it reviews, then exits.
    Reviewer,
}

impl NodeKind {
    /// Runtime derives from the archetype — never stored separately.
    pub fn agent_type(self) -> AgentType {
        match self {
            NodeKind::Root | NodeKind::Tl => AgentType::Claude,
            NodeKind::Dev | NodeKind::Worker | NodeKind::Reviewer => AgentType::Gemini,
        }
    }
    /// The `role_def` key / wire string.
    pub fn role_str(self) -> &'static str {
        match self {
            NodeKind::Root => "root",
            NodeKind::Tl => "tl",
            NodeKind::Dev => "dev",
            NodeKind::Worker => "worker",
            NodeKind::Reviewer => "reviewer",
        }
    }
}

/// Runtime — used by the **delivery last-hop only** (the Claude/Gemini/Shoal switch).
/// For a tree node it equals `node_kind.agent_type()`. Shoal is a companion /
/// external-rmcp participant, **not** a per-op spawn archetype.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AgentType {
    Claude,
    Gemini,
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
}

/// One line of an ingestion inbox — the **wire** form. The runtime stamps `from` (the
/// true sender; `Agent(me)` for a node send, `Synthetic(src)` for an event injection),
/// `ts`, and the schema version `v`; the [`Message`] is flattened in, so the line is
/// exactly `{v,ts,from,kind,summary,text}`. **Ordering is the append order**
/// (line order) — no message-id is carried; at-least-once redelivery may show the agent
/// a duplicate line, which is benign. `v` defaults and unknown fields are tolerated (no
/// `deny_unknown_fields`) — a mixed-version swarm won't crash.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct IngestionEntry {
    #[serde(default)]
    pub v: u32,
    pub ts: DateTime<Utc>,
    pub from: Persona,
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
    /// a node-to-node **system signal** — consumed by the recipient's *sidecar*, NOT rendered
    /// into its LLM conversation unless the handler decides the agent must act. This one
    /// envelope variant is just the sidecar-vs-LLM routing bit; the real, granular identifiers
    /// are the [`SystemMessage`] variant tags (so `MessageKind` doesn't bloat per signal).
    System(SystemMessage),
}

/// A directed control **message**. Lifecycle **records** (`AgentSpawned`/`AgentStarted`)
/// live in the json record log, not here.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ControlKind {
    Shutdown { grace_ms: u32 },
}

/// System signals carried over the bus and handled by the recipient's sidecar (see
/// [`MessageKind::System`]). Serde-tagged on `type` (`review_approved` / `review_denied` /
/// `review_changes`) — **granular, flat, extensible**: new node-to-node control signals are new
/// variants here, never a churn of the core envelope. There is no catch-all variant: an unknown
/// `type` fails to deserialize the whole bus line, which the inbound loop's tolerant parser then
/// skips + logs — the swarm won't crash, but that one message is dropped. (Add a
/// `#[serde(other)]` catch-all here if graceful per-variant forward-compat is ever needed for a
/// mixed-version swarm.)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum SystemMessage {
    /// The reviewer approved `branch@sha`. The submitter's sidecar auto-escalates `[READY]`
    /// upward (no LLM turn) iff `sha` still matches the submitter's HEAD.
    ReviewApproved { branch: Branch, sha: String },
    /// The reviewer rejected with feedback. Rendered + delivered to the submitter's LLM to address.
    ReviewDenied {
        branch: Branch,
        sha: String,
        message: String,
    },
    /// The reviewer committed a counter-proposal to `changes_branch`. Rendered + delivered to the
    /// submitter's LLM to `merge` + re-submit.
    ReviewChanges {
        branch: Branch,
        sha: String,
        changes_branch: Branch,
        message: String,
    },
    /// A node finished a turn and is yielding control (its stop hook fired). The envelope's
    /// stamped `from` says *which* node; `summary` is a short human-readable note the parent may
    /// render. Deliberately minimal: v1 just notifies on every stop. Refinement (dedupe,
    /// richer state derived from the stop hook's payload) lands later in the parent's
    /// `handle_system`, not by growing this variant.
    ChildIdle { summary: String },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn node_kind_derives_runtime_and_role() {
        assert_eq!(NodeKind::Root.agent_type(), AgentType::Claude);
        assert_eq!(NodeKind::Tl.agent_type(), AgentType::Claude);
        assert_eq!(NodeKind::Dev.agent_type(), AgentType::Gemini);
        assert_eq!(NodeKind::Worker.agent_type(), AgentType::Gemini);
        assert_eq!(NodeKind::Dev.role_str(), "dev");
    }

    fn an(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }

    #[test]
    fn node_path_name_parent_child() {
        let p = NodePath::new(vec![an("dev"), an("auth-claude"), an("oauth-gemini")]).unwrap();
        assert_eq!(p.name().as_str(), "oauth-gemini");
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
            msg: Message {
                text: MessageBody::new("PR #5 approved".into()).unwrap(),
                summary: Summary::new("approved".into()).unwrap(),
                kind: MessageKind::Event,
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
}
