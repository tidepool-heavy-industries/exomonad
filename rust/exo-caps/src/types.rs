//! Domain types: validated newtypes + the identity/message vocabulary.
//!
//! Newtypes validate at construction (`new` → `CapResult`); once built, always valid
//! (per `.claude/rules/rust.md`). **Serde deserializes *through* the constructor**
//! (`#[serde(try_from)]`), so a value read from disk (papers, a bus line) is validated
//! too — there is no "transparent" hole that lets an invalid value in. Serialization is
//! transparent (the inner value). See docs 01/03.

use crate::error::{CapError, CapResult};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

// ── identity newtypes ────────────────────────────────────────────────────────

/// Tree address; `name()` = last segment, `parent()` = prefix. A **list**, not a
/// dot-string — branch names may contain `.`, so a joined form can't round-trip.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "Vec<String>")]
pub struct NodePath(Vec<String>);

impl NodePath {
    pub fn new(segments: Vec<String>) -> CapResult<Self> {
        if segments.is_empty() {
            return Err(CapError::invalid("NodePath", "must have at least one segment"));
        }
        if let Some(bad) = segments
            .iter()
            .find(|s| s.is_empty() || s.contains('/') || s.contains('\\'))
        {
            return Err(CapError::invalid(
                "NodePath",
                format!("segment empty or contains a path separator: {bad:?}"),
            ));
        }
        Ok(NodePath(segments))
    }
    pub fn segments(&self) -> &[String] {
        &self.0
    }
    /// The node's own name = the last segment. (Always valid: a `NodePath` is non-empty
    /// and its segments carry no path separators, so this satisfies `AgentName`.)
    pub fn name(&self) -> AgentName {
        AgentName(self.0.last().cloned().unwrap_or_default())
    }
    /// The parent address = prefix (the tree is prefix-containment); `None` for the root.
    pub fn parent(&self) -> Option<NodePath> {
        (self.0.len() > 1).then(|| NodePath(self.0[..self.0.len() - 1].to_vec()))
    }
    /// Extend one level: `self ++ [name]`.
    pub fn child(&self, name: &AgentName) -> NodePath {
        let mut s = self.0.clone();
        s.push(name.0.clone());
        NodePath(s)
    }
}

impl TryFrom<Vec<String>> for NodePath {
    type Error = CapError;
    fn try_from(v: Vec<String>) -> CapResult<Self> {
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
            return Err(CapError::invalid("Branch", format!("not a valid git ref name: {s:?}")));
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
                seg.chars()
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
            return Err(CapError::invalid("PaneId", format!("not a %N pane id: {s:?}")));
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

/// A message `id` (ulid) — minted by the [`Clock`](crate::Clock) at append, for
/// ordering / optional dedup. NOT the cursor (the cursor is a byte-offset — doc 02).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct MessageId(String);

impl MessageId {
    pub fn new(s: String) -> CapResult<Self> {
        if s.is_empty() {
            return Err(CapError::invalid("MessageId", "must be non-empty"));
        }
        Ok(MessageId(s))
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for MessageId {
    type Error = CapError;
    fn try_from(s: String) -> CapResult<Self> {
        MessageId::new(s)
    }
}

/// Plain message body; bounded length, no C0 control chars except `\t \n \r`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String")]
pub struct MessageBody(String);

impl MessageBody {
    /// 1 MiB — generous for an agent message, a guard against a runaway body.
    pub const MAX_LEN: usize = 1 << 20;

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

// ── archetype & runtime ──────────────────────────────────────────────────────

/// A node's archetype — the **one** stored identity enum. `role` (the `role_def` key)
/// is the variant; `agent_type` **derives**. Only the four real archetypes are
/// representable, so `(Root, Gemini)` / `(Worker, Claude)` are unnameable. See doc 03.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NodeKind {
    Root,
    Tl,
    Dev,
    Worker,
}

impl NodeKind {
    /// Runtime derives from the archetype — never stored separately.
    pub fn agent_type(self) -> AgentType {
        match self {
            NodeKind::Root | NodeKind::Tl => AgentType::Claude,
            NodeKind::Dev | NodeKind::Worker => AgentType::Gemini,
        }
    }
    /// The `role_def` key / wire string.
    pub fn role_str(self) -> &'static str {
        match self {
            NodeKind::Root => "root",
            NodeKind::Tl => "tl",
            NodeKind::Dev => "dev",
            NodeKind::Worker => "worker",
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

/// A bus message — plain-text body + a `kind` tag (the only structure). `id`/`ts` are
/// stamped by the runtime at append (the cursor is a byte-offset, NOT the `id`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Message {
    pub from: Persona,
    pub text: MessageBody,
    pub summary: String,
    pub kind: MessageKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageKind {
    /// peer/agent message.
    Chat,
    /// a world event — routed to `on_world_event`, which parses the body into a typed
    /// `WorldEvent` (in `exo-policy`). Bare tag: the detail rides the plain-text body,
    /// not the enum (keeps the body CC-last-hop-friendly).
    Event,
    /// lifecycle (exomonad-internal) — see `ControlKind`.
    Control(ControlKind),
}

/// A directed control **message**. Lifecycle **records** (`AgentSpawned`/`AgentStarted`)
/// live in the json record log, not here.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ControlKind {
    Shutdown { grace_ms: u32 },
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

    #[test]
    fn node_path_name_parent_child() {
        let p = NodePath::new(vec!["dev".into(), "auth-claude".into(), "oauth-gemini".into()])
            .unwrap();
        assert_eq!(p.name().as_str(), "oauth-gemini");
        assert_eq!(p.parent().unwrap().name().as_str(), "auth-claude");
        let root = NodePath::new(vec!["dev".into()]).unwrap();
        assert!(root.parent().is_none());
        let kid = root.child(&AgentName::new("auth-claude".into()).unwrap());
        assert_eq!(kid.segments(), &["dev", "auth-claude"]);
    }

    #[test]
    fn newtypes_reject_bad_input() {
        assert!(NodePath::new(vec![]).is_err());
        assert!(NodePath::new(vec!["a/b".into()]).is_err());
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
        let p = NodePath::new(vec!["dev".into(), "v1.2".into()]).unwrap();
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
    fn message_round_trips() {
        let m = Message {
            from: Persona::Synthetic(SyntheticName::new("github".into()).unwrap()),
            text: MessageBody::new("PR #5 approved".into()).unwrap(),
            summary: "approved".into(),
            kind: MessageKind::Event,
        };
        let json = serde_json::to_string(&m).unwrap();
        let back: Message = serde_json::from_str(&json).unwrap();
        assert_eq!(m, back);
    }
}
