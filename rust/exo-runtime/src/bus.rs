//! `impl Bus for Runtime` — append-only ingestion-inbox delivery. **The genuinely-new
//! piece** (not adapted from a service).
//!
//! **Leaf R4.** The bus is *a jsonl file*: append a line, read new lines from a saved
//! byte-offset. NO queue abstraction, NO `exo-mailbox` crate.
//!
//! `deliver` only does the **append** half:
//! 1. Resolve `Addressee` → `InboxPath`:
//!    - `Parent` → `self.parent_inbox` (held in papers; `BusError::Unresolved` if `None`).
//!    - `Child(name)` → fold the parent's `children.jsonl`
//!      (`exo_caps::fold_children`) and look up the child's stored `inbox`.
//! 2. Wrap the policy [`Message`] in an [`IngestionEntry`]: stamp `from = Agent(self.name())`,
//!    `ts = Utc::now()`, `v = 1` (the runtime stamps the envelope — policy cannot spoof it).
//! 3. Serialize to one line + `\n`. If it would exceed `PIPE_BUF` (4096), **spill** (claim-check):
//!    write the full entry to a `.spill/` side-file (tmp+rename) and append a small pointer line
//!    instead (`IngestionEntry::spill = Some(path)`, a stub `msg`). So every inbox line stays
//!    ≤ PIPE_BUF (one atomic append) while the *payload* can be arbitrarily large — a rich review
//!    verdict, a long report. The reader (`inbound::resolve_spilled`) loads the side-file
//!    transparently. The side-file is written **before** the pointer (the append is the commit), so
//!    a crash between them orphans a harmless unreferenced file, never a torn line.
//! 4. Append with `OpenOptions::new().create(true).append(true)` + a single `write_all` of
//!    the whole line (one atomic `write(2)` since it's ≤ PIPE_BUF). **No fsync.** Assumes a
//!    local fs. Never read-modify-write the file.
//!
//! The **read/cursor/`notify`-watch** side is the inbound loop's job (Wave 2, N2b),
//! consuming this cap's appends — NOT implemented here. The cursor is a byte-offset
//! advanced via temp+rename; that lives with the reader, not the writer.
//!
//! HARD RULE: use `tokio::fs` (or `spawn_blocking`) — never block the executor.

use crate::runtime::Runtime;
use async_trait::async_trait;
use chrono::Utc;
use exo_caps::{
    Addressee, Bus, BusError, ChildState, InboxPath, IngestionEntry, Message, MessageBody,
    MessageKind, Persona, SpawnError,
};
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::io::AsyncWriteExt;
use tracing::{error, info};

/// Max bytes for one atomic inbox append. A `write(2)` of ≤ `PIPE_BUF` is atomic on a local fs, so
/// concurrent writers never interleave/tear. An entry that would exceed this is **spilled** to a
/// side-file and replaced by a small pointer line (see `Bus::deliver`).
const PIPE_BUF: usize = 4096;

/// Monotonic counter disambiguating spill filenames written by this process.
static SPILL_SEQ: AtomicU64 = AtomicU64::new(0);

impl Runtime {
    /// Resolve a policy-facing [`Addressee`] to the concrete inbox file to append to.
    /// Internal to the runtime — never exposed to policy.
    pub(crate) async fn resolve_inbox(&self, to: &Addressee) -> Result<InboxPath, BusError> {
        match to {
            Addressee::Parent => self
                .parent_inbox
                .clone()
                .ok_or_else(|| BusError::Unresolved(to.clone())),
            Addressee::Child(name) => {
                let children = self.read_children().await.map_err(|e| match e {
                    SpawnError::Io(io) => BusError::Io(io),
                    SpawnError::Failed { detail, .. } => {
                        BusError::Io(std::io::Error::other(detail))
                    }
                    SpawnError::UnknownChild(child) => {
                        BusError::Unresolved(Addressee::Child(child))
                    }
                })?;

                let Some(c) = children.get(name) else {
                    return Err(BusError::Unresolved(to.clone()));
                };
                // A tombstoned child's inbox has no reader, and its recorded pane may since have
                // been recycled onto a different live agent — fail loud rather than silently
                // appending into a black hole.
                match &c.state {
                    ChildState::Reaped => Err(BusError::Tombstoned {
                        child: name.clone(),
                        state: "reaped",
                    }),
                    ChildState::Died => Err(BusError::Tombstoned {
                        child: name.clone(),
                        state: "died",
                    }),
                    ChildState::Live | ChildState::Submitted { .. } => Ok(c.inbox.clone()),
                }
            }
        }
    }

    /// Claim-check overflow: write the full (oversized) `entry` to a side-file in a `.spill/` dir
    /// next to `inbox`, returning its path. The caller appends a small pointer line referencing it.
    /// Ordering is the safety guarantee — the side-file is fully written (tmp + rename, so a reader
    /// never sees a torn file) **before** the pointer is appended, and the pointer append is the
    /// commit point. A crash between the two orphans a harmless unreferenced side-file, never a torn
    /// inbox line.
    async fn write_spill(
        &self,
        inbox: &InboxPath,
        entry: &IngestionEntry,
    ) -> Result<String, BusError> {
        let body = serde_json::to_vec(entry).map_err(|e| BusError::Append {
            detail: e.to_string(),
        })?;
        let dir = inbox
            .as_path()
            .parent()
            .map(|p| p.join(".spill"))
            .ok_or_else(|| BusError::Append {
                detail: format!(
                    "inbox path {} has no parent for the .spill dir",
                    inbox.as_path().display()
                ),
            })?;
        tokio::fs::create_dir_all(&dir).await?;
        let seq = SPILL_SEQ.fetch_add(1, Ordering::Relaxed);
        let nanos = entry.ts.timestamp_nanos_opt().unwrap_or(0);
        let path = dir.join(format!("spill-{nanos}-{seq}.json"));
        let tmp = dir.join(format!("spill-{nanos}-{seq}.json.tmp"));
        tokio::fs::write(&tmp, &body).await?;
        tokio::fs::rename(&tmp, &path).await?;
        Ok(path.to_string_lossy().into_owned())
    }
}

#[async_trait]
impl Bus for Runtime {
    async fn deliver(&self, to: Addressee, msg: Message) -> Result<(), BusError> {
        let inbox = self.resolve_inbox(&to).await?;

        let summary = msg.summary.as_str().to_string();
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(self.name()),
            id: Some(uuid::Uuid::new_v4().to_string()),
            spill: None,
            msg,
        };

        let mut line = serde_json::to_string(&entry).map_err(|e| BusError::Append {
            detail: e.to_string(),
        })?;
        line.push('\n');

        if line.len() > PIPE_BUF {
            // Too big for one atomic append. Spill the full entry to a side-file and append a small
            // claim-check pointer instead (the reader resolves it transparently). The bus thus
            // delivers arbitrarily-large payloads — a rich review verdict, a long report — without
            // ever writing a non-atomic (>PIPE_BUF) inbox line.
            let spill_path = self.write_spill(&inbox, &entry).await?;
            let pointer = IngestionEntry {
                v: 1,
                ts: entry.ts,
                from: entry.from.clone(),
                // The pointer and the spilled side-file entry are ONE logical message — they must
                // share an id, not each mint their own.
                id: entry.id.clone(),
                spill: Some(spill_path.clone()),
                msg: Message {
                    text: MessageBody::new("[spilled: full content in side-file]".into())
                        .expect("static spill-stub body is valid"),
                    summary: entry.msg.summary.clone(),
                    kind: MessageKind::Chat,
                    reply_to: None,
                },
            };
            line = serde_json::to_string(&pointer).map_err(|e| BusError::Append {
                detail: e.to_string(),
            })?;
            line.push('\n');
            // The pointer is small by construction; assert defensively (a pathologically long inbox
            // path is the only way it could overflow).
            if line.len() > PIPE_BUF {
                return Err(BusError::Append {
                    detail: format!(
                        "spill pointer line {} bytes exceeds PIPE_BUF {} (inbox path too long?)",
                        line.len(),
                        PIPE_BUF
                    ),
                });
            }
            info!(to = %to, summary = %summary, spill = %spill_path, "Bus::deliver: oversized entry spilled to side-file");
        }

        let path = inbox.as_path();
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let mut file = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .await?;

        if let Err(e) = file.write_all(line.as_bytes()).await {
            error!(to = %to, summary = %summary, "Bus::deliver FAILED: {e}");
            return Err(e.into());
        }
        // tokio's File buffers and does NOT flush on drop — without this the line is lost.
        // This is a kernel-buffer flush, not fsync: the bytes reach the page cache (surviving
        // a process crash), matching the "no fsync" durability level.
        file.flush().await?;
        info!(to = %to, summary = %summary, "Bus::deliver OK");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{
        AgentName, Branch, ChildRecord, MessageBody, MessageKind, NodePath, PaneId, Summary,
    };
    use std::io::Write;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_deliver_parent() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("parent_inbox.jsonl");
        let inbox = InboxPath::new(inbox_path.clone());

        let node_path = NodePath::new(vec![AgentName::new("child".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let msg = Message {
            text: MessageBody::new("hello".into()).unwrap(),
            summary: Summary::new("hi".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };

        runtime
            .deliver(Addressee::Parent, msg.clone())
            .await
            .unwrap();

        let content = std::fs::read_to_string(&inbox_path).unwrap();
        let entry: IngestionEntry = serde_json::from_str(&content).unwrap();

        assert_eq!(entry.from, Persona::Agent(runtime.name()));
        assert_eq!(entry.msg, msg);
    }

    #[tokio::test]
    async fn test_deliver_spills_oversized_entry() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("parent_inbox.jsonl");
        let inbox = InboxPath::new(inbox_path.clone());

        let node_path = NodePath::new(vec![AgentName::new("child".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        // A body large enough that the serialized envelope exceeds PIPE_BUF (≈ a rich verdict).
        let large_body = "A".repeat(4000);
        let msg = Message {
            text: MessageBody::new(large_body.clone()).unwrap(),
            summary: Summary::new("big verdict".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };

        // Delivery SUCCEEDS via the claim-check spill (was an error before).
        runtime
            .deliver(Addressee::Parent, msg.clone())
            .await
            .unwrap();

        // The inbox holds exactly one line, and it fits a single atomic append.
        let content = std::fs::read_to_string(&inbox_path).unwrap();
        let lines: Vec<&str> = content.lines().collect();
        assert_eq!(lines.len(), 1);
        assert!(
            lines[0].len() < PIPE_BUF,
            "the pointer line must fit one atomic append"
        );

        // That line is a pointer; the side-file holds the full entry with the original body.
        let pointer: IngestionEntry = serde_json::from_str(lines[0]).unwrap();
        let spill = pointer
            .spill
            .expect("oversized entry carries a spill pointer");
        let full: IngestionEntry = serde_json::from_slice(&std::fs::read(&spill).unwrap()).unwrap();
        assert!(
            full.spill.is_none(),
            "the full entry carries no further pointer"
        );
        assert_eq!(full.msg, msg);
        assert_eq!(full.msg.text.as_str(), large_body);
    }

    #[tokio::test]
    async fn test_resolve_child_inbox() {
        let dir = tempdir().unwrap();
        let exo_dir = dir.path().join(".exo");
        std::fs::create_dir_all(&exo_dir).unwrap();
        let children_file = exo_dir.join("children.jsonl");

        let child_name = AgentName::new("kid".into()).unwrap();
        let child_inbox = InboxPath::new(dir.path().join("kid.jsonl"));

        let record = ChildRecord::Spawned {
            child: child_name.clone(),
            kind: exo_caps::ChildKind::Inline,
            pane: exo_caps::PaneId::new("%1".into()).unwrap(),
            inbox: child_inbox.clone(),
            model_label: None,
            model: None,
            directives_hash: None,
        };

        let line = serde_json::to_string(&record).unwrap() + "\n";
        std::fs::write(&children_file, line).unwrap();

        let node_path = NodePath::new(vec![AgentName::new("parent".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            None,
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%100".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let resolved = runtime
            .resolve_inbox(&Addressee::Child(child_name))
            .await
            .unwrap();
        assert_eq!(resolved, child_inbox);
    }

    #[tokio::test]
    async fn test_max_body_spills_and_inbox_line_stays_atomic() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("inbox.jsonl");
        let inbox = InboxPath::new(inbox_path.clone());

        let node_path = NodePath::new(vec![AgentName::new("node".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        // A max-size body GUARANTEES the envelope exceeds PIPE_BUF → must spill, never error.
        let msg = Message {
            text: MessageBody::new("A".repeat(MessageBody::MAX_LEN)).unwrap(),
            summary: Summary::new("overflow".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };
        runtime.deliver(Addressee::Parent, msg).await.unwrap();

        // Every byte of the inbox is ≤ PIPE_BUF (the atomic-append invariant holds).
        let content = std::fs::read_to_string(&inbox_path).unwrap();
        assert!(content.len() <= PIPE_BUF);
    }

    #[tokio::test]
    async fn test_small_message_does_not_spill() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("inbox.jsonl");
        let inbox = InboxPath::new(inbox_path.clone());

        let node_path = NodePath::new(vec![AgentName::new("node".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let msg = Message {
            text: MessageBody::new("hello".into()).unwrap(),
            summary: Summary::new("hi".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };
        runtime.deliver(Addressee::Parent, msg).await.unwrap();

        // The common path is byte-identical to before: no `spill` field on the wire, no .spill dir.
        let content = std::fs::read_to_string(&inbox_path).unwrap();
        let entry: IngestionEntry = serde_json::from_str(content.lines().next().unwrap()).unwrap();
        assert!(entry.spill.is_none());
        assert!(
            !content.contains("\"spill\""),
            "spill is omitted from the wire when None"
        );
        assert!(!dir.path().join(".spill").exists());
    }

    #[tokio::test]
    async fn test_deliver_all_addressee_variants() {
        let dir = tempdir().unwrap();
        let root = dir.path().to_path_buf();

        // 1. Setup Parent inbox
        let parent_inbox_path = root.join("parent.jsonl");
        let parent_inbox = InboxPath::new(parent_inbox_path.clone());

        // 2. Setup Children (Inline and Worktree)
        let exo_dir = root.join(".exo");
        std::fs::create_dir_all(&exo_dir).unwrap();
        let children_file = exo_dir.join("children.jsonl");

        let inline_name = AgentName::new("inline-kid".into()).unwrap();
        let inline_inbox_path = root.join("inline.jsonl");
        let inline_inbox = InboxPath::new(inline_inbox_path.clone());

        let worktree_name = AgentName::new("worktree-kid".into()).unwrap();
        let worktree_inbox_path = root.join("worktree.jsonl");
        let worktree_inbox = InboxPath::new(worktree_inbox_path.clone());

        let records = vec![
            ChildRecord::Spawned {
                child: inline_name.clone(),
                kind: exo_caps::ChildKind::Inline,
                pane: PaneId::new("%2".into()).unwrap(),
                inbox: inline_inbox,
                model_label: None,
                model: None,
                directives_hash: None,
            },
            ChildRecord::Spawned {
                child: worktree_name.clone(),
                kind: exo_caps::ChildKind::Worktree,
                pane: PaneId::new("%3".into()).unwrap(),
                inbox: worktree_inbox,
                model_label: None,
                model: None,
                directives_hash: None,
            },
        ];

        for r in records {
            let line = serde_json::to_string(&r).unwrap() + "\n";
            std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&children_file)
                .unwrap()
                .write_all(line.as_bytes())
                .unwrap();
        }

        let node_path = NodePath::new(vec![AgentName::new("me".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            root,
            Some(parent_inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let msg = Message {
            text: MessageBody::new("hi".into()).unwrap(),
            summary: Summary::new("greeting".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };

        // Deliver to Parent
        runtime
            .deliver(Addressee::Parent, msg.clone())
            .await
            .unwrap();
        assert!(parent_inbox_path.exists());

        // Deliver to Child (inline)
        runtime
            .deliver(Addressee::Child(inline_name), msg.clone())
            .await
            .unwrap();
        assert!(inline_inbox_path.exists());

        // Deliver to Child (worktree)
        runtime
            .deliver(Addressee::Child(worktree_name), msg.clone())
            .await
            .unwrap();
        assert!(worktree_inbox_path.exists());
    }

    #[tokio::test]
    async fn test_deliver_append_order() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("inbox.jsonl");
        let inbox = InboxPath::new(inbox_path.clone());

        let node_path = NodePath::new(vec![AgentName::new("node".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let msg1 = Message {
            text: MessageBody::new("first".into()).unwrap(),
            summary: Summary::new("1".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };
        let msg2 = Message {
            text: MessageBody::new("second".into()).unwrap(),
            summary: Summary::new("2".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };

        runtime
            .deliver(Addressee::Parent, msg1.clone())
            .await
            .unwrap();
        runtime
            .deliver(Addressee::Parent, msg2.clone())
            .await
            .unwrap();

        let content = std::fs::read_to_string(&inbox_path).unwrap();
        let mut lines = content.lines();

        let entry1: IngestionEntry = serde_json::from_str(lines.next().unwrap()).unwrap();
        let entry2: IngestionEntry = serde_json::from_str(lines.next().unwrap()).unwrap();

        assert_eq!(entry1.msg, msg1);
        assert_eq!(entry2.msg, msg2);
        assert!(lines.next().is_none());
    }

    #[tokio::test]
    async fn test_resolve_child_inbox_tolerates_malformed_line() {
        // A torn/garbage line in children.jsonl must NOT block resolution of the good children.
        let dir = tempdir().unwrap();
        let exo_dir = dir.path().join(".exo");
        std::fs::create_dir_all(&exo_dir).unwrap();

        let good_name = AgentName::new("kid".into()).unwrap();
        let good_inbox = InboxPath::new(dir.path().join("kid.jsonl"));
        let good = ChildRecord::Spawned {
            child: good_name.clone(),
            kind: exo_caps::ChildKind::Inline,
            pane: PaneId::new("%1".into()).unwrap(),
            inbox: good_inbox.clone(),
            model_label: None,
            model: None,
            directives_hash: None,
        };

        // garbage line, then a good record (mirrors a crash-torn append followed by a fresh one)
        let content = format!(
            "{{ not valid json\n{}\n",
            serde_json::to_string(&good).unwrap()
        );
        std::fs::write(exo_dir.join("children.jsonl"), content).unwrap();

        let node_path = NodePath::new(vec![AgentName::new("parent".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            None,
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%100".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let resolved = runtime
            .resolve_inbox(&Addressee::Child(good_name))
            .await
            .unwrap();
        assert_eq!(resolved, good_inbox);
    }

    #[tokio::test]
    async fn tombstoned_delivery_errs() {
        let dir = tempdir().unwrap();
        let exo_dir = dir.path().join(".exo");
        std::fs::create_dir_all(&exo_dir).unwrap();
        let children_file = exo_dir.join("children.jsonl");

        let child_name = AgentName::new("kid".into()).unwrap();
        let records = [
            ChildRecord::Spawned {
                child: child_name.clone(),
                kind: exo_caps::ChildKind::Inline,
                pane: PaneId::new("%1".into()).unwrap(),
                inbox: InboxPath::new(dir.path().join("kid.jsonl")),
                model_label: None,
                model: None,
                directives_hash: None,
            },
            ChildRecord::Reaped {
                child: child_name.clone(),
                at: None,
            },
        ];
        let body: String = records
            .iter()
            .map(|r| format!("{}\n", serde_json::to_string(r).unwrap()))
            .collect();
        std::fs::write(&children_file, body).unwrap();

        let node_path = NodePath::new(vec![AgentName::new("parent".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            None,
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%100".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let msg = Message {
            text: MessageBody::new("hi".into()).unwrap(),
            summary: Summary::new("hi".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };
        match runtime
            .deliver(Addressee::Child(child_name.clone()), msg)
            .await
        {
            Err(BusError::Tombstoned { child, state }) => {
                assert_eq!(child, child_name);
                assert_eq!(state, "reaped");
            }
            other => panic!("expected Tombstoned, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn every_delivery_carries_an_id() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("inbox.jsonl");
        let inbox = InboxPath::new(inbox_path.clone());

        let node_path = NodePath::new(vec![AgentName::new("node".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let msg = Message {
            text: MessageBody::new("hello".into()).unwrap(),
            summary: Summary::new("hi".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };
        runtime.deliver(Addressee::Parent, msg).await.unwrap();

        let content = std::fs::read_to_string(&inbox_path).unwrap();
        let entry: IngestionEntry = serde_json::from_str(content.lines().next().unwrap()).unwrap();
        assert!(entry.id.is_some(), "every delivery must carry a fresh id");
    }

    #[tokio::test]
    async fn spill_pointer_id_matches_spilled_entry() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("inbox.jsonl");
        let inbox = InboxPath::new(inbox_path.clone());

        let node_path = NodePath::new(vec![AgentName::new("node".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let msg = Message {
            text: MessageBody::new("A".repeat(4000)).unwrap(),
            summary: Summary::new("big".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: None,
        };
        runtime.deliver(Addressee::Parent, msg).await.unwrap();

        let content = std::fs::read_to_string(&inbox_path).unwrap();
        let pointer: IngestionEntry =
            serde_json::from_str(content.lines().next().unwrap()).unwrap();
        let spill_path = pointer.spill.clone().expect("oversized entry spills");
        let spilled: IngestionEntry =
            serde_json::from_slice(&std::fs::read(&spill_path).unwrap()).unwrap();

        assert!(pointer.id.is_some());
        assert_eq!(
            pointer.id, spilled.id,
            "the pointer and its spilled side-file entry are one logical message and must share an id"
        );
    }
}
