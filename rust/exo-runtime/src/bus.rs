//! `impl Bus for Runtime` — append-only ingestion-inbox delivery. **The genuinely-new
//! piece** (not adapted from a service).
//!
//! **Leaf R4.** The bus is *a jsonl file*: append a line, read new lines from a saved
//! byte-offset. NO queue abstraction, NO `exo-mailbox` crate.
//!
//! `deliver` only does the **append** half:
//! 1. Resolve `Addressee` → `InboxPath`:
//!    - `Parent` → `self.parent_inbox` (held in papers; `BusError::Unresolved` if `None`).
//!    - `InlineChild(name)` / `WorktreeChild(name)` → fold the parent's `children.jsonl`
//!      (`exo_caps::fold_children`) and look up the child's stored `inbox`.
//! 2. Wrap the policy [`Message`] in an [`IngestionEntry`]: stamp `from = Agent(self.name())`,
//!    `ts = Utc::now()`, `v = 1` (the runtime stamps the envelope — policy cannot spoof it).
//! 3. Serialize to one line + `\n`. **Assert the serialized line ≤ `PIPE_BUF` (4096)**;
//!    error (`BusError::Append`) if it would overflow — the bus NEVER spills. Bulk content
//!    is a sender-written side-file referenced by path, never inlined.
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
use exo_caps::{Addressee, Bus, BusError, InboxPath, IngestionEntry, Message, Persona, SpawnError};
use tokio::io::AsyncWriteExt;
use tracing::{error, info};

impl Runtime {
    /// Resolve a policy-facing [`Addressee`] to the concrete inbox file to append to.
    /// Internal to the runtime — never exposed to policy.
    pub(crate) async fn resolve_inbox(&self, to: &Addressee) -> Result<InboxPath, BusError> {
        match to {
            Addressee::Parent => self
                .parent_inbox
                .clone()
                .ok_or_else(|| BusError::Unresolved(to.clone())),
            Addressee::InlineChild(name) | Addressee::WorktreeChild(name) => {
                let children = self.read_children().await.map_err(|e| match e {
                    SpawnError::Io(io) => BusError::Io(io),
                    SpawnError::Failed { detail, .. } => {
                        BusError::Io(std::io::Error::other(detail))
                    }
                })?;

                children
                    .get(name)
                    .map(|c| c.inbox.clone())
                    .ok_or_else(|| BusError::Unresolved(to.clone()))
            }
        }
    }
}

#[async_trait]
impl Bus for Runtime {
    async fn deliver(&self, to: Addressee, msg: Message) -> Result<(), BusError> {
        let inbox = self.resolve_inbox(&to).await?;

        // A delivery down to a child is a poke that will wake it — mark it busy so the idle gate
        // doesn't treat a just-poked child as idle (paired with `mark_child_idle` on `ChildIdle`).
        if let Addressee::InlineChild(name) | Addressee::WorktreeChild(name) = &to {
            self.mark_child_busy(name);
        }

        let summary = msg.summary.as_str().to_string();
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(self.name()),
            msg,
        };

        let mut line = serde_json::to_string(&entry).map_err(|e| BusError::Append {
            detail: e.to_string(),
        })?;
        line.push('\n');

        const PIPE_BUF: usize = 4096;
        if line.len() > PIPE_BUF {
            return Err(BusError::Append {
                detail: format!("line {} bytes exceeds PIPE_BUF {}", line.len(), PIPE_BUF),
            });
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
            error!(to = ?to, summary = %summary, "Bus::deliver FAILED: {e}");
            return Err(e.into());
        }
        // tokio's File buffers and does NOT flush on drop — without this the line is lost.
        // This is a kernel-buffer flush, not fsync: the bytes reach the page cache (surviving
        // a process crash), matching the "no fsync" durability level.
        file.flush().await?;
        info!(to = ?to, summary = %summary, "Bus::deliver OK");
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
        );

        let msg = Message {
            text: MessageBody::new("hello".into()).unwrap(),
            summary: Summary::new("hi".into()).unwrap(),
            kind: MessageKind::Chat,
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
    async fn test_deliver_overflow() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("parent_inbox.jsonl");
        let inbox = InboxPath::new(inbox_path);

        let node_path = NodePath::new(vec![AgentName::new("child".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            Some(inbox),
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
        );

        // Build a body that is large enough that when combined with the envelope it exceeds 4096.
        let large_body = "A".repeat(4000);
        let msg = Message {
            text: MessageBody::new(large_body).unwrap(),
            summary: Summary::new("large".into()).unwrap(),
            kind: MessageKind::Chat,
        };

        let res = runtime.deliver(Addressee::Parent, msg).await;
        match res {
            Err(BusError::Append { detail }) => assert!(detail.contains("exceeds PIPE_BUF")),
            _ => panic!("Expected Append error, got {:?}", res),
        }
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
        );

        let resolved = runtime
            .resolve_inbox(&Addressee::InlineChild(child_name))
            .await
            .unwrap();
        assert_eq!(resolved, child_inbox);
    }

    #[tokio::test]
    async fn test_deliver_overflow_no_spill() {
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
        );

        // Max MessageBody is 4096, which GUARANTEES the total line exceeds 4096.
        let msg = Message {
            text: MessageBody::new("A".repeat(MessageBody::MAX_LEN)).unwrap(),
            summary: Summary::new("overflow".into()).unwrap(),
            kind: MessageKind::Chat,
        };

        let res = runtime.deliver(Addressee::Parent, msg).await;
        match res {
            Err(BusError::Append { detail }) => assert!(detail.contains("exceeds PIPE_BUF")),
            _ => panic!("Expected Append error, got {:?}", res),
        }

        // NO-SPILL: The file must not exist or be empty.
        if inbox_path.exists() {
            let content = std::fs::read(&inbox_path).unwrap();
            assert!(content.is_empty(), "File should be empty on overflow");
        }
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
            },
            ChildRecord::Spawned {
                child: worktree_name.clone(),
                kind: exo_caps::ChildKind::Worktree,
                pane: PaneId::new("%3".into()).unwrap(),
                inbox: worktree_inbox,
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
        );

        let msg = Message {
            text: MessageBody::new("hi".into()).unwrap(),
            summary: Summary::new("greeting".into()).unwrap(),
            kind: MessageKind::Chat,
        };

        // Deliver to Parent
        runtime
            .deliver(Addressee::Parent, msg.clone())
            .await
            .unwrap();
        assert!(parent_inbox_path.exists());

        // Deliver to InlineChild
        runtime
            .deliver(Addressee::InlineChild(inline_name), msg.clone())
            .await
            .unwrap();
        assert!(inline_inbox_path.exists());

        // Deliver to WorktreeChild
        runtime
            .deliver(Addressee::WorktreeChild(worktree_name), msg.clone())
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
        );

        let msg1 = Message {
            text: MessageBody::new("first".into()).unwrap(),
            summary: Summary::new("1".into()).unwrap(),
            kind: MessageKind::Chat,
        };
        let msg2 = Message {
            text: MessageBody::new("second".into()).unwrap(),
            summary: Summary::new("2".into()).unwrap(),
            kind: MessageKind::Chat,
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
        );

        let resolved = runtime
            .resolve_inbox(&Addressee::InlineChild(good_name))
            .await
            .unwrap();
        assert_eq!(resolved, good_inbox);
    }
}
