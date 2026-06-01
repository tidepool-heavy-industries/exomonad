//! **N2b — Inbound loop.** Drive the Bus READ side of this node's own ingestion inbox — the
//! cursor/restart half the `Bus` cap (write side) left for Wave 2. Per
//! `docs/design/swarm/02-bus-and-sidecar.md` *Cursor & restart*, implement EXACTLY:
//!
//! - **Cursor = byte-offset** in a sibling `pane-N.cursor`. Resume = seek + read forward, O(1).
//! - **Watch via the `notify` crate** (event-driven, never a poll loop, never hand-rolled
//!   inotify); on each wake re-read from the cursor (absorbs coalesced events).
//! - **Read only up to the last `\n`** — a torn trailing line is re-read once complete.
//! - **Advance the cursor AFTER a successful last-hop delivery**, written **temp + rename**
//!   (atomic replace — a "small" overwrite is NOT crash-atomic). At-least-once, never dropped/corrupted.
//! - **Missing cursor** (fresh node) → start at current EOF; don't replay history.
//! - Parse each line as [`IngestionEntry`] (tolerant: serde defaults, no `deny_unknown_fields`).
//!
//! Then route each new entry by `kind`:
//! - `Chat` → [`crate::dispatch::dispatch`] (N2a last-hop).
//! - `Event` → parse the body into [`exo_policy::WorldEvent`] → `exo_policy::on_world_event`
//!   → act (`InjectMessage` = append to own inbox; `NotifyParent` = append to parent inbox).
//! - `Control(Shutdown { grace_ms })` → after the grace, self-kill OWN pane (the node knows
//!   `$TMUX_PANE`) — reaping pane + agent + sidecar in one shot.

use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use chrono::Utc;
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::mpsc;
use tracing::{error, info, warn};

use exo_caps::{ControlKind, IngestionEntry, Message, MessageKind, Persona, SyntheticName};
use exo_policy::events::{on_world_event, EventAction, WorldEvent};

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Watch the node's own ingestion inbox and route each new entry until shutdown.
pub async fn watch(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let inbox_path = ctx.own_inbox.as_path().to_path_buf();
    let cursor_path = inbox_path.with_extension("cursor");

    // Initialize cursor
    let mut offset = if cursor_path.exists() {
        match std::fs::read_to_string(&cursor_path) {
            Ok(s) => s.trim().parse::<u64>().unwrap_or_else(|_| {
                warn!("malformed cursor at {:?}, starting at EOF", cursor_path);
                get_eof(&inbox_path)
            }),
            Err(e) => {
                warn!(
                    "failed to read cursor at {:?}: {}, starting at EOF",
                    cursor_path, e
                );
                get_eof(&inbox_path)
            }
        }
    } else {
        let eof = get_eof(&inbox_path);
        save_cursor(&cursor_path, eof)?;
        eof
    };

    info!(
        "starting inbound loop for {:?} at offset {}",
        inbox_path, offset
    );

    // Setup notify watcher
    let (tx, mut rx) = mpsc::channel(100);
    let mut watcher = RecommendedWatcher::new(
        move |res: notify::Result<Event>| {
            if let Ok(event) = res {
                if event.kind.is_modify() || event.kind.is_create() {
                    let _ = tx.blocking_send(());
                }
            }
        },
        Config::default(),
    )
    .map_err(std::io::Error::other)?;

    // Watch the parent directory because watching a file directly can be unreliable
    // with some editors/tools that use rename-over-original.
    if let Some(parent) = inbox_path.parent() {
        watcher
            .watch(parent, RecursiveMode::NonRecursive)
            .map_err(std::io::Error::other)?;
    }

    let handler = RealHandler { ctx: ctx.clone() };

    // Initial pass to catch anything already there
    process_inbox(&handler, &inbox_path, &cursor_path, &mut offset).await?;

    while let Some(()) = rx.recv().await {
        // Drain any coalesced events
        while rx.try_recv().is_ok() {}

        if process_inbox(&handler, &inbox_path, &cursor_path, &mut offset).await? {
            // Shutdown received
            break;
        }
    }

    Ok(())
}

fn get_eof(path: &Path) -> u64 {
    File::open(path)
        .and_then(|f| f.metadata())
        .map(|m| m.len())
        .unwrap_or(0)
}

fn save_cursor(path: &Path, offset: u64) -> std::io::Result<()> {
    let tmp_path = path.with_extension("cursor.tmp");
    {
        let mut f = File::create(&tmp_path)?;
        writeln!(f, "{}", offset)?;
        f.sync_all()?; // Ensure it's on disk before rename
    }
    std::fs::rename(tmp_path, path)
}

#[async_trait]
trait InboundHandler {
    async fn handle(&self, entry: &IngestionEntry) -> NodeResult<Option<bool>>;
    async fn append_self(&self, text: String, summary: String) -> NodeResult<()>;
    async fn append_parent(&self, text: String, summary: String) -> NodeResult<()>;
}

struct RealHandler {
    ctx: Arc<NodeContext>,
}

#[async_trait]
impl InboundHandler for RealHandler {
    async fn handle(&self, entry: &IngestionEntry) -> NodeResult<Option<bool>> {
        match &entry.msg.kind {
            MessageKind::Chat => {
                crate::dispatch::dispatch(&self.ctx, entry).await?;
                Ok(Some(false))
            }
            MessageKind::Event => {
                let world_event: WorldEvent = match serde_json::from_str(entry.msg.text.as_str()) {
                    Ok(ev) => ev,
                    Err(e) => {
                        warn!("failed to parse WorldEvent from entry: {}", e);
                        return Ok(None);
                    }
                };

                let action = on_world_event(&*self.ctx.runtime, &world_event).await;
                match action {
                    EventAction::InjectMessage { text, summary } => {
                        self.append_self(text, summary).await?;
                    }
                    EventAction::NotifyParent { text, summary } => {
                        self.append_parent(text, summary).await?;
                    }
                    EventAction::NoAction => {}
                }
                Ok(Some(false))
            }
            MessageKind::Control(ControlKind::Shutdown { grace_ms }) => {
                info!("shutdown received, sleeping {}ms", grace_ms);
                tokio::time::sleep(Duration::from_millis(*grace_ms as u64)).await;
                exo_caps::Tmux::kill_pane(&*self.ctx.runtime, &self.ctx.own_pane)
                    .await
                    .map_err(|e| std::io::Error::other(e.to_string()))?;
                Ok(Some(true))
            }
        }
    }

    async fn append_self(&self, text: String, summary: String) -> NodeResult<()> {
        append_to_inbox_file(self.ctx.own_inbox.as_path(), text, summary)
    }

    async fn append_parent(&self, text: String, summary: String) -> NodeResult<()> {
        if let Some(parent_inbox) = &self.ctx.parent_inbox {
            append_to_inbox_file(parent_inbox.as_path(), text, summary)
        } else {
            warn!("received NotifyParent action but no parent_inbox configured");
            Ok(())
        }
    }
}

fn append_to_inbox_file(path: &Path, text: String, summary: String) -> NodeResult<()> {
    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        from: Persona::Synthetic(
            SyntheticName::new("self".to_string())
                .map_err(|e| std::io::Error::other(e.to_string()))?,
        ),
        msg: Message {
            text: exo_caps::MessageBody::new(text)
                .map_err(|e| std::io::Error::other(e.to_string()))?,
            summary: exo_caps::Summary::new(summary)
                .map_err(|e| std::io::Error::other(e.to_string()))?,
            kind: MessageKind::Chat,
        },
    };

    let mut line = serde_json::to_vec(&entry)
        .map_err(|e| std::io::Error::other(format!("json serialize failed: {}", e)))?;
    line.push(b'\n');

    let mut file = OpenOptions::new().append(true).create(true).open(path)?;
    file.write_all(&line)?;
    Ok(())
}

/// Returns true if shutdown was requested
async fn process_inbox<H: InboundHandler>(
    handler: &H,
    inbox_path: &Path,
    cursor_path: &Path,
    offset: &mut u64,
) -> NodeResult<bool> {
    let mut file = match File::open(inbox_path) {
        Ok(f) => f,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(e) => return Err(e.into()),
    };

    let file_len = file.metadata()?.len();
    if *offset >= file_len {
        return Ok(false);
    }

    file.seek(SeekFrom::Start(*offset))?;

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    // Find the last newline to avoid processing torn lines
    let last_newline = match buffer.iter().rposition(|&b| b == b'\n') {
        Some(pos) => pos,
        None => return Ok(false), // No complete lines
    };

    let complete_data = &buffer[..=last_newline];

    for line_bytes in complete_data.split(|&b| b == b'\n') {
        if line_bytes.is_empty() {
            continue;
        }

        let line_len = line_bytes.len() as u64;
        let entry: IngestionEntry = match serde_json::from_slice(line_bytes) {
            Ok(e) => e,
            Err(e) => {
                warn!("failed to parse ingestion entry: {}", e);
                // Advance past malformed line
                *offset += line_len + 1;
                save_cursor(cursor_path, *offset)?;
                continue;
            }
        };

        match handler.handle(&entry).await {
            Ok(Some(true)) => {
                // Shutdown
                *offset += line_len + 1;
                save_cursor(cursor_path, *offset)?;
                return Ok(true);
            }
            Ok(_) => {
                // Success (or no-op), advance cursor
                *offset += line_len + 1;
                save_cursor(cursor_path, *offset)?;
            }
            Err(e) => {
                error!("failed to route entry: {}. will retry on next wake", e);
                // DO NOT advance cursor. Break batch to retry later.
                return Ok(false);
            }
        }
    }

    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, MessageBody, Summary};
    use std::sync::Mutex;
    use tempfile::tempdir;

    struct MockHandler {
        delivered: Arc<Mutex<Vec<IngestionEntry>>>,
        fail_on: Option<String>,
    }

    #[async_trait]
    impl InboundHandler for MockHandler {
        async fn handle(&self, entry: &IngestionEntry) -> NodeResult<Option<bool>> {
            if let Some(fail_text) = &self.fail_on {
                if entry.msg.text.as_str() == fail_text {
                    return Err(std::io::Error::other("mock failure").into());
                }
            }
            self.delivered.lock().unwrap().push(entry.clone());
            Ok(Some(false))
        }
        async fn append_self(&self, _text: String, _summary: String) -> NodeResult<()> {
            Ok(())
        }
        async fn append_parent(&self, _text: String, _summary: String) -> NodeResult<()> {
            Ok(())
        }
    }

    fn write_entry(path: &Path, text: &str) {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("test".to_string()).unwrap()),
            msg: Message {
                text: MessageBody::new(text.to_string()).unwrap(),
                summary: Summary::new("test".to_string()).unwrap(),
                kind: MessageKind::Chat,
            },
        };
        let mut line = serde_json::to_vec(&entry).unwrap();
        line.push(b'\n');
        let mut f = OpenOptions::new()
            .append(true)
            .create(true)
            .open(path)
            .unwrap();
        f.write_all(&line).unwrap();
    }

    #[tokio::test]
    async fn test_process_inbox_basic() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = dir.path().join("pane-1.cursor");
        let mut offset = 0;

        write_entry(&inbox_path, "one");
        write_entry(&inbox_path, "two");
        write_entry(&inbox_path, "three");

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };

        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
            .await
            .unwrap();

        let d = delivered.lock().unwrap();
        assert_eq!(d.len(), 3);
        assert_eq!(d[0].msg.text.as_str(), "one");
        assert_eq!(d[2].msg.text.as_str(), "three");
        assert_eq!(offset, get_eof(&inbox_path));
    }

    #[tokio::test]
    async fn test_process_inbox_torn_line() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = dir.path().join("pane-1.cursor");
        let mut offset = 0;

        write_entry(&inbox_path, "one");
        // Write partial line without newline
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("test".to_string()).unwrap()),
            msg: Message {
                text: MessageBody::new("partial".to_string()).unwrap(),
                summary: Summary::new("test".to_string()).unwrap(),
                kind: MessageKind::Chat,
            },
        };
        let line = serde_json::to_vec(&entry).unwrap();
        let mut f = OpenOptions::new()
            .append(true)
            .create(true)
            .open(&inbox_path)
            .unwrap();
        f.write_all(&line).unwrap();

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };

        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
            .await
            .unwrap();

        let d = delivered.lock().unwrap();
        assert_eq!(d.len(), 1);
        assert_eq!(d[0].msg.text.as_str(), "one");
        // Cursor should be at the end of the first line
        let first_line_len =
            File::open(&inbox_path).unwrap().metadata().unwrap().len() - line.len() as u64;
        assert_eq!(offset, first_line_len);
    }

    #[tokio::test]
    async fn test_process_inbox_at_least_once() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = dir.path().join("pane-1.cursor");
        let mut offset = 0;

        write_entry(&inbox_path, "one");
        write_entry(&inbox_path, "two");

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: Some("two".to_string()),
        };

        // Should deliver "one", fail on "two", and NOT advance cursor past "two"
        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
            .await
            .unwrap();

        {
            let d = delivered.lock().unwrap();
            assert_eq!(d.len(), 1);
            assert_eq!(d[0].msg.text.as_str(), "one");
        }

        // Offset should be after "one" but before "two"
        // Let's find real offset
        let f = File::open(&inbox_path).unwrap();
        let mut reader = std::io::BufReader::new(f);
        let mut line = String::new();
        std::io::BufRead::read_line(&mut reader, &mut line).unwrap();
        let expected_offset = line.len() as u64;
        assert_eq!(offset, expected_offset);

        // Second pass with NO failure
        let handler2 = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };
        process_inbox(&handler2, &inbox_path, &cursor_path, &mut offset)
            .await
            .unwrap();

        {
            let d = delivered.lock().unwrap();
            assert_eq!(d.len(), 2);
            assert_eq!(d[1].msg.text.as_str(), "two");
        }
        assert_eq!(offset, get_eof(&inbox_path));
    }

    #[tokio::test]
    async fn test_missing_cursor_starts_at_eof() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = inbox_path.with_extension("cursor");

        write_entry(&inbox_path, "pre-existing");

        // Simulate watch(ctx) start
        let mut offset = get_eof(&inbox_path);
        save_cursor(&cursor_path, offset).unwrap();

        write_entry(&inbox_path, "new");

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };

        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
            .await
            .unwrap();

        let d = delivered.lock().unwrap();
        assert_eq!(d.len(), 1);
        assert_eq!(d[0].msg.text.as_str(), "new");
    }
}
