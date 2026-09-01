//! Listen-channel server (N6) — runs in the sidecar; owns the socket and the connected-client
//! slot that `dispatch` delivers through.

use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration;

use exo_framework::Exomonad;
use exo_runtime::Runtime;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader, BufWriter};
use tokio::net::unix::{OwnedReadHalf, OwnedWriteHalf};
use tokio::net::UnixListener;
use tokio::sync::mpsc;
use tracing::{info, warn};

use super::{ListenAck, ListenFrame};
use crate::bootstrap::NodeContext;
use crate::error::{NodeError, NodeResult};

/// How long a delivered frame may wait for its ack. The client's obligation is
/// print + flush + one-line ack — microseconds; this covers scheduler stalls while keeping the
/// worst pre-retry latency well under the inbound loop's 15s retry tick.
const ACK_TIMEOUT: Duration = Duration::from_secs(2);

/// A delivery attempt over the listen channel failed. Both variants leave the inbound cursor
/// unadvanced (via `dispatch` erring) so the entry is retried — the bus queues until a live
/// client acks.
#[derive(Debug, thiserror::Error)]
pub enum ListenDeliverError {
    /// No `exo listen` client is attached — the agent hasn't armed (or re-armed) its Monitor.
    #[error("no listener attached")]
    NoListener,
    /// A client is attached but the frame couldn't be written or wasn't acked in time; the
    /// connection is presumed dead and the slot has been cleared.
    #[error("listener delivery failed: {0}")]
    AckFailed(String),
}

/// The connected-client slot. At most one client delivers at a time (**latest-wins** — see
/// [`serve`]); deliveries are serialized under the inner mutex, so frames never interleave and
/// `seq` is strictly increasing per connection.
pub struct ListenerSlot {
    inner: tokio::sync::Mutex<Option<ListenerHandle>>,
    /// Cheap lock-free read for the status publisher and senders.
    connected: AtomicBool,
    /// Monotonic connection counter — guards a stale reader's clear-on-EOF against clobbering
    /// a replacement connection installed after it.
    generation: AtomicU64,
}

struct ListenerHandle {
    gen: u64,
    next_seq: u64,
    writer: BufWriter<OwnedWriteHalf>,
    acks: mpsc::UnboundedReceiver<u64>,
}

impl std::fmt::Debug for ListenerSlot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ListenerSlot")
            .field("connected", &self.is_connected())
            .finish()
    }
}

impl Default for ListenerSlot {
    fn default() -> Self {
        Self::new()
    }
}

impl ListenerSlot {
    pub fn new() -> Self {
        Self {
            inner: tokio::sync::Mutex::new(None),
            connected: AtomicBool::new(false),
            generation: AtomicU64::new(0),
        }
    }

    pub fn is_connected(&self) -> bool {
        self.connected.load(Ordering::SeqCst)
    }

    /// Install a freshly-accepted client (latest-wins: any previous handle is dropped, so the
    /// previous client's read side sees EOF and it exits cleanly — and its writer being dropped
    /// means no frame can ever reach it after the swap). Returns the connection's generation and
    /// the ack sender its reader task feeds.
    pub(super) async fn install(
        &self,
        writer: OwnedWriteHalf,
    ) -> (u64, mpsc::UnboundedSender<u64>) {
        let gen = self.generation.fetch_add(1, Ordering::SeqCst) + 1;
        let (ack_tx, acks) = mpsc::unbounded_channel();
        let handle = ListenerHandle {
            gen,
            next_seq: 0,
            writer: BufWriter::new(writer),
            acks,
        };
        *self.inner.lock().await = Some(handle);
        self.connected.store(true, Ordering::SeqCst);
        (gen, ack_tx)
    }

    /// Clear the slot iff it still holds connection `gen` — a reader observing its connection's
    /// EOF must not clobber a newer connection installed after it.
    pub(super) async fn clear_if_gen(&self, gen: u64) {
        let mut guard = self.inner.lock().await;
        if guard.as_ref().map(|h| h.gen) == Some(gen) {
            *guard = None;
            self.connected.store(false, Ordering::SeqCst);
        }
    }

    /// Deliver one payload to the attached client and await its ack. `Ok(())` means the client
    /// flushed the payload to stdout — the caller may advance the inbound cursor. Any failure
    /// clears the slot (the connection is presumed dead) and errs, leaving the cursor pinned.
    pub async fn try_deliver(&self, text: &str) -> Result<(), ListenDeliverError> {
        let mut guard = self.inner.lock().await;
        let handle = guard.as_mut().ok_or(ListenDeliverError::NoListener)?;

        handle.next_seq += 1;
        let seq = handle.next_seq;
        let frame = ListenFrame {
            seq,
            text: text.to_string(),
        };
        let mut line = serde_json::to_vec(&frame)
            .map_err(|e| ListenDeliverError::AckFailed(format!("encode frame: {e}")))?;
        line.push(b'\n');

        let write = async {
            handle.writer.write_all(&line).await?;
            handle.writer.flush().await
        };
        if let Err(e) = write.await {
            Self::clear_locked(&mut guard, &self.connected);
            return Err(ListenDeliverError::AckFailed(format!("write frame: {e}")));
        }

        loop {
            match tokio::time::timeout(ACK_TIMEOUT, handle.acks.recv()).await {
                // A stale (lower) seq can arrive if a previous delivery timed out just as its
                // ack landed; skip it and keep waiting for ours.
                Ok(Some(s)) if s < seq => continue,
                Ok(Some(s)) if s == seq => return Ok(()),
                Ok(Some(s)) => {
                    Self::clear_locked(&mut guard, &self.connected);
                    return Err(ListenDeliverError::AckFailed(format!(
                        "protocol violation: ack seq {s} > delivered seq {seq}"
                    )));
                }
                Ok(None) => {
                    Self::clear_locked(&mut guard, &self.connected);
                    return Err(ListenDeliverError::AckFailed("client reader gone".into()));
                }
                Err(_) => {
                    Self::clear_locked(&mut guard, &self.connected);
                    return Err(ListenDeliverError::AckFailed(format!(
                        "ack timeout after {ACK_TIMEOUT:?}"
                    )));
                }
            }
        }
    }

    fn clear_locked(guard: &mut Option<ListenerHandle>, connected: &AtomicBool) {
        *guard = None;
        connected.store(false, Ordering::SeqCst);
    }
}

/// Serve the listen socket. Binds `paths::listen_sock(home, run_id, own_pane)` (same bind ritual
/// as the hooksock: mkdir parent, remove-before-bind, `0o600`), accepts clients, and installs
/// each into `ctx.listener` **latest-wins** — re-arming after a `/clear`, a resume, or a Monitor
/// auto-stop is the common case, and the sidecar cannot promptly tell a zombie predecessor from
/// a live one, so a new client always wins and the old one exits on EOF. Each install pings
/// `ctx.inbox_wake` so entries queued while no listener was attached drain immediately.
///
/// Spawned as a background task by [`run_node`](crate::run_node) and aborted when the outbound
/// serve loop returns; an error here is logged, never fatal.
#[tracing::instrument(skip(ctx), fields(node = %ctx.runtime.name().as_str()))]
pub async fn serve<D: Exomonad<Caps = Runtime>>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
    let home = std::env::var("HOME").map_err(|_| NodeError::MissingContext("HOME"))?;
    let sock = exo_caps::paths::listen_sock(Path::new(&home), &ctx.run_id, &ctx.own_pane);

    if let Some(parent) = sock.parent() {
        std::fs::create_dir_all(parent)?;
    }
    // Remove a stale socket so bind() can't fail with EADDRINUSE. NotFound is fine.
    match std::fs::remove_file(&sock) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(e.into()),
    }

    let listener = UnixListener::bind(&sock)?;
    std::fs::set_permissions(&sock, std::fs::Permissions::from_mode(0o600))?;
    info!(socket = %sock.display(), "listen: listening");

    loop {
        let (stream, _addr) = listener.accept().await?;
        let (read_half, write_half) = stream.into_split();
        let (gen, ack_tx) = ctx.listener.install(write_half).await;
        info!(
            gen,
            "listen: client attached; waking inbound to drain any backlog"
        );
        ctx.inbox_wake.notify_one();

        let ctx = ctx.clone();
        tokio::spawn(async move {
            read_acks(read_half, &ack_tx).await;
            // Drop the ack sender BEFORE clearing: an in-flight `try_deliver` holds the slot
            // lock while awaiting acks, and the closed channel is what unblocks it promptly.
            drop(ack_tx);
            ctx.listener.clear_if_gen(gen).await;
            info!(gen, "listen: client detached");
        });
    }
}

/// Feed `ListenAck` lines from the client into the slot's ack channel until EOF or a protocol
/// error. Returning ends the connection's reader task, which clears the slot for this generation.
pub(super) async fn read_acks(read_half: OwnedReadHalf, ack_tx: &mpsc::UnboundedSender<u64>) {
    let mut lines = BufReader::new(read_half).lines();
    loop {
        match lines.next_line().await {
            Ok(Some(line)) => match serde_json::from_str::<ListenAck>(&line) {
                Ok(ack) => {
                    if ack_tx.send(ack.seq).is_err() {
                        return;
                    }
                }
                Err(e) => {
                    warn!("listen: bad ack line ({e}); dropping connection");
                    return;
                }
            },
            Ok(None) => return,
            Err(e) => {
                warn!("listen: ack read error ({e}); dropping connection");
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::AsyncBufReadExt;
    use tokio::net::UnixStream;

    /// Split a socketpair into (server write half installed later, client end).
    fn pair() -> (OwnedWriteHalf, OwnedReadHalf, UnixStream) {
        let (server, client) = UnixStream::pair().unwrap();
        let (server_read, server_write) = server.into_split();
        (server_write, server_read, client)
    }

    /// A well-behaved client task: read each frame off `client`, ack its seq.
    fn spawn_acking_client(client: UnixStream) -> tokio::task::JoinHandle<Vec<ListenFrame>> {
        tokio::spawn(async move {
            let (read, mut write) = client.into_split();
            let mut lines = BufReader::new(read).lines();
            let mut seen = Vec::new();
            while let Ok(Some(line)) = lines.next_line().await {
                let frame: ListenFrame = serde_json::from_str(&line).unwrap();
                let mut ack = serde_json::to_vec(&ListenAck { seq: frame.seq }).unwrap();
                ack.push(b'\n');
                seen.push(frame);
                if write.write_all(&ack).await.is_err() {
                    break;
                }
                let _ = write.flush().await;
            }
            seen
        })
    }

    #[tokio::test]
    async fn no_listener_is_the_fast_path() {
        let slot = ListenerSlot::new();
        assert!(!slot.is_connected());
        assert!(matches!(
            slot.try_deliver("hi").await,
            Err(ListenDeliverError::NoListener)
        ));
    }

    #[tokio::test]
    async fn acked_delivery_succeeds() {
        let slot = ListenerSlot::new();
        let (write, read, client) = pair();
        let (_gen, ack_tx) = slot.install(write).await;
        tokio::spawn(async move { read_acks(read, &ack_tx).await });
        let client_task = spawn_acking_client(client);

        assert!(slot.is_connected());
        slot.try_deliver("first\nwith lines").await.unwrap();
        slot.try_deliver("second").await.unwrap();

        drop(slot); // drops the writer → client sees EOF and returns
        let seen = client_task.await.unwrap();
        assert_eq!(seen.len(), 2);
        assert_eq!(seen[0].seq, 1);
        assert_eq!(seen[0].text, "first\nwith lines");
        assert_eq!(seen[1].seq, 2);
    }

    #[tokio::test]
    async fn silent_client_times_out_and_clears() {
        // Real-time test: waits out ACK_TIMEOUT (2s) — tokio's test-util pause isn't enabled.
        let slot = ListenerSlot::new();
        let (write, _read, _client) = pair(); // client never acks; keep both ends alive
        let (_gen, _ack_tx) = slot.install(write).await; // keep ack_tx alive: channel open, no acks

        match slot.try_deliver("hello").await {
            Err(ListenDeliverError::AckFailed(detail)) => assert!(detail.contains("timeout")),
            other => panic!("expected ack timeout, got {other:?}"),
        }
        assert!(!slot.is_connected(), "a dead connection clears the slot");
    }

    #[tokio::test]
    async fn latest_wins_replaces_and_first_client_sees_eof() {
        let slot = ListenerSlot::new();

        let (write1, _read1, client1) = pair();
        let (gen1, _ack_tx1) = slot.install(write1).await;

        let (write2, read2, client2) = pair();
        let (gen2, ack_tx2) = slot.install(write2).await;
        assert!(gen2 > gen1);
        tokio::spawn(async move { read_acks(read2, &ack_tx2).await });
        let client2_task = spawn_acking_client(client2);

        // The replaced connection's writer was dropped at the swap → client1 sees EOF.
        let (read1c, _w) = client1.into_split();
        let mut lines1 = BufReader::new(read1c).lines();
        assert_eq!(lines1.next_line().await.unwrap(), None);

        // A stale reader's clear must not clobber the replacement…
        slot.clear_if_gen(gen1).await;
        assert!(slot.is_connected());

        // …and delivery reaches only the new client.
        slot.try_deliver("to the second").await.unwrap();
        drop(slot);
        let seen = client2_task.await.unwrap();
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].text, "to the second");
    }

    #[tokio::test]
    async fn stale_ack_is_skipped() {
        let slot = ListenerSlot::new();
        let (write, _read, _client) = pair();
        let (_gen, ack_tx) = slot.install(write).await;

        // A late ack from a previously-timed-out delivery (seq 0 < ours) must be skipped, then
        // the real ack accepted.
        ack_tx.send(0).unwrap();
        ack_tx.send(1).unwrap();
        slot.try_deliver("hello").await.unwrap();
    }
}
