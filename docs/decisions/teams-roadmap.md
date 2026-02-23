# Teams Integration Roadmap

**Status:** Active

Open work items for the Claude Teams integration. Consolidated from individual ADRs (`claude-teams-integration.md`, `event-delivery-zellij-stdin.md`, `teams-synthetic-members.md`).

## Wave 2a (Parallel)

### Lock Coordination Protocol

**Priority:** High — correctness risk under concurrency.

Our `teams_mailbox::write_to_inbox()` does atomic temp+rename but doesn't coordinate with Claude Code's `.lock` sidecar protocol. Under high concurrency (multiple workers completing simultaneously), this risks lost updates via concurrent read-modify-write.

**Design:**

New module `services/file_lock.rs` implementing Claude Code's exact lock protocol:

1. **Acquire**: `open(lock_path, O_CREAT|O_EXCL|O_WRONLY, 0600)` — atomic creation
2. **Metadata**: Write `{"pid": N, "host": "...", "createdAt": "...", "ttlSeconds": 30}` + fsync
3. **Stale detection**: If lock exists, check mtime > TTL (30s) OR PID not alive (`kill(pid, 0)`)
4. **Retry**: Backoff with jitter, timeout after ~5s
5. **Release**: Delete lock file. `Drop` impl for best-effort cleanup on panic.

```rust
// services/file_lock.rs
pub struct FileLock { path: PathBuf }

impl FileLock {
    pub fn acquire(path: &Path, ttl: Duration) -> io::Result<Self>;
    pub fn release(self) -> io::Result<()>;
}

impl Drop for FileLock {
    fn drop(&mut self) { /* best-effort delete */ }
}
```

**Changes:**
- Create `services/file_lock.rs`
- Modify `services/teams_mailbox.rs`: wrap read-modify-write in lock acquire/release
- Add `fsync` on directory after rename (currently missing)
- Lock path: `{inbox_file}.lock` (e.g., `inboxes/team-lead.json.lock`)

**Writer-side only.** InboxWatcher reads are safe (parse failure = retry on next 500ms poll).

**Verification:** Concurrent write test (two threads appending to same inbox).

### Delivery Retry

**Priority:** Medium — reliability improvement.

Currently `delivery::deliver_to_agent()` tries Teams once, then Zellij once, then gives up. If both fail, the event is lost (only the JSONL log preserves it).

**Design:**

Add retry with backoff to `delivery.rs`:
- Teams: retry 2x with 100ms backoff (transient lock contention)
- Zellij: no retry (if the pane doesn't exist, retrying won't help)
- If both fail after retries: log error, return `DeliveryResult::Failed`

**Changes:**
- Modify `services/delivery.rs`: add retry loop around Teams write
- No new files

## Wave 2b (After Lock)

### Inbox Compaction

**Priority:** Medium — operational health.

Inbox files grow unboundedly. Idle notifications (every 2-4s per teammate) are the main contributor. Over long sessions, inbox files grow to megabytes, slowing read-modify-write operations.

**Design:**

New function `teams_mailbox::compact_inbox()`:
- Acquire `.lock` (uses file_lock.rs from Wave 2a)
- Read inbox array
- Never delete messages with `read == false`
- For `read == true` messages:
  - Keep last 500 messages OR last 7 days (whichever is larger)
  - For idle_notification protocol messages: keep only last 10 per sender
- Write compacted array back atomically

**Trigger options:**
- On write, if array length > 1000 (piggyback on existing lock acquisition)
- Or: periodic background task (every 5 minutes)

**Changes:**
- Add `compact_inbox()` to `services/teams_mailbox.rs`
- Call from `write_to_inbox()` when array exceeds threshold, or from a background task

## Wave 3 (Future)

### Permission Cascade

3-tier permission model: agent < TL < human. Typed permission checking for sensitive operations (git push, file deletion, external API calls). Requires its own design pass — deferred.

### Zellij Popup Approval UI

Human escalation via structured popup dialogs. Infrastructure exists in `exomonad-plugin` but is disabled because it blocks the WASM plugin lock for the entire popup duration. Needs suspend/resume mechanism.

### Headless Mode

Zellij fallback unavailable in headless/CI environments. Teams-only delivery works but depends on Claude Code polling. No current workaround for environments without Zellij.

### Message Ordering

If two children complete simultaneously, delivery order to the parent is non-deterministic. Not a correctness issue (each message is self-contained) but could confuse agents expecting sequential processing.

### Claude Code Version Sensitivity

The Teams on-disk format is observed from Claude Code 2.1.x behavior, not a stable public API. Field names, directory structure, and protocol message types may change. Integration should be validated after Claude Code upgrades.
