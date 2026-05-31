//! `impl Bus for Runtime` — append-only ingestion-inbox delivery. **The genuinely-new
//! piece** (not adapted from a service).
//!
//! **Leaf R4.** The bus is *a jsonl file*: append a line, read new lines from a saved
//! byte-offset. NO queue abstraction, NO `exo-mailbox` crate. See doc 02.
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
use exo_caps::{Addressee, Bus, BusError, InboxPath, Message};

impl Runtime {
    /// Resolve a policy-facing [`Addressee`] to the concrete inbox file to append to.
    /// Internal to the runtime — never exposed to policy (per doc 03).
    pub(crate) fn resolve_inbox(&self, _to: &Addressee) -> Result<InboxPath, BusError> {
        todo!(
            "R4: Parent => self.parent_inbox.clone().ok_or(Unresolved); \
             Inline/Worktree Child(name) => fold children.jsonl, look up child.inbox"
        )
    }
}

#[async_trait]
impl Bus for Runtime {
    async fn deliver(&self, _to: Addressee, _msg: Message) -> Result<(), BusError> {
        todo!(
            "R4: resolve_inbox -> wrap Message in IngestionEntry (stamp from/ts/v) -> \
             serialize one line + \\n -> assert <= PIPE_BUF (no spill) -> append (no fsync)"
        )
    }
}
