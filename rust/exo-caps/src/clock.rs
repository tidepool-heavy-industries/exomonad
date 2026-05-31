//! `Clock` capability — wall-clock + id minting, behind a cap so policy stays
//! deterministically testable (a mock clock yields fixed time/ids).

use crate::types::MessageId;
use chrono::{DateTime, Utc};

pub trait Clock {
    /// Wall-clock instant, stamped on a bus append. A real `chrono` time, not a string —
    /// serialize it to RFC3339 only at the wire edge.
    fn now(&self) -> DateTime<Utc>;
    /// A fresh monotonic ulid — the message `id`, for ordering / optional dedup.
    /// (NOT the cursor: the cursor is a byte-offset — see doc 02.)
    fn new_id(&self) -> MessageId;
}
