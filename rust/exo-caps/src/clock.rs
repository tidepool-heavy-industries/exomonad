//! `Clock` capability — wall-clock behind a cap so policy stays deterministically
//! testable (a mock clock yields a fixed time). No id-minting: the bus carries no
//! message-id (ordering is the append order — doc 02).

use chrono::{DateTime, Utc};

pub trait Clock {
    /// Wall-clock instant, stamped on a bus append. A real `chrono` time, not a string —
    /// serialize it to RFC3339 only at the wire edge.
    fn now(&self) -> DateTime<Utc>;
}
