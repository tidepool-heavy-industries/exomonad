//! Event types for Claude subprocess integration

use crate::types::{InterruptSignal, StreamEvent};

/// Events that can be received from external sources (Claude subprocess)
#[derive(Clone, Debug)]
pub enum TuiEvent {
    /// A stream event from Claude
    Claude(StreamEvent),
    /// An interrupt signal
    Interrupt(InterruptSignal),
    /// The Claude process has exited
    ProcessExit,
}
