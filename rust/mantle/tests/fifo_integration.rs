//! Integration tests for FIFO types.
//!
//! Note: Full SignalFifo tests with reader/writer are complex due to
//! FIFO blocking semantics. These tests focus on lifecycle and cleanup.

use std::thread;
use std::time::Duration;

use mantle::error::ZellijCcError;
use mantle::events::InterruptSignal;
use mantle::fifo::write_signal;

/// Test ResultFifo creation and cleanup.
#[test]
fn test_result_fifo_lifecycle() {
    use mantle::fifo::ResultFifo;

    let path = {
        let result_fifo = ResultFifo::new().unwrap();
        let path = result_fifo.path().to_path_buf();
        assert!(path.exists(), "FIFO should exist");
        path
    };

    // Give drop time to clean up
    thread::sleep(Duration::from_millis(100));

    assert!(!path.exists(), "FIFO should be cleaned up after drop");
}

/// Test InterruptSignal serialization round-trip.
#[test]
fn test_interrupt_signal_roundtrip() {
    let original = InterruptSignal {
        signal_type: "transition".to_string(),
        state: Some("need_more_context".to_string()),
        reason: Some("Missing type definitions".to_string()),
    };

    let json = serde_json::to_string(&original).unwrap();
    let decoded: InterruptSignal = serde_json::from_str(&json).unwrap();

    assert_eq!(decoded.signal_type, original.signal_type);
    assert_eq!(decoded.state, original.state);
    assert_eq!(decoded.reason, original.reason);
}

/// Test that write_signal fails gracefully with nonexistent path.
#[test]
fn test_write_signal_nonexistent_path() {
    let signal = InterruptSignal {
        signal_type: "test".to_string(),
        state: None,
        reason: None,
    };

    let result = write_signal(std::path::Path::new("/nonexistent/fifo"), &signal);
    assert!(matches!(result, Err(ZellijCcError::FifoOpen { .. })));
}

/// Test InterruptSignal with None fields.
#[test]
fn test_interrupt_signal_none_fields() {
    let signal = InterruptSignal {
        signal_type: "escalate".to_string(),
        state: None,
        reason: None,
    };

    let json = serde_json::to_string(&signal).unwrap();
    // Verify None fields are omitted (skip_serializing_if)
    assert!(!json.contains("state"));
    assert!(!json.contains("reason"));

    let decoded: InterruptSignal = serde_json::from_str(&json).unwrap();
    assert_eq!(decoded.state, None);
    assert_eq!(decoded.reason, None);
}
