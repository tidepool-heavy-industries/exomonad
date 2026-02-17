//! Integration test for wait_for_event tool via WASM.

use async_trait::async_trait;
use exomonad_core::handlers::EventHandler;
use exomonad_core::services::EventQueue;
use exomonad_core::{EffectHandler, EffectResult, RuntimeBuilder};
use exomonad_proto::effects::events::{event::EventType, Event, WorkerComplete};
use prost::Message;
use serde_json::{json, Value};
use std::sync::Arc;
use std::time::Duration;

fn wasm_binary_bytes() -> Vec<u8> {
    let manifest = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = manifest.join("../../.exo/wasm/wasm-guest-unified.wasm");
    assert!(
        path.exists(),
        "WASM binary not found at {path:?}. Build with `just wasm-all`."
    );
    std::fs::read(&path).expect("Failed to read WASM binary")
}

// Mock Log Handler to satisfy runtime requirements
struct MockLogHandler;
#[async_trait]
impl EffectHandler for MockLogHandler {
    fn namespace(&self) -> &str {
        "log"
    }
    async fn handle(
        &self,
        _type: &str,
        _payload: &[u8],
        _ctx: &exomonad_core::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        use exomonad_proto::effects::log::LogResponse;
        Ok(LogResponse { success: true }.encode_to_vec())
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_wait_for_event_roundtrip() {
    let wasm_bytes = wasm_binary_bytes();
    let event_queue = Arc::new(EventQueue::new());

    // Real EventHandler (no remote, default session "default")
    let event_handler = EventHandler::new(event_queue.clone(), None, Some("default".to_string()));

    let runtime = RuntimeBuilder::new()
        .with_effect_handler(MockLogHandler)
        .with_effect_handler(event_handler)
        .with_wasm_bytes(wasm_bytes)
        .build()
        .await
        .expect("Failed to build runtime");

    // Spawn background task to notify after delay
    let queue_clone = event_queue.clone();
    tokio::spawn(async move {
        tokio::time::sleep(Duration::from_millis(100)).await;

        let event = Event {
            event_id: 0,
            event_type: Some(EventType::WorkerComplete(WorkerComplete {
                worker_id: "test-worker".to_string(),
                status: "success".to_string(),
                changes: vec![],
                message: "Done".to_string(),
            })),
        };

        queue_clone.notify_event("default", event).await;
    });

    // Call wait_for_event tool via WASM
    let input = json!({
        "toolName": "wait_for_event",
        "toolArgs": {
            "types": ["worker_complete"],
            "timeout_secs": 5
        }
    });

    let output: Value = runtime
        .plugin_manager()
        .call("handle_mcp_call", &input)
        .await
        .expect("handle_mcp_call failed");

    println!("Tool output: {:#}", output);

    assert_eq!(output["success"], true);

    let result = &output["result"];
    let event = &result["event"];

    // Check event structure (JSON representation of protobuf)
    let event_type = &event["event_type"];
    assert!(event_type["worker_complete"].is_object());
    assert_eq!(event_type["worker_complete"]["worker_id"], "test-worker");
}
