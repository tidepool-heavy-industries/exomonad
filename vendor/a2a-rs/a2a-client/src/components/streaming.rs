//! Server-Sent Events (SSE) streaming components

use a2a_rs::services::{AsyncA2AClient, StreamItem};
use axum::response::sse::{Event, KeepAlive, Sse};
use futures::StreamExt;
use std::{convert::Infallible, sync::Arc, time::Duration};
use tracing::{error, info, warn};

use crate::WebA2AClient;

/// Create an SSE stream for task updates
///
/// This function handles:
/// - WebSocket streaming if available
/// - Fallback to HTTP polling
/// - Automatic retry logic
/// - Serialization to JSON events
pub fn create_sse_stream(
    client: Arc<WebA2AClient>,
    task_id: String,
) -> Sse<impl futures::Stream<Item = Result<Event, Infallible>>> {
    let stream = async_stream::stream! {
        // Check if we have a WebSocket client
        if let Some(ws_client) = client.websocket() {
            info!("Attempting to subscribe to task {} via WebSocket", task_id);

            let mut retry_count = 0;
            let max_retries = 60; // 60 retries with 1 second delay = 1 minute

            loop {
                match ws_client.subscribe_to_task(&task_id, Some(50)).await {
                    Ok(mut event_stream) => {
                        info!("Successfully subscribed to task {} via WebSocket", task_id);

                        while let Some(result) = event_stream.next().await {
                            match result {
                                Ok(stream_item) => {
                                    let (event_type, event_data) = match &stream_item {
                                        StreamItem::Task(task) => {
                                            match serde_json::to_string(task) {
                                                Ok(json) => ("task-update", json),
                                                Err(e) => {
                                                    error!("Failed to serialize task: {}", e);
                                                    continue;
                                                }
                                            }
                                        }
                                        StreamItem::StatusUpdate(status) => {
                                            match serde_json::to_string(status) {
                                                Ok(json) => ("task-status", json),
                                                Err(e) => {
                                                    error!("Failed to serialize status: {}", e);
                                                    continue;
                                                }
                                            }
                                        }
                                        StreamItem::ArtifactUpdate(artifact) => {
                                            match serde_json::to_string(artifact) {
                                                Ok(json) => ("artifact", json),
                                                Err(e) => {
                                                    error!("Failed to serialize artifact: {}", e);
                                                    continue;
                                                }
                                            }
                                        }
                                    };

                                    yield Ok(Event::default()
                                        .event(event_type)
                                        .data(event_data));
                                }
                                Err(e) => {
                                    warn!("Stream error (continuing): {}", e);
                                    continue;
                                }
                            }
                        }
                        break;
                    }
                    Err(e) => {
                        retry_count += 1;

                        if retry_count <= max_retries {
                            if retry_count == 1 {
                                info!("Task {} not ready yet, will retry", task_id);
                            }
                            tokio::time::sleep(Duration::from_secs(1)).await;
                            continue;
                        } else {
                            warn!("Failed to subscribe after {} retries: {}, falling back to polling", max_retries, e);
                            loop {
                                match client.http.get_task(&task_id, Some(50)).await {
                                    Ok(task) => {
                                        let task_json = match serde_json::to_string(&task) {
                                            Ok(json) => json,
                                            Err(e) => {
                                                error!("Failed to serialize task: {}", e);
                                                tokio::time::sleep(Duration::from_secs(2)).await;
                                                continue;
                                            }
                                        };

                                        yield Ok(Event::default()
                                            .event("task-update")
                                            .data(task_json));
                                    }
                                    Err(_) => {
                                        // Task doesn't exist yet, keep polling silently
                                    }
                                }

                                tokio::time::sleep(Duration::from_secs(2)).await;
                            }
                        }
                    }
                }
            }
        } else {
            // Fallback: Poll for updates every 2 seconds
            warn!("WebSocket not available, using polling fallback for task {}", task_id);
            loop {
                match client.http.get_task(&task_id, Some(50)).await {
                    Ok(task) => {
                        let task_json = match serde_json::to_string(&task) {
                            Ok(json) => json,
                            Err(e) => {
                                error!("Failed to serialize task: {}", e);
                                continue;
                            }
                        };

                        yield Ok(Event::default()
                            .event("task-update")
                            .data(task_json));
                    }
                    Err(_) => {
                        // Task doesn't exist yet, keep polling silently
                    }
                }

                tokio::time::sleep(Duration::from_secs(2)).await;
            }
        }
    };

    Sse::new(stream).keep_alive(KeepAlive::default())
}
