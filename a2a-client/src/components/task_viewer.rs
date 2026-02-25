//! Generic task viewing components

use a2a_rs::domain::{Part as MessagePart, Task};
use serde::Serialize;

/// View model for a task in a list
#[derive(Debug, Serialize, Clone)]
pub struct TaskView {
    pub task_id: String,
    pub state: String,
    pub message_count: usize,
    pub last_message_preview: Option<String>,
}

impl TaskView {
    /// Create a TaskView from an A2A Task
    pub fn from_task(task: Task) -> Self {
        let message_count = task.history.as_ref().map(|h| h.len()).unwrap_or(0);
        let last_message_preview = task.history.as_ref().and_then(|h| {
            h.last().and_then(|msg| {
                msg.parts.iter().find_map(|part| match part {
                    MessagePart::Text { text, .. } => {
                        Some(text.chars().take(100).collect::<String>())
                    }
                    _ => None,
                })
            })
        });

        Self {
            task_id: task.id,
            state: format!("{:?}", task.status.state),
            message_count,
            last_message_preview,
        }
    }
}

/// View model for a single message
#[derive(Debug, Serialize, Clone)]
pub struct MessageView {
    pub id: String,
    pub role: String,
    pub content: String,
}

impl MessageView {
    /// Create a MessageView from an A2A Message
    pub fn from_message(msg: a2a_rs::domain::Message) -> Self {
        // Extract text content from message parts
        let content = msg
            .parts
            .iter()
            .map(|part| match part {
                MessagePart::Text { text, .. } => text.clone(),
                MessagePart::File { file, .. } => format!(
                    "[File: {}]",
                    file.name.as_ref().unwrap_or(&"unnamed".to_string())
                ),
                MessagePart::Data { data, .. } => {
                    let name = data
                        .get("name")
                        .and_then(|v| v.as_str())
                        .unwrap_or("unnamed");
                    format!("[Data: {}]", name)
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        Self {
            id: msg.message_id,
            role: format!("{:?}", msg.role),
            content,
        }
    }

    /// Create a MessageView with JSON parsing for structured responses
    pub fn from_message_with_json_parsing(msg: a2a_rs::domain::Message) -> Self {
        let content = msg
            .parts
            .iter()
            .filter_map(|part| match part {
                MessagePart::Text { text, .. } => Some(text.clone()),
                _ => None,
            })
            .collect::<Vec<_>>()
            .join("\n");

        // Try to parse as JSON for better display
        let display_content =
            if let Ok(json_value) = serde_json::from_str::<serde_json::Value>(&content) {
                if let Some(obj) = json_value.as_object() {
                    match obj.get("type").and_then(|v| v.as_str()) {
                        Some("form") => obj
                            .get("instructions")
                            .and_then(|v| v.as_str())
                            .unwrap_or("Please fill out the form.")
                            .to_string(),
                        Some("result") => {
                            let message = obj
                                .get("message")
                                .and_then(|v| v.as_str())
                                .unwrap_or("Request processed.");
                            let status = obj
                                .get("status")
                                .and_then(|v| v.as_str())
                                .unwrap_or("unknown");
                            format!("{}\n\nStatus: {}", message, status)
                        }
                        _ => serde_json::to_string_pretty(&json_value).unwrap_or(content.clone()),
                    }
                } else {
                    content.clone()
                }
            } else {
                content.clone()
            };

        Self {
            id: msg.message_id,
            role: format!("{:?}", msg.role),
            content: display_content,
        }
    }
}
