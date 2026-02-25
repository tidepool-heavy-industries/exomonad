//! Formatting utilities for displaying A2A types

use a2a_rs::domain::{Part, TaskState};

/// Format a task state for display
pub fn format_task_state(state: &TaskState) -> String {
    match state {
        TaskState::Submitted => "Submitted",
        TaskState::Working => "Working",
        TaskState::InputRequired => "Input Required",
        TaskState::Completed => "Completed",
        TaskState::Canceled => "Canceled",
        TaskState::Failed => "Failed",
        TaskState::Rejected => "Rejected",
        TaskState::AuthRequired => "Auth Required",
        TaskState::Unknown => "Unknown",
    }
    .to_string()
}

/// Extract and format text content from message parts
pub fn format_message_content(parts: &[Part]) -> String {
    parts
        .iter()
        .map(|part| match part {
            Part::Text { text, .. } => text.clone(),
            Part::File { file, .. } => format!(
                "[File: {}]",
                file.name.as_ref().unwrap_or(&"unnamed".to_string())
            ),
            Part::Data { data, .. } => {
                let name = data
                    .get("name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unnamed");
                format!("[Data: {}]", name)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Truncate text for preview display
pub fn truncate_preview(text: &str, max_len: usize) -> String {
    if text.len() <= max_len {
        text.to_string()
    } else {
        format!("{}...", text.chars().take(max_len).collect::<String>())
    }
}
