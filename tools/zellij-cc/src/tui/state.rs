//! Application state for the TUI

use crate::types::{ContentBlock, InterruptSignal, ResultEvent, StreamEvent};
use std::collections::HashSet;

/// Result returned when TUI exits, containing collected data
pub struct TuiResult {
    /// All events received during the session
    pub events: Vec<StreamEvent>,
    /// The final result event (if received)
    pub result_event: Option<ResultEvent>,
    /// All interrupt signals received
    pub interrupts: Vec<InterruptSignal>,
}

/// Display representation of an event for the TUI
#[derive(Debug, Clone)]
pub struct DisplayEvent {
    /// Whether this event can be expanded (has collapsible details)
    pub expandable: bool,
    /// Summary line for collapsed display
    pub summary: String,
    /// Detailed lines for expanded display
    pub details: Vec<String>,
}

impl DisplayEvent {
    pub fn from_stream_event(event: &StreamEvent) -> Self {
        let (expandable, summary, details) = match event {
            StreamEvent::System(s) => {
                // Use chars() to safely truncate UTF-8 strings without panicking
                let short_id: String = s.session_id.chars().take(8).collect();
                (
                    false,
                    format!("Session {} | Model: {}", short_id, s.model),
                    vec![],
                )
            }
            StreamEvent::Assistant(a) => {
                let mut summaries = Vec::new();
                let mut all_details = Vec::new();
                let mut has_expandable = false;

                for block in &a.message.content {
                    match block {
                        ContentBlock::Text { text } => {
                            // Cache char count to avoid iterating twice
                            let char_count = text.chars().count();
                            let preview: String = text.chars().take(60).collect();
                            if char_count > 60 {
                                summaries.push(format!("{}...", preview));
                            } else {
                                summaries.push(preview);
                            }
                        }
                        ContentBlock::ToolUse { name, input, .. } => {
                            has_expandable = true;
                            summaries.push(format!("Tool: {}", name));
                            if let Some(obj) = input.as_object() {
                                let total_params = obj.len();
                                for (k, v) in obj.iter().take(5) {
                                    let v_str = v.to_string();
                                    // Cache char count to avoid iterating twice
                                    let char_count = v_str.chars().count();
                                    let preview: String = v_str.chars().take(60).collect();
                                    if char_count > 60 {
                                        all_details.push(format!("  {}: {}...", k, preview));
                                    } else {
                                        all_details.push(format!("  {}: {}", k, preview));
                                    }
                                }
                                // Show truncation indicator if there are more parameters
                                if total_params > 5 {
                                    all_details.push(format!("  ... and {} more", total_params - 5));
                                }
                            }
                        }
                        ContentBlock::ToolResult { content, is_error, .. } => {
                            let status = if is_error.unwrap_or(false) { "✗" } else { "✓" };
                            // Cache char count to avoid iterating twice
                            let char_count = content.chars().count();
                            let preview: String = content.chars().take(50).collect();
                            if char_count > 50 {
                                summaries.push(format!("{} {}...", status, preview));
                            } else {
                                summaries.push(format!("{} {}", status, preview));
                            }
                        }
                    }
                }

                (
                    has_expandable,
                    summaries.join(" | "),
                    all_details,
                )
            }
            StreamEvent::User(_) => (false, "User input".to_string(), vec![]),
            StreamEvent::Result(r) => {
                let status = if r.is_error { "Error" } else { "Success" };
                (
                    false,
                    format!(
                        "Complete: {} | Turns: {} | Cost: ${:.4}",
                        status,
                        r.num_turns.unwrap_or(0),
                        r.total_cost_usd.unwrap_or(0.0)
                    ),
                    vec![],
                )
            }
        };

        Self {
            expandable,
            summary,
            details,
        }
    }
}

/// Main application state
pub struct AppState {
    // Session info (from SystemEvent)
    pub session_id: Option<String>,
    pub model: Option<String>,

    // Accumulated events
    pub events: Vec<StreamEvent>,
    pub display_events: Vec<DisplayEvent>,

    // UI state
    pub selected_index: usize,
    pub expanded_items: HashSet<usize>,

    // Stats (from ResultEvent)
    pub total_cost_usd: f64,
    pub num_turns: i64,
    pub is_complete: bool,
    pub is_error: bool,

    // Channel state
    pub channel_disconnected: bool,

    // Interrupts
    pub interrupts: Vec<InterruptSignal>,

    // Result event for final output
    pub result_event: Option<ResultEvent>,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            session_id: None,
            model: None,
            events: Vec::new(),
            display_events: Vec::new(),
            selected_index: 0,
            expanded_items: HashSet::new(),
            total_cost_usd: 0.0,
            num_turns: 0,
            is_complete: false,
            is_error: false,
            channel_disconnected: false,
            interrupts: Vec::new(),
            result_event: None,
        }
    }
}

impl AppState {
    /// Process a new event and update state
    pub fn process_event(&mut self, event: StreamEvent) {
        // Update session info from System event
        if let StreamEvent::System(ref s) = event {
            self.session_id = Some(s.session_id.clone());
            self.model = Some(s.model.clone());
        }

        // Update stats from Result event
        if let StreamEvent::Result(ref r) = event {
            self.total_cost_usd = r.total_cost_usd.unwrap_or(0.0);
            self.num_turns = r.num_turns.unwrap_or(0);
            self.is_complete = true;
            self.is_error = r.is_error;
            self.result_event = Some(r.clone());
        }

        // Add display event
        let display_event = DisplayEvent::from_stream_event(&event);
        self.display_events.push(display_event);
        self.events.push(event);

        // Auto-scroll to bottom
        if !self.display_events.is_empty() {
            self.selected_index = self.display_events.len() - 1;
        }
    }

    /// Add an interrupt signal
    pub fn add_interrupt(&mut self, signal: InterruptSignal) {
        self.interrupts.push(signal);
    }

    /// Convert to TuiResult for return to caller
    pub fn into_result(self) -> TuiResult {
        TuiResult {
            events: self.events,
            result_event: self.result_event,
            interrupts: self.interrupts,
        }
    }

    /// Toggle expansion of the currently selected item
    pub fn toggle_expand(&mut self) {
        if self.selected_index < self.display_events.len()
            && self.display_events[self.selected_index].expandable
        {
            if self.expanded_items.contains(&self.selected_index) {
                self.expanded_items.remove(&self.selected_index);
            } else {
                self.expanded_items.insert(self.selected_index);
            }
        }
    }

    /// Expand all expandable items
    pub fn expand_all(&mut self) {
        for (i, event) in self.display_events.iter().enumerate() {
            if event.expandable {
                self.expanded_items.insert(i);
            }
        }
    }

    /// Collapse all items
    pub fn collapse_all(&mut self) {
        self.expanded_items.clear();
    }

    /// Move selection up
    pub fn scroll_up(&mut self) {
        if self.selected_index > 0 {
            self.selected_index -= 1;
        }
    }

    /// Move selection down
    pub fn scroll_down(&mut self) {
        if self.selected_index + 1 < self.display_events.len() {
            self.selected_index += 1;
        }
    }

    /// Jump to top
    pub fn go_to_top(&mut self) {
        // Check empty for consistency with go_to_bottom()
        if !self.display_events.is_empty() {
            self.selected_index = 0;
        }
    }

    /// Jump to bottom
    pub fn go_to_bottom(&mut self) {
        if !self.display_events.is_empty() {
            self.selected_index = self.display_events.len() - 1;
        }
    }
}
