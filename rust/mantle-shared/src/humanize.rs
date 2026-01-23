//! Human-readable output formatting for Claude Code events.
//!
//! Provides terminal-friendly output for the wrap command's pane display.

use crate::events::{ContentBlock, InterruptSignal, StreamEvent};
use std::io::{stderr, Write};

/// Print a stream event in human-readable format.
///
/// This is displayed in the zellij pane for operator visibility.
/// Uses println! for direct terminal output (not tracing, since this
/// is intentional human-facing output, not structured logging).
pub fn print_event_humanized(event: &StreamEvent) {
    match event {
        StreamEvent::System(s) => {
            let short_id = if s.session_id.len() >= 8 {
                &s.session_id[..8]
            } else {
                &s.session_id
            };
            println!("━━━ Session {short_id} ━━━");
            println!("Model: {}", s.model);
            println!();
        }
        StreamEvent::Assistant(a) => {
            for block in &a.message.content {
                match block {
                    ContentBlock::Text { text } => {
                        println!("{text}");
                    }
                    ContentBlock::ToolUse { name, input, .. } => {
                        println!("\n┌─ {name} ─────────────────");
                        // Show key input params, truncated
                        if let Some(obj) = input.as_object() {
                            for (k, v) in obj.iter().take(3) {
                                let v_str = v.to_string();
                                let char_count = v_str.chars().count();
                                let preview: String = v_str.chars().take(60).collect();
                                if char_count > 60 {
                                    println!("│ {k}: {preview}...");
                                } else {
                                    println!("│ {k}: {preview}");
                                }
                            }
                            if obj.len() > 3 {
                                let more = obj.len() - 3;
                                println!("│ ... ({more} more fields)");
                            }
                        }
                        println!("└─────────────────────────");
                    }
                    ContentBlock::ToolResult {
                        content, is_error, ..
                    } => {
                        let status = if is_error.unwrap_or(false) {
                            "✗"
                        } else {
                            "✓"
                        };
                        let char_count = content.chars().count();
                        let preview: String = content.chars().take(100).collect();
                        if char_count > 100 {
                            println!("  {status} {preview}...");
                        } else {
                            println!("  {status} {preview}");
                        }
                    }
                }
            }
        }
        StreamEvent::User(_) => {
            // Usually just tool results, already handled above
        }
        StreamEvent::Result(r) => {
            println!();
            println!("━━━ Complete ━━━");
            let status = if r.is_error { "error" } else { "success" };
            println!("Status: {status}");
            let turns = r.num_turns.unwrap_or(0);
            println!("Turns: {turns}");
            let cost = r.total_cost_usd.unwrap_or(0.0);
            println!("Cost: ${cost:.4}");
        }
    }
}

/// Print an interrupt signal notification.
pub fn print_interrupt(signal: &InterruptSignal) {
    let sig_type = &signal.signal_type;
    let sig_state = &signal.state;
    println!("\n⚡ Interrupt: {sig_type} (state: {sig_state:?})");
}

/// Print a stream event to stderr in human-readable format.
///
/// Like [`print_event_humanized`] but outputs to stderr instead of stdout.
/// Used when stdout needs to be reserved for structured output.
pub fn eprint_event_humanized(event: &StreamEvent) {
    match event {
        StreamEvent::System(s) => {
            let short_id = if s.session_id.len() >= 8 {
                &s.session_id[..8]
            } else {
                &s.session_id
            };
            eprintln!("━━━ Session {short_id} ━━━");
            eprintln!("Model: {}", s.model);
            eprintln!();
        }
        StreamEvent::Assistant(a) => {
            for block in &a.message.content {
                match block {
                    ContentBlock::Text { text } => {
                        eprintln!("{text}");
                    }
                    ContentBlock::ToolUse { name, input, .. } => {
                        eprintln!("\n┌─ {name} ─────────────────");
                        if let Some(obj) = input.as_object() {
                            for (k, v) in obj.iter().take(3) {
                                let v_str = v.to_string();
                                let char_count = v_str.chars().count();
                                let preview: String = v_str.chars().take(60).collect();
                                if char_count > 60 {
                                    eprintln!("│ {k}: {preview}...");
                                } else {
                                    eprintln!("│ {k}: {preview}");
                                }
                            }
                            if obj.len() > 3 {
                                let more = obj.len() - 3;
                                eprintln!("│ ... ({more} more fields)");
                            }
                        }
                        eprintln!("└─────────────────────────");
                    }
                    ContentBlock::ToolResult {
                        content, is_error, ..
                    } => {
                        let status = if is_error.unwrap_or(false) {
                            "✗"
                        } else {
                            "✓"
                        };
                        let char_count = content.chars().count();
                        let preview: String = content.chars().take(100).collect();
                        if char_count > 100 {
                            eprintln!("  {status} {preview}...");
                        } else {
                            eprintln!("  {status} {preview}");
                        }
                    }
                }
            }
        }
        StreamEvent::User(_) => {
            // Usually just tool results, already handled above
        }
        StreamEvent::Result(r) => {
            eprintln!();
            eprintln!("━━━ Complete ━━━");
            let status = if r.is_error { "error" } else { "success" };
            eprintln!("Status: {status}");
            let turns = r.num_turns.unwrap_or(0);
            eprintln!("Turns: {turns}");
            let cost = r.total_cost_usd.unwrap_or(0.0);
            eprintln!("Cost: ${cost:.4}");
        }
    }
    // Flush stderr to ensure output appears immediately
    let _ = stderr().flush();
}
