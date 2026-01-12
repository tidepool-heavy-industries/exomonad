//! Stream-json parser for Claude Code output.
//!
//! Parses the JSONL stream from Claude Code and builds a [`RunResult`].

use mantle_shared::events::{ResultEvent, RunResult, StreamEvent};
use std::io::{BufRead, BufReader, Read};
use tracing::warn;

/// Sanitize a line from TTY output by stripping ANSI escape codes and control characters.
///
/// This handles:
/// - ANSI escape sequences (ESC[...m, ESC[...A, etc.)
/// - Carriage returns from TTY line handling
/// - Other control characters that would break JSON parsing
fn sanitize_line(line: &str) -> String {
    let mut result = String::with_capacity(line.len());
    let mut chars = line.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            // ANSI escape sequence: ESC followed by [
            '\x1b' => {
                // Check for CSI sequence (ESC [)
                if chars.peek() == Some(&'[') {
                    chars.next(); // consume '['
                    // Skip until we hit a letter (the terminator)
                    while let Some(&next) = chars.peek() {
                        chars.next();
                        if next.is_ascii_alphabetic() {
                            break;
                        }
                    }
                }
                // Otherwise skip just the ESC
            }
            // Strip carriage returns
            '\r' => {}
            // Strip other control chars except tab and newline
            '\x00'..='\x08' | '\x0b' | '\x0c' | '\x0e'..='\x1f' | '\x7f' => {}
            // Keep everything else
            _ => result.push(c),
        }
    }

    result
}

/// Parser for Claude Code's stream-json output.
///
/// Collects streaming events and builds the final [`RunResult`] when complete.
pub struct StreamParser {
    events: Vec<StreamEvent>,
    result_event: Option<ResultEvent>,
}

impl StreamParser {
    /// Create a new parser.
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
            result_event: None,
        }
    }

    /// Process a single JSON line. Returns parsed event on success.
    ///
    /// Sanitizes the line first to remove ANSI codes and control characters
    /// that could interfere with JSON parsing.
    pub fn process_line(&mut self, line: &str) -> Option<StreamEvent> {
        let sanitized = sanitize_line(line);
        if sanitized.trim().is_empty() {
            return None;
        }

        match serde_json::from_str::<StreamEvent>(&sanitized) {
            Ok(event) => {
                if let StreamEvent::Result(ref r) = event {
                    // Take the FIRST result event, ignore subsequent ones.
                    // Claude Code sometimes sends success followed by spurious error_during_execution.
                    if self.result_event.is_none() {
                        self.result_event = Some(r.clone());
                    }
                }
                self.events.push(event.clone());
                Some(event)
            }
            Err(e) => {
                let truncated: String = line.chars().take(80).collect();
                warn!(error = %e, line = %truncated, "Failed to parse stream-json line");
                None
            }
        }
    }

    /// Parse all lines from a reader, calling the callback for each event.
    pub fn parse_stream<R: Read, F>(&mut self, reader: R, mut on_event: F) -> std::io::Result<()>
    where
        F: FnMut(&StreamEvent),
    {
        let buf_reader = BufReader::new(reader);
        for line_result in buf_reader.lines() {
            let line = line_result?;
            if let Some(event) = self.process_line(&line) {
                on_event(&event);
            }
        }
        Ok(())
    }

    /// Build the final [`RunResult`] from collected events.
    ///
    /// # Arguments
    ///
    /// * `exit_code` - Container exit code
    /// * `session_tag` - Optional session tag for correlation
    /// * `tool_calls` - Tool calls captured from control socket (decision tools)
    /// * `stderr_output` - Captured stderr for error diagnosis
    pub fn build_result(
        self,
        exit_code: i32,
        session_tag: Option<String>,
        tool_calls: Vec<mantle_shared::events::ToolCall>,
        stderr_output: Option<String>,
    ) -> RunResult {
        let tool_calls_opt = if tool_calls.is_empty() {
            None
        } else {
            Some(tool_calls)
        };

        RunResult::from_events(
            self.events,
            self.result_event,
            exit_code,
            session_tag,
            vec![], // No interrupts in the new architecture
            tool_calls_opt,
            stderr_output,
        )
    }

    /// Check if we received a result event.
    pub fn has_result(&self) -> bool {
        self.result_event.is_some()
    }

    /// Get the session ID if available.
    pub fn session_id(&self) -> Option<&str> {
        // Try result event first
        if let Some(ref r) = self.result_event {
            if let Some(ref id) = r.session_id {
                return Some(id);
            }
        }

        // Fall back to system/init event
        self.events.iter().find_map(|e| {
            if let StreamEvent::System(s) = e {
                Some(s.session_id.as_str())
            } else {
                None
            }
        })
    }
}

impl Default for StreamParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_INIT: &str = r#"{"type":"system","subtype":"init","session_id":"abc-123","tools":["Read"],"model":"claude-sonnet-4-20250514"}"#;
    const SAMPLE_TEXT: &str = r#"{"type":"assistant","message":{"content":[{"type":"text","text":"hello"}]}}"#;
    const SAMPLE_RESULT: &str = r#"{"type":"result","subtype":"success","is_error":false,"result":"done","session_id":"abc-123","total_cost_usd":0.05,"num_turns":2,"permission_denials":[],"modelUsage":{}}"#;

    #[test]
    fn test_parse_stream() {
        let stream = format!("{}\n{}\n{}\n", SAMPLE_INIT, SAMPLE_TEXT, SAMPLE_RESULT);
        let mut parser = StreamParser::new();
        let mut event_count = 0;

        parser
            .parse_stream(stream.as_bytes(), |_| event_count += 1)
            .unwrap();

        assert_eq!(event_count, 3);
        assert!(parser.has_result());
        assert_eq!(parser.session_id(), Some("abc-123"));
    }

    #[test]
    fn test_build_result() {
        let mut parser = StreamParser::new();
        parser.process_line(SAMPLE_INIT);
        parser.process_line(SAMPLE_RESULT);

        let result = parser.build_result(0, Some("test-tag".to_string()), vec![], None);

        assert_eq!(result.exit_code, 0);
        assert!(!result.is_error);
        assert_eq!(result.session_id, "abc-123");
        assert_eq!(result.session_tag, Some("test-tag".to_string()));
        assert_eq!(result.num_turns, 2);
    }

    #[test]
    fn test_handles_empty_lines() {
        let mut parser = StreamParser::new();
        assert!(parser.process_line("").is_none());
        assert!(parser.process_line("   ").is_none());
    }

    #[test]
    fn test_handles_invalid_json() {
        let mut parser = StreamParser::new();
        assert!(parser.process_line("not json").is_none());
        assert!(parser.process_line("{broken").is_none());
    }

    #[test]
    fn test_sanitize_line_strips_ansi_codes() {
        // Simple color code
        let line = "\x1b[32mGreen text\x1b[0m";
        assert_eq!(sanitize_line(line), "Green text");

        // Bold and color
        let line = "\x1b[1;31mBold red\x1b[0m";
        assert_eq!(sanitize_line(line), "Bold red");

        // Cursor movement
        let line = "\x1b[2Amove up\x1b[2Bmove down";
        assert_eq!(sanitize_line(line), "move upmove down");
    }

    #[test]
    fn test_sanitize_line_strips_carriage_return() {
        let line = "line with\r carriage return";
        assert_eq!(sanitize_line(line), "line with carriage return");

        // Windows-style line ending (just \r, not \r\n since \n is already stripped by BufReader)
        let line = "progress: 50%\r100%";
        assert_eq!(sanitize_line(line), "progress: 50%100%");
    }

    #[test]
    fn test_sanitize_line_strips_control_chars() {
        // Null byte
        let line = "text with\x00null";
        assert_eq!(sanitize_line(line), "text withnull");

        // Bell
        let line = "alert\x07here";
        assert_eq!(sanitize_line(line), "alerthere");

        // Backspace
        let line = "back\x08space";
        assert_eq!(sanitize_line(line), "backspace");
    }

    #[test]
    fn test_sanitize_line_preserves_json() {
        let json = r#"{"type":"result","subtype":"success","is_error":false}"#;
        assert_eq!(sanitize_line(json), json);
    }

    #[test]
    fn test_sanitize_line_with_ansi_in_json() {
        // JSON with ANSI codes embedded in actual bytes (not escaped in JSON string)
        // This simulates corrupted output with raw ANSI bytes
        let line = format!(r#"{{"text":"{}red{}"}}"#, "\x1b[31m", "\x1b[0m");
        let expected = r#"{"text":"red"}"#;
        assert_eq!(sanitize_line(&line), expected);
    }
}
