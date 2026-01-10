//! Stream-json parser for Claude Code output.
//!
//! Parses the JSONL stream from Claude Code and builds a [`RunResult`].

use mantle_shared::events::{ResultEvent, RunResult, StreamEvent};
use std::io::{BufRead, BufReader, Read};
use tracing::warn;

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
    pub fn process_line(&mut self, line: &str) -> Option<StreamEvent> {
        if line.trim().is_empty() {
            return None;
        }

        match serde_json::from_str::<StreamEvent>(line) {
            Ok(event) => {
                if let StreamEvent::Result(ref r) = event {
                    self.result_event = Some(r.clone());
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
    pub fn build_result(self, exit_code: i32, session_tag: Option<String>) -> RunResult {
        RunResult::from_events(
            self.events,
            self.result_event,
            exit_code,
            session_tag,
            vec![], // No interrupts in the new architecture
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

        let result = parser.build_result(0, Some("test-tag".to_string()));

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
}
