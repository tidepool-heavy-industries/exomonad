//! Shared utilities for mantle binaries.
//!
//! Common helper functions used across both `mantle` and `mantle-agent`.

use shell_escape::escape;
use std::borrow::Cow;
use std::path::PathBuf;

/// Find the mantle-agent binary path.
///
/// Search order:
/// 1. Same directory as current executable
/// 2. PATH via `which`
/// 3. Falls back to "mantle-agent" (relies on PATH at runtime)
///
/// # Example
///
/// ```ignore
/// let agent = find_mantle_agent_binary();
/// // Returns something like "/usr/local/bin/mantle-agent"
/// ```
pub fn find_mantle_agent_binary() -> PathBuf {
    // Try same directory as current executable
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            let candidate = dir.join("mantle-agent");
            if candidate.exists() {
                return candidate;
            }
        }
    }

    // Try PATH via which
    if let Ok(output) = std::process::Command::new("which")
        .arg("mantle-agent")
        .output()
    {
        if output.status.success() {
            let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !path.is_empty() {
                return PathBuf::from(path);
            }
        }
    }

    // Fallback - will use PATH at runtime
    PathBuf::from("mantle-agent")
}

/// Shell-escape a string for safe use in shell commands.
///
/// Uses `shell_escape` crate to handle special characters properly.
///
/// # Example
///
/// ```
/// use mantle_shared::util::shell_quote;
///
/// let escaped = shell_quote("hello world");
/// assert_eq!(escaped, "'hello world'");
/// ```
pub fn shell_quote(s: &str) -> Cow<'_, str> {
    escape(Cow::Borrowed(s))
}

/// Build the final prompt, optionally prepending injected context.
///
/// When context is provided, it's prepended with a double newline separator
/// to clearly delineate it from the actual prompt.
///
/// # Example
///
/// ```
/// use mantle_shared::util::build_prompt;
///
/// let prompt = build_prompt("continue working", Some("CONTEXT: file.rs modified"));
/// assert_eq!(prompt, "CONTEXT: file.rs modified\n\ncontinue working");
/// ```
pub fn build_prompt(prompt: &str, inject_context: Option<&str>) -> String {
    match inject_context {
        Some(ctx) => format!("{}\n\n{}", ctx, prompt),
        None => prompt.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_prompt_without_context() {
        let result = build_prompt("do the thing", None);
        assert_eq!(result, "do the thing");
    }

    #[test]
    fn test_build_prompt_with_context() {
        let result = build_prompt("continue working", Some("CONTEXT: file.rs was modified"));
        assert_eq!(result, "CONTEXT: file.rs was modified\n\ncontinue working");
    }

    #[test]
    fn test_build_prompt_with_multiline_context() {
        let ctx = "CONTEXT:\n- file1.rs modified\n- file2.rs added";
        let result = build_prompt("proceed", Some(ctx));
        assert_eq!(
            result,
            "CONTEXT:\n- file1.rs modified\n- file2.rs added\n\nproceed"
        );
    }

    #[test]
    fn test_build_prompt_with_special_chars() {
        let ctx = "CONTEXT: user said \"hello\" & 'goodbye'";
        let result = build_prompt("continue", Some(ctx));
        assert!(result.contains("\"hello\""));
        assert!(result.contains("&"));
        assert!(result.contains("'goodbye'"));
    }

    #[test]
    fn test_shell_quote_simple() {
        let result = shell_quote("hello");
        assert_eq!(result, "hello");
    }

    #[test]
    fn test_shell_quote_with_spaces() {
        let result = shell_quote("hello world");
        assert_eq!(result, "'hello world'");
    }
}
