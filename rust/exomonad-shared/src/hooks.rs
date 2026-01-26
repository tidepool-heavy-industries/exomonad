//! Hook configuration generation for Claude Code.
//!
//! Generates `settings.local.json` with hooks that forward to `exomonad hook <event>`.
//! The configuration is per-session and cleaned up when the session ends.
//!
//! ## Hook Flow
//!
//! ```text
//! Claude Code         settings.local.json       exomonad hook
//! [hook trigger] ---> [hook command]       ---> [subcommand]
//!                                               reads stdin, forwards to socket
//! ```
//!
//! ## Configuration Structure
//!
//! Generated hooks use the exomonad binary path to call `hook <event>`:
//! ```json
//! {
//!   "hooks": {
//!     "PreToolUse": [{"matcher": "*", "hooks": [
//!       {"type": "command", "command": "/path/to/exomonad hook pre-tool-use"}
//!     ]}]
//!   }
//! }
//! ```

use crate::error::{ExoMonadError, Result};
use serde_json::{json, Value};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{debug, warn};

/// Marker key we add to track our generated hooks.
/// Used for cleanup to identify which hooks we added.
const EXOMONAD_MARKER: &str = "_exomonad_generated";

/// All hook event types we generate configuration for.
const HOOK_EVENTS: &[(&str, &str, bool)] = &[
    // (event_name, subcommand_arg, needs_matcher)
    ("PreToolUse", "pre-tool-use", true),
    ("PostToolUse", "post-tool-use", true),
    ("PermissionRequest", "permission-request", true),
    ("Notification", "notification", false),
    ("Stop", "stop", false),
    ("SubagentStop", "subagent-stop", false),
    ("PreCompact", "pre-compact", false),
    ("SessionStart", "session-start", false),
    ("SessionEnd", "session-end", false),
    ("UserPromptSubmit", "user-prompt-submit", false),
];

/// Hook configuration for Claude Code.
///
/// This struct manages the lifecycle of hook configuration:
/// - Created before starting Claude Code
/// - Cleaned up after session ends (including on error)
pub struct HookConfig {
    /// Path to the .claude directory (kept for potential future use)
    #[allow(dead_code)]
    claude_dir: PathBuf,
    /// Path to settings.local.json
    settings_path: PathBuf,
    /// Original content before our modifications (for cleanup)
    original_content: Option<String>,
    /// Whether we created the settings file (vs modified existing)
    created_file: bool,
}

impl HookConfig {
    /// Generate hook configuration for a Claude Code session.
    ///
    /// Creates or updates `.claude/settings.local.json` in the given working
    /// directory with hooks that forward to `<binary> hook <event>`.
    ///
    /// # Arguments
    ///
    /// * `cwd` - Working directory for the Claude Code session
    /// * `binary_path` - Path to the binary that handles hooks (exomonad or exomonad)
    ///
    /// # Returns
    ///
    /// A `HookConfig` guard that will clean up on drop.
    pub fn generate(cwd: &Path, binary_path: &Path) -> Result<Self> {
        Self::generate_with_binary(cwd, binary_path)
    }

    /// Generate hook configuration using a specific binary path.
    ///
    /// This is the core implementation - `generate()` delegates to this.
    /// Use this when you need to explicitly specify which binary handles hooks
    /// (e.g., `exomonad` when running inside a container).
    pub fn generate_with_binary(cwd: &Path, binary_path: &Path) -> Result<Self> {
        let claude_dir = cwd.join(".claude");
        let settings_path = claude_dir.join("settings.local.json");

        // Ensure .claude directory exists
        if !claude_dir.exists() {
            fs::create_dir_all(&claude_dir).map_err(ExoMonadError::Io)?;
            debug!(path = %claude_dir.display(), "Created .claude directory");
        }

        // Read existing settings if present
        let (original_content, mut settings) = if settings_path.exists() {
            let content = fs::read_to_string(&settings_path).map_err(ExoMonadError::Io)?;
            let settings: Value =
                serde_json::from_str(&content).map_err(|e| ExoMonadError::JsonParse { source: e })?;
            (Some(content), settings)
        } else {
            (None, json!({}))
        };

        let created_file = original_content.is_none();

        // Generate hook commands
        let hooks = generate_hook_config(binary_path);

        // Merge our hooks with existing
        if let Some(existing_hooks) = settings.get("hooks") {
            // Merge each hook event
            let mut merged = existing_hooks.clone();
            if let (Some(merged_obj), Some(new_hooks)) = (merged.as_object_mut(), hooks.as_object())
            {
                for (key, value) in new_hooks {
                    merged_obj.insert(key.clone(), value.clone());
                }
            }
            settings["hooks"] = merged;
        } else {
            settings["hooks"] = hooks;
        }

        // Add our marker
        settings[EXOMONAD_MARKER] = json!(true);

        // Write settings
        let content =
            serde_json::to_string_pretty(&settings).map_err(ExoMonadError::JsonSerialize)?;
        fs::write(&settings_path, &content).map_err(ExoMonadError::Io)?;

        debug!(
            path = %settings_path.display(),
            "Generated hook configuration"
        );

        Ok(Self {
            claude_dir,
            settings_path,
            original_content,
            created_file,
        })
    }

    /// Path to the generated settings file.
    pub fn settings_path(&self) -> &Path {
        &self.settings_path
    }

    /// Manually clean up the hook configuration.
    ///
    /// This is called automatically on drop, but can be called explicitly
    /// if needed.
    pub fn cleanup(&mut self) -> Result<()> {
        if !self.settings_path.exists() {
            return Ok(());
        }

        if self.created_file {
            // We created the file, just delete it
            fs::remove_file(&self.settings_path).map_err(ExoMonadError::Io)?;
            debug!(
                path = %self.settings_path.display(),
                "Removed generated settings.local.json"
            );
        } else if let Some(ref original) = self.original_content {
            // Restore original content
            fs::write(&self.settings_path, original).map_err(ExoMonadError::Io)?;
            debug!(
                path = %self.settings_path.display(),
                "Restored original settings.local.json"
            );
        } else {
            // Try to remove just our hooks
            if let Ok(content) = fs::read_to_string(&self.settings_path) {
                if let Ok(mut settings) = serde_json::from_str::<Value>(&content) {
                    // Remove our marker
                    if let Some(obj) = settings.as_object_mut() {
                        obj.remove(EXOMONAD_MARKER);

                        // Remove our hook entries
                        if let Some(hooks) = obj.get_mut("hooks") {
                            if let Some(hooks_obj) = hooks.as_object_mut() {
                                for (event_name, _, _) in HOOK_EVENTS {
                                    // Only remove if it looks like ours
                                    if let Some(entries) = hooks_obj.get(*event_name) {
                                        if is_exomonad_hook(entries) {
                                            hooks_obj.remove(*event_name);
                                        }
                                    }
                                }
                            }
                        }

                        // Write back cleaned settings
                        if let Ok(cleaned) = serde_json::to_string_pretty(&settings) {
                            fs::write(&self.settings_path, cleaned).ok();
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

impl Drop for HookConfig {
    fn drop(&mut self) {
        if let Err(e) = self.cleanup() {
            warn!(error = %e, "Failed to clean up hook configuration");
        }
    }
}

/// Generate the hooks configuration object.
fn generate_hook_config(exomonad_path: &Path) -> Value {
    let mut hooks = serde_json::Map::new();

    for (event_name, subcommand, needs_matcher) in HOOK_EVENTS {
        let command = format!("{} hook {}", exomonad_path.display(), subcommand);

        let hook_entry = json!({
            "type": "command",
            "command": command
        });

        let config = if *needs_matcher {
            // Tool-related hooks need a matcher
            json!([{
                "matcher": "*",
                "hooks": [hook_entry]
            }])
        } else {
            // Other hooks don't use matcher
            json!([{
                "hooks": [hook_entry]
            }])
        };

        hooks.insert(event_name.to_string(), config);
    }

    Value::Object(hooks)
}

/// Check if a hook entry looks like one we generated.
fn is_exomonad_hook(entries: &Value) -> bool {
    if let Some(arr) = entries.as_array() {
        for entry in arr {
            if let Some(hooks_arr) = entry.get("hooks").and_then(|h| h.as_array()) {
                for hook in hooks_arr {
                    if let Some(cmd) = hook.get("command").and_then(|c| c.as_str()) {
                        if cmd.contains("exomonad hook") {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_generate_hook_config() {
        let path = PathBuf::from("/usr/bin/exomonad");
        let config = generate_hook_config(&path);

        // Check PreToolUse has matcher
        let pre_tool = &config["PreToolUse"];
        assert!(pre_tool.is_array());
        let first = &pre_tool[0];
        assert_eq!(first["matcher"], "*");
        assert!(first["hooks"][0]["command"]
            .as_str()
            .unwrap()
            .contains("hook pre-tool-use"));

        // Check Stop doesn't have matcher at top level
        let stop = &config["Stop"];
        assert!(stop.is_array());
        let first = &stop[0];
        assert!(first.get("matcher").is_none());
        assert!(first["hooks"][0]["command"]
            .as_str()
            .unwrap()
            .contains("hook stop"));
    }

    #[test]
    fn test_hook_config_lifecycle() {
        let temp_dir = TempDir::new().unwrap();
        let cwd = temp_dir.path();
        let exomonad = PathBuf::from("/test/exomonad");

        // Generate config
        let config = HookConfig::generate(cwd, &exomonad).unwrap();

        // Verify file was created
        assert!(config.settings_path().exists());

        // Verify content
        let content = fs::read_to_string(config.settings_path()).unwrap();
        assert!(content.contains("PreToolUse"));
        assert!(content.contains("exomonad hook pre-tool-use"));
        assert!(content.contains(EXOMONAD_MARKER));

        // Drop config (triggers cleanup)
        drop(config);

        // File should be removed (we created it)
        let settings_path = cwd.join(".claude/settings.local.json");
        assert!(!settings_path.exists());
    }

    #[test]
    fn test_hook_config_with_existing() {
        let temp_dir = TempDir::new().unwrap();
        let cwd = temp_dir.path();
        let claude_dir = cwd.join(".claude");
        fs::create_dir_all(&claude_dir).unwrap();

        // Create existing settings
        let existing = json!({
            "some_setting": true,
            "hooks": {
                "CustomHook": [{"hooks": [{"type": "command", "command": "echo custom"}]}]
            }
        });
        let settings_path = claude_dir.join("settings.local.json");
        fs::write(
            &settings_path,
            serde_json::to_string_pretty(&existing).unwrap(),
        )
        .unwrap();

        // Generate our config
        let exomonad = PathBuf::from("/test/exomonad");
        let config = HookConfig::generate(cwd, &exomonad).unwrap();

        // Verify merge
        let content = fs::read_to_string(config.settings_path()).unwrap();
        let settings: Value = serde_json::from_str(&content).unwrap();

        // Our hooks added
        assert!(settings["hooks"]["PreToolUse"].is_array());
        // Existing hooks preserved
        assert!(settings["hooks"]["CustomHook"].is_array());
        // Existing settings preserved
        assert_eq!(settings["some_setting"], true);

        // Drop and verify restoration
        drop(config);

        let restored = fs::read_to_string(&settings_path).unwrap();
        let restored: Value = serde_json::from_str(&restored).unwrap();
        assert_eq!(restored["some_setting"], true);
        // Our hooks removed
        assert!(
            restored["hooks"]["PreToolUse"].is_null()
                || !restored["hooks"]["PreToolUse"].is_array()
        );
    }

    #[test]
    fn test_is_exomonad_hook() {
        let our_hook = json!([{
            "matcher": "*",
            "hooks": [{"type": "command", "command": "/bin/exomonad hook pre-tool-use"}]
        }]);
        assert!(is_exomonad_hook(&our_hook));

        let other_hook = json!([{
            "matcher": "*",
            "hooks": [{"type": "command", "command": "echo hello"}]
        }]);
        assert!(!is_exomonad_hook(&other_hook));
    }
}
