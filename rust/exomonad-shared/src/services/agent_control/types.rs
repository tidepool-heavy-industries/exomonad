//! Shared agent-launch types used by both classic mode (`AgentControlService`)
//! and experimental node mode (`exo-runtime`'s Spawner).
//!
//! These are extracted into the shared crate because `launch.rs` (also shared)
//! and both runtimes need `AgentType`/`ClaudeSpawnFlags` without linking classic.

use serde::{Deserialize, Serialize};

/// Agent type for spawned agents.
///
/// Determines which CLI tool to use when spawning an agent in a tmux window.
/// Each type has different command names and prompt flags.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
#[derive(Default)]
pub enum AgentType {
    /// Claude Code CLI (spawns with `claude --prompt '...'`).
    Claude,

    /// Gemini CLI (spawns with `gemini --prompt-interactive '...'`).
    ///
    /// Default agent type.
    #[default]
    Gemini,

    /// Custom binary agent (e.g., shoal-agent).
    Shoal,

    /// Plain long-running process (no MCP, no agent identity, no worktree).
    /// Used for companion processes like mock servers, log tailers, etc.
    Process,
}

/// Static metadata for each agent type, replacing per-method match dispatch.
pub(crate) struct AgentMetadata {
    pub(crate) command: &'static str,
    pub(crate) prompt_flag: &'static str,
    pub(crate) suffix: &'static str,
    pub(crate) emoji: &'static str,
    pub(crate) default_model: &'static str,
}

pub(crate) const CLAUDE_META: AgentMetadata = AgentMetadata {
    command: "claude",
    prompt_flag: "",
    suffix: "claude",
    emoji: "\u{1F916}", // 🤖
    default_model: "claude-3-5-sonnet-20241022",
};

pub(crate) const GEMINI_META: AgentMetadata = AgentMetadata {
    command: "gemini",
    prompt_flag: "--prompt-interactive",
    suffix: "gemini",
    emoji: "\u{1F48E}", // 💎
    default_model: "gemini",
};

pub(crate) const SHOAL_META: AgentMetadata = AgentMetadata {
    command: "shoal-agent",
    prompt_flag: "",
    suffix: "shoal",
    emoji: "\u{1F30A}", // 🌊
    default_model: "gemini",
};

pub(crate) const PROCESS_META: AgentMetadata = AgentMetadata {
    command: "",
    prompt_flag: "",
    suffix: "process",
    emoji: "\u{2699}\u{FE0F}", // ⚙️
    default_model: "",
};

impl AgentType {
    /// The default LLM model for this agent type.
    pub fn default_model(&self) -> &'static str {
        self.metadata().default_model
    }

    pub(crate) fn metadata(&self) -> &'static AgentMetadata {
        match self {
            AgentType::Claude => &CLAUDE_META,
            AgentType::Gemini => &GEMINI_META,
            AgentType::Shoal => &SHOAL_META,
            AgentType::Process => &PROCESS_META,
        }
    }

    pub(crate) fn command(&self) -> &'static str {
        self.metadata().command
    }
    pub(crate) fn prompt_flag(&self) -> &'static str {
        self.metadata().prompt_flag
    }
    /// Agent type suffix for naming (e.g., "claude", "gemini").
    pub fn suffix(&self) -> &'static str {
        self.metadata().suffix
    }
    /// Emoji for display in tmux windows.
    pub fn emoji(&self) -> &'static str {
        self.metadata().emoji
    }

    /// tmux window display name for an agent with this type and slug.
    pub fn tab_display_name(&self, slug: &str) -> String {
        format!("{} {}", self.emoji(), slug)
    }

    /// Infer agent type from a worktree directory name (e.g., "feature-a-claude" → Claude).
    pub fn from_dir_name(dir_name: &str) -> Self {
        if dir_name.ends_with("-claude") {
            AgentType::Claude
        } else if dir_name.ends_with("-shoal") {
            AgentType::Shoal
        } else if dir_name.ends_with("-process") {
            AgentType::Process
        } else {
            AgentType::Gemini
        }
    }
}

/// Claude-specific spawn flags for permission control.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClaudeSpawnFlags {
    /// Permission mode. None = --dangerously-skip-permissions.
    pub permission_mode: Option<crate::domain::PermissionMode>,
    /// Tool patterns to allow (e.g., "Read", "Grep").
    pub allowed_tools: Vec<String>,
    /// Tool patterns to disallow (e.g., "Bash").
    pub disallowed_tools: Vec<String>,
    /// `--model` to launch with (e.g. "sonnet"). `None` inherits the launcher's default model.
    pub model: Option<String>,
    /// `--settings <path>` — a private settings file (hooks) merged over the cwd's settings, so the
    /// node never writes the shared cwd's `.claude/settings.local.json`. `None` ⇒ no flag.
    pub settings_path: Option<String>,
    /// `--mcp-config <path>` — a private MCP config merged over the cwd's `.mcp.json` (plain, NOT
    /// `--strict-mcp-config`, so the user's own MCP servers survive). `None` ⇒ no flag.
    pub mcp_config_path: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_type_command() {
        assert_eq!(AgentType::Claude.command(), "claude");
        assert_eq!(AgentType::Gemini.command(), "gemini");
    }

    #[test]
    fn test_agent_type_prompt_flag() {
        assert_eq!(AgentType::Claude.prompt_flag(), "");
        assert_eq!(AgentType::Gemini.prompt_flag(), "--prompt-interactive");
    }

    #[test]
    fn test_agent_type_default_model() {
        assert_eq!(
            AgentType::Claude.default_model(),
            "claude-3-5-sonnet-20241022"
        );
        assert_eq!(AgentType::Gemini.default_model(), "gemini");
        assert_eq!(AgentType::Shoal.default_model(), "gemini");
    }

    #[test]
    fn test_agent_type_suffix() {
        assert_eq!(AgentType::Claude.suffix(), "claude");
        assert_eq!(AgentType::Gemini.suffix(), "gemini");
    }

    #[test]
    fn test_agent_type_default() {
        assert_eq!(AgentType::default(), AgentType::Gemini);
    }

    #[test]
    fn test_agent_type_emoji() {
        assert_eq!(AgentType::Claude.emoji(), "🤖");
        assert_eq!(AgentType::Gemini.emoji(), "💎");
    }

    #[test]
    fn test_agent_type_deserialization() {
        use serde_json;

        let claude: AgentType = serde_json::from_str("\"claude\"").unwrap();
        assert_eq!(claude, AgentType::Claude);

        let gemini: AgentType = serde_json::from_str("\"gemini\"").unwrap();
        assert_eq!(gemini, AgentType::Gemini);

        // Invalid agent type should fail at parse boundary
        let invalid = serde_json::from_str::<AgentType>("\"invalid\"");
        assert!(invalid.is_err());
    }
}
