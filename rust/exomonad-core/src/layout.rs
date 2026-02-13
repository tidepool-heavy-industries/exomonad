//! Generate Zellij KDL layouts for ExoMonad agents.
//!
//! Provides layout generation with zjstatus (Solarized Dark theme) and
//! consistent styling across all agent tabs.

use askama::Template;
use std::path::Path;

/// Resolve the exomonad-plugin WASM path to an absolute `file:` URL.
///
/// Uses `EXOMONAD_PLUGIN_PATH` env var or the default install location.
/// Returns the expanded absolute path (e.g., `file:/home/user/.config/zellij/plugins/exomonad-plugin.wasm`).
/// Both the KDL layout and the popup service must use this same path so Zellij
/// routes pipe messages to the pre-loaded instance instead of spawning a second one.
pub fn resolve_plugin_path() -> Option<String> {
    let raw = std::env::var("EXOMONAD_PLUGIN_PATH")
        .unwrap_or_else(|_| "~/.config/zellij/plugins/exomonad-plugin.wasm".to_string());

    let expanded = if let Some(rest) = raw.strip_prefix("~/") {
        dirs::home_dir()
            .map(|home| home.join(rest).to_string_lossy().to_string())
            .unwrap_or(raw.clone())
    } else {
        raw.clone()
    };

    if std::path::Path::new(&expanded).exists() {
        Some(format!("file:{}", expanded))
    } else {
        None
    }
}

/// Parameters for generating an agent tab.
pub struct AgentTabParams<'a> {
    /// Display name for the tab (e.g., "🤖 473-refactor")
    pub tab_name: &'a str,
    /// Name for the main pane
    pub pane_name: &'a str,
    /// Command to run (already escaped for shell)
    pub command: &'a str,
    /// Working directory (absolute path)
    pub cwd: &'a Path,
    /// Shell to use (e.g., "/bin/zsh")
    pub shell: &'a str,
    /// Whether this tab should be focused
    pub focus: bool,
    /// Whether the pane should close when the command exits.
    /// True for short-lived agents, false for interactive sessions.
    pub close_on_exit: bool,
}

/// Template for an agent tab with interaction pane and status plugin.
#[derive(Template)]
#[template(path = "agent_tab.kdl.j2", escape = "none")]
struct AgentTab {
    tab_name: String,
    pane_name: String,
    command: String,
    cwd: String,
    shell: String,
    focus: bool,
    close_on_exit: bool,
}

/// Template for a single subagent layout (wraps agent tab with zjstatus).
#[derive(Template)]
#[template(path = "subagent.kdl.j2", escape = "none")]
struct SubagentLayout {
    agent_tab: String,
    plugin_path: Option<String>,
}

/// Template for the main layout with multiple agent tabs.
#[derive(Template)]
#[template(path = "main.kdl.j2", escape = "none")]
struct MainLayout {
    agent_tabs: Vec<String>,
    plugin_path: Option<String>,
}

/// Generate a complete layout for a single agent tab with zjstatus.
///
/// Returns a KDL layout string ready to write to a file and pass to
/// `zellij action new-tab --layout <path>`.
pub fn generate_agent_layout(params: &AgentTabParams) -> Result<String, askama::Error> {
    let tab = AgentTab {
        tab_name: params.tab_name.to_string(),
        pane_name: params.pane_name.to_string(),
        command: params.command.to_string(),
        cwd: params.cwd.display().to_string(),
        shell: params.shell.to_string(),
        focus: params.focus,
        close_on_exit: params.close_on_exit,
    }
    .render()?;

    SubagentLayout {
        agent_tab: tab,
        plugin_path: resolve_plugin_path(),
    }
    .render()
}

/// Generate a complete layout with multiple agent tabs and zjstatus.
///
/// Used for the main TL session with multiple tabs.
pub fn generate_main_layout(tabs: Vec<AgentTabParams>) -> Result<String, askama::Error> {
    let rendered_tabs: Result<Vec<_>, _> = tabs
        .iter()
        .map(|params| {
            AgentTab {
                tab_name: params.tab_name.to_string(),
                pane_name: params.pane_name.to_string(),
                command: params.command.to_string(),
                cwd: params.cwd.display().to_string(),
                shell: params.shell.to_string(),
                focus: params.focus,
                close_on_exit: params.close_on_exit,
            }
            .render()
        })
        .collect();

    MainLayout {
        agent_tabs: rendered_tabs?,
        plugin_path: resolve_plugin_path(),
    }
    .render()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_agent_layout() {
        let params = AgentTabParams {
            tab_name: "🤖 473-test",
            pane_name: "Agent",
            command: "claude --prompt 'test'",
            cwd: Path::new("/tmp/test"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();

        // Verify key components
        assert!(layout.contains("tab name=\"🤖 473-test\""));
        assert!(layout.contains("focus=true"));
        assert!(layout.contains("zjstatus.wasm"));
        assert!(layout.contains("/bin/zsh"));
        assert!(layout.contains("claude --prompt 'test'"));
        assert!(layout.contains("/tmp/test"));
    }

    #[test]
    fn test_generate_main_layout() {
        let tabs = vec![
            AgentTabParams {
                tab_name: "Tab1",
                pane_name: "P1",
                command: "cmd1",
                cwd: Path::new("/tmp/1"),
                shell: "/bin/zsh",
                focus: true,
                close_on_exit: true,
            },
            AgentTabParams {
                tab_name: "Tab2",
                pane_name: "P2",
                command: "cmd2",
                cwd: Path::new("/tmp/2"),
                shell: "/bin/zsh",
                focus: false,
                close_on_exit: true,
            },
        ];

        let layout = generate_main_layout(tabs).unwrap();

        assert!(layout.contains("tab name=\"Tab1\""));
        assert!(layout.contains("tab name=\"Tab2\""));
        assert!(layout.contains("zjstatus.wasm"));
    }

    // === Edge case tests ===

    #[test]
    fn test_tab_name_with_emoji() {
        let params = AgentTabParams {
            tab_name: "🤖 473-fix",
            pane_name: "Agent",
            command: "echo test",
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        assert!(layout.contains("tab name=\"🤖 473-fix\""));
    }

    #[test]
    fn test_tab_name_with_special_chars() {
        let params = AgentTabParams {
            tab_name: "Tab-123_test",
            pane_name: "Agent",
            command: "echo test",
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        assert!(layout.contains("tab name=\"Tab-123_test\""));
    }

    #[test]
    fn test_command_with_single_quotes() {
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "Agent",
            command: "claude --prompt 'hello world'",
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        assert!(layout.contains("claude --prompt 'hello world'"));
    }

    #[test]
    fn test_command_with_double_quotes() {
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "Agent",
            command: r#"echo "hello world""#,
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        // Command should be in the layout (may need escaping depending on template)
        assert!(layout.contains("echo"));
        assert!(layout.contains("hello world"));
    }

    #[test]
    fn test_cwd_with_spaces() {
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "Agent",
            command: "echo test",
            cwd: Path::new("/path/with spaces/project"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        assert!(layout.contains("/path/with spaces/project"));
    }

    #[test]
    fn test_focus_false() {
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "Agent",
            command: "echo test",
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: false,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        // When focus=false, the tab line should not have focus=true
        // (KDL omits the attribute rather than setting it to false)
        assert!(layout.contains("tab name=\"Test\" {"));
        // But pane still has focus=true (inner pane focus, not tab focus)
        assert!(layout.contains("pane name=\"Agent\" focus=true"));
    }

    #[test]
    fn test_empty_pane_name() {
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "",
            command: "echo test",
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        // Should still generate valid KDL with empty name
        assert!(layout.contains("pane name=\"\""));
    }

    // === Structure validation tests ===

    #[test]
    fn test_layout_has_zjstatus() {
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "Agent",
            command: "echo test",
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        assert!(
            layout.contains("zjstatus.wasm"),
            "Layout should include zjstatus plugin"
        );
    }

    #[test]
    fn test_layout_close_on_exit() {
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "Agent",
            command: "echo test",
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        assert!(
            layout.contains("close_on_exit true"),
            "Layout should include close_on_exit true"
        );
    }

    #[test]
    fn test_main_layout_multiple_tabs() {
        let tabs = vec![
            AgentTabParams {
                tab_name: "Tab1",
                pane_name: "P1",
                command: "cmd1",
                cwd: Path::new("/tmp/1"),
                shell: "/bin/zsh",
                focus: true,
                close_on_exit: true,
            },
            AgentTabParams {
                tab_name: "Tab2",
                pane_name: "P2",
                command: "cmd2",
                cwd: Path::new("/tmp/2"),
                shell: "/bin/zsh",
                focus: false,
                close_on_exit: true,
            },
            AgentTabParams {
                tab_name: "Tab3",
                pane_name: "P3",
                command: "cmd3",
                cwd: Path::new("/tmp/3"),
                shell: "/bin/zsh",
                focus: false,
                close_on_exit: true,
            },
        ];

        let layout = generate_main_layout(tabs).unwrap();

        // All tabs should be present
        assert!(layout.contains("tab name=\"Tab1\""));
        assert!(layout.contains("tab name=\"Tab2\""));
        assert!(layout.contains("tab name=\"Tab3\""));

        // Should have zjstatus
        assert!(layout.contains("zjstatus.wasm"));
    }

    #[test]
    fn test_different_shells() {
        for shell in ["/bin/bash", "/bin/zsh", "/usr/bin/fish"] {
            let params = AgentTabParams {
                tab_name: "Test",
                pane_name: "Agent",
                command: "echo test",
                cwd: Path::new("/tmp"),
                shell,
                focus: true,
                close_on_exit: true,
            };

            let layout = generate_agent_layout(&params).unwrap();
            assert!(
                layout.contains(shell),
                "Layout should contain shell: {}",
                shell
            );
        }
    }

    #[test]
    fn test_long_command() {
        let long_command = "claude --prompt 'This is a very long prompt that contains multiple sentences and should still be handled correctly by the layout generator without any truncation or issues'";
        let params = AgentTabParams {
            tab_name: "Test",
            pane_name: "Agent",
            command: long_command,
            cwd: Path::new("/tmp"),
            shell: "/bin/zsh",
            focus: true,
            close_on_exit: true,
        };

        let layout = generate_agent_layout(&params).unwrap();
        assert!(layout.contains("very long prompt"));
    }

    #[test]
    fn test_empty_tabs_list() {
        let tabs: Vec<AgentTabParams> = vec![];
        let layout = generate_main_layout(tabs).unwrap();
        // Should still produce valid KDL with zjstatus
        assert!(layout.contains("zjstatus.wasm"));
    }

    #[test]
    fn test_main_layout_includes_plugin_when_path_set() {
        let layout = MainLayout {
            agent_tabs: vec![],
            plugin_path: Some("file:/home/test/.config/zellij/plugins/exomonad-plugin.wasm".to_string()),
        }
        .render()
        .unwrap();

        assert!(
            layout.contains("exomonad-plugin.wasm"),
            "Layout should include pre-loaded plugin pane"
        );
        assert!(
            layout.contains("file:/home/test/.config/zellij/plugins/exomonad-plugin.wasm"),
            "Layout should use absolute path for plugin"
        );
    }

    #[test]
    fn test_main_layout_omits_plugin_when_path_none() {
        let layout = MainLayout {
            agent_tabs: vec![],
            plugin_path: None,
        }
        .render()
        .unwrap();

        assert!(
            !layout.contains("exomonad-plugin"),
            "Layout should not include plugin pane when path is None"
        );
    }

    #[test]
    fn test_subagent_layout_includes_plugin() {
        let tab = AgentTab {
            tab_name: "Test".to_string(),
            pane_name: "Agent".to_string(),
            command: "echo test".to_string(),
            cwd: "/tmp".to_string(),
            shell: "/bin/zsh".to_string(),
            focus: true,
            close_on_exit: true,
        }
        .render()
        .unwrap();

        let layout = SubagentLayout {
            agent_tab: tab,
            plugin_path: Some("file:/home/test/.config/zellij/plugins/exomonad-plugin.wasm".to_string()),
        }
        .render()
        .unwrap();

        assert!(layout.contains("exomonad-plugin.wasm"));
    }
}
