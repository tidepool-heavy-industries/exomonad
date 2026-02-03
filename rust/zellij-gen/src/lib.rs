//! Generate Zellij KDL layouts for ExoMonad agents.
//!
//! Provides layout generation with zjstatus (Solarized Dark theme) and
//! consistent styling across all agent tabs.

use askama::Template;
use std::path::Path;

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
}

/// Template for a single subagent layout (wraps agent tab with zjstatus).
#[derive(Template)]
#[template(path = "subagent.kdl.j2", escape = "none")]
struct SubagentLayout {
    agent_tab: String,
}

/// Template for the main layout with multiple agent tabs.
#[derive(Template)]
#[template(path = "main.kdl.j2", escape = "none")]
struct MainLayout {
    agent_tabs: Vec<String>,
}

/// Generate a complete layout for a single agent tab with zjstatus.
///
/// Returns a KDL layout string ready to write to a file and pass to
/// `zellij action new-tab --layout <path>`.
pub fn generate_agent_layout(params: &AgentTabParams) -> Result<String, askama::Error> {
    // Render the agent tab
    let tab = AgentTab {
        tab_name: params.tab_name.to_string(),
        pane_name: params.pane_name.to_string(),
        command: params.command.to_string(),
        cwd: params.cwd.display().to_string(),
        shell: params.shell.to_string(),
        focus: params.focus,
    }
    .render()?;

    // Wrap in subagent layout (includes zjstatus)
    SubagentLayout { agent_tab: tab }.render()
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
            }
            .render()
        })
        .collect();

    MainLayout {
        agent_tabs: rendered_tabs?,
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
        };

        let layout = generate_agent_layout(&params).unwrap();

        // Verify key components
        assert!(layout.contains("tab name=\"🤖 473-test\""));
        assert!(layout.contains("focus=true"));
        assert!(layout.contains("zjstatus.wasm"));
        assert!(layout.contains("exomonad-plugin.wasm"));
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
            },
            AgentTabParams {
                tab_name: "Tab2",
                pane_name: "P2",
                command: "cmd2",
                cwd: Path::new("/tmp/2"),
                shell: "/bin/zsh",
                focus: false,
            },
        ];

        let layout = generate_main_layout(tabs).unwrap();

        assert!(layout.contains("tab name=\"Tab1\""));
        assert!(layout.contains("tab name=\"Tab2\""));
        assert!(layout.contains("zjstatus.wasm"));
    }
}
