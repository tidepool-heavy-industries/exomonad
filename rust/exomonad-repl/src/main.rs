mod app;
mod mcp_client;
mod schema_parser;
mod ui;

use anyhow::Result;
use app::{Id, Model, Msg};
use clap::Parser;
use mcp_client::McpClient;
use std::path::PathBuf;
use std::sync::Arc;
use tuirealm::{Update};

#[derive(Parser)]
#[command(name = "exomonad-repl")]
#[command(about = "TUI REPL for testing MCP tools")]
struct Args {
    /// Path to the WASM plugin
    #[arg(long, env = "EXOMONAD_WASM_PATH")]
    wasm: PathBuf,
}

struct TerminalGuard<'a>(&'a mut Model);

impl<'a> Drop for TerminalGuard<'a> {
    fn drop(&mut self) {
        let _ = self.0.terminal.leave_alternate_screen();
        let _ = self.0.terminal.disable_raw_mode();
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    
    // Initialize MCP client
    let client = Arc::new(McpClient::new(args.wasm).await?);
    let tools = client.list_tools().await?;
    let tool_names: Vec<String> = tools.iter().map(|t| t.name.clone()).collect();

    // Initialize model
    let mut model = Model::new(tools);
    
    // Register components
    model.app.mount(Id::ToolList, Box::new(ui::ToolList::new(tool_names)), vec![])?;
    model.app.mount(Id::Form, Box::new(ui::Form::default()), vec![])?;
    model.app.mount(Id::Results, Box::new(ui::Results::default()), vec![])?;
    model.app.mount(Id::GlobalListener, Box::new(ui::GlobalListener), vec![])?;
    
    // Active ToolList
    model.app.active(&Id::ToolList)?;
    
    // Setup terminal
    model.terminal.enter_alternate_screen()?;
    model.terminal.enable_raw_mode()?;
    
    // Ensure cleanup on exit/panic
    let mut _guard = TerminalGuard(&mut model);
    
    while !_guard.0.quit {
        if _guard.0.redo_render {
            _guard.0.terminal.draw(|f| {
                let chunks = tuirealm::ratatui::layout::Layout::default()
                    .direction(tuirealm::ratatui::layout::Direction::Horizontal)
                    .constraints([
                        tuirealm::ratatui::layout::Constraint::Percentage(30),
                        tuirealm::ratatui::layout::Constraint::Percentage(70),
                    ])
                    .split(f.area());
                
                let right_chunks = tuirealm::ratatui::layout::Layout::default()
                    .direction(tuirealm::ratatui::layout::Direction::Vertical)
                    .constraints([
                        tuirealm::ratatui::layout::Constraint::Percentage(50),
                        tuirealm::ratatui::layout::Constraint::Percentage(50),
                    ])
                    .split(chunks[1]);
                
                _guard.0.app.view(&Id::ToolList, f, chunks[0]);
                _guard.0.app.view(&Id::Form, f, right_chunks[0]);
                _guard.0.app.view(&Id::Results, f, right_chunks[1]);
            })?;
            _guard.0.redo_render = false;
        }
        
        match _guard.0.app.tick(tuirealm::PollStrategy::Once) {
            Err(err) => {
                eprintln!("Error during UI tick: {err}");
                _guard.0.quit = true;
            }
            Ok(messages) if !messages.is_empty() => {
                _guard.0.redo_render = true;
                for msg in messages {
                    match msg {
                        Msg::ToolSelected(name) => {
                            if let Some(tool) = _guard.0.tools.iter().find(|t| t.name == name) {
                                let mut form = ui::Form::default();
                                form.set_tool(tool);
                                _guard.0.app.remount(Id::Form, Box::new(form), vec![])?;
                                _guard.0.app.active(&Id::Form)?;
                            }
                        }
                        Msg::ExecuteTool(name, args) => {
                            // Direct await since main is async
                            let result = client.call_tool(&name, args).await;
                            
                            let mut results_comp = ui::Results::default();
                            match result {
                                Ok(val) => {
                                    results_comp.set_text(&serde_json::to_string_pretty(&val).unwrap_or_else(|_| "Failed to serialize".to_string()));
                                }
                                Err(e) => {
                                    results_comp.set_text(&format!("Error: {}", e));
                                }
                            }
                            _guard.0.app.remount(Id::Results, Box::new(results_comp), vec![])?;
                        }
                        Msg::SwitchFocus(id) => {
                            _guard.0.app.active(&id)?;
                        }
                        Msg::AppClose => _guard.0.quit = true,
                        _ => {
                            let mut m = Some(msg);
                            while let Some(next) = _guard.0.update(m) {
                                m = Some(next);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    
    // Cleanup happens via Drop
    Ok(())
}
