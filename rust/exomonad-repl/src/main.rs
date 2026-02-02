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
    
    // Main loop
    model.terminal.enter_alternate_screen()?;
    model.terminal.enable_raw_mode()?;
    
    while !model.quit {
        if model.redo_render {
            model.terminal.draw(|f| {
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
                
                model.app.view(&Id::ToolList, f, chunks[0]);
                model.app.view(&Id::Form, f, right_chunks[0]);
                model.app.view(&Id::Results, f, right_chunks[1]);
            })?;
            model.redo_render = false;
        }
        
        match model.app.tick(tuirealm::PollStrategy::Once) {
            Err(_) => {
                // Handle error
            }
            Ok(messages) if !messages.is_empty() => {
                model.redo_render = true;
                for msg in messages {
                    match msg {
                        Msg::ToolSelected(name) => {
                            if let Some(tool) = model.tools.iter().find(|t| t.name == name) {
                                let mut form = ui::Form::default();
                                form.set_tool(tool);
                                model.app.remount(Id::Form, Box::new(form), vec![])?;
                                model.app.active(&Id::Form)?;
                            }
                        }
                        Msg::ExecuteTool(name, args) => {
                            let client_clone = client.clone();
                            let name_clone = name.clone();
                            let args_clone = args.clone();
                            
                            // For now we'll just block, or we can use a message to signal completion.
                            // To use a message, we'd need a UserEvent in tuirealm.
                            // Let's just do it synchronously for now to see it working.
                            
                            let result = tokio::runtime::Handle::current().block_on(async move {
                                client_clone.call_tool(&name_clone, args_clone).await
                            });
                            
                            let mut results_comp = ui::Results::default();
                            match result {
                                Ok(val) => {
                                    results_comp.set_text(&serde_json::to_string_pretty(&val).unwrap_or_else(|_| "Failed to serialize".to_string()));
                                }
                                Err(e) => {
                                    results_comp.set_text(&format!("Error: {}", e));
                                }
                            }
                            model.app.remount(Id::Results, Box::new(results_comp), vec![])?;
                        }
                        Msg::AppClose => model.quit = true,
                        _ => {
                            let mut m = Some(msg);
                            while let Some(next) = model.update(m) {
                                m = Some(next);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    
    model.terminal.leave_alternate_screen()?;
    model.terminal.disable_raw_mode()?;
    
    Ok(())
}