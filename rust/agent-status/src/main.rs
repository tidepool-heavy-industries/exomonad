mod api;
mod component;
mod components;
mod state;

use anyhow::Result;
use clap::Parser;
use crossterm::{
    event::{DisableMouseCapture, EnableMouseCapture, Event, EventStream, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use futures::StreamExt;
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Tabs},
    Terminal,
};
use std::{io, time::Duration, sync::{Arc, Mutex}};
use tokio::sync::mpsc;
use api::ApiClient;
use state::DashboardState;
use component::{Component, Action};
use components::{overview::OverviewComponent, logs::LogsComponent, controls::ControlsComponent};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long, default_value = "http://localhost:7432")]
    control_server: String,
}

enum AppCommand {
    StopAgent(String),
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // Setup logging
    tracing_subscriber::fmt::init();

    // Setup state
    let state = Arc::new(Mutex::new(DashboardState::default()));
    
    let state_clone = state.clone();
    let client = ApiClient::new(args.control_server.clone());
    let client = Arc::new(client);
    let client_clone = client.clone();

    // Command channel
    let (tx, mut rx) = mpsc::channel::<AppCommand>(10);

    // Command processor
    let client_cmd = client.clone();
    tokio::spawn(async move {
        while let Some(cmd) = rx.recv().await {
            match cmd {
                AppCommand::StopAgent(id) => {
                    if let Err(err) = client_cmd.stop_agent(&id).await {
                        tracing::error!(agent_id = %id, error = ?err, "failed to stop agent");
                    }
                }
            }
        }
    });

    // Polling task
    tokio::spawn(async move {
        loop {
            // 1. Fetch Agents
            match client_clone.get_agents().await {
                Ok(agents) => {
                    let mut s = state_clone.lock().expect("State lock poisoned");
                    s.agents = agents;
                    s.connected = true;
                    s.last_updated = Some(chrono::Local::now().to_rfc3339());
                    
                    // Adjust selection if out of bounds
                    if !s.agents.is_empty() && s.selected_index >= s.agents.len() {
                        s.selected_index = s.agents.len() - 1;
                    }
                }
                Err(_) => {
                    let mut s = state_clone.lock().expect("State lock poisoned");
                    s.connected = false;
                }
            }

            // 2. Fetch Logs if needed
            let selected_id = {
                let s = state_clone.lock().expect("State lock poisoned");
                if s.agents.is_empty() { None } else { Some(s.agents[s.selected_index].id.clone()) }
            };

            if let Some(id) = selected_id {
                if let Ok(logs) = client_clone.get_logs(&id).await {
                    let mut s = state_clone.lock().expect("State lock poisoned");
                    s.logs_cache.insert(id, logs);
                }
            }

            tokio::time::sleep(Duration::from_secs(2)).await;
        }
    });

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Run app
    let res = run_app(&mut terminal, state, tx).await;

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        eprintln!("{:?}", err);
    }

    Ok(())
}

async fn run_app<B: Backend>(
    terminal: &mut Terminal<B>, 
    state: Arc<Mutex<DashboardState>>,
    tx: mpsc::Sender<AppCommand>
) -> io::Result<()> {
    let mut tab_index = 0;
    let tabs = vec!["Overview", "Logs", "Controls"];
    let mut reader = EventStream::new();

    loop {
        // Draw
        terminal.draw(|f| {
             let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(1)
                .constraints([Constraint::Length(3), Constraint::Min(0)].as_ref())
                .split(f.area());

            // Tabs
            let titles: Vec<_> = tabs
                .iter()
                .map(|t| {
                    let (first, rest) = t.split_at(1);
                    Line::from(vec![
                        Span::styled(first, Style::default().fg(Color::Yellow)),
                        Span::styled(rest, Style::default().fg(Color::Green)),
                    ])
                })
                .collect();
            
            let tabs_widget = Tabs::new(titles)
                .block(Block::default().borders(Borders::ALL).title("Agent Dashboard"))
                .highlight_style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))
                .select(tab_index);
            
            f.render_widget(tabs_widget, chunks[0]);

            // Component Rendering
            let s = state.lock().expect("State lock poisoned");
            match tab_index {
                0 => {
                    let mut comp = OverviewComponent { state: &s };
                    comp.render(f, chunks[1]);
                }
                1 => {
                    let mut comp = LogsComponent { state: &s };
                    comp.render(f, chunks[1]);
                }
                2 => {
                    let mut comp = ControlsComponent { state: &s };
                    comp.render(f, chunks[1]);
                }
                _ => {}
            }
        }).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        // Event Loop
        let event = tokio::select! {
            // We can add a ticker here if we wanted constant refreshes independent of backend updates
            // _ = tokio::time::sleep(Duration::from_millis(100)) => None, 
            maybe_event = reader.next() => maybe_event,
        };

        match event {
            Some(Ok(Event::Key(key))) => {
                 // Global keys
                 match key.code {
                    KeyCode::Char('q') => return Ok(()),
                    KeyCode::Right | KeyCode::Tab => {
                        tab_index = (tab_index + 1) % tabs.len();
                        continue;
                    }
                    KeyCode::Left => {
                        if tab_index > 0 {
                            tab_index -= 1;
                        } else {
                            tab_index = tabs.len() - 1;
                        }
                        continue;
                    }
                    _ => {}
                }

                // Component keys
                let mut s = state.lock().expect("State lock poisoned");
                let action = match tab_index {
                    0 => {
                        let mut comp = OverviewComponent { state: &s };
                        comp.handle_key_event(key)
                    },
                    1 => {
                        let mut comp = LogsComponent { state: &s };
                        comp.handle_key_event(key)
                    },
                    2 => {
                        let mut comp = ControlsComponent { state: &s };
                        comp.handle_key_event(key)
                    }
                    _ => Ok(Action::None),
                };

                // Handle Actions
                match action {
                    Ok(Action::SelectNext) => {
                        if !s.agents.is_empty() {
                            s.selected_index = (s.selected_index + 1) % s.agents.len();
                        }
                    }
                    Ok(Action::SelectPrev) => {
                        if !s.agents.is_empty() {
                             if s.selected_index > 0 {
                                s.selected_index -= 1;
                            } else {
                                s.selected_index = s.agents.len() - 1;
                            }
                        }
                    }
                    Ok(Action::KillAgent) => {
                         if !s.agents.is_empty() {
                            let id = s.agents[s.selected_index].id.clone();
                            let tx = tx.clone();
                            tokio::spawn(async move {
                                let _ = tx.send(AppCommand::StopAgent(id)).await;
                            });
                        }
                    }
                    Ok(Action::Quit) => return Ok(()),
                    _ => {}
                }
            }
            Some(Ok(_)) => {} // Other events
            Some(Err(e)) => return Err(io::Error::new(io::ErrorKind::Other, e)),
            None => break,
        }
    }

    Ok(())
}
