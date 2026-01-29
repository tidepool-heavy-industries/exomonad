mod api;
mod state;

use anyhow::Result;
use clap::Parser;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Tabs, Wrap},
    Terminal,
};
use std::{io, time::Duration, sync::{Arc, Mutex}};
use tokio::sync::mpsc;
use api::ApiClient;
use state::DashboardState;

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
                    let _ = client_cmd.stop_agent(&id).await;
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
                    let mut s = state_clone.lock().unwrap();
                    s.agents = agents;
                    s.connected = true;
                    s.last_updated = Some(chrono::Local::now().to_rfc3339());
                    
                    // Adjust selection if out of bounds
                    if !s.agents.is_empty() && s.selected_index >= s.agents.len() {
                        s.selected_index = s.agents.len() - 1;
                    }
                }
                Err(_) => {
                    let mut s = state_clone.lock().unwrap();
                    s.connected = false;
                }
            }

            // 2. Fetch Logs if needed (logic: always fetch logs for selected agent for simplicity)
            let selected_id = {
                let s = state_clone.lock().unwrap();
                if s.agents.is_empty() { None } else { Some(s.agents[s.selected_index].id.clone()) }
            };

            if let Some(id) = selected_id {
                if let Ok(logs) = client_clone.get_logs(&id).await {
                    let mut s = state_clone.lock().unwrap();
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

    loop {
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

            // Content
            let s = state.lock().unwrap();
            
            match tab_index {
                0 => render_overview(f, chunks[1], &s),
                1 => render_logs(f, chunks[1], &s),
                2 => render_controls(f, chunks[1], &s),
                _ => {},
            };
            
        }).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        if crossterm::event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                let mut s = state.lock().unwrap();
                match key.code {
                    KeyCode::Char('q') => return Ok(()),
                    KeyCode::Right | KeyCode::Tab => {
                        tab_index = (tab_index + 1) % tabs.len();
                    }
                    KeyCode::Left => {
                        if tab_index > 0 {
                            tab_index -= 1;
                        } else {
                            tab_index = tabs.len() - 1;
                        }
                    }
                    KeyCode::Down | KeyCode::Char('j') => {
                        if !s.agents.is_empty() {
                            s.selected_index = (s.selected_index + 1) % s.agents.len();
                        }
                    }
                    KeyCode::Up | KeyCode::Char('k') => {
                        if !s.agents.is_empty() {
                            if s.selected_index > 0 {
                                s.selected_index -= 1;
                            } else {
                                s.selected_index = s.agents.len() - 1;
                            }
                        }
                    }
                    KeyCode::Char('K') | KeyCode::Char('x') => {
                        // Kill/Stop agent
                        if !s.agents.is_empty() {
                            let id = s.agents[s.selected_index].id.clone();
                            let tx = tx.clone();
                            tokio::spawn(async move {
                                let _ = tx.send(AppCommand::StopAgent(id)).await;
                            });
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

fn render_overview(f: &mut ratatui::Frame, area: ratatui::layout::Rect, state: &DashboardState) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(40), Constraint::Percentage(60)].as_ref())
        .split(area);

    // Left: List of Agents
    let items: Vec<ListItem> = state.agents
        .iter()
        .map(|agent| {
            let style = if agent.status == "running" {
                Style::default().fg(Color::Green)
            } else {
                Style::default().fg(Color::Red)
            };
            let content = Line::from(vec![
                Span::styled(format!("● {}", agent.id), style),
                Span::raw(format!(" [{}]", agent.status)),
            ]);
            ListItem::new(content)
        })
        .collect();

    let mut list_state = ListState::default();
    list_state.select(Some(state.selected_index));

    let list = List::new(items)
        .block(Block::default().borders(Borders::ALL).title("Agents"))
        .highlight_style(Style::default().bg(Color::DarkGray).add_modifier(Modifier::BOLD));

    f.render_stateful_widget(list, chunks[0], &mut list_state);

    // Right: Details
    if state.agents.is_empty() {
        let text = Paragraph::new("No agents active.").block(Block::default().borders(Borders::ALL));
        f.render_widget(text, chunks[1]);
    } else {
        let agent = &state.agents[state.selected_index];
        let text = vec![
            Line::from(vec![Span::raw("ID: "), Span::styled(&agent.id, Style::default().fg(Color::Cyan))]),
            Line::from(vec![Span::raw("Container: "), Span::raw(&agent.container_id)]),
            Line::from(vec![Span::raw("Status: "), Span::raw(&agent.status)]),
            Line::from(vec![Span::raw("Started: "), Span::raw(&agent.started_at)]),
            Line::from(""),
            Line::from(vec![Span::styled("Last Action:", Style::default().add_modifier(Modifier::BOLD))]),
            Line::from(agent.last_action.clone().unwrap_or_default()),
            Line::from(""),
            Line::from(vec![Span::styled("Blocker:", Style::default().fg(Color::Red))]),
            Line::from(agent.blocker.clone().unwrap_or_default()),
        ];
        
        let details = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title("Details"))
            .wrap(Wrap { trim: true });
        
        f.render_widget(details, chunks[1]);
    }
}

fn render_logs(f: &mut ratatui::Frame, area: ratatui::layout::Rect, state: &DashboardState) {
    if state.agents.is_empty() {
        let text = Paragraph::new("No agents active.").block(Block::default().borders(Borders::ALL));
        f.render_widget(text, area);
        return;
    }

    let agent = &state.agents[state.selected_index];
    let logs = state.logs_cache.get(&agent.id).map(|s| s.as_str()).unwrap_or("Loading logs...");

    let p = Paragraph::new(logs)
        .block(Block::default().borders(Borders::ALL).title(format!("Logs: {}", agent.id)))
        .wrap(Wrap { trim: false }); // Logs usually better unwrapped or scrolled, sticking to basic for now
    
    f.render_widget(p, area);
}

fn render_controls(f: &mut ratatui::Frame, area: ratatui::layout::Rect, state: &DashboardState) {
    let text = vec![
        Line::from("Available Actions:"),
        Line::from(""),
        Line::from(vec![Span::styled("[K] / [x]", Style::default().fg(Color::Red)), Span::raw(" Stop/Kill Selected Agent")]),
        Line::from(""),
        Line::from(vec![Span::styled("Status: ", Style::default().add_modifier(Modifier::BOLD))]),
        Line::from(if state.connected { 
            Span::styled("Connected to Control Server", Style::default().fg(Color::Green)) 
        } else { 
            Span::styled("Disconnected (Retrying...)", Style::default().fg(Color::Red)) 
        }),
    ];
    
    let p = Paragraph::new(text)
        .block(Block::default().borders(Borders::ALL).title("Controls"));
    
    f.render_widget(p, area);
}
