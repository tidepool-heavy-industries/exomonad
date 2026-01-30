mod api;
mod app;
mod backend;
mod components;
mod state;

use anyhow::Result;
use clap::Parser;
use crossterm::{
    event::{DisableMouseCapture, EnableMouseCapture},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use std::{
    io,
    sync::{Arc, RwLock},
    time::Duration,
};
use tuirealm::{
    Event, Update,
    application::Application,
    event::{Key, KeyEvent},
    listener::{EventListenerCfg, SyncPort},
    ratatui::{
        layout::{Constraint, Direction, Layout},
        style::{Color, Modifier, Style},
        widgets::{Block, Borders, Tabs},
    },
    terminal::TerminalBridge,
};

use api::ApiClient;
use app::{Id, Model, Msg, UserEvent};
use backend::BackendPort;
use components::{controls::ControlsComponent, logs::LogsComponent, overview::OverviewComponent};
use state::DashboardState;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long, default_value = "http://localhost:7432")]
    control_server: String,
}

impl Update<Msg> for Model {
    fn update(&mut self, msg: Option<Msg>) -> Option<Msg> {
        if let Some(msg) = msg {
            match msg {
                Msg::SelectNext => {
                    let mut s = self.state.write().unwrap();
                    if !s.agents.is_empty() {
                        s.selected_index = (s.selected_index + 1) % s.agents.len();
                        self.redraw = true;
                    }
                }
                Msg::SelectPrev => {
                    let mut s = self.state.write().unwrap();
                    if !s.agents.is_empty() {
                        if s.selected_index > 0 {
                            s.selected_index -= 1;
                        } else {
                            s.selected_index = s.agents.len() - 1;
                        }
                        self.redraw = true;
                    }
                }
                Msg::KillAgent(_) => {}
                Msg::None => {
                    self.redraw = true;
                }
            }
        }
        None
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    tracing_subscriber::fmt::init();

    let shared_state = Arc::new(RwLock::new(DashboardState::default()));
    let client = Arc::new(ApiClient::new(args.control_server.clone()));

    // Setup TUI
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;

    // In tuirealm 3.3.0, TerminalBridge handles terminal setup
    let mut terminal = TerminalBridge::new(tuirealm::terminal::CrosstermTerminalAdapter::new()?);

    let mut model = Model::new(shared_state.clone());

    // Note: max_poll is the 3rd argument for SyncPort::new in tuirealm 3.3?
    // Wait, previous error said: "argument #3 of type `usize` is missing".
    // Checking documentation via error message: SyncPort::new(poll, interval, max_poll)
    let port = SyncPort::new(
        Box::new(BackendPort::new(client.clone(), shared_state.clone())),
        Duration::from_millis(100),
        10, // max_poll events per tick
    );

    let mut app: Application<Id, Msg, UserEvent> = Application::init(
        EventListenerCfg::default()
            .port(port)
            .tick_interval(Duration::from_millis(200)),
    );

    // Mount Components
    app.mount(
        Id::Overview,
        Box::new(OverviewComponent::new(shared_state.clone())),
        vec![],
    )?;
    app.mount(
        Id::Logs,
        Box::new(LogsComponent::new(shared_state.clone())),
        vec![],
    )?;
    app.mount(
        Id::Controls,
        Box::new(ControlsComponent::new(shared_state.clone())),
        vec![],
    )?;

    app.active(&Id::Overview)?;

    // Main Loop
    while !model.quit {
        // Application::tick handles everything and returns messages
        if let Ok(messages) = app.tick(tuirealm::PollStrategy::UpTo(5)) {
            for msg in messages {
                if let Msg::KillAgent(id) = &msg {
                    let client_clone = client.clone();
                    let id_clone = id.clone();
                    tokio::spawn(async move {
                        let _ = client_clone.stop_agent(&id_clone).await;
                    });
                }
                model.update(Some(msg));
            }
        }

        if model.redraw {
            terminal.raw_mut().draw(|f| {
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .margin(1)
                    .constraints([Constraint::Length(3), Constraint::Min(0)].as_ref())
                    .split(f.area());

                let titles = vec!["Overview", "Logs", "Controls"];
                let tabs = Tabs::new(titles)
                    .block(
                        Block::default()
                            .borders(Borders::ALL)
                            .title("Agent Dashboard"),
                    )
                    .highlight_style(
                        Style::default()
                            .fg(Color::Yellow)
                            .add_modifier(Modifier::BOLD),
                    )
                    .select(model.tab_index);
                f.render_widget(tabs, chunks[0]);

                let active_id = match model.tab_index {
                    0 => &Id::Overview,
                    1 => &Id::Logs,
                    2 => &Id::Controls,
                    _ => &Id::Overview,
                };
                let _ = app.view(active_id, f, chunks[1]);
            })?;
            model.redraw = false;
        }

        // Manual global key handling for tab switching since components only handle their own focus
        // We can check if we have pending events that were not consumed?
        // Actually, let's just make the active component forward unhandled keys?
        // Or simpler: We can poll for input manually using crossterm event stream alongside the app?
        // NO, Application::tick consumes events.
        //
        // Correct approach: Components should forward keys they don't handle, OR we add a global listener.
        // But `app.tick` returns messages.
        //
        // For this simple app, we can make components return a specific Msg for tab switching
        // or just handle it in the components (as we did in the components::overview::OverviewComponent::on).
        // Wait, I removed the tab handling from components in the last update.
        // I should add it back to components or handle it globally.
        //
        // Let's add tab handling to all components.
    }

    // Cleanup
    disable_raw_mode()?;
    execute!(io::stdout(), LeaveAlternateScreen, DisableMouseCapture)?;

    Ok(())
}
