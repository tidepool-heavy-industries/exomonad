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
    widgets::{Block, Borders, Paragraph, Tabs},
    Terminal,
};
use std::{io, time::Duration};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {}

#[tokio::main]
async fn main() -> Result<()> {
    let _args = Args::parse();

    // Setup logging
    tracing_subscriber::fmt::init();

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Run app
    let res = run_app(&mut terminal).await;

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

async fn run_app<B: Backend>(terminal: &mut Terminal<B>) -> io::Result<()> {
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
            let content = match tab_index {
                0 => render_overview(),
                1 => render_logs(),
                2 => render_controls(),
                _ => Paragraph::new("Unknown tab"),
            };
            
            f.render_widget(content, chunks[1]);
        }).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        if crossterm::event::poll(Duration::from_millis(250))? {
            if let Event::Key(key) = event::read()? {
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
                    _ => {}
                }
            }
        }
    }
}

fn render_overview<'a>() -> Paragraph<'a> {
    let text = vec![
        Line::from(vec![
            Span::raw("Status: "),
            Span::styled("Idle", Style::default().fg(Color::Green)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::raw("Active Tool: "),
            Span::styled("None", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from("System Metrics:"),
        Line::from("  CPU: 12%"),
        Line::from("  Mem: 256MB"),
    ];
    Paragraph::new(text).block(Block::default().borders(Borders::ALL).title("Overview"))
}

fn render_logs<'a>() -> Paragraph<'a> {
    let text = vec![
        Line::from("12:00:01 [INFO] Agent started"),
        Line::from("12:00:02 [INFO] Connected to control server"),
        Line::from("12:00:05 [WARN] Low memory warning (simulation)"),
    ];
    Paragraph::new(text).block(Block::default().borders(Borders::ALL).title("Recent Logs"))
}

fn render_controls<'a>() -> Paragraph<'a> {
    let text = vec![
        Line::from("[P] Pause Agent"),
        Line::from("[R] Restart Session"),
        Line::from("[C] Clear Context"),
    ];
    Paragraph::new(text).block(Block::default().borders(Borders::ALL).title("Actions"))
}