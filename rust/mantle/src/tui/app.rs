//! Main TUI application using ratatui

use super::events::TuiEvent;
use super::state::{AppState, TuiResult};
use crate::error::Result;

use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Frame, Terminal,
};
use std::io::{stdout, Stdout};
use std::sync::mpsc::Receiver;
use std::time::Duration;

/// Guard to ensure terminal is restored on drop (including panics)
struct TerminalGuard {
    terminal: Terminal<CrosstermBackend<Stdout>>,
}

impl TerminalGuard {
    fn new() -> Result<Self> {
        enable_raw_mode()?;
        stdout().execute(EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout());
        let terminal = Terminal::new(backend)?;
        Ok(Self { terminal })
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let _ = stdout().execute(LeaveAlternateScreen);
    }
}

/// Action returned from input handling
enum Action {
    /// Continue running
    Continue,
    /// Quit the TUI
    Quit,
}

/// Main entry point for the TUI
pub fn run(event_rx: Receiver<TuiEvent>) -> Result<TuiResult> {
    let mut guard = TerminalGuard::new()?;
    let mut state = AppState::default();

    loop {
        // Draw current state
        guard.terminal.draw(|frame| render(&state, frame))?;

        // Poll for TUI events from channel (non-blocking)
        while let Ok(tui_event) = event_rx.try_recv() {
            handle_tui_event(&mut state, tui_event);
        }

        // Poll for keyboard input with timeout
        if event::poll(Duration::from_millis(50))? {
            if let Event::Key(key) = event::read()? {
                // Only handle key press events, not release
                if key.kind == KeyEventKind::Press {
                    match handle_key(&mut state, key.code, key.modifiers) {
                        Action::Continue => {}
                        Action::Quit => break,
                    }
                }
            }
        }
    }

    Ok(state.into_result())
}

/// Handle a TUI event from the Claude subprocess
fn handle_tui_event(state: &mut AppState, event: TuiEvent) {
    match event {
        TuiEvent::Claude(stream_event) => {
            state.process_event(stream_event);
        }
        TuiEvent::Interrupt(signal) => {
            state.add_interrupt(signal);
        }
        TuiEvent::ProcessExit => {
            state.channel_disconnected = true;
        }
    }
}

/// Handle keyboard input
fn handle_key(state: &mut AppState, code: KeyCode, modifiers: KeyModifiers) -> Action {
    // Ctrl-C always force quits
    if code == KeyCode::Char('c') && modifiers.contains(KeyModifiers::CONTROL) {
        return Action::Quit;
    }

    match code {
        // Navigation
        KeyCode::Char('j') | KeyCode::Down => state.scroll_down(),
        KeyCode::Char('k') | KeyCode::Up => state.scroll_up(),
        KeyCode::Char('J') | KeyCode::PageDown => {
            for _ in 0..10 {
                state.scroll_down();
            }
        }
        KeyCode::Char('K') | KeyCode::PageUp => {
            for _ in 0..10 {
                state.scroll_up();
            }
        }
        KeyCode::Char('g') => state.go_to_top(),
        KeyCode::Char('G') => state.go_to_bottom(),

        // Expand/collapse
        KeyCode::Enter => state.toggle_expand(),
        KeyCode::Char('e') => state.expand_all(),
        KeyCode::Char('C') => state.collapse_all(), // Shift-C to avoid confusion

        // Quit (only when complete or disconnected)
        KeyCode::Char('q') => {
            if state.is_complete || state.channel_disconnected {
                return Action::Quit;
            }
        }
        _ => {}
    }
    Action::Continue
}

/// Render the full TUI
fn render(state: &AppState, frame: &mut Frame) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3), // Status bar
            Constraint::Min(5),    // Event list
            Constraint::Length(3), // Help bar
        ])
        .split(frame.area());

    render_status_bar(state, frame, chunks[0]);
    render_event_list(state, frame, chunks[1]);
    render_help_bar(state, frame, chunks[2]);
}

/// Render the status bar at the top
fn render_status_bar(state: &AppState, frame: &mut Frame, area: Rect) {
    let session = state
        .session_id
        .as_ref()
        .map(|s| {
            let short: String = s.chars().take(8).collect();
            short
        })
        .unwrap_or_else(|| "...".to_string());

    let model = state.model.as_deref().unwrap_or("...");

    let status = if state.is_complete {
        if state.is_error {
            "ERROR"
        } else {
            "DONE"
        }
    } else if state.channel_disconnected {
        "DISCONNECTED"
    } else {
        "RUNNING"
    };

    let text = format!(
        " {} | {} | {} | Turns: {} | ${:.4}",
        session, model, status, state.num_turns, state.total_cost_usd
    );

    let style = if state.is_error {
        Style::default().fg(Color::Red)
    } else if state.is_complete {
        Style::default().fg(Color::Green)
    } else if state.channel_disconnected {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default().fg(Color::Cyan)
    };

    let block = Block::default()
        .borders(Borders::ALL)
        .title(" Claude Code ")
        .border_style(style);

    let paragraph = Paragraph::new(text).block(block);
    frame.render_widget(paragraph, area);
}

/// Render the scrollable event list
fn render_event_list(state: &AppState, frame: &mut Frame, area: Rect) {
    let mut items: Vec<ListItem> = Vec::new();

    for (i, event) in state.display_events.iter().enumerate() {
        let is_selected = i == state.selected_index;
        let is_expanded = state.expanded_items.contains(&i);

        // Prefix: arrow if expandable, bullet otherwise
        let prefix = if event.expandable {
            if is_expanded {
                "▼ "
            } else {
                "▶ "
            }
        } else {
            "• "
        };

        let style = if is_selected {
            Style::default().bg(Color::DarkGray)
        } else {
            Style::default()
        };

        items.push(ListItem::new(format!("{}{}", prefix, event.summary)).style(style));

        // Add details if expanded
        if is_expanded {
            for detail in &event.details {
                items.push(
                    ListItem::new(format!("  {}", detail))
                        .style(Style::default().fg(Color::DarkGray)),
                );
            }
        }
    }

    let block = Block::default()
        .borders(Borders::ALL)
        .title(format!(" Events ({}) ", state.display_events.len()));

    let list = List::new(items).block(block);
    frame.render_widget(list, area);
}

/// Render the help bar at the bottom
fn render_help_bar(state: &AppState, frame: &mut Frame, area: Rect) {
    let quit_hint = if state.is_complete || state.channel_disconnected {
        "q:quit"
    } else {
        "Ctrl-C:force quit"
    };

    let text = format!(
        " j/k:scroll | J/K:page | g/G:top/bottom | Enter:expand | e/C:all | {} ",
        quit_hint
    );

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::DarkGray));

    let paragraph = Paragraph::new(text)
        .block(block)
        .style(Style::default().fg(Color::DarkGray));

    frame.render_widget(paragraph, area);
}
