//! Main TUI application logic using ratatui directly

use anyhow::{Context, Result};
use crossterm::event::{self, DisableMouseCapture, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph};
use ratatui::Terminal;
use std::io::{stdout, Stdout};
use std::sync::mpsc::{Receiver, TryRecvError};
use std::time::Duration;

use super::events::TuiEvent;
use super::state::{AppState, DisplayEvent, TuiResult};

/// Terminal cleanup guard
struct TerminalGuard;

impl TerminalGuard {
    fn new() -> Result<Self> {
        enable_raw_mode().context("Failed to enable raw mode")?;
        execute!(stdout(), EnterAlternateScreen).context("Failed to enter alternate screen")?;
        Ok(Self)
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let _ = execute!(stdout(), LeaveAlternateScreen, DisableMouseCapture);
    }
}

/// Main TUI application
struct App {
    state: AppState,
    list_state: ListState,
    quit: bool,
}

impl App {
    fn new() -> Self {
        let mut list_state = ListState::default();
        list_state.select(Some(0));
        Self {
            state: AppState::default(),
            list_state,
            quit: false,
        }
    }

    fn handle_key(&mut self, key: KeyEvent) {
        match (key.code, key.modifiers) {
            (KeyCode::Char('q'), KeyModifiers::NONE) => {
                if self.state.is_complete {
                    self.quit = true;
                }
            }
            (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                // Force-quit: allow exiting even when the session is not complete
                self.quit = true;
            }
            (KeyCode::Char('j'), KeyModifiers::NONE) | (KeyCode::Down, KeyModifiers::NONE) => {
                self.state.scroll_down();
                self.list_state.select(Some(self.state.selected_index));
            }
            (KeyCode::Char('k'), KeyModifiers::NONE) | (KeyCode::Up, KeyModifiers::NONE) => {
                self.state.scroll_up();
                self.list_state.select(Some(self.state.selected_index));
            }
            (KeyCode::Char('J'), KeyModifiers::SHIFT) | (KeyCode::PageDown, KeyModifiers::NONE) => {
                for _ in 0..10 {
                    self.state.scroll_down();
                }
                self.list_state.select(Some(self.state.selected_index));
            }
            (KeyCode::Char('K'), KeyModifiers::SHIFT) | (KeyCode::PageUp, KeyModifiers::NONE) => {
                for _ in 0..10 {
                    self.state.scroll_up();
                }
                self.list_state.select(Some(self.state.selected_index));
            }
            (KeyCode::Char('g'), KeyModifiers::NONE) => {
                self.state.go_to_top();
                self.list_state.select(Some(self.state.selected_index));
            }
            (KeyCode::Char('G'), KeyModifiers::SHIFT) => {
                self.state.go_to_bottom();
                self.list_state.select(Some(self.state.selected_index));
            }
            (KeyCode::Enter, KeyModifiers::NONE) => {
                self.state.toggle_expand();
            }
            (KeyCode::Char('e'), KeyModifiers::NONE) => {
                self.state.expand_all();
            }
            (KeyCode::Char('c'), KeyModifiers::NONE) => {
                self.state.collapse_all();
            }
            _ => {}
        }
    }

    fn handle_tui_event(&mut self, event: TuiEvent) {
        match event {
            TuiEvent::Claude(stream_event) => {
                self.state.process_event(stream_event);
                self.list_state.select(Some(self.state.selected_index));
            }
            TuiEvent::Interrupt(signal) => {
                self.state.add_interrupt(signal);
            }
            TuiEvent::ProcessExit => {
                if !self.state.is_complete {
                    self.state.is_complete = true;
                }
            }
        }
    }

    fn render(&mut self, terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> Result<()> {
        terminal.draw(|frame| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(1), // Status bar
                    Constraint::Min(3),    // Event list
                    Constraint::Length(1), // Help bar
                ])
                .split(frame.area());

            self.render_status_bar(frame, chunks[0]);
            self.render_event_list(frame, chunks[1]);
            self.render_help_bar(frame, chunks[2]);
        })?;
        Ok(())
    }

    fn render_status_bar(&self, frame: &mut ratatui::Frame, area: Rect) {
        let session = self.state.session_id.as_deref().unwrap_or("...");
        // Use chars() to safely truncate UTF-8 strings without panicking
        let short_session: String = session.chars().take(8).collect();
        let model = self.state.model.as_deref().unwrap_or("...");

        let status = if self.state.is_complete {
            if self.state.is_error {
                Span::styled(" ERROR ", Style::default().fg(Color::White).bg(Color::Red))
            } else if self.state.channel_disconnected && self.state.result_event.is_none() {
                // Channel disconnected without receiving a result event - process may have crashed
                Span::styled(" EXITED ", Style::default().fg(Color::White).bg(Color::Magenta))
            } else {
                Span::styled(" DONE ", Style::default().fg(Color::Black).bg(Color::Green))
            }
        } else {
            Span::styled(" RUNNING ", Style::default().fg(Color::Black).bg(Color::Yellow))
        };

        let line = Line::from(vec![
            Span::styled(" Session: ", Style::default().fg(Color::Gray)),
            Span::styled(short_session, Style::default().fg(Color::Cyan)),
            Span::styled(" | Model: ", Style::default().fg(Color::Gray)),
            Span::styled(model, Style::default().fg(Color::White)),
            Span::styled(" | Turns: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", self.state.num_turns),
                Style::default().fg(Color::White),
            ),
            Span::styled(" | Cost: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("${:.4}", self.state.total_cost_usd),
                Style::default().fg(Color::Green),
            ),
            Span::raw(" "),
            status,
        ]);

        let paragraph = Paragraph::new(line)
            .style(Style::default().bg(Color::DarkGray));

        frame.render_widget(paragraph, area);
    }

    fn render_event_list(&mut self, frame: &mut ratatui::Frame, area: Rect) {
        let items: Vec<ListItem> = self
            .state
            .display_events
            .iter()
            .enumerate()
            .map(|(idx, event)| self.render_event_item(idx, event))
            .collect();

        let list = List::new(items)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray))
                    .title(" Events ")
                    .title_style(Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            )
            .highlight_style(Style::default());

        frame.render_stateful_widget(list, area, &mut self.list_state);
    }

    fn render_event_item(&self, idx: usize, event: &DisplayEvent) -> ListItem<'static> {
        let is_selected = idx == self.state.selected_index;
        let is_expanded = self.state.expanded_items.contains(&idx);

        let prefix = if event.expandable {
            if is_expanded { "▼ " } else { "▶ " }
        } else {
            "  "
        };

        let mut lines = vec![];

        // Main line style
        let style = if is_selected {
            Style::default()
                .fg(Color::Black)
                .bg(Color::Cyan)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(Color::White)
        };

        lines.push(Line::from(vec![
            Span::styled(prefix.to_string(), style),
            Span::styled(event.summary.clone(), style),
        ]));

        // Detail lines if expanded
        if is_expanded && !event.details.is_empty() {
            for detail in &event.details {
                let detail_style = if is_selected {
                    Style::default().fg(Color::Black).bg(Color::Cyan)
                } else {
                    Style::default().fg(Color::Gray)
                };
                lines.push(Line::from(Span::styled(detail.clone(), detail_style)));
            }
        }

        ListItem::new(Text::from(lines))
    }

    fn render_help_bar(&self, frame: &mut ratatui::Frame, area: Rect) {
        let key_style = Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD);
        let desc_style = Style::default().fg(Color::Gray);

        let mut spans = vec![
            Span::raw(" "),
            Span::styled("j/k", key_style),
            Span::styled(": scroll  ", desc_style),
            Span::styled("Enter", key_style),
            Span::styled(": expand  ", desc_style),
            Span::styled("e/c", key_style),
            Span::styled(": expand/collapse all  ", desc_style),
            Span::styled("g/G", key_style),
            Span::styled(": top/bottom  ", desc_style),
        ];

        // Show appropriate quit option based on state
        if self.state.is_complete {
            spans.push(Span::styled("q", key_style));
            spans.push(Span::styled(": quit", desc_style));
        } else {
            spans.push(Span::styled("Ctrl-C", key_style));
            spans.push(Span::styled(": force quit", desc_style));
        }

        let paragraph = Paragraph::new(Line::from(spans))
            .style(Style::default().bg(Color::DarkGray));

        frame.render_widget(paragraph, area);
    }
}

/// Run the TUI application
pub fn run(event_rx: Receiver<TuiEvent>) -> Result<TuiResult> {
    // Setup terminal (guard ensures cleanup on all exit paths)
    let _guard = TerminalGuard::new()?;

    let backend = CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend).context("Failed to create terminal")?;

    let mut app = App::new();

    // Main event loop
    while !app.quit {
        // Render
        app.render(&mut terminal)?;

        // Poll for TUI events from Claude (non-blocking)
        loop {
            match event_rx.try_recv() {
                Ok(tui_event) => app.handle_tui_event(tui_event),
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    // Mark channel as disconnected for status display
                    app.state.channel_disconnected = true;
                    if !app.state.is_complete {
                        app.state.is_complete = true;
                    }
                    break;
                }
            }
        }

        // Poll for keyboard input with timeout
        if event::poll(Duration::from_millis(50))? {
            if let Event::Key(key) = event::read()? {
                app.handle_key(key);
            }
        }
    }

    Ok(app.state.into_result())
}
