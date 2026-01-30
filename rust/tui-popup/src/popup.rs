use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind, MouseEventKind,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use std::fs::File;
use tuirealm::MockComponent;
use tuirealm::command::{Cmd, Direction};
use tuirealm::ratatui::Terminal;
use tuirealm::ratatui::backend::CrosstermBackend;
use tuirealm::ratatui::layout::Rect;

use crate::protocol::{PopupDefinition, PopupResult};
use crate::realm::PopupComponent;

/// Run a single popup (blocking, synchronous).
///
/// Renders to /dev/tty, allowing the TUI to work even when stdout is
/// redirected to a file for capturing the result.
///
/// The popup-tui event loop pattern:
/// 1. Setup terminal on /dev/tty (raw mode, alternate screen, mouse capture)
/// 2. Create PopupComponent from definition
/// 3. Event loop: render + handle keyboard/mouse
/// 4. Return PopupResult when user submits or cancels
/// 5. Cleanup terminal
pub fn run_popup_with_tty(definition: PopupDefinition) -> anyhow::Result<PopupResult> {
    // Setup terminal on /dev/tty
    let mut terminal = setup_terminal()?;

    // Create popup component
    let mut popup = PopupComponent::new(definition);
    let mut popup_area: Option<Rect> = None;

    // Event loop
    let result = loop {
        // Render popup
        terminal.draw(|f| {
            let area = centered_rect(80, 80, f.area());
            popup_area = Some(area);
            popup.view(f, area);
        })?;

        // Read event (blocking)
        match event::read()? {
            Event::Key(key) => {
                if key.kind == KeyEventKind::Press {
                    match handle_key(&mut popup, key.code)? {
                        PopupAction::Submit => {
                            break popup.submit();
                        }
                        PopupAction::Cancel => {
                            break popup.cancel();
                        }
                        PopupAction::Continue => {}
                    }
                }
            }
            Event::Mouse(mouse) => {
                if mouse.kind == MouseEventKind::Down(event::MouseButton::Left) {
                    if let Some(area) = popup_area {
                        if mouse.column >= area.x
                            && mouse.column < area.x + area.width
                            && mouse.row >= area.y
                            && mouse.row < area.y + area.height
                        {
                            popup.handle_click(mouse.column, mouse.row);
                        }
                    }
                }
            }
            _ => {}
        }
    };

    // Cleanup terminal
    cleanup_terminal(terminal)?;

    Ok(result)
}

/// Handle keyboard input for popup.
enum PopupAction {
    Submit,
    Cancel,
    Continue,
}

fn handle_key(popup: &mut PopupComponent, key: KeyCode) -> anyhow::Result<PopupAction> {
    match key {
        KeyCode::Esc => Ok(PopupAction::Cancel),
        KeyCode::Enter => Ok(PopupAction::Submit),
        KeyCode::Tab => {
            popup.perform(Cmd::Move(Direction::Down));
            Ok(PopupAction::Continue)
        }
        KeyCode::BackTab => {
            popup.perform(Cmd::Move(Direction::Up));
            Ok(PopupAction::Continue)
        }
        KeyCode::Left => {
            popup.perform(Cmd::Move(Direction::Left));
            Ok(PopupAction::Continue)
        }
        KeyCode::Right => {
            popup.perform(Cmd::Move(Direction::Right));
            Ok(PopupAction::Continue)
        }
        KeyCode::Up => {
            popup.perform(Cmd::Move(Direction::Up));
            Ok(PopupAction::Continue)
        }
        KeyCode::Down => {
            popup.perform(Cmd::Move(Direction::Down));
            Ok(PopupAction::Continue)
        }
        KeyCode::Char(' ') => {
            if popup.is_focused_textbox() {
                popup.perform(Cmd::Type(' '));
            } else {
                popup.perform(Cmd::Submit);
            }
            Ok(PopupAction::Continue)
        }
        KeyCode::Char(c) => {
            popup.perform(Cmd::Type(c));
            Ok(PopupAction::Continue)
        }
        KeyCode::Backspace => {
            popup.perform(Cmd::Cancel);
            Ok(PopupAction::Continue)
        }
        _ => Ok(PopupAction::Continue),
    }
}

/// Create centered rect (popup-tui pattern).
fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    use tuirealm::ratatui::layout::{Constraint, Direction as RatatuiDirection, Layout};

    let popup_layout = Layout::default()
        .direction(RatatuiDirection::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(r);

    Layout::default()
        .direction(RatatuiDirection::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(popup_layout[1])[1]
}

/// Setup terminal on /dev/tty (raw mode, alternate screen, mouse capture).
fn setup_terminal() -> anyhow::Result<Terminal<CrosstermBackend<File>>> {
    enable_raw_mode()?;

    // Open /dev/tty directly - works even with stdout redirected
    let mut tty = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")
        .map_err(|e| anyhow::anyhow!("Failed to open /dev/tty: {}", e))?;

    execute!(tty, EnterAlternateScreen, EnableMouseCapture)?;

    let backend = CrosstermBackend::new(tty);
    let terminal = Terminal::new(backend)?;

    Ok(terminal)
}

/// Cleanup terminal (leave alternate screen, disable raw mode, disable mouse).
fn cleanup_terminal(mut terminal: Terminal<CrosstermBackend<File>>) -> anyhow::Result<()> {
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    Ok(())
}
