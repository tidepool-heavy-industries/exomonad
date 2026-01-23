use anyhow::{Context, Result};
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind, MouseEventKind,
    },
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::io;
use tokio::sync::mpsc;
use tracing::{debug, info};
use tuirealm::command::{Cmd, Direction};
use tuirealm::ratatui::backend::CrosstermBackend;
use tuirealm::ratatui::layout::Rect;
use tuirealm::ratatui::Terminal;
use tuirealm::MockComponent;

use crate::protocol::{PopupDefinition, PopupResult};
use crate::realm::PopupComponent;

/// Main application event loop.
///
/// This now uses the popup-tui pattern:
/// 1. Receive PopupDefinition from Haskell via async channel
/// 2. Spawn blocking task to run synchronous popup event loop
/// 3. Send PopupResult back to Haskell via async channel
///
/// The popup event loop (run_popup) is blocking and uses crossterm::event::read()
/// directly, so we wrap it in tokio::task::spawn_blocking.
pub async fn run(
    mut msg_rx: mpsc::Receiver<PopupDefinition>,
    res_tx: mpsc::Sender<PopupResult>,
) -> Result<()> {
    info!("Starting TUI event loop");

    loop {
        tokio::select! {
            // Receive PopupDefinition from Haskell
            Some(definition) = msg_rx.recv() => {
                debug!(title = %definition.title, "Received PopupDefinition");

                // Run popup in blocking task (popup-tui is synchronous)
                let result = tokio::task::spawn_blocking(move || {
                    run_popup(definition)
                })
                .await
                .context("Popup task panicked")??;

                debug!(button = %result.button, "Popup completed");

                // Send result back to Haskell
                if res_tx.send(result).await.is_err() {
                    info!("Receiver dropped, exiting event loop");
                    break;
                }
            }

            else => {
                info!("Channel closed, exiting event loop");
                break;
            }
        }
    }

    info!("TUI event loop exited");
    Ok(())
}

/// Run a single popup (blocking, synchronous).
///
/// This is the popup-tui event loop pattern:
/// 1. Setup terminal (raw mode, alternate screen, mouse capture)
/// 2. Create PopupComponent from definition
/// 3. Event loop: render + handle keyboard/mouse
/// 4. Return PopupResult when user submits or cancels
/// 5. Cleanup terminal
fn run_popup(definition: PopupDefinition) -> Result<PopupResult> {
    // Setup terminal
    let mut terminal = setup_terminal().context("Failed to setup terminal")?;

    // Create popup component
    let mut popup = PopupComponent::new(definition);
    let mut popup_area: Option<Rect> = None;

    // Event loop
    let result = loop {
        // Render popup
        terminal
            .draw(|f| {
                let area = centered_rect(80, 80, f.area());
                popup_area = Some(area);
                popup.view(f, area);
            })
            .context("Failed to render popup")?;

        // Read event (blocking)
        match event::read().context("Failed to read event")? {
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
    cleanup_terminal(terminal).context("Failed to cleanup terminal")?;

    Ok(result)
}

/// Handle keyboard input for popup.
enum PopupAction {
    Submit,
    Cancel,
    Continue,
}

fn handle_key(popup: &mut PopupComponent, key: KeyCode) -> Result<PopupAction> {
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

/// Setup terminal (raw mode, alternate screen, mouse capture).
fn setup_terminal() -> Result<Terminal<CrosstermBackend<io::Stdout>>> {
    enable_raw_mode().context("Failed to enable raw mode")?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)
        .context("Failed to enter alternate screen")?;

    let backend = CrosstermBackend::new(stdout);
    let terminal = Terminal::new(backend).context("Failed to create terminal")?;

    Ok(terminal)
}

/// Cleanup terminal (leave alternate screen, disable raw mode, disable mouse).
fn cleanup_terminal(mut terminal: Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
    disable_raw_mode().context("Failed to disable raw mode")?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )
    .context("Failed to leave alternate screen")?;
    terminal.show_cursor().context("Failed to show cursor")?;

    Ok(())
}
