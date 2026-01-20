use anyhow::{Context, Result};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, MouseEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, layout::Rect, Terminal};
use std::io;
use std::time::Duration;
use tokio::sync::mpsc;
use tracing::{debug, info};

use crate::input::{handle_key_event, handle_mouse_click};
use crate::protocol::{Interaction, UISpec};
use crate::render::{render_idle, render_ui};
use crate::ui_stack::UIStack;

enum EventOutcome {
    Interaction(Interaction),
    Exit,
    Nothing,
}

/// Main application event loop.
///
/// Setup:
/// 1. Enter raw mode + alternate screen
/// 2. Create terminal
/// 3. Initialize UI stack and focus state
///
/// Loop:
/// 1. Render current UI
/// 2. tokio::select! over:
///    - UISpec messages from Haskell
///    - Keyboard polling (crossterm limitation)
/// 3. Update UI stack or send Interaction
/// 4. Exit when stack is empty
///
/// Cleanup:
/// 1. Restore terminal (leave alternate screen, disable raw mode)
pub async fn run(
    mut msg_rx: mpsc::Receiver<UISpec>,
    int_tx: mpsc::Sender<Interaction>,
) -> Result<()> {
    // Setup terminal
    info!("Starting TUI event loop");
    let mut terminal = setup_terminal().context("Failed to setup terminal")?;

    let mut ui_stack = UIStack::new();
    let mut focus_idx = 0;

    // Event loop
    let mut terminal_area = terminal.get_frame().area();

    loop {
        // Render current UI and capture terminal area for mouse hit testing
        terminal
            .draw(|f| {
                terminal_area = f.area();
                if let Some(spec) = ui_stack.current() {
                    render_ui(f, spec, focus_idx);
                } else {
                    render_idle(f);
                }
            })
            .context("Failed to render UI")?;

        tokio::select! {
            // Handle UISpec from Haskell
            Some(spec) = msg_rx.recv() => {
                debug!(ui_id = %spec.id, "Received UISpec");
                ui_stack.push(spec);
                focus_idx = 0;  // Reset focus for new UI
            }

            // Poll input events (crossterm doesn't integrate with tokio)
            _ = tokio::time::sleep(Duration::from_millis(100)) => {
                match poll_input(&ui_stack, &mut focus_idx, terminal_area)? {
                    EventOutcome::Interaction(interaction) => {
                        debug!(interaction = ?interaction, "Sending interaction");

                        // Send interaction to Haskell
                        if int_tx.send(interaction).await.is_err() {
                            info!("Receiver dropped, exiting event loop");
                            break;
                        }

                        // Pop UI after interaction (Phase 1: immediate close)
                        // Phase 2/3: More sophisticated lifecycle management
                        ui_stack.pop();
                        focus_idx = 0;  // Reset focus for previous UI
                    }
                    EventOutcome::Exit => {
                        info!("Exit requested via keyboard");
                        break;
                    }
                    EventOutcome::Nothing => {}
                }
            }
        }
    }

    // Cleanup terminal
    cleanup_terminal(terminal).context("Failed to cleanup terminal")?;
    info!("TUI event loop exited");

    Ok(())
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
    execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)
        .context("Failed to leave alternate screen")?;
    terminal.show_cursor().context("Failed to show cursor")?;

    Ok(())
}

/// Poll input events (keyboard and mouse) and handle them.
///
/// Returns Some(Interaction) if user completed an action (Enter on button/input, or mouse click).
/// Returns None if no action or only navigation (Tab, arrow keys).
fn poll_input(
    ui_stack: &UIStack,
    focus_idx: &mut usize,
    terminal_area: Rect,
) -> Result<EventOutcome> {
    // Check if event is available (non-blocking)
    if !event::poll(Duration::ZERO).context("Failed to poll events")? {
        return Ok(EventOutcome::Nothing);
    }

    // Read the event
    let evt = event::read().context("Failed to read event")?;

    match evt {
        Event::Key(key_event) => {
            // Ctrl+C: request exit
            if key_event.code == KeyCode::Char('c')
                && key_event.modifiers.contains(event::KeyModifiers::CONTROL)
            {
                info!("Ctrl+C pressed, requesting exit");
                return Ok(EventOutcome::Exit);
            }

            // Handle key event (navigation, interaction)
            Ok(match handle_key_event(key_event, ui_stack, focus_idx)? {
                Some(i) => EventOutcome::Interaction(i),
                None => EventOutcome::Nothing,
            })
        }
        Event::Mouse(mouse_event) => {
            // Handle mouse click
            if mouse_event.kind == MouseEventKind::Down(event::MouseButton::Left) {
                if let Some((interaction, clicked_idx)) =
                    handle_mouse_click(mouse_event.column, mouse_event.row, ui_stack, terminal_area)?
                {
                    // Update focus to clicked element
                    *focus_idx = clicked_idx;
                    return Ok(EventOutcome::Interaction(interaction));
                }
            }
            Ok(EventOutcome::Nothing)
        }
        _ => Ok(EventOutcome::Nothing),
    }
}
