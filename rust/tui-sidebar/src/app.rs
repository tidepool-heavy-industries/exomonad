use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{backend::CrosstermBackend, Terminal};
use std::io;
use std::time::Duration;
use tokio::sync::mpsc;
use tracing::{debug, info};

use crate::input::handle_key_event;
use crate::protocol::{Interaction, UISpec};
use crate::render::render_ui;
use crate::ui_stack::UIStack;

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
    loop {
        // Render current UI
        if let Some(spec) = ui_stack.current() {
            terminal
                .draw(|f| render_ui(f, spec, focus_idx))
                .context("Failed to render UI")?;
        }

        tokio::select! {
            // Handle UISpec from Haskell
            Some(spec) = msg_rx.recv() => {
                debug!(ui_id = %spec.id, "Received UISpec");
                ui_stack.push(spec);
                focus_idx = 0;  // Reset focus for new UI
            }

            // Poll keyboard (crossterm doesn't integrate with tokio)
            _ = tokio::time::sleep(Duration::from_millis(100)) => {
                if let Some(interaction) = poll_keyboard(&ui_stack, &mut focus_idx)? {
                    debug!(interaction = ?interaction, "Sending interaction");

                    // Send interaction to Haskell
                    if int_tx.send(interaction).await.is_err() {
                        info!("Receiver dropped, exiting event loop");
                        break;
                    }

                    // Pop UI after interaction (Phase 1: immediate close)
                    // Phase 2/3: More sophisticated lifecycle management
                    ui_stack.pop();

                    if ui_stack.is_empty() {
                        info!("UI stack empty, exiting");
                        break;
                    }

                    focus_idx = 0;  // Reset focus for previous UI
                }
            }
        }
    }

    // Cleanup terminal
    cleanup_terminal(terminal).context("Failed to cleanup terminal")?;
    info!("TUI event loop exited");

    Ok(())
}

/// Setup terminal (raw mode, alternate screen).
fn setup_terminal() -> Result<Terminal<CrosstermBackend<io::Stdout>>> {
    enable_raw_mode().context("Failed to enable raw mode")?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen).context("Failed to enter alternate screen")?;

    let backend = CrosstermBackend::new(stdout);
    let terminal = Terminal::new(backend).context("Failed to create terminal")?;

    Ok(terminal)
}

/// Cleanup terminal (leave alternate screen, disable raw mode).
fn cleanup_terminal(mut terminal: Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
    disable_raw_mode().context("Failed to disable raw mode")?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)
        .context("Failed to leave alternate screen")?;
    terminal.show_cursor().context("Failed to show cursor")?;

    Ok(())
}

/// Poll keyboard and handle input.
///
/// Returns Some(Interaction) if user completed an action (Enter on button/input).
/// Returns None if no action or only navigation (Tab).
fn poll_keyboard(ui_stack: &UIStack, focus_idx: &mut usize) -> Result<Option<Interaction>> {
    // Check if event is available (non-blocking)
    if !event::poll(Duration::ZERO).context("Failed to poll events")? {
        return Ok(None);
    }

    // Read the event
    let evt = event::read().context("Failed to read event")?;

    match evt {
        Event::Key(key_event) => {
            // Ctrl+C: exit immediately
            if key_event.code == KeyCode::Char('c')
                && key_event.modifiers.contains(event::KeyModifiers::CONTROL)
            {
                info!("Ctrl+C pressed, exiting");
                std::process::exit(0);
            }

            // Handle key event (navigation, interaction)
            handle_key_event(key_event, ui_stack, focus_idx)
        }
        _ => Ok(None),
    }
}
