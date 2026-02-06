use std::collections::{BTreeMap, VecDeque};
use zellij_tile::prelude::*;
use ratatui::{
    backend::WindowSize,
    layout::{Constraint, Direction, Layout, Rect, Size, Position},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, Paragraph, Clear},
    Terminal,
};

mod protocol;
use exomonad_ui_protocol::{
    transport, AgentEvent, CoordinatorAgentState, StateUpdate,
};
use protocol::{PluginMessage, PluginState};

/// Active popup state for simple choice lists.
///
/// Uses pipe-based communication: launched via `zellij pipe --plugin`,
/// responds via `cli_pipe_output()`.
struct ActivePopup {
    /// CLI pipe ID for sending response back via cli_pipe_output()
    pipe_id: String,
    /// Request ID for correlation
    request_id: String,
    /// Title for the popup
    title: String,
    /// Choice items to display
    items: Vec<String>,
    /// Currently selected item index
    selected_index: usize,
}

#[derive(Default)]
struct ExoMonadPlugin {
    status_state: PluginState,
    status_message: String,
    active_popup: Option<ActivePopup>,
    events: VecDeque<AgentEvent>,
    terminal: Option<Terminal<ZellijBackend>>,
    /// Agent state from the coordinator plugin.
    coordinator_agents: Vec<CoordinatorAgentState>,
}

register_plugin!(ExoMonadPlugin);

struct ZellijBackend;

impl ratatui::backend::Backend for ZellijBackend {
    fn draw<'a, I>(&mut self, content: I) -> Result<(), std::io::Error>
    where
        I: Iterator<Item = (u16, u16, &'a ratatui::buffer::Cell)>,
    {
        use std::fmt::Write;
        let mut buffer = String::with_capacity(content.size_hint().0 * 10);

        // Track the currently active style to avoid redundant ANSI sequences.
        let mut current_fg: Option<Color> = None;
        let mut current_bg: Option<Color> = None;
        let mut current_mod: Modifier = Modifier::empty();

        for (x, y, cell) in content {
            // Move cursor
            write!(buffer, "\x1b[{};{}H", y + 1, x + 1).unwrap();

            // Desired style for this cell
            let desired_fg = if cell.fg == Color::Reset {
                None
            } else {
                Some(cell.fg)
            };
            let desired_bg = if cell.bg == Color::Reset {
                None
            } else {
                Some(cell.bg)
            };
            let desired_mod = cell.modifier;

            if desired_fg != current_fg || desired_bg != current_bg || desired_mod != current_mod {
                let desired_is_default =
                    desired_fg.is_none() && desired_bg.is_none() && desired_mod.is_empty();
                let current_is_default =
                    current_fg.is_none() && current_bg.is_none() && current_mod.is_empty();

                if desired_is_default {
                    // Return to default style
                    write!(buffer, "\x1b[0m").unwrap();
                } else {
                    if !current_is_default {
                        // Different non-default style: reset before applying new style
                        // Optimizing this further would require complex diffing of ANSI codes,
                        // so a hard reset is safer and simpler for now.
                        // Note: This may cause flicker or incorrect rendering for complex
                        // modifier transitions (e.g. BOLD -> ITALIC), but it's a trade-off
                        // for code simplicity and safety in this custom backend.
                        write!(buffer, "\x1b[0m").unwrap();
                    }
                    // Apply desired foreground/background colors
                    if let Some(fg) = desired_fg {
                        write!(buffer, "{}", color_to_ansi(fg, false)).unwrap();
                    }
                    if let Some(bg) = desired_bg {
                        write!(buffer, "{}", color_to_ansi(bg, true)).unwrap();
                    }
                    // Apply desired modifiers
                    if desired_mod.contains(Modifier::BOLD) {
                        write!(buffer, "\x1b[1m").unwrap();
                    }
                    if desired_mod.contains(Modifier::UNDERLINED) {
                        write!(buffer, "\x1b[4m").unwrap();
                    }
                    if desired_mod.contains(Modifier::REVERSED) {
                        write!(buffer, "\x1b[7m").unwrap();
                    }
                    if desired_mod.contains(Modifier::DIM) {
                        write!(buffer, "\x1b[2m").unwrap();
                    }
                    if desired_mod.contains(Modifier::ITALIC) {
                        write!(buffer, "\x1b[3m").unwrap();
                    }
                }

                current_fg = desired_fg;
                current_bg = desired_bg;
                current_mod = desired_mod;
            }

            write!(buffer, "{}", cell.symbol()).unwrap();
        }

        // Ensure we leave the terminal in a default style state after drawing.
        write!(buffer, "\x1b[0m").unwrap();

        use std::io::Write as IoWrite;
        std::io::stdout().write_all(buffer.as_bytes())?;
        std::io::stdout().flush()?;
        Ok(())
    }

    fn hide_cursor(&mut self) -> Result<(), std::io::Error> { Ok(()) }
    fn show_cursor(&mut self) -> Result<(), std::io::Error> { Ok(()) }
    fn get_cursor_position(&mut self) -> Result<Position, std::io::Error> { Ok(Position::new(0, 0)) }
    fn set_cursor_position<P: Into<Position>>(&mut self, _pos: P) -> Result<(), std::io::Error> { Ok(()) }
    fn clear(&mut self) -> Result<(), std::io::Error> { 
        print!("\x1b[2J"); 
        Ok(()) 
    }
    // In this WASM/Zellij backend we don't have direct access to the real terminal size here.
    // The actual size is managed externally (the render loop resizes using rows/cols), but
    // ratatui may still call `size()` for layout calculations. Returning a non-zero fallback
    // avoids degenerate 0x0 layouts while keeping behavior predictable.
    fn size(&self) -> Result<Size, std::io::Error> { Ok(Size::new(80, 24)) }
    fn window_size(&mut self) -> Result<WindowSize, std::io::Error> {
        Ok(WindowSize {
            columns_rows: Size::new(80, 24),
            // Pixel dimensions are not known in this backend; 0x0 indicates "unknown".
            pixels: Size::new(0, 0),
        })
    }
    fn flush(&mut self) -> Result<(), std::io::Error> { Ok(()) }
}

fn color_to_ansi(color: Color, bg: bool) -> String {
    let layer_offset = if bg { 10 } else { 0 };
    match color {
        Color::Reset => "\x1b[0m".to_string(),
        Color::Black => format!("\x1b[{}m", 30 + layer_offset),
        Color::Red => format!("\x1b[{}m", 31 + layer_offset),
        Color::Green => format!("\x1b[{}m", 32 + layer_offset),
        Color::Yellow => format!("\x1b[{}m", 33 + layer_offset),
        Color::Blue => format!("\x1b[{}m", 34 + layer_offset),
        Color::Magenta => format!("\x1b[{}m", 35 + layer_offset),
        Color::Cyan => format!("\x1b[{}m", 36 + layer_offset),
        Color::Gray => format!("\x1b[{}m", 37 + layer_offset), // Standard Gray/White
        Color::DarkGray => format!("\x1b[{}m", 90 + layer_offset),
        Color::LightRed => format!("\x1b[{}m", 91 + layer_offset),
        Color::LightGreen => format!("\x1b[{}m", 92 + layer_offset),
        Color::LightYellow => format!("\x1b[{}m", 93 + layer_offset),
        Color::LightBlue => format!("\x1b[{}m", 94 + layer_offset),
        Color::LightMagenta => format!("\x1b[{}m", 95 + layer_offset),
        Color::LightCyan => format!("\x1b[{}m", 96 + layer_offset),
        Color::White => format!("\x1b[{}m", 97 + layer_offset),
        Color::Indexed(i) => format!("\x1b[{};5;{}m", if bg { 48 } else { 38 }, i),
        Color::Rgb(r, g, b) => format!("\x1b[{};2;{};{};{}m", if bg { 48 } else { 38 }, r, g, b),
    }
}

impl ZellijPlugin for ExoMonadPlugin {
    fn load(&mut self, _configuration: BTreeMap<String, String>) {
        request_permission(&[PermissionType::ReadCliPipes]);
        subscribe(&[EventType::CustomMessage, EventType::Key]);
        self.status_state = PluginState::Idle;
        self.status_message = "Ready.".to_string();
        self.events = VecDeque::new();
        // Initialize terminal with ZellijBackend
        if let Ok(term) = Terminal::new(ZellijBackend) {
            self.terminal = Some(term);
        }
    }

    fn pipe(&mut self, pipe_message: PipeMessage) -> bool {
        // Handle popup requests via CLI pipe:
        // zellij pipe --plugin file:X.wasm --name exomonad:popup -- "request_id|title|item1,item2,item3"
        if pipe_message.name == transport::POPUP_PIPE {
            // Extract pipe_id from CLI source
            let pipe_id = match &pipe_message.source {
                PipeSource::Cli(id) => id.clone(),
                _ => {
                    self.status_state = PluginState::Error;
                    self.status_message = "Popup must come from CLI pipe".to_string();
                    return true;
                }
            };

            // Parse payload: "request_id|title|item1,item2,item3"
            let payload = pipe_message.payload.unwrap_or_default();
            let parts: Vec<&str> = payload.splitn(3, '|').collect();

            if parts.len() < 3 {
                self.status_state = PluginState::Error;
                self.status_message = "Invalid payload format".to_string();
                cli_pipe_output(&pipe_id, "ERROR:Invalid payload format");
                return true;
            }

            let request_id = parts[0].to_string();
            let title = parts[1].to_string();
            let items: Vec<String> = parts[2]
                .split(',')
                .filter(|s| !s.is_empty())
                .map(String::from)
                .collect();

            if items.is_empty() {
                self.status_state = PluginState::Error;
                self.status_message = "Popup items cannot be empty".to_string();
                cli_pipe_output(&pipe_id, &format!("{}:ERROR:No items provided", request_id));
                return true;
            }

            // Block CLI input while popup is active
            block_cli_pipe_input(&pipe_id);

            // CRITICAL: Make headless plugin visible as floating pane
            show_self(true);

            self.active_popup = Some(ActivePopup {
                pipe_id,
                request_id,
                title,
                items,
                selected_index: 0,
            });
            self.status_state = PluginState::Waiting;
            self.status_message = "Waiting for selection...".to_string();
            return true; // Request re-render
        }

        // Handle pipe messages from zellij pipe --name exomonad-events
        if pipe_message.name == "exomonad-events" {
            if let Some(payload) = pipe_message.payload {
                match serde_json::from_str::<AgentEvent>(&payload) {
                    Ok(agent_event) => {
                        self.events.push_back(agent_event);
                        // Keep only last 100 events
                        if self.events.len() > 100 {
                            self.events.pop_front();
                        }
                        return true; // Request re-render
                    }
                    Err(e) => {
                        self.status_state = PluginState::Error;
                        self.status_message = format!("Invalid event payload: {}", e);
                        return true;
                    }
                }
            }
        }

        // Handle coordinator state updates
        if pipe_message.name == "exomonad-coordinator-state" {
            if let Some(payload) = pipe_message.payload {
                match serde_json::from_str::<StateUpdate>(&payload) {
                    Ok(StateUpdate::FullState { agents }) => {
                        self.coordinator_agents = agents;
                        return true; // Request re-render
                    }
                    Err(e) => {
                        self.status_state = PluginState::Error;
                        self.status_message = format!("Invalid coordinator state: {}", e);
                        return true;
                    }
                }
            }
        }

        false
    }

    fn update(&mut self, event: Event) -> bool {
        let mut should_render = false;
        match event {
            Event::CustomMessage(name, payload) => {
                if name == "exomonad" {
                    match serde_json::from_str::<PluginMessage>(&payload) {
                        Ok(msg) => match msg {
                            PluginMessage::Status { state, message } => {
                                self.status_state = state;
                                self.status_message = message;
                                should_render = true;
                            }
                            PluginMessage::ClosePopup => {
                                self.active_popup = None;
                                self.status_state = PluginState::Idle;
                                self.status_message = "Ready.".to_string();
                                should_render = true;
                            }
                        },
                        Err(e) => {
                            self.status_state = PluginState::Error;
                            self.status_message = format!("Invalid payload: {}", e);
                            should_render = true;
                        }
                    }
                }
            }
            Event::Key(key) => {
                if let Some(popup) = &mut self.active_popup {
                    if popup.items.is_empty() {
                        return false;
                    }

                    match key.bare_key {
                        BareKey::Esc => {
                            // Send cancel response: "{request_id}:CANCELLED"
                            let response = format!("{}:CANCELLED", popup.request_id);
                            cli_pipe_output(&popup.pipe_id, &response);
                            unblock_cli_pipe_input(&popup.pipe_id);
                            close_self(); // Destroy plugin pane after responding
                        }
                        BareKey::Down | BareKey::Char('j') => {
                            if popup.selected_index < popup.items.len().saturating_sub(1) {
                                popup.selected_index += 1;
                                should_render = true;
                            }
                        }
                        BareKey::Up | BareKey::Char('k') => {
                            if popup.selected_index > 0 {
                                popup.selected_index -= 1;
                                should_render = true;
                            }
                        }
                        BareKey::Enter => {
                            // Send submit response: "{request_id}:{selected_item}"
                            let selected = popup
                                .items
                                .get(popup.selected_index)
                                .cloned()
                                .unwrap_or_default();
                            let response = format!("{}:{}", popup.request_id, selected);
                            cli_pipe_output(&popup.pipe_id, &response);
                            unblock_cli_pipe_input(&popup.pipe_id);
                            close_self(); // Destroy plugin pane after responding
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        should_render
    }

    fn render(&mut self, rows: usize, cols: usize) {
        // Initialize terminal if not already present (failsafe for load)
        if self.terminal.is_none() {
            if let Ok(term) = Terminal::new(ZellijBackend) {
                self.terminal = Some(term);
            }
        }

        if let Some(terminal) = &mut self.terminal {
            // Force size as we are in WASM and can't detect it reliably
            let width_u16 = if cols > u16::MAX as usize { u16::MAX } else { cols as u16 };
            let height_u16 = if rows > u16::MAX as usize { u16::MAX } else { rows as u16 };
            if let Err(e) = terminal.resize(Rect::new(0, 0, width_u16, height_u16)) {
                eprintln!("Failed to resize terminal: {}", e);
                return;
            }

            let res = terminal.draw(|f| {
                let area = f.area();
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Length(3), // Status bar
                        Constraint::Min(0),    // Events
                    ])
                    .split(area);

                // Status Bar
                let status_block = Block::default().borders(Borders::ALL).title("ExoMonad");
                let status_color = match self.status_state {
                    PluginState::Error => Color::Red,
                    PluginState::Thinking => Color::Yellow,
                    PluginState::Waiting => Color::Cyan,
                    PluginState::Idle => Color::Blue,
                };
                
                let status_text = Line::from(vec![
                    Span::styled(format!("[{}] ", self.status_state), Style::default().fg(status_color).add_modifier(Modifier::BOLD)),
                    Span::raw(&self.status_message),
                ]);
                
                let p = Paragraph::new(status_text).block(status_block);
                f.render_widget(p, chunks[0]);

                // Events List
                // NOTE: `self.events` is a VecDeque with oldest events at the front and newest at
                // the back, with a capacity of 100 where the oldest entries are dropped first.
                // We intentionally iterate in reverse and take the first 20 elements so that the
                // 20 most recent events are shown with the newest at the top of the list. This
                // "newest-first" ordering is the intended UX for the events view.
                let event_items: Vec<ListItem> = self.events.iter().rev().take(20).map(|e| {
                    let (time, content, color) = match e {
                        AgentEvent::AgentStarted { agent_id, timestamp } => (timestamp, format!("{} started", agent_id), Color::Green),
                        AgentEvent::AgentStopped { agent_id, timestamp } => (timestamp, format!("{} done", agent_id), Color::Blue),
                        AgentEvent::StopHookBlocked { agent_id, reason, timestamp } => (timestamp, format!("{} blocked: {}", agent_id, reason), Color::Red),
                        AgentEvent::HookReceived { agent_id, hook_type, timestamp } => (timestamp, format!("{} hook: {}", agent_id, hook_type), Color::Cyan),
                        AgentEvent::PrFiled { agent_id, pr_number, timestamp } => (timestamp, format!("{} PR #{}", agent_id, pr_number), Color::Magenta),
                        AgentEvent::CopilotReviewed { agent_id, comment_count, timestamp } => (timestamp, format!("{} copilot: {} comments", agent_id, comment_count), Color::Yellow),
                        AgentEvent::AgentStuck { agent_id, failed_stop_count, timestamp } => (timestamp, format!("{} ⚠ STUCK ({} failed stops)", agent_id, failed_stop_count), Color::Red),
                    };
                    
                    ListItem::new(Line::from(vec![
                        Span::styled(format!("{} ", format_timestamp(time)), Style::default().fg(Color::DarkGray)),
                        Span::styled(content, Style::default().fg(color)),
                    ]))
                }).collect();

                let events_list = List::new(event_items)
                    .block(Block::default().borders(Borders::ALL).title("Events"));
                f.render_widget(events_list, chunks[1]);

                // Popup Overlay - Simple choice list
                if let Some(popup) = &self.active_popup {
                    let popup_area = if area.width < 4 || area.height < 4 {
                        area
                    } else {
                        Rect::new(
                            area.width / 4,
                            area.height / 4,
                            area.width / 2,
                            area.height / 2,
                        )
                    };

                    f.render_widget(Clear, popup_area);

                    let block = Block::default()
                        .title(popup.title.as_str())
                        .borders(Borders::ALL)
                        .style(Style::default().bg(Color::DarkGray));

                    let inner_area = block.inner(popup_area);
                    f.render_widget(block, popup_area);

                    let layout = Layout::default()
                        .direction(Direction::Vertical)
                        .constraints([Constraint::Min(0), Constraint::Length(1)])
                        .split(inner_area);

                    // Render choice items
                    let items: Vec<ListItem> = popup
                        .items
                        .iter()
                        .enumerate()
                        .map(|(i, item)| {
                            let is_selected = i == popup.selected_index;
                            let style = if is_selected {
                                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
                            } else {
                                Style::default()
                            };
                            let prefix = if is_selected { "> " } else { "  " };
                            ListItem::new(Line::from(vec![
                                Span::styled(prefix, style),
                                Span::styled(item.clone(), style),
                            ]))
                        })
                        .collect();

                    let list = List::new(items).block(Block::default());
                    f.render_widget(list, layout[0]);

                    let help_text = Paragraph::new("Enter: Select  Esc: Cancel  j/k or Arrows: Move")
                        .style(Style::default().fg(Color::Gray));
                    f.render_widget(help_text, layout[1]);
                }
            });

            if let Err(e) = res {
                eprintln!("Render failed: {}", e);
            }
        }
    }
}

fn format_timestamp(timestamp: &str) -> String {
    if let Ok(datetime) = chrono::DateTime::parse_from_rfc3339(timestamp) {
        datetime.format("%H:%M").to_string()
    } else {
        timestamp.chars().take(5).collect()
    }
}