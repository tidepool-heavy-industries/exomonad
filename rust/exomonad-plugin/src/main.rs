use std::collections::{BTreeMap, HashMap, VecDeque};
use zellij_tile::prelude::*;
use ratatui::{
    backend::WindowSize,
    layout::{Constraint, Direction, Layout, Rect, Size, Position, Alignment},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, Paragraph, Clear, Padding},
    Terminal,
};

mod protocol;
use exomonad_core::ui_protocol::{self, transport, AgentEvent, CoordinatorAgentState, StateUpdate};
use protocol::{PluginMessage, PluginState};

// Solarized Dark Palette
const COLOR_BASE03: Color = Color::Rgb(0, 43, 54);
// const COLOR_BASE02: Color = Color::Rgb(7, 54, 66); // Unused for now
const COLOR_BASE01: Color = Color::Rgb(88, 110, 117);
// const COLOR_BASE00: Color = Color::Rgb(101, 123, 131); // Unused for now
// const COLOR_BASE0: Color = Color::Rgb(131, 148, 150); // Unused for now
const COLOR_BASE1: Color = Color::Rgb(147, 161, 161);
// const COLOR_BASE2: Color = Color::Rgb(238, 232, 213); // Unused for now
const COLOR_BASE3: Color = Color::Rgb(253, 246, 227);
const COLOR_YELLOW: Color = Color::Rgb(181, 137, 0);
const COLOR_ORANGE: Color = Color::Rgb(203, 75, 22);
const COLOR_RED: Color = Color::Rgb(220, 50, 47);
const COLOR_MAGENTA: Color = Color::Rgb(211, 54, 130);
// const COLOR_VIOLET: Color = Color::Rgb(108, 113, 196); // Unused for now
const COLOR_BLUE: Color = Color::Rgb(38, 139, 210);
const COLOR_CYAN: Color = Color::Rgb(42, 161, 152);
const COLOR_GREEN: Color = Color::Rgb(133, 153, 0);

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
    /// Tab position → tab name mapping, rebuilt from TabUpdate events.
    tab_names: HashMap<usize, String>,
    /// Cached pane manifest from last PaneUpdate event.
    pane_manifest_cache: Option<PaneManifest>,
    /// Tab name → terminal pane ID mapping, rebuilt when either tab or pane data updates.
    tab_pane_map: HashMap<String, u32>,
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
            // Move cursor (write! to String is infallible)
            let _ = write!(buffer, "\x1b[{};{}H", y + 1, x + 1);

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
                    let _ = write!(buffer, "\x1b[0m");
                } else {
                    if !current_is_default {
                        // Different non-default style: reset before applying new style
                        let _ = write!(buffer, "\x1b[0m");
                    }
                    // Apply desired foreground/background colors
                    if let Some(fg) = desired_fg {
                        let _ = write!(buffer, "{}", color_to_ansi(fg, false));
                    }
                    if let Some(bg) = desired_bg {
                        let _ = write!(buffer, "{}", color_to_ansi(bg, true));
                    }
                    // Apply desired modifiers
                    if desired_mod.contains(Modifier::BOLD) {
                        let _ = write!(buffer, "\x1b[1m");
                    }
                    if desired_mod.contains(Modifier::UNDERLINED) {
                        let _ = write!(buffer, "\x1b[4m");
                    }
                    if desired_mod.contains(Modifier::REVERSED) {
                        let _ = write!(buffer, "\x1b[7m");
                    }
                    if desired_mod.contains(Modifier::DIM) {
                        let _ = write!(buffer, "\x1b[2m");
                    }
                    if desired_mod.contains(Modifier::ITALIC) {
                        let _ = write!(buffer, "\x1b[3m");
                    }
                }

                current_fg = desired_fg;
                current_bg = desired_bg;
                current_mod = desired_mod;
            }

            let _ = write!(buffer, "{}", cell.symbol());
        }

        // Ensure we leave the terminal in a default style state after drawing.
        let _ = write!(buffer, "\x1b[0m");

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

impl ExoMonadPlugin {
    /// Rebuild tab_pane_map by correlating tab_names (from TabUpdate) with
    /// pane_manifest_cache (from PaneUpdate). For each tab, picks the first
    /// non-plugin, non-floating, non-exited terminal pane.
    fn rebuild_tab_pane_map(&mut self) {
        self.tab_pane_map.clear();
        let manifest = match &self.pane_manifest_cache {
            Some(m) => m,
            None => return,
        };
        for (tab_pos, panes) in &manifest.panes {
            if let Some(tab_name) = self.tab_names.get(tab_pos) {
                if let Some(pane) = panes.iter().find(|p| !p.is_plugin && !p.is_floating && !p.exited) {
                    self.tab_pane_map.insert(tab_name.clone(), pane.id);
                }
            }
        }
    }
}

impl ZellijPlugin for ExoMonadPlugin {
    fn load(&mut self, _configuration: BTreeMap<String, String>) {
        request_permission(&[
            PermissionType::ReadCliPipes,
            PermissionType::ChangeApplicationState,
            PermissionType::WriteToStdin,
            PermissionType::ReadApplicationState,
        ]);
        subscribe(&[
            EventType::CustomMessage,
            EventType::Key,
            EventType::TabUpdate,
            EventType::PaneUpdate,
        ]);
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

            // Parse payload: JSON or "request_id|title|item1,item2,item3"
            let payload = pipe_message.payload.unwrap_or_default();
            let trimmed_payload = payload.trim_start();

            if trimmed_payload.starts_with('{') {
                // Structured JSON payload (PopupRequest)
                match serde_json::from_str::<ui_protocol::PopupRequest>(trimmed_payload) {
                    Ok(req) => {
                        let items: Vec<String> = req
                            .definition
                            .components
                            .iter()
                            .filter_map(|c| match c {
                                ui_protocol::Component::Choice { options, .. } => {
                                    Some(options.clone())
                                }
                                _ => None,
                            })
                            .flatten()
                            .collect();

                        if items.is_empty() {
                            self.status_state = PluginState::Error;
                            self.status_message = "JSON popup must have at least one choice component".to_string();
                            // Don't include ':' to avoid misinterpretation as successful selection
                            cli_pipe_output(&pipe_id, &format!("ERROR_NO_ITEMS:{}\n", req.request_id));
                            unblock_cli_pipe_input(&pipe_id);
                            return true;
                        }

                        block_cli_pipe_input(&pipe_id);
                        show_self(true); // Toggle floating layer visible
                        self.active_popup = Some(ActivePopup {
                            pipe_id,
                            request_id: req.request_id,
                            title: req.definition.title,
                            items,
                            selected_index: 0,
                        });
                        self.status_state = PluginState::Waiting;
                        self.status_message = "Waiting for selection...".to_string();
                        return true;
                    }
                    Err(e) => {
                        self.status_state = PluginState::Error;
                        self.status_message = format!("Invalid JSON popup: {}", e);
                        cli_pipe_output(&pipe_id, &format!("ERROR_INVALID_JSON_{}\n", e));
                        unblock_cli_pipe_input(&pipe_id);
                        return true;
                    }
                }
            }

            let parts: Vec<&str> = payload.splitn(3, '|').collect();

            if parts.len() < 3 {
                self.status_state = PluginState::Error;
                self.status_message = "Invalid payload format".to_string();
                cli_pipe_output(&pipe_id, "ERROR:Invalid payload format\n");
                unblock_cli_pipe_input(&pipe_id);
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
                cli_pipe_output(&pipe_id, &format!("{}:ERROR:No items provided\n", request_id));
                unblock_cli_pipe_input(&pipe_id);
                return true;
            }

            block_cli_pipe_input(&pipe_id);
            show_self(true); // Toggle floating layer visible
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

        // Handle inject-input requests: write text into a target pane resolved by tab name
        if pipe_message.name == transport::INJECT_INPUT_PIPE {
            if let Some(payload) = pipe_message.payload {
                match serde_json::from_str::<serde_json::Value>(&payload) {
                    Ok(val) => {
                        let tab_name = val["tab_name"].as_str().unwrap_or("");
                        let text = val["text"].as_str().unwrap_or("");

                        if let Some(&pane_id) = self.tab_pane_map.get(tab_name) {
                            write_chars_to_pane_id(text, PaneId::Terminal(pane_id));
                            // Send Enter key to submit the input
                            write_to_pane_id(vec![13], PaneId::Terminal(pane_id));
                        } else {
                            eprintln!(
                                "[exomonad-plugin] inject-input: tab '{}' not found in pane map ({} entries)",
                                tab_name,
                                self.tab_pane_map.len()
                            );
                        }
                    }
                    Err(e) => {
                        eprintln!("[exomonad-plugin] inject-input: invalid JSON: {}", e);
                    }
                }
            }
            // Unblock CLI pipe if from CLI source
            if let PipeSource::Cli(id) = &pipe_message.source {
                unblock_cli_pipe_input(id);
            }
            return true;
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
                                hide_self();
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
            Event::TabUpdate(tab_infos) => {
                self.tab_names.clear();
                for tab in &tab_infos {
                    self.tab_names.insert(tab.position, tab.name.clone());
                }
                self.rebuild_tab_pane_map();
            }
            Event::PaneUpdate(pane_manifest) => {
                // Store raw pane data and rebuild the tab_name → pane_id map
                self.pane_manifest_cache = Some(pane_manifest);
                self.rebuild_tab_pane_map();
            }
            Event::Key(key) => {
                if let Some(popup) = &mut self.active_popup {
                    if popup.items.is_empty() {
                        return false;
                    }

                    match key.bare_key {
                        BareKey::Esc => {
                            let response = format!("{}:CANCELLED\n", popup.request_id);
                            cli_pipe_output(&popup.pipe_id, &response);
                            unblock_cli_pipe_input(&popup.pipe_id);
                            // Drop mutable borrow before clearing state
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
                            let selected = popup
                                .items
                                .get(popup.selected_index)
                                .cloned()
                                .unwrap_or_default();
                            let response = format!("{}:{}\n", popup.request_id, selected);
                            cli_pipe_output(&popup.pipe_id, &response);
                            unblock_cli_pipe_input(&popup.pipe_id);
                            // Drop mutable borrow before clearing state
                        }
                        _ => {}
                    }
                }

                // Dismiss after Esc/Enter: clear popup state, re-render as idle status
                if matches!(key.bare_key, BareKey::Esc | BareKey::Enter) && self.active_popup.is_some() {
                    self.active_popup = None;
                    self.status_state = PluginState::Idle;
                    self.status_message = "Ready.".to_string();
                    should_render = true;
                    hide_self();
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
                let status_block = Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(COLOR_BASE01))
                    .title(" ExoMonad ")
                    .title_style(Style::default().fg(COLOR_BASE1));
                
                let (status_color, status_icon) = match self.status_state {
                    PluginState::Error => (COLOR_RED, "●"),
                    PluginState::Thinking => (COLOR_YELLOW, "●"),
                    PluginState::Waiting => (COLOR_ORANGE, "●"),
                    PluginState::Idle => (COLOR_BLUE, "●"),
                };
                
                let state_text = match self.status_state {
                    PluginState::Idle => "IDLE",
                    PluginState::Thinking => "THINKING",
                    PluginState::Waiting => "WAITING",
                    PluginState::Error => "ERROR",
                };

                let status_text = Line::from(vec![
                    Span::styled(format!(" {} ", status_icon), Style::default().fg(status_color)),
                    Span::styled(format!("{}  ", state_text), Style::default().fg(status_color).add_modifier(Modifier::BOLD)),
                    Span::styled(&self.status_message, Style::default().fg(COLOR_BASE1)),
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
                        AgentEvent::AgentStarted { agent_id, timestamp } => (timestamp, format!("{} started", agent_id), COLOR_GREEN),
                        AgentEvent::AgentStopped { agent_id, timestamp } => (timestamp, format!("{} done", agent_id), COLOR_BLUE),
                        AgentEvent::StopHookBlocked { agent_id, reason, timestamp } => (timestamp, format!("{} blocked: {}", agent_id, reason), COLOR_RED),
                        AgentEvent::HookReceived { agent_id, hook_type, timestamp } => (timestamp, format!("{} hook: {}", agent_id, hook_type), COLOR_CYAN),
                        AgentEvent::PrFiled { agent_id, pr_number, timestamp } => (timestamp, format!("{} PR #{}", agent_id, pr_number), COLOR_MAGENTA),
                        AgentEvent::CopilotReviewed { agent_id, comment_count, timestamp } => (timestamp, format!("{} copilot: {} comments", agent_id, comment_count), COLOR_YELLOW),
                        AgentEvent::AgentStuck { agent_id, failed_stop_count, timestamp } => (timestamp, format!("{} ⚠ STUCK ({} failed stops)", agent_id, failed_stop_count), COLOR_RED),
                    };
                    
                    ListItem::new(Line::from(vec![
                        Span::styled(format!(" {} ", format_timestamp(time)), Style::default().fg(COLOR_BASE01)),
                        Span::styled(content, Style::default().fg(color)),
                    ]))
                }).collect();

                let events_list = List::new(event_items)
                    .block(Block::default()
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(COLOR_BASE01))
                        .title(" Events ")
                        .title_style(Style::default().fg(COLOR_BASE1)));
                f.render_widget(events_list, chunks[1]);

                // Popup - Full pane rendering (plugin is shown via show_self(true) as dedicated floating pane)
                if let Some(popup) = &self.active_popup {
                    // Use full area since this is a dedicated popup pane, not an overlay
                    let popup_area = area;

                    f.render_widget(Clear, popup_area);

                    let block = Block::default()
                        .title(Line::from(vec![
                            Span::styled(format!(" {} ", popup.title), Style::default().fg(COLOR_BASE3).add_modifier(Modifier::BOLD))
                        ]))
                        .title_alignment(Alignment::Center)
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(COLOR_CYAN))
                        .style(Style::default().bg(COLOR_BASE03))
                        .padding(Padding::horizontal(1));

                    let inner_area = block.inner(popup_area);
                    f.render_widget(block, popup_area);

                    let layout = Layout::default()
                        .direction(Direction::Vertical)
                        .constraints([Constraint::Min(0), Constraint::Length(2)]) // increased for better help text spacing
                        .split(inner_area);

                    // Render choice items
                    let items: Vec<ListItem> = popup
                        .items
                        .iter()
                        .enumerate()
                        .map(|(i, item)| {
                            let is_selected = i == popup.selected_index;
                            if is_selected {
                                let style = Style::default().bg(COLOR_BLUE).fg(COLOR_BASE3).add_modifier(Modifier::BOLD);
                                ListItem::new(Line::from(vec![
                                    Span::raw(" "), // Padding
                                    Span::raw(item.clone()),
                                ])).style(style)
                            } else {
                                let style = Style::default().fg(COLOR_BASE1);
                                ListItem::new(Line::from(vec![
                                    Span::styled(format!("{}. ", i + 1), Style::default().fg(COLOR_BASE01)),
                                    Span::styled(item.clone(), style),
                                ]))
                            }
                        })
                        .collect();

                    let list = List::new(items).block(Block::default());
                    f.render_widget(list, layout[0]);

                    // Help text with separator
                    let separator = Block::default().borders(Borders::TOP).border_style(Style::default().fg(COLOR_BASE01));
                    let help_area = layout[1];
                    f.render_widget(separator, help_area);
                    
                    // Render text inside the help area (offset by 1 due to border)
                    let help_text_area = Rect { y: help_area.y + 1, height: 1, ..help_area };
                    
                    let key_style = Style::default().fg(COLOR_BASE1).add_modifier(Modifier::BOLD);
                    let desc_style = Style::default().fg(COLOR_BASE01);
                    
                    let help_text = Line::from(vec![
                        Span::styled("Enter", key_style), Span::styled(": Select  ", desc_style),
                        Span::styled("Esc", key_style), Span::styled(": Cancel  ", desc_style),
                        Span::styled("↑/↓/j/k", key_style), Span::styled(": Move", desc_style),
                    ]);
                    
                    f.render_widget(Paragraph::new(help_text).alignment(Alignment::Center), help_text_area);
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