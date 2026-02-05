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
use exomonad_ui_protocol::AgentEvent;
use protocol::{
    Component, ElementValue, PluginMessage, PluginState, PopupDefinition, PopupState, VisibilityRule,
};

#[derive(Default)]
struct ExoMonadPlugin {
    status_state: PluginState,
    status_message: String,
    active_popup: Option<(String, PopupDefinition, PopupState)>,
    selected_index: usize,
    sub_index: usize,
    events: VecDeque<AgentEvent>,
    terminal: Option<Terminal<ZellijBackend>>,
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
                            PluginMessage::Popup {
                                request_id,
                                definition,
                            } => {
                                let state = PopupState::new(&definition);
                                self.active_popup = Some((request_id, definition, state));
                                self.selected_index = 0;
                                self.sub_index = 0;
                                self.status_state = PluginState::Waiting;
                                self.status_message = "Waiting for input...".to_string();
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
                if let Some((req_id, def, state)) = &mut self.active_popup {
                    if def.components.is_empty() {
                        return false;
                    }

                    // Component interaction logic
                    if let Some(comp) = def.components.get(self.selected_index) {
                        match comp {
                            Component::Textbox { id, .. } => {
                                // Handle text input directly if it's a character
                                if let BareKey::Char(c) = key.bare_key {
                                    // Filter control characters, allow standard input
                                    if !key.has_modifiers(&[KeyModifier::Ctrl, KeyModifier::Alt]) {
                                        if let Some(ElementValue::Text(s)) =
                                            state.values.get_mut(id)
                                        {
                                            s.push(c);
                                            return true;
                                        }
                                    }
                                } else if let BareKey::Backspace = key.bare_key {
                                    if let Some(ElementValue::Text(s)) = state.values.get_mut(id) {
                                        s.pop();
                                        return true;
                                    }
                                }
                            }
                            Component::Slider { id, min, max, .. } => {
                                if let BareKey::Left = key.bare_key {
                                    if let Some(ElementValue::Number(v)) = state.values.get_mut(id)
                                    {
                                        *v = (*v - 1.0).max(*min);
                                        return true;
                                    }
                                } else if let BareKey::Right = key.bare_key {
                                    if let Some(ElementValue::Number(v)) = state.values.get_mut(id)
                                    {
                                        *v = (*v + 1.0).min(*max);
                                        return true;
                                    }
                                }
                            }
                            Component::Multiselect { id, options, .. } => {
                                if let BareKey::Left = key.bare_key {
                                    if self.sub_index > 0 {
                                        self.sub_index -= 1;
                                        return true;
                                    }
                                } else if let BareKey::Right = key.bare_key {
                                    if self.sub_index < options.len().saturating_sub(1) {
                                        self.sub_index += 1;
                                        return true;
                                    }
                                } else if let BareKey::Char(' ') = key.bare_key {
                                    if let Some(ElementValue::MultiChoice(vec)) =
                                        state.values.get_mut(id)
                                    {
                                        if let Some(val) = vec.get_mut(self.sub_index) {
                                            *val = !*val;
                                            return true;
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    // Global/Navigation keys
                    match key.bare_key {
                        BareKey::Esc => {
                            run_command(
                                &["exomonad", "reply", "--id", req_id, "--cancel"],
                                BTreeMap::new(),
                            );
                            self.active_popup = None;
                            self.status_state = PluginState::Idle;
                            should_render = true;
                        }
                        BareKey::Down | BareKey::Char('j') => {
                            if self.selected_index < def.components.len().saturating_sub(1) {
                                self.selected_index += 1;
                                self.sub_index = 0; // Reset sub-index on component change
                                should_render = true;
                            }
                        }
                        BareKey::Up | BareKey::Char('k') => {
                            if self.selected_index > 0 {
                                self.selected_index -= 1;
                                self.sub_index = 0;
                                should_render = true;
                            }
                        }
                        BareKey::Char(' ') | BareKey::Enter => {
                            if let Some(comp) = def.components.get(self.selected_index) {
                                match comp {
                                    Component::Checkbox { id, .. } => {
                                        if let Some(ElementValue::Boolean(b)) =
                                            state.values.get_mut(id)
                                        {
                                            *b = !*b;
                                        }
                                    }
                                    Component::Choice { id, options, .. } => {
                                        if let Some(ElementValue::Choice(idx)) =
                                            state.values.get_mut(id)
                                        {
                                            *idx = (*idx + 1) % options.len();
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            should_render = true;
                        }
                        BareKey::Char('s') if key.has_modifiers(&[KeyModifier::Ctrl]) => {
                            match serde_json::to_string(&state.to_json_values()) {
                                Ok(json_values) => {
                                    run_command(
                                        &[
                                            "exomonad",
                                            "reply",
                                            "--id",
                                            req_id,
                                            "--payload",
                                            &json_values,
                                        ],
                                        BTreeMap::new(),
                                    );

                                    self.active_popup = None;
                                    self.status_state = PluginState::Idle;
                                    should_render = true;
                                }
                                Err(e) => {
                                    self.status_state = PluginState::Error;
                                    self.status_message = format!("Serialize failed: {}", e);
                                    should_render = true;
                                }
                            }
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
                    PluginState::Running => Color::Green,
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

                // Popup Overlay
                if let Some((_, def, state)) = &self.active_popup {
                    let popup_area = if area.width < 4 || area.height < 4 {
                        // On very small terminals, use the full area to avoid
                        // creating a too-small or invalid popup rectangle.
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
                        .title(def.title.as_str())
                        .borders(Borders::ALL)
                        .style(Style::default().bg(Color::DarkGray));
                    
                    let inner_area = block.inner(popup_area);
                    f.render_widget(block, popup_area);
                    
                    let layout = Layout::default()
                        .direction(Direction::Vertical)
                        .constraints([Constraint::Min(0), Constraint::Length(2)]) // Content + Help
                        .split(inner_area);
                    
                    let mut content_items = Vec::new();
                    
                    for (i, comp) in def.components.iter().enumerate() {
                        if let Some(rule) = comp.visible_when() {
                            if !evaluate_visibility(rule, state) {
                                continue;
                            }
                        }

                        let is_selected = i == self.selected_index;
                        let style = if is_selected { Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD) } else { Style::default() };
                        let prefix = if is_selected { "> " } else { "  " };

                        match comp {
                            Component::Text { content, .. } => {
                                content_items.push(ListItem::new(Line::from(vec![
                                    Span::styled(prefix, style),
                                    Span::styled(content.clone(), style),
                                ])));
                            }
                            Component::Checkbox { id, label, .. } => {
                                let checked = match state.values.get(id) {
                                    Some(ElementValue::Boolean(true)) => "[x]",
                                    _ => "[ ]",
                                };
                                content_items.push(ListItem::new(Line::from(vec![
                                    Span::styled(format!("{}{}: ", prefix, checked), style),
                                    Span::styled(label.clone(), style),
                                ])));
                            }
                            Component::Textbox { label, id, placeholder, .. } => {
                                let txt = state.get_text(id).unwrap_or("");
                                // Show placeholder only if text is empty AND the field is NOT selected
                                let display_txt = if txt.is_empty() {
                                    if is_selected {
                                        "".to_string()
                                    } else {
                                        placeholder.as_deref().unwrap_or("").to_string()
                                    }
                                } else {
                                    txt.to_string()
                                };
                                
                                let cursor_char = if is_selected { "|" } else { "" };
                                
                                content_items.push(ListItem::new(Line::from(vec![
                                    Span::styled(format!("{}{}: ", prefix, label), style),
                                    Span::styled(format!("[{}{}]", display_txt, cursor_char), if is_selected { style.bg(Color::Blue) } else { style }),
                                ])));
                            }
                            Component::Slider { label, id, min, max, .. } => {
                                let val = state.get_number(id).unwrap_or(*min);
                                content_items.push(ListItem::new(Line::from(vec![
                                    Span::styled(format!("{}{}: {:.1} ({:.1}-{:.1})", prefix, label, val, min, max), style),
                                ])));
                            }
                            Component::Choice { id, label, options, .. } => {
                                let idx = match state.values.get(id) {
                                    Some(ElementValue::Choice(i)) => *i,
                                    _ => 0,
                                };
                                let val = options.get(idx).map(|s| s.as_str()).unwrap_or("?");
                                content_items.push(ListItem::new(Line::from(vec![
                                    Span::styled(format!("{}{}: <{}>", prefix, label, val), style),
                                ])));
                            }
                            Component::Multiselect { label, id, options, .. } => {
                                content_items.push(ListItem::new(Line::from(vec![
                                    Span::styled(format!("{}{}:", prefix, label), style),
                                ])));
                                if let Some(ElementValue::MultiChoice(vals)) = state.values.get(id) {
                                    for (j, opt) in options.iter().enumerate() {
                                        let is_opt_selected = is_selected && j == self.sub_index;
                                        let sub_prefix = if is_opt_selected { "    >> " } else { "       " };
                                        let checked = if *vals.get(j).unwrap_or(&false) { "[x]" } else { "[ ]" };
                                        
                                        let opt_style = if is_opt_selected { Style::default().fg(Color::Yellow) } else { Style::default() };
                                        content_items.push(ListItem::new(Line::from(vec![
                                            Span::styled(format!("{}{}{}", sub_prefix, checked, opt), opt_style),
                                        ])));
                                    }
                                }
                            }
                            Component::Group { label, .. } => {
                                content_items.push(ListItem::new(Line::from(vec![
                                    Span::styled(format!("{}--- {} ---", prefix, label), Style::default().add_modifier(Modifier::UNDERLINED)),
                                ])));
                            }
                        }
                    }
                    
                    let content_list = List::new(content_items).block(Block::default());
                    f.render_widget(content_list, layout[0]);
                    
                    let help_text = Paragraph::new("Ctrl+S: Submit  Esc: Cancel  Enter/Space: Toggle  Arrows: Move")
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

fn evaluate_visibility(rule: &VisibilityRule, state: &PopupState) -> bool {
    match rule {
        VisibilityRule::Checked(id) => state.get_boolean(id).unwrap_or(false),
        VisibilityRule::Equals(map) => {
            for (id, _expected) in map {
                if let Some(_idx) = state.get_choice(id) {
                    return true;
                }
            }
            true
        }
        _ => true,
    }
}