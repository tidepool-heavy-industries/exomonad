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

/// Render mode for the popup area (form vs wizard).
enum RenderMode {
    Form,
    Wizard,
}

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

/// Active form state for rendering all component types.
///
/// Uses pipe-based communication: launched via `zellij pipe --plugin`,
/// responds via `cli_pipe_output()`.
struct ActiveForm {
    /// CLI pipe ID for sending response back via cli_pipe_output()
    pipe_id: String,
    /// Request ID for correlation
    request_id: String,
    /// The popup form definition.
    definition: ui_protocol::PopupDefinition,
    /// Runtime state tracking current values.
    state: ui_protocol::PopupState,
    /// Index into `interactive_ids` — which component has focus.
    focused_index: usize,
    /// Component IDs for interactive (focusable) components, filtered by visibility.
    interactive_ids: Vec<String>,
    /// Sub-index for list-type components (Choice highlighted row, Multiselect highlighted row).
    sub_index: HashMap<String, usize>,
    /// Cursor position within the focused Textbox.
    text_cursor: usize,
}

impl ActiveForm {
    fn new(pipe_id: String, request_id: String, definition: ui_protocol::PopupDefinition) -> Self {
        let state = ui_protocol::PopupState::new(&definition);
        let interactive_ids = Self::compute_interactive_ids(&definition.components, &state);
        // Initialize sub_index for Choice and Multiselect components
        let mut sub_index = HashMap::new();
        for c in &definition.components {
            match c {
                ui_protocol::Component::Choice { id, default, .. } => {
                    sub_index.insert(id.clone(), default.unwrap_or(0));
                }
                ui_protocol::Component::Multiselect { id, .. } => {
                    sub_index.insert(id.clone(), 0);
                }
                _ => {}
            }
        }
        Self {
            pipe_id,
            request_id,
            definition,
            state,
            focused_index: 0,
            interactive_ids,
            sub_index,
            text_cursor: 0,
        }
    }

    fn compute_interactive_ids(components: &[ui_protocol::Component], state: &ui_protocol::PopupState) -> Vec<String> {
        components.iter().filter(|c| {
            // Must be visible
            if !state.is_visible(c) {
                return false;
            }
            // Must be interactive (not Text or Group)
            !matches!(c, ui_protocol::Component::Text { .. } | ui_protocol::Component::Group { .. })
        }).map(|c| c.id().to_string()).collect()
    }

    fn recompute_interactive_ids(&mut self) {
        self.interactive_ids = Self::compute_interactive_ids(&self.definition.components, &self.state);
        if self.focused_index >= self.interactive_ids.len() && !self.interactive_ids.is_empty() {
            self.focused_index = self.interactive_ids.len() - 1;
        }
    }

    fn focused_id(&self) -> Option<&str> {
        self.interactive_ids.get(self.focused_index).map(|s| s.as_str())
    }

    fn focused_component(&self) -> Option<&ui_protocol::Component> {
        let id = self.focused_id()?;
        self.definition.components.iter().find(|c| c.id() == id)
    }

    fn to_popup_result(&self, button: &str) -> ui_protocol::PopupResult {
        ui_protocol::PopupResult {
            button: button.to_string(),
            values: self.state.to_json_values(),
            time_spent_seconds: None,
        }
    }
}

/// Active wizard state for multi-pane wizard navigation.
///
/// Wraps an ActiveForm for the current pane with wizard-specific state
/// (pane history, collected values across panes, transition logic).
struct ActiveWizard {
    pipe_id: String,
    request_id: String,
    wizard: ui_protocol::WizardDefinition,
    current_pane: String,
    pane_history: Vec<String>,
    collected_values: HashMap<String, serde_json::Value>,
    /// Form state for the current pane.
    form: ActiveForm,
}

impl ActiveWizard {
    fn new(pipe_id: String, request_id: String, wizard: ui_protocol::WizardDefinition) -> Option<Self> {
        let start_pane_name = wizard.start.clone();
        let pane = wizard.panes.get(&start_pane_name)?;
        let definition = ui_protocol::PopupDefinition {
            title: pane.title.clone(),
            components: pane.elements.clone(),
        };
        let form = ActiveForm::new(pipe_id.clone(), request_id.clone(), definition);
        Some(Self {
            pipe_id,
            request_id,
            wizard,
            current_pane: start_pane_name.clone(),
            pane_history: vec![start_pane_name],
            collected_values: HashMap::new(),
            form,
        })
    }

    /// Whether the current pane is terminal (no transition — submit ends the wizard).
    fn is_terminal(&self) -> bool {
        self.wizard.panes.get(&self.current_pane)
            .map_or(true, |p| p.then_transition.is_none())
    }

    /// Resolve the next pane based on the current pane's transition rule and form state.
    fn resolve_transition(&self) -> Option<String> {
        let pane = self.wizard.panes.get(&self.current_pane)?;
        let transition = pane.then_transition.as_ref()?;
        match transition {
            ui_protocol::Transition::Goto(target) => Some(target.clone()),
            ui_protocol::Transition::Branch(map) => {
                for (field_id, value_map) in map {
                    // Check choice fields (by label)
                    if let Some(label) = self.form.state
                        .get_choice_label(field_id, &self.form.definition.components)
                    {
                        if let Some(target) = value_map.get(&label) {
                            return Some(target.clone());
                        }
                    }
                    // Check boolean fields ("true"/"false")
                    if let Some(val) = self.form.state.get_boolean(field_id) {
                        let key = if val { "true" } else { "false" };
                        if let Some(target) = value_map.get(key) {
                            return Some(target.clone());
                        }
                    }
                }
                None
            }
        }
    }

    /// Advance to a target pane, saving current pane values.
    fn advance_to(&mut self, target: &str) {
        // Collect current pane values
        self.collected_values.insert(
            self.current_pane.clone(),
            self.form.state.to_json_values(),
        );

        if let Some(pane) = self.wizard.panes.get(target) {
            self.current_pane = target.to_string();
            self.pane_history.push(target.to_string());
            let definition = ui_protocol::PopupDefinition {
                title: pane.title.clone(),
                components: pane.elements.clone(),
            };
            self.form = ActiveForm::new(
                self.pipe_id.clone(),
                self.request_id.clone(),
                definition,
            );
        }
    }

    /// Navigate back to the previous pane. Returns false if already at the start.
    fn go_back(&mut self) -> bool {
        if self.pane_history.len() <= 1 {
            return false;
        }
        // Save current values
        self.collected_values.insert(
            self.current_pane.clone(),
            self.form.state.to_json_values(),
        );
        // Pop current pane
        self.pane_history.pop();
        let prev = self.pane_history.last().unwrap().clone();

        if let Some(pane) = self.wizard.panes.get(&prev) {
            self.current_pane = prev.clone();
            let definition = ui_protocol::PopupDefinition {
                title: pane.title.clone(),
                components: pane.elements.clone(),
            };
            self.form = ActiveForm::new(
                self.pipe_id.clone(),
                self.request_id.clone(),
                definition,
            );
            // Restore previously collected values for this pane
            if let Some(prev_values) = self.collected_values.get(&prev) {
                if let Some(obj) = prev_values.as_object() {
                    for (k, v) in obj {
                        match v {
                            serde_json::Value::Number(n) => {
                                if let Some(f) = n.as_f64() {
                                    self.form.state.set_number(k, f as f32);
                                }
                            }
                            serde_json::Value::Bool(b) => {
                                self.form.state.set_boolean(k, *b);
                            }
                            serde_json::Value::String(s) => {
                                self.form.state.set_text(k, s.clone());
                            }
                            serde_json::Value::Array(arr) => {
                                // MultiChoice: array of bools
                                let bools: Vec<bool> = arr.iter()
                                    .map(|v| v.as_bool().unwrap_or(false))
                                    .collect();
                                if bools.iter().all(|_| true) {
                                    self.form.state.set_multichoice(k, bools);
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            self.form.recompute_interactive_ids();
            true
        } else {
            false
        }
    }

    /// Build the wizard result with values from all visited panes.
    fn to_wizard_result(&self, button: &str) -> ui_protocol::WizardResult {
        let mut values = self.collected_values.clone();
        // Include current pane values
        values.insert(self.current_pane.clone(), self.form.state.to_json_values());
        ui_protocol::WizardResult {
            button: button.to_string(),
            values,
            panes_visited: self.pane_history.clone(),
        }
    }

    /// Build breadcrumb trail string from pane history.
    fn breadcrumbs(&self) -> String {
        self.pane_history.iter()
            .enumerate()
            .map(|(i, name)| {
                if i == self.pane_history.len() - 1 {
                    format!("[{}]", name)
                } else {
                    name.clone()
                }
            })
            .collect::<Vec<_>>()
            .join(" > ")
    }
}

#[derive(Default)]
struct ExoMonadPlugin {
    status_state: PluginState,
    status_message: String,
    active_form: Option<ActiveForm>,
    active_wizard: Option<ActiveWizard>,
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
    /// This plugin instance's own pane ID (from get_plugin_ids()).
    own_pane_id: u32,
    /// The tab name this plugin instance lives in, derived from PaneManifest correlation.
    own_tab_name: Option<String>,
    /// Pane IDs awaiting a deferred Enter keypress (after text injection).
    /// Delayed to ensure Node.js/Ink processes the text and Enter as separate data events.
    pending_enter: Vec<u32>,
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
        self.own_tab_name = None;
        let manifest = match &self.pane_manifest_cache {
            Some(m) => m,
            None => return,
        };
        for (tab_pos, panes) in &manifest.panes {
            if let Some(tab_name) = self.tab_names.get(tab_pos) {
                if let Some(pane) = panes.iter().find(|p| !p.is_plugin && !p.is_floating && !p.exited) {
                    self.tab_pane_map.insert(tab_name.clone(), pane.id);
                }
                // Determine which tab this plugin instance lives in
                if panes.iter().any(|p| p.is_plugin && p.id == self.own_pane_id) {
                    self.own_tab_name = Some(tab_name.clone());
                }
            }
        }
    }
}

/// Handle form-level key events (navigation, input, toggles).
/// Shared between simple form mode and wizard mode (which delegates to its inner form).
fn handle_form_key(form: &mut ActiveForm, key: &KeyWithModifier, should_render: &mut bool) {
    let has_shift = key.key_modifiers.contains(&KeyModifier::Shift);
    match key.bare_key {
        BareKey::Tab => {
            if !form.interactive_ids.is_empty() {
                if has_shift {
                    form.focused_index = if form.focused_index == 0 {
                        form.interactive_ids.len() - 1
                    } else {
                        form.focused_index - 1
                    };
                } else {
                    form.focused_index = (form.focused_index + 1) % form.interactive_ids.len();
                }
                if let Some(id) = form.focused_id() {
                    if let Some(ui_protocol::ElementValue::Text(t)) = form.state.values.get(id) {
                        form.text_cursor = t.len();
                    }
                }
                *should_render = true;
            }
        }
        BareKey::Up | BareKey::Char('k') => {
            if let Some(component) = form.focused_component().cloned() {
                match &component {
                    ui_protocol::Component::Choice { id, .. } => {
                        let sub = form.sub_index.entry(id.clone()).or_insert(0);
                        if *sub > 0 {
                            *sub -= 1;
                            form.state.set_choice(id, *sub);
                            form.recompute_interactive_ids();
                            *should_render = true;
                        }
                    }
                    ui_protocol::Component::Multiselect { id, .. } => {
                        let sub = form.sub_index.entry(id.clone()).or_insert(0);
                        if *sub > 0 {
                            *sub -= 1;
                            *should_render = true;
                        }
                    }
                    _ => {}
                }
            }
        }
        BareKey::Down | BareKey::Char('j') => {
            if let Some(component) = form.focused_component().cloned() {
                match &component {
                    ui_protocol::Component::Choice { id, options, .. } => {
                        let sub = form.sub_index.entry(id.clone()).or_insert(0);
                        if *sub < options.len().saturating_sub(1) {
                            *sub += 1;
                            form.state.set_choice(id, *sub);
                            form.recompute_interactive_ids();
                            *should_render = true;
                        }
                    }
                    ui_protocol::Component::Multiselect { id, options, .. } => {
                        let sub = form.sub_index.entry(id.clone()).or_insert(0);
                        if *sub < options.len().saturating_sub(1) {
                            *sub += 1;
                            *should_render = true;
                        }
                    }
                    _ => {}
                }
            }
        }
        BareKey::Left => {
            if let Some(component) = form.focused_component().cloned() {
                match &component {
                    ui_protocol::Component::Slider { id, min, .. } => {
                        if let Some(val) = form.state.get_number(id) {
                            let new_val = (val - 1.0).max(*min);
                            form.state.set_number(id, new_val);
                            form.recompute_interactive_ids();
                            *should_render = true;
                        }
                    }
                    ui_protocol::Component::Textbox { .. } => {
                        if form.text_cursor > 0 {
                            form.text_cursor -= 1;
                            *should_render = true;
                        }
                    }
                    _ => {}
                }
            }
        }
        BareKey::Right => {
            if let Some(component) = form.focused_component().cloned() {
                match &component {
                    ui_protocol::Component::Slider { id, max, .. } => {
                        if let Some(val) = form.state.get_number(id) {
                            let new_val = (val + 1.0).min(*max);
                            form.state.set_number(id, new_val);
                            form.recompute_interactive_ids();
                            *should_render = true;
                        }
                    }
                    ui_protocol::Component::Textbox { id, .. } => {
                        if let Some(text) = form.state.get_text(id) {
                            if form.text_cursor < text.len() {
                                form.text_cursor += 1;
                                *should_render = true;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        BareKey::Char(' ') => {
            if let Some(component) = form.focused_component().cloned() {
                match &component {
                    ui_protocol::Component::Checkbox { id, .. } => {
                        let val = form.state.get_boolean(id).unwrap_or(false);
                        form.state.set_boolean(id, !val);
                        form.recompute_interactive_ids();
                        *should_render = true;
                    }
                    ui_protocol::Component::Multiselect { id, .. } => {
                        let sub = *form.sub_index.get(id.as_str()).unwrap_or(&0);
                        if let Some(mc) = form.state.get_multichoice(id) {
                            let mut mc = mc.to_vec();
                            if sub < mc.len() {
                                mc[sub] = !mc[sub];
                                form.state.set_multichoice(id, mc);
                                form.recompute_interactive_ids();
                                *should_render = true;
                            }
                        }
                    }
                    ui_protocol::Component::Textbox { id, .. } => {
                        if let Some(text) = form.state.get_text(id).map(|s| s.to_string()) {
                            let mut t = text;
                            let cursor = form.text_cursor.min(t.len());
                            t.insert(cursor, ' ');
                            form.text_cursor = cursor + 1;
                            form.state.set_text(id, t);
                            *should_render = true;
                        }
                    }
                    _ => {}
                }
            }
        }
        BareKey::Backspace => {
            if let Some(component) = form.focused_component().cloned() {
                if let ui_protocol::Component::Textbox { id, .. } = &component {
                    if let Some(text) = form.state.get_text(id).map(|s| s.to_string()) {
                        let mut t = text;
                        if form.text_cursor > 0 && form.text_cursor <= t.len() {
                            t.remove(form.text_cursor - 1);
                            form.text_cursor -= 1;
                            form.state.set_text(id, t);
                            *should_render = true;
                        }
                    }
                }
            }
        }
        BareKey::Delete => {
            if let Some(component) = form.focused_component().cloned() {
                if let ui_protocol::Component::Textbox { id, .. } = &component {
                    if let Some(text) = form.state.get_text(id).map(|s| s.to_string()) {
                        let mut t = text;
                        if form.text_cursor < t.len() {
                            t.remove(form.text_cursor);
                            form.state.set_text(id, t);
                            *should_render = true;
                        }
                    }
                }
            }
        }
        BareKey::Char(ch) => {
            if let Some(component) = form.focused_component().cloned() {
                if let ui_protocol::Component::Textbox { id, .. } = &component {
                    if let Some(text) = form.state.get_text(id).map(|s| s.to_string()) {
                        let mut t = text;
                        let cursor = form.text_cursor.min(t.len());
                        t.insert(cursor, ch);
                        form.text_cursor = cursor + 1;
                        form.state.set_text(id, t);
                        *should_render = true;
                    }
                }
            }
        }
        _ => {}
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
            EventType::Timer,
        ]);
        self.status_state = PluginState::Idle;
        self.status_message = "Ready.".to_string();
        self.events = VecDeque::new();
        self.own_pane_id = get_plugin_ids().plugin_id;
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
                // Check target_tab routing: only render in the targeted plugin instance.
                // Other instances unblock the pipe and ignore the request.
                if let Ok(json) = serde_json::from_str::<serde_json::Value>(trimmed_payload) {
                    if let Some(target) = json.get("target_tab").and_then(|v| v.as_str()) {
                        if let Some(own_tab) = &self.own_tab_name {
                            if own_tab != target {
                                unblock_cli_pipe_input(&pipe_id);
                                return true;
                            }
                        }
                    }
                }

                // Try WizardRequest first (has "wizard" key)
                if let Ok(req) = serde_json::from_str::<ui_protocol::WizardRequest>(trimmed_payload) {
                    block_cli_pipe_input(&pipe_id);
                    show_self(true);
                    match ActiveWizard::new(pipe_id.clone(), req.request_id, req.wizard) {
                        Some(wizard) => {
                            self.active_wizard = Some(wizard);
                            self.status_state = PluginState::Waiting;
                            self.status_message = "Wizard active...".to_string();
                        }
                        None => {
                            self.status_state = PluginState::Error;
                            self.status_message = "Invalid wizard: start pane not found".to_string();
                            cli_pipe_output(&pipe_id, "ERROR_INVALID_WIZARD\n");
                            unblock_cli_pipe_input(&pipe_id);
                        }
                    }
                    return true;
                }

                // Try PopupRequest (simple form)
                match serde_json::from_str::<ui_protocol::PopupRequest>(trimmed_payload) {
                    Ok(req) => {
                        block_cli_pipe_input(&pipe_id);
                        show_self(true);
                        self.active_form = Some(ActiveForm::new(
                            pipe_id,
                            req.request_id,
                            req.definition,
                        ));
                        self.status_state = PluginState::Waiting;
                        self.status_message = "Waiting for input...".to_string();
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

            // Legacy pipe format: "request_id|title|item1,item2,item3"
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

            // Convert legacy format to PopupDefinition with a single Choice
            let definition = ui_protocol::PopupDefinition {
                title: title.clone(),
                components: vec![ui_protocol::Component::Choice {
                    id: "selection".to_string(),
                    label: String::new(),
                    options: items,
                    default: Some(0),
                    visible_when: None,
                }],
            };

            block_cli_pipe_input(&pipe_id);
            show_self(true);
            self.active_form = Some(ActiveForm::new(pipe_id, request_id, definition));
            self.status_state = PluginState::Waiting;
            self.status_message = "Waiting for input...".to_string();
            return true;
        }

        // Handle inject-input requests: write text into a target pane resolved by tab name.
        // Only the plugin instance residing in the target tab acts, preventing
        // duplicate writes from the broadcast delivery to all instances.
        if pipe_message.name == transport::INJECT_INPUT_PIPE {
            if let Some(payload) = pipe_message.payload {
                match serde_json::from_str::<serde_json::Value>(&payload) {
                    Ok(val) => {
                        let tab_name = match val["tab_name"].as_str() {
                            Some(t) => t,
                            None => {
                                eprintln!("[exomonad-plugin] inject-input: missing 'tab_name' string field");
                                return true;
                            }
                        };

                        // Dedup: only the instance in the target tab should write.
                        // If own_tab_name is None (before first PaneUpdate), fall through
                        // to avoid silently dropping the message from all instances.
                        if let Some(own_tab) = &self.own_tab_name {
                            if own_tab != tab_name {
                                return true;
                            }
                        }

                        let text = match val["text"].as_str() {
                            Some(t) => t,
                            None => {
                                eprintln!("[exomonad-plugin] inject-input: missing 'text' string field");
                                return true;
                            }
                        };

                        if let Some(&pane_id) = self.tab_pane_map.get(tab_name) {
                            write_chars_to_pane_id(text, PaneId::Terminal(pane_id));
                            // Defer the Enter keypress so the OS kernel flushes the text
                            // and Node.js/Ink processes it as a separate data event.
                            // Without this delay, Ink treats text+Enter as a single paste
                            // and key.return never fires.
                            self.pending_enter.push(pane_id);
                            set_timeout(0.1);
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
                                self.active_form = None;
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
                // Wizard mode key handling
                if let Some(wizard) = &mut self.active_wizard {
                    let mut dismiss = false;

                    match key.bare_key {
                        BareKey::Esc => {
                            // Cancel wizard — return partial results
                            let result = wizard.to_wizard_result("cancelled");
                            let response = serde_json::to_string(&serde_json::json!({
                                "request_id": wizard.request_id,
                                "result": result,
                            })).unwrap_or_default();
                            cli_pipe_output(&wizard.pipe_id, &format!("{}\n", response));
                            unblock_cli_pipe_input(&wizard.pipe_id);
                            dismiss = true;
                        }
                        BareKey::Enter => {
                            if wizard.is_terminal() {
                                // Submit wizard — collect all values
                                let result = wizard.to_wizard_result("submit");
                                let response = serde_json::to_string(&serde_json::json!({
                                    "request_id": wizard.request_id,
                                    "result": result,
                                })).unwrap_or_default();
                                cli_pipe_output(&wizard.pipe_id, &format!("{}\n", response));
                                unblock_cli_pipe_input(&wizard.pipe_id);
                                dismiss = true;
                            } else {
                                // Advance to next pane via transition
                                if let Some(target) = wizard.resolve_transition() {
                                    wizard.advance_to(&target);
                                    should_render = true;
                                }
                            }
                        }
                        BareKey::Backspace => {
                            // Back navigation: only if not in a textbox
                            let in_textbox = wizard.form.focused_component()
                                .map_or(false, |c| matches!(c, ui_protocol::Component::Textbox { .. }));
                            if !in_textbox {
                                if wizard.go_back() {
                                    should_render = true;
                                }
                            } else {
                                // Delegate to form (textbox backspace)
                                handle_form_key(&mut wizard.form, &key, &mut should_render);
                            }
                        }
                        _ => {
                            // Delegate all other keys to the inner form
                            handle_form_key(&mut wizard.form, &key, &mut should_render);
                        }
                    }

                    if dismiss {
                        self.active_wizard = None;
                        self.status_state = PluginState::Idle;
                        self.status_message = "Ready.".to_string();
                        should_render = true;
                        hide_self();
                    }
                }

                // Simple form mode key handling
                if let Some(form) = &mut self.active_form {
                    let mut dismiss = false;

                    match key.bare_key {
                        BareKey::Esc => {
                            let result = form.to_popup_result("cancelled");
                            let response = serde_json::to_string(&ui_protocol::PopupResponse {
                                request_id: form.request_id.clone(),
                                result,
                            }).unwrap_or_default();
                            cli_pipe_output(&form.pipe_id, &format!("{}\n", response));
                            unblock_cli_pipe_input(&form.pipe_id);
                            dismiss = true;
                        }
                        BareKey::Enter => {
                            let result = form.to_popup_result("submit");
                            let response = serde_json::to_string(&ui_protocol::PopupResponse {
                                request_id: form.request_id.clone(),
                                result,
                            }).unwrap_or_default();
                            cli_pipe_output(&form.pipe_id, &format!("{}\n", response));
                            unblock_cli_pipe_input(&form.pipe_id);
                            dismiss = true;
                        }
                        _ => handle_form_key(form, &key, &mut should_render),
                    }

                    if dismiss {
                        self.active_form = None;
                        self.status_state = PluginState::Idle;
                        self.status_message = "Ready.".to_string();
                        should_render = true;
                        hide_self();
                    }
                }
            }
            Event::Timer(_) => {
                // Flush deferred Enter keypresses for inject-input.
                const ENTER_KEY: u8 = 13;
                for pane_id in self.pending_enter.drain(..) {
                    write_to_pane_id(vec![ENTER_KEY], PaneId::Terminal(pane_id));
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

                // Determine which mode to render: wizard, form, or neither
                let render_mode: Option<RenderMode> = if self.active_wizard.is_some() {
                    Some(RenderMode::Wizard)
                } else if self.active_form.is_some() {
                    Some(RenderMode::Form)
                } else {
                    None
                };

                if let Some(mode) = render_mode {
                    let popup_area = area;
                    f.render_widget(Clear, popup_area);

                    // Get title and form reference based on mode
                    let (title, breadcrumbs, is_terminal) = match &mode {
                        RenderMode::Wizard => {
                            let w = self.active_wizard.as_ref().unwrap();
                            (
                                format!("{} — {}", w.wizard.title, w.form.definition.title),
                                Some(w.breadcrumbs()),
                                w.is_terminal(),
                            )
                        }
                        RenderMode::Form => {
                            let f = self.active_form.as_ref().unwrap();
                            (f.definition.title.clone(), None, true)
                        }
                    };

                    let block = Block::default()
                        .title(Line::from(vec![
                            Span::styled(format!(" {} ", title), Style::default().fg(COLOR_BASE3).add_modifier(Modifier::BOLD))
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
                        .constraints([Constraint::Min(0), Constraint::Length(2)])
                        .split(inner_area);

                    let mut lines: Vec<Line> = Vec::new();

                    // Breadcrumbs for wizard mode
                    if let Some(crumbs) = &breadcrumbs {
                        lines.push(Line::from(Span::styled(
                            format!("  {}", crumbs),
                            Style::default().fg(COLOR_BASE01).add_modifier(Modifier::ITALIC),
                        )));
                        lines.push(Line::from(""));
                    }

                    // Get form reference for component rendering
                    let form = match &mode {
                        RenderMode::Wizard => &self.active_wizard.as_ref().unwrap().form,
                        RenderMode::Form => self.active_form.as_ref().unwrap(),
                    };

                    let focused_id = form.focused_id().unwrap_or("");

                    for component in &form.definition.components {
                        if !form.state.is_visible(component) {
                            continue;
                        }

                        let is_focused = component.id() == focused_id;
                        let focus_indicator = if is_focused { "▸ " } else { "  " };
                        let label_style = if is_focused {
                            Style::default().fg(COLOR_YELLOW).add_modifier(Modifier::BOLD)
                        } else {
                            Style::default().fg(COLOR_BASE1)
                        };

                        match component {
                            ui_protocol::Component::Text { content, .. } => {
                                for text_line in content.lines() {
                                    lines.push(Line::from(Span::styled(
                                        format!("  {}", text_line),
                                        Style::default().fg(COLOR_BASE1),
                                    )));
                                }
                                lines.push(Line::from(""));
                            }
                            ui_protocol::Component::Group { label, .. } => {
                                lines.push(Line::from(vec![
                                    Span::styled(
                                        format!("── {} ", label),
                                        Style::default().fg(COLOR_CYAN).add_modifier(Modifier::BOLD),
                                    ),
                                    Span::styled(
                                        "─".repeat(inner_area.width.saturating_sub(label.len() as u16 + 5) as usize),
                                        Style::default().fg(COLOR_BASE01),
                                    ),
                                ]));
                            }
                            ui_protocol::Component::Checkbox { id, label, .. } => {
                                let checked = form.state.get_boolean(id).unwrap_or(false);
                                let check_char = if checked { "x" } else { " " };
                                let check_style = if checked {
                                    Style::default().fg(COLOR_GREEN)
                                } else {
                                    Style::default().fg(COLOR_BASE01)
                                };
                                lines.push(Line::from(vec![
                                    Span::styled(focus_indicator, label_style),
                                    Span::styled(format!("[{}] ", check_char), check_style),
                                    Span::styled(label.as_str(), label_style),
                                ]));
                            }
                            ui_protocol::Component::Slider { id, label, min, max, .. } => {
                                let val = form.state.get_number(id).unwrap_or(*min);
                                let range = max - min;
                                let bar_width = 20usize;
                                let filled = if range > 0.0 {
                                    ((val - min) / range * bar_width as f32) as usize
                                } else {
                                    0
                                };
                                let empty = bar_width.saturating_sub(filled);
                                lines.push(Line::from(vec![
                                    Span::styled(focus_indicator, label_style),
                                    Span::styled(format!("{}: ", label), label_style),
                                ]));
                                lines.push(Line::from(vec![
                                    Span::raw("  ["),
                                    Span::styled("=".repeat(filled), Style::default().fg(COLOR_BLUE)),
                                    Span::styled("|", Style::default().fg(COLOR_YELLOW)),
                                    Span::styled("-".repeat(empty), Style::default().fg(COLOR_BASE01)),
                                    Span::raw("] "),
                                    Span::styled(format!("{:.0}", val), Style::default().fg(COLOR_BASE3)),
                                ]));
                            }
                            ui_protocol::Component::Choice { id, label, options, .. } => {
                                if !label.is_empty() {
                                    lines.push(Line::from(vec![
                                        Span::styled(focus_indicator, label_style),
                                        Span::styled(format!("{}:", label), label_style),
                                    ]));
                                }
                                let selected = form.state.get_choice(id).unwrap_or(0);
                                for (i, option) in options.iter().enumerate() {
                                    let is_selected = i == selected;
                                    if is_selected {
                                        lines.push(Line::from(vec![
                                            Span::raw("  "),
                                            Span::styled(
                                                format!(" {} ", option),
                                                Style::default().bg(COLOR_BLUE).fg(COLOR_BASE3).add_modifier(Modifier::BOLD),
                                            ),
                                        ]));
                                    } else {
                                        lines.push(Line::from(vec![
                                            Span::styled(format!("  {}. ", i + 1), Style::default().fg(COLOR_BASE01)),
                                            Span::styled(option.as_str(), Style::default().fg(COLOR_BASE1)),
                                        ]));
                                    }
                                }
                            }
                            ui_protocol::Component::Multiselect { id, label, options, .. } => {
                                lines.push(Line::from(vec![
                                    Span::styled(focus_indicator, label_style),
                                    Span::styled(format!("{}:", label), label_style),
                                ]));
                                let selections = form.state.get_multichoice(id).unwrap_or(&[]);
                                let highlighted = *form.sub_index.get(id.as_str()).unwrap_or(&0);
                                for (i, option) in options.iter().enumerate() {
                                    let is_checked = selections.get(i).copied().unwrap_or(false);
                                    let is_highlighted = is_focused && i == highlighted;
                                    let check_char = if is_checked { "x" } else { " " };
                                    let style = if is_highlighted {
                                        Style::default().fg(COLOR_BASE3).add_modifier(Modifier::BOLD)
                                    } else if is_checked {
                                        Style::default().fg(COLOR_GREEN)
                                    } else {
                                        Style::default().fg(COLOR_BASE1)
                                    };
                                    lines.push(Line::from(vec![
                                        Span::raw("  "),
                                        Span::styled(format!("[{}] ", check_char), style),
                                        Span::styled(option.as_str(), style),
                                    ]));
                                }
                            }
                            ui_protocol::Component::Textbox { id, label, placeholder, .. } => {
                                lines.push(Line::from(vec![
                                    Span::styled(focus_indicator, label_style),
                                    Span::styled(format!("{}:", label), label_style),
                                ]));
                                let text = form.state.get_text(id).unwrap_or("").to_string();
                                let display_text = if text.is_empty() {
                                    placeholder.as_deref().unwrap_or("").to_string()
                                } else {
                                    text.clone()
                                };
                                let text_style = if text.is_empty() {
                                    Style::default().fg(COLOR_BASE01)
                                } else {
                                    Style::default().fg(COLOR_BASE3)
                                };
                                let border_color = if is_focused { COLOR_CYAN } else { COLOR_BASE01 };

                                let box_width = inner_area.width.saturating_sub(4) as usize;
                                lines.push(Line::from(Span::styled(
                                    format!("  ┌{}┐", "─".repeat(box_width)),
                                    Style::default().fg(border_color),
                                )));
                                let truncated: String = display_text.chars().take(box_width).collect();
                                let padded: String = format!("{:width$}", truncated, width = box_width);
                                lines.push(Line::from(vec![
                                    Span::styled("  │".to_string(), Style::default().fg(border_color)),
                                    Span::styled(padded, text_style),
                                    Span::styled("│".to_string(), Style::default().fg(border_color)),
                                ]));
                                lines.push(Line::from(Span::styled(
                                    format!("  └{}┘", "─".repeat(box_width)),
                                    Style::default().fg(border_color),
                                )));
                            }
                        }
                    }

                    let paragraph = Paragraph::new(lines);
                    f.render_widget(paragraph, layout[0]);

                    // Help text with separator
                    let separator = Block::default().borders(Borders::TOP).border_style(Style::default().fg(COLOR_BASE01));
                    let help_area = layout[1];
                    f.render_widget(separator, help_area);

                    let help_text_area = Rect { y: help_area.y + 1, height: 1, ..help_area };

                    let key_style = Style::default().fg(COLOR_BASE1).add_modifier(Modifier::BOLD);
                    let desc_style = Style::default().fg(COLOR_BASE01);

                    let help_text = match &mode {
                        RenderMode::Wizard => {
                            let has_back = self.active_wizard.as_ref()
                                .map_or(false, |w| w.pane_history.len() > 1);
                            let action = if is_terminal { "Submit" } else { "Next" };
                            let mut spans = vec![
                                Span::styled("Enter", key_style), Span::styled(format!(": {}  ", action), desc_style),
                                Span::styled("Esc", key_style), Span::styled(": Cancel  ", desc_style),
                            ];
                            if has_back {
                                spans.push(Span::styled("Bksp", key_style));
                                spans.push(Span::styled(": Back  ", desc_style));
                            }
                            spans.push(Span::styled("Tab", key_style));
                            spans.push(Span::styled(": Next field  ", desc_style));
                            Line::from(spans)
                        }
                        RenderMode::Form => {
                            Line::from(vec![
                                Span::styled("Enter", key_style), Span::styled(": Submit  ", desc_style),
                                Span::styled("Esc", key_style), Span::styled(": Cancel  ", desc_style),
                                Span::styled("Tab", key_style), Span::styled(": Next  ", desc_style),
                                Span::styled("↑↓", key_style), Span::styled(": Select", desc_style),
                            ])
                        }
                    };

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