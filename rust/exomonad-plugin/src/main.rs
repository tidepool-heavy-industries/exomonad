use std::collections::BTreeMap;
use zellij_tile::prelude::*;

mod protocol;
use protocol::{
    Component, ElementValue, PluginMessage, PopupDefinition, PopupState, VisibilityRule,
};

#[derive(Default)]
struct ExoMonadPlugin {
    status_state: String,
    status_message: String,
    active_popup: Option<(String, PopupDefinition, PopupState)>,
    selected_index: usize,
    // Used for navigation within a component (e.g. Multiselect options)
    sub_index: usize,
}

register_plugin!(ExoMonadPlugin);

impl ZellijPlugin for ExoMonadPlugin {
    fn load(&mut self, _configuration: BTreeMap<String, String>) {
        subscribe(&[EventType::CustomMessage, EventType::Key]);
        self.status_state = "IDLE".to_string();
        self.status_message = "Ready.".to_string();
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
                                should_render = true;
                            }
                            PluginMessage::ClosePopup => {
                                self.active_popup = None;
                                should_render = true;
                            }
                        },
                        Err(e) => {
                            self.status_state = "ERROR".to_string();
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
                                            should_render = true;
                                            // Don't fall through to navigation
                                            return true;
                                        }
                                    }
                                } else if let BareKey::Backspace = key.bare_key {
                                    if let Some(ElementValue::Text(s)) = state.values.get_mut(id) {
                                        s.pop();
                                        should_render = true;
                                        return true;
                                    }
                                }
                            }
                            Component::Slider { id, min, max, .. } => {
                                if let BareKey::Left = key.bare_key {
                                    if let Some(ElementValue::Number(v)) = state.values.get_mut(id)
                                    {
                                        *v = (*v - 1.0).max(*min);
                                        should_render = true;
                                        return true;
                                    }
                                } else if let BareKey::Right = key.bare_key {
                                    if let Some(ElementValue::Number(v)) = state.values.get_mut(id)
                                    {
                                        *v = (*v + 1.0).min(*max);
                                        should_render = true;
                                        return true;
                                    }
                                }
                            }
                            Component::Multiselect { id, options, .. } => {
                                if let BareKey::Left = key.bare_key {
                                    if self.sub_index > 0 {
                                        self.sub_index -= 1;
                                        should_render = true;
                                        return true;
                                    }
                                } else if let BareKey::Right = key.bare_key {
                                    if self.sub_index < options.len().saturating_sub(1) {
                                        self.sub_index += 1;
                                        should_render = true;
                                        return true;
                                    }
                                } else if let BareKey::Char(' ') = key.bare_key {
                                    if let Some(ElementValue::MultiChoice(vec)) =
                                        state.values.get_mut(id)
                                    {
                                        if let Some(val) = vec.get_mut(self.sub_index) {
                                            *val = !*val;
                                            should_render = true;
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
                            let _ = run_command(
                                &["exomonad-sidecar", "reply", "--id", req_id, "--cancel"],
                                BTreeMap::new(),
                            );
                            self.active_popup = None;
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
                                    let _ = run_command(
                                        &[
                                            "exomonad-sidecar",
                                            "reply",
                                            "--id",
                                            req_id,
                                            "--payload",
                                            &json_values,
                                        ],
                                        BTreeMap::new(),
                                    );

                                    self.active_popup = None;
                                    should_render = true;
                                }
                                Err(e) => {
                                    self.status_state = "ERROR".to_string();
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

    fn render(&mut self, _rows: usize, _cols: usize) {
        println!(
            "ExoMonad Status: [{}] {}",
            self.status_state, self.status_message
        );

        if let Some((_, def, state)) = &self.active_popup {
            println!("\n--- POPUP: {} ---", def.title);
            for (i, comp) in def.components.iter().enumerate() {
                if let Some(rule) = comp.visible_when() {
                    if !evaluate_visibility(rule, state) {
                        continue;
                    }
                }

                let is_selected = i == self.selected_index;
                let prefix = if is_selected { "> " } else { "  " };

                match comp {
                    Component::Text { content, .. } => println!("{}{}", prefix, content),
                    Component::Checkbox { id, label, .. } => {
                        let checked = match state.values.get(id) {
                            Some(ElementValue::Boolean(true)) => "[x]",
                            _ => "[ ]",
                        };
                        println!("{}{}: {}", prefix, checked, label);
                    }
                    Component::Choice {
                        id, label, options, ..
                    } => {
                        let idx = match state.values.get(id) {
                            Some(ElementValue::Choice(i)) => *i,
                            _ => 0,
                        };
                        let val = options.get(idx).map(|s| s.as_str()).unwrap_or("?");
                        println!(
                            "{}{} <{}>
",
                            prefix, label, val
                        );
                    }
                    Component::Slider {
                        label,
                        id,
                        min,
                        max,
                        ..
                    } => {
                        let val = state.get_number(id).unwrap_or(*min);
                        println!("{}{} [{:.1}] ({:.1}-{:.1})", prefix, label, val, min, max);
                    }
                    Component::Textbox { label, id, placeholder, .. } => {
                        let txt = state.get_text(id).unwrap_or("");
                        let display_txt = if is_selected {
                            format!("{}|", txt)
                        } else if txt.is_empty() {
                            if let Some(p) = placeholder {
                                format!("({})", p)
                            } else {
                                "".to_string()
                            }
                        } else {
                            txt.to_string()
                        };
                        println!(
                            "{}{} [{}]",
                            prefix, label, display_txt
                        );
                    }
                    Component::Multiselect {
                        label, id, options, ..
                    } => {
                        println!("{}{}:", prefix, label);
                        if let Some(ElementValue::MultiChoice(vals)) = state.values.get(id) {
                            for (j, opt) in options.iter().enumerate() {
                                let is_opt_selected = is_selected && j == self.sub_index;
                                let sub_prefix = if is_opt_selected { "  >> " } else { "     " };
                                let checked = if *vals.get(j).unwrap_or(&false) {
                                    "[x]"
                                } else {
                                    "[ ]"
                                };
                                println!("{}{}{}", sub_prefix, checked, opt);
                            }
                        }
                    }
                    Component::Group { label, .. } => {
                        println!("\n{}--- {} ---", prefix, label);
                    }
                }
            }
            println!("-------------------");
            println!("(Ctrl+S to Submit, Esc to Cancel)");
        }
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

trait KeyModifierExt {
    fn has_modifiers(&self, modifiers: &[KeyModifier]) -> bool;
}

impl KeyModifierExt for KeyWithModifier {
    fn has_modifiers(&self, modifiers: &[KeyModifier]) -> bool {
        modifiers.iter().all(|m| self.key_modifiers.contains(m))
    }
}
