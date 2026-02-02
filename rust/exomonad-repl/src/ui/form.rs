use tuirealm::command::{Cmd, CmdResult};
use tuirealm::event::{Key, KeyEvent};
use tuirealm::props::{Alignment, BorderType, Borders, Color, BorderSides};
use tuirealm::{Component, Event, MockComponent, NoUserEvent, State};
use tui_realm_stdlib::Input;

use crate::app::Msg;
use crate::schema_parser::{ToolSchema, PropertyKind};

pub struct Form {
    title: String,
    description: String,
    schema: Option<ToolSchema>,
    // We'll store values here for now
    values: std::collections::HashMap<String, String>,
    focused_idx: usize,
}

impl Default for Form {
    fn default() -> Self {
        Self {
            title: "No tool selected".to_string(),
            description: "".to_string(),
            schema: None,
            values: std::collections::HashMap::new(),
            focused_idx: 0,
        }
    }
}

impl Form {
    pub fn set_tool(&mut self, tool: &ToolDefinition) {
        self.title = tool.name.clone();
        self.description = tool.description.clone();
        self.schema = Some(ToolSchema::from_json(&tool.input_schema));
        self.values.clear();
        self.focused_idx = 0;
    }

    pub fn get_values(&self) -> serde_json::Value {
        let mut map = serde_json::Map::new();
        if let Some(schema) = &self.schema {
            for prop in &schema.properties {
                if let Some(val) = self.values.get(&prop.name) {
                    // Try to parse based on kind
                    let json_val = match &prop.kind {
                        PropertyKind::Boolean { .. } => {
                            serde_json::Value::Bool(val.to_lowercase() == "true")
                        }
                        PropertyKind::Number { .. } => {
                            val.parse::<f64>().map(|n| serde_json::json!(n)).unwrap_or(serde_json::Value::String(val.clone()))
                        }
                        PropertyKind::Array { .. } => {
                            // Split by comma for now
                            let items: Vec<serde_json::Value> = val.split(',')
                                .map(|s| serde_json::Value::String(s.trim().to_string()))
                                .collect();
                            serde_json::Value::Array(items)
                        }
                        _ => serde_json::Value::String(val.clone()),
                    };
                    map.insert(prop.name.clone(), json_val);
                }
            }
        }
        serde_json::Value::Object(map)
    }

    pub fn get_current_tool(&self) -> String {
        self.title.clone()
    }
}

use exomonad_sidecar::mcp::ToolDefinition;

impl MockComponent for Form {
    fn view(&mut self, frame: &mut tuirealm::Frame, area: tuirealm::ratatui::layout::Rect) {
        use tuirealm::ratatui::layout::{Constraint, Direction, Layout};
        use tuirealm::ratatui::widgets::{Block, Borders as TuiBorders, Paragraph as TuiParagraph};

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3), // Title
                Constraint::Length(2), // Description
                Constraint::Min(0),    // Fields
            ])
            .split(area);

        frame.render_widget(
            TuiParagraph::new(self.title.clone())
                .block(Block::default().borders(TuiBorders::ALL).title("Tool")),
            chunks[0],
        );

        frame.render_widget(
            TuiParagraph::new(self.description.clone()),
            chunks[1],
        );

        if let Some(schema) = &self.schema {
            let field_constraints: Vec<Constraint> = schema.properties.iter()
                .map(|_| Constraint::Length(3))
                .collect();
            
            let field_chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints(field_constraints)
                .split(chunks[2]);

            for (i, prop) in schema.properties.iter().enumerate() {
                if i >= field_chunks.len() { break; }
                
                let is_focused = i == self.focused_idx;
                let border_color = if is_focused { Color::Yellow } else { Color::White };
                
                let value = self.values.get(&prop.name).cloned().unwrap_or_default();
                
                let label = if prop.required {
                    format!("{} *", prop.name)
                } else {
                    prop.name.clone()
                };

                let mut input = Input::default()
                    .borders(Borders::default().color(border_color).sides(BorderSides::ALL).modifiers(BorderType::Rounded))
                    .title(label, Alignment::Left)
                    .value(value);
                
                input.view(frame, field_chunks[i]);
            }
        }
    }

    fn query(&self, _attr: tuirealm::props::Attribute) -> Option<tuirealm::props::AttrValue> {
        None
    }

    fn attr(&mut self, _attr: tuirealm::props::Attribute, _value: tuirealm::props::AttrValue) {
    }

    fn state(&self) -> State {
        State::None
    }

    fn perform(&mut self, _cmd: Cmd) -> CmdResult {
        CmdResult::None
    }
}

impl Component<Msg, NoUserEvent> for Form {
    fn on(&mut self, ev: Event<NoUserEvent>) -> Option<Msg> {
        match ev {
            Event::Keyboard(KeyEvent { code: Key::Tab, .. }) => {
                if let Some(schema) = &self.schema {
                    self.focused_idx = (self.focused_idx + 1) % schema.properties.len();
                }
                Some(Msg::None)
            }
            Event::Keyboard(KeyEvent { code: Key::BackTab, .. }) => {
                if let Some(schema) = &self.schema {
                    if self.focused_idx == 0 {
                        self.focused_idx = schema.properties.len() - 1;
                    } else {
                        self.focused_idx -= 1;
                    }
                }
                Some(Msg::None)
            }
            Event::Keyboard(KeyEvent { code: Key::Char(c), .. }) => {
                if let Some(schema) = &self.schema {
                    if let Some(prop) = schema.properties.get(self.focused_idx) {
                        let val = self.values.entry(prop.name.clone()).or_insert(String::new());
                        val.push(c);
                    }
                }
                Some(Msg::None)
            }
            Event::Keyboard(KeyEvent { code: Key::Backspace, .. }) => {
                if let Some(schema) = &self.schema {
                    if let Some(prop) = schema.properties.get(self.focused_idx) {
                        if let Some(val) = self.values.get_mut(&prop.name) {
                            val.pop();
                        }
                    }
                }
                Some(Msg::None)
            }
            Event::Keyboard(KeyEvent { code: Key::Enter, .. }) => {
                Some(Msg::ExecuteTool(self.get_current_tool(), self.get_values()))
            }
            _ => None,
        }
    }
}