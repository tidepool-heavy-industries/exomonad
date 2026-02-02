use tuirealm::command::{Cmd, CmdResult};
use tuirealm::event::{Key, KeyEvent};
use tuirealm::props::{Alignment, BorderType, Borders, Color, TextSpan, BorderSides};
use tuirealm::{Component, Event, MockComponent, NoUserEvent, State, StateValue};
use tui_realm_stdlib::List;

use crate::app::Msg;

pub struct ToolList {
    component: List,
    tools: Vec<String>,
}

impl ToolList {
    pub fn new(tools: Vec<String>) -> Self {
        let mut rows = Vec::new();
        for tool in &tools {
            rows.push(vec![TextSpan::from(tool.clone())]);
        }

        Self {
            component: List::default()
                .borders(Borders::default().sides(BorderSides::ALL).modifiers(BorderType::Rounded))
                .title("Tools", Alignment::Center)
                .highlighted_color(Color::Yellow)
                .highlighted_str("▶ ")
                .rows(rows),
            tools,
        }
    }
}

impl MockComponent for ToolList {
    fn view(&mut self, frame: &mut tuirealm::Frame, area: tuirealm::ratatui::layout::Rect) {
        self.component.view(frame, area);
    }

    fn query(&self, attr: tuirealm::props::Attribute) -> Option<tuirealm::props::AttrValue> {
        self.component.query(attr)
    }

    fn attr(&mut self, attr: tuirealm::props::Attribute, value: tuirealm::props::AttrValue) {
        self.component.attr(attr, value);
    }

    fn state(&self) -> State {
        self.component.state()
    }

    fn perform(&mut self, cmd: Cmd) -> CmdResult {
        self.component.perform(cmd)
    }
}

impl Component<Msg, NoUserEvent> for ToolList {
    fn on(&mut self, ev: Event<NoUserEvent>) -> Option<Msg> {
        let cmd = match ev {
            Event::Keyboard(KeyEvent {
                code: Key::Down, ..
            }) => Cmd::Move(tuirealm::command::Direction::Down),
            Event::Keyboard(KeyEvent {
                code: Key::Up, ..
            }) => Cmd::Move(tuirealm::command::Direction::Up),
            Event::Keyboard(KeyEvent {
                code: Key::Enter, ..
            }) => {
                let state = self.state();
                if let State::One(StateValue::Usize(idx)) = state {
                    if let Some(tool) = self.tools.get(idx) {
                        return Some(Msg::ToolSelected(tool.clone()));
                    }
                }
                return None;
            }
            _ => return None,
        };

        match self.perform(cmd) {
            CmdResult::Changed(_) => Some(Msg::None),
            _ => None,
        }
    }
}
