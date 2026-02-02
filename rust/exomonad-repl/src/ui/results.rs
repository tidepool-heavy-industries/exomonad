use tui_realm_stdlib::Textarea;
use tuirealm::command::{Cmd, CmdResult};
use tuirealm::event::{Key, KeyEvent};
use tuirealm::props::{Alignment, BorderSides, BorderType, Borders, TextSpan};
use tuirealm::{Component, Event, MockComponent, NoUserEvent, State};

use crate::app::Msg;

pub struct Results {
    component: Textarea,
}

impl Default for Results {
    fn default() -> Self {
        Self {
            component: Textarea::default()
                .borders(
                    Borders::default()
                        .sides(BorderSides::ALL)
                        .modifiers(BorderType::Rounded),
                )
                .title("Results", Alignment::Center),
        }
    }
}

impl MockComponent for Results {
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

impl Component<Msg, NoUserEvent> for Results {
    fn on(&mut self, ev: Event<NoUserEvent>) -> Option<Msg> {
        let cmd = match ev {
            Event::Keyboard(KeyEvent {
                code: Key::Down, ..
            }) => Cmd::Scroll(tuirealm::command::Direction::Down),
            Event::Keyboard(KeyEvent { code: Key::Up, .. }) => {
                Cmd::Scroll(tuirealm::command::Direction::Up)
            }
            _ => return None,
        };

        match self.perform(cmd) {
            CmdResult::Changed(_) => Some(Msg::None),
            _ => None,
        }
    }
}

impl Results {
    pub fn set_text(&mut self, text: &str) {
        let mut spans = Vec::new();
        for line in text.lines() {
            spans.push(TextSpan::from(line));
        }
        let component = std::mem::take(&mut self.component);
        self.component = component.text_rows(spans);
    }
}
