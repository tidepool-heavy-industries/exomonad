use tuirealm::command::{Cmd, CmdResult};
use tuirealm::event::{Key, KeyEvent, KeyModifiers};
use tuirealm::{Component, Event, MockComponent, NoUserEvent, State};

use crate::app::{Msg};

pub struct GlobalListener;

impl MockComponent for GlobalListener {
    fn view(&mut self, _frame: &mut tuirealm::Frame, _area: tuirealm::ratatui::layout::Rect) {}

    fn query(&self, _attr: tuirealm::props::Attribute) -> Option<tuirealm::props::AttrValue> {
        None
    }

    fn attr(&mut self, _attr: tuirealm::props::Attribute, _value: tuirealm::props::AttrValue) {}

    fn state(&self) -> State {
        State::None
    }

    fn perform(&mut self, _cmd: Cmd) -> CmdResult {
        CmdResult::None
    }
}

impl Component<Msg, NoUserEvent> for GlobalListener {
    fn on(&mut self, ev: Event<NoUserEvent>) -> Option<Msg> {
        match ev {
            Event::Keyboard(KeyEvent {
                code: Key::Char('c'),
                modifiers: KeyModifiers::CONTROL,
            }) => Some(Msg::AppClose),
            Event::Keyboard(KeyEvent {
                code: Key::Char('q'), ..
            }) => Some(Msg::AppClose),
            _ => None,
        }
    }
}
