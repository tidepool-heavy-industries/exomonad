use std::sync::{Arc, RwLock};
use tuirealm::{
    Component, Event, MockComponent, State,
    command::{Cmd, CmdResult},
    event::{Key, KeyEvent},
    props::{AttrValue, Attribute},
    ratatui::{
        Frame,
        layout::Rect,
        widgets::{Block, Borders, Paragraph, Wrap},
    },
};

use crate::app::{Msg, UserEvent};
use crate::state::DashboardState;

pub struct LogsComponent {
    pub state: Arc<RwLock<DashboardState>>,
}

impl LogsComponent {
    pub fn new(state: Arc<RwLock<DashboardState>>) -> Self {
        Self { state }
    }
}

impl MockComponent for LogsComponent {
    fn view(&mut self, f: &mut Frame, area: Rect) {
        let s = self.state.read().unwrap();
        if s.agents.is_empty() {
            let text =
                Paragraph::new("No agents active.").block(Block::default().borders(Borders::ALL));
            f.render_widget(text, area);
            return;
        }

        let agent = match s.agents.get(s.selected_index) {
            Some(a) => a,
            None => return,
        };

        let logs = s
            .logs_cache
            .get(&agent.id)
            .map(|s| s.as_str())
            .unwrap_or("Loading logs...");

        let p = Paragraph::new(logs)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title(format!("Logs: {}", agent.id)),
            )
            .wrap(Wrap { trim: false });

        f.render_widget(p, area);
    }

    fn query(&self, _attr: Attribute) -> Option<AttrValue> {
        None
    }

    fn attr(&mut self, _attr: Attribute, _value: AttrValue) {}

    fn state(&self) -> State {
        State::None
    }

    fn perform(&mut self, _cmd: Cmd) -> CmdResult {
        CmdResult::None
    }
}

impl Component<Msg, UserEvent> for LogsComponent {
    fn on(&mut self, ev: Event<UserEvent>) -> Option<Msg> {
        match ev {
            Event::User(UserEvent::Tick) => Some(Msg::None),
            Event::Keyboard(KeyEvent { code, .. }) => match code {
                Key::Down | Key::Char('j') => Some(Msg::SelectNext),
                Key::Up | Key::Char('k') => Some(Msg::SelectPrev),
                _ => None,
            },
            _ => None,
        }
    }
}
