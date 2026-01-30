use std::sync::{Arc, RwLock};
use tuirealm::{
    Component, Event, MockComponent, State,
    command::{Cmd, CmdResult},
    event::{Key, KeyEvent},
    props::{AttrValue, Attribute},
    ratatui::{
        Frame,
        layout::Rect,
        style::{Color, Modifier, Style},
        text::{Line, Span},
        widgets::{Block, Borders, Paragraph},
    },
};

use crate::app::{Msg, UserEvent};
use crate::state::DashboardState;

pub struct ControlsComponent {
    pub state: Arc<RwLock<DashboardState>>,
}

impl ControlsComponent {
    pub fn new(state: Arc<RwLock<DashboardState>>) -> Self {
        Self { state }
    }
}

impl MockComponent for ControlsComponent {
    fn view(&mut self, f: &mut Frame, area: Rect) {
        let s = self.state.read().unwrap();
        let text = vec![
            Line::from("Available Actions:"),
            Line::from(""),
            Line::from(vec![
                Span::styled("[K] / [x]", Style::default().fg(Color::Red)),
                Span::raw(" Stop/Kill Selected Agent"),
            ]),
            Line::from(""),
            Line::from(vec![Span::styled(
                "Status: ",
                Style::default().add_modifier(Modifier::BOLD),
            )]),
            Line::from(if s.connected {
                Span::styled(
                    "Connected to Control Server",
                    Style::default().fg(Color::Green),
                )
            } else {
                Span::styled(
                    "Disconnected (Retrying...)",
                    Style::default().fg(Color::Red),
                )
            }),
        ];

        let p =
            Paragraph::new(text).block(Block::default().borders(Borders::ALL).title("Controls"));

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

impl Component<Msg, UserEvent> for ControlsComponent {
    fn on(&mut self, ev: Event<UserEvent>) -> Option<Msg> {
        match ev {
            Event::User(UserEvent::Tick) => Some(Msg::None),
            Event::Keyboard(KeyEvent { code, .. }) => match code {
                Key::Char('K') | Key::Char('x') => {
                    let s = self.state.read().unwrap();
                    if let Some(agent) = s.agents.get(s.selected_index) {
                        Some(Msg::KillAgent(agent.id.clone()))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }
}
