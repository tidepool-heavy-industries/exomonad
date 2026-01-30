use std::sync::{Arc, RwLock};
use tuirealm::{
    Component, Event, MockComponent, State,
    command::{Cmd, CmdResult},
    event::{Key, KeyEvent},
    props::{AttrValue, Attribute, Props},
    ratatui::{
        Frame,
        layout::{Constraint, Direction, Layout, Rect},
        style::{Color, Modifier, Style},
        text::{Line, Span},
        widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Wrap},
    },
};

use crate::app::{Msg, UserEvent};
use crate::state::DashboardState;

pub struct OverviewComponent {
    pub state: Arc<RwLock<DashboardState>>,
    pub props: Props,
}

impl OverviewComponent {
    pub fn new(state: Arc<RwLock<DashboardState>>) -> Self {
        Self {
            state,
            props: Props::default(),
        }
    }
}

impl MockComponent for OverviewComponent {
    fn view(&mut self, f: &mut Frame, area: Rect) {
        let s = self.state.read().unwrap();
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(40), Constraint::Percentage(60)].as_ref())
            .split(area);

        // Left: List of Agents
        let items: Vec<ListItem> = s
            .agents
            .iter()
            .map(|agent| {
                let style = if agent.status == "running" {
                    Style::default().fg(Color::Green)
                } else {
                    Style::default().fg(Color::Red)
                };
                let content = Line::from(vec![
                    Span::styled(format!("● {}", agent.id), style),
                    Span::raw(format!(" [{}]", agent.status)),
                ]);
                ListItem::new(content)
            })
            .collect();

        let mut list_state = ListState::default();
        let selection = if s.agents.is_empty() {
            None
        } else {
            Some(s.selected_index.min(s.agents.len().saturating_sub(1)))
        };
        list_state.select(selection);

        let list = List::new(items)
            .block(Block::default().borders(Borders::ALL).title("Agents"))
            .highlight_style(
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD),
            );

        f.render_stateful_widget(list, chunks[0], &mut list_state);

        // Right: Details
        if s.agents.is_empty() {
            let text =
                Paragraph::new("No agents active.").block(Block::default().borders(Borders::ALL));
            f.render_widget(text, chunks[1]);
        } else if let Some(agent) = s.agents.get(s.selected_index) {
            let text = vec![
                Line::from(vec![
                    Span::raw("ID: "),
                    Span::styled(&agent.id, Style::default().fg(Color::Cyan)),
                ]),
                Line::from(vec![
                    Span::raw("Container: "),
                    Span::raw(&agent.container_id),
                ]),
                Line::from(vec![Span::raw("Status: "), Span::raw(&agent.status)]),
                Line::from(vec![Span::raw("Started: "), Span::raw(&agent.started_at)]),
                Line::from(""),
                Line::from(vec![Span::styled(
                    "Last Action:",
                    Style::default().add_modifier(Modifier::BOLD),
                )]),
                Line::from(agent.last_action.clone().unwrap_or_default()),
                Line::from(""),
                Line::from(vec![Span::styled(
                    "Blocker:",
                    Style::default().fg(Color::Red),
                )]),
                Line::from(agent.blocker.clone().unwrap_or_default()),
            ];

            let details = Paragraph::new(text)
                .block(Block::default().borders(Borders::ALL).title("Details"))
                .wrap(Wrap { trim: true });

            f.render_widget(details, chunks[1]);
        }
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

impl Component<Msg, UserEvent> for OverviewComponent {
    fn on(&mut self, ev: Event<UserEvent>) -> Option<Msg> {
        match ev {
            Event::User(UserEvent::Tick) => Some(Msg::None),
            Event::Keyboard(KeyEvent { code, .. }) => match code {
                Key::Down | Key::Char('j') => Some(Msg::SelectNext),
                Key::Up | Key::Char('k') => Some(Msg::SelectPrev),
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
