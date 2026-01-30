use anyhow::Result;
use crossterm::event::{KeyCode, KeyEvent};
use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Wrap},
    Frame,
};
use crate::component::{Component, Action};
use crate::state::DashboardState;

pub struct OverviewComponent<'a> {
    pub state: &'a DashboardState,
}

impl<'a> Component for OverviewComponent<'a> {
    fn handle_key_event(&mut self, key: KeyEvent) -> Result<Action> {
        match key.code {
            KeyCode::Down | KeyCode::Char('j') => Ok(Action::SelectNext),
            KeyCode::Up | KeyCode::Char('k') => Ok(Action::SelectPrev),
            KeyCode::Char('K') | KeyCode::Char('x') => Ok(Action::KillAgent),
            _ => Ok(Action::None),
        }
    }

    fn render(&mut self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(40), Constraint::Percentage(60)].as_ref())
            .split(area);

        // Left: List of Agents
        let items: Vec<ListItem> = self.state.agents
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
        let selection = if self.state.agents.is_empty() { 
            None 
        } else { 
            Some(self.state.selected_index.min(self.state.agents.len().saturating_sub(1))) 
        };
        list_state.select(selection);

        let list = List::new(items)
            .block(Block::default().borders(Borders::ALL).title("Agents"))
            .highlight_style(Style::default().bg(Color::DarkGray).add_modifier(Modifier::BOLD));

        f.render_stateful_widget(list, chunks[0], &mut list_state);

        // Right: Details
        if self.state.agents.is_empty() {
            let text = Paragraph::new("No agents active.").block(Block::default().borders(Borders::ALL));
            f.render_widget(text, chunks[1]);
        } else if let Some(agent) = self.state.agents.get(self.state.selected_index) {
            let text = vec![
                Line::from(vec![Span::raw("ID: "), Span::styled(&agent.id, Style::default().fg(Color::Cyan))]),
                Line::from(vec![Span::raw("Container: "), Span::raw(&agent.container_id)]),
                Line::from(vec![Span::raw("Status: "), Span::raw(&agent.status)]),
                Line::from(vec![Span::raw("Started: "), Span::raw(&agent.started_at)]),
                Line::from(""),
                Line::from(vec![Span::styled("Last Action:", Style::default().add_modifier(Modifier::BOLD))]),
                Line::from(agent.last_action.clone().unwrap_or_default()),
                Line::from(""),
                Line::from(vec![Span::styled("Blocker:", Style::default().fg(Color::Red))]),
                Line::from(agent.blocker.clone().unwrap_or_default()),
            ];
            
            let details = Paragraph::new(text)
                .block(Block::default().borders(Borders::ALL).title("Details"))
                .wrap(Wrap { trim: true });
            
            f.render_widget(details, chunks[1]);
        }
    }
}
