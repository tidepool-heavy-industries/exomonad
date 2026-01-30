use anyhow::Result;
use crossterm::event::{KeyCode, KeyEvent};
use ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};
use crate::component::{Component, Action};
use crate::state::DashboardState;

pub struct ControlsComponent<'a> {
    pub state: &'a DashboardState,
}

impl<'a> Component for ControlsComponent<'a> {
    fn handle_key_event(&mut self, key: KeyEvent) -> Result<Action> {
        match key.code {
            KeyCode::Char('K') | KeyCode::Char('x') => Ok(Action::KillAgent),
            _ => Ok(Action::None),
        }
    }

    fn render(&mut self, f: &mut Frame, area: Rect) {
        let text = vec![
            Line::from("Available Actions:"),
            Line::from(""),
            Line::from(vec![Span::styled("[K] / [x]", Style::default().fg(Color::Red)), Span::raw(" Stop/Kill Selected Agent")]),
            Line::from(""),
            Line::from(vec![Span::styled("Status: ", Style::default().add_modifier(Modifier::BOLD))]),
            Line::from(if self.state.connected { 
                Span::styled("Connected to Control Server", Style::default().fg(Color::Green)) 
            } else { 
                Span::styled("Disconnected (Retrying...)", Style::default().fg(Color::Red)) 
            }),
        ];
        
        let p = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title("Controls"));
        
        f.render_widget(p, area);
    }
}
