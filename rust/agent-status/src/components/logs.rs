use anyhow::Result;
use crossterm::event::{KeyCode, KeyEvent};
use ratatui::{
    layout::Rect,
    widgets::{Block, Borders, Paragraph, Wrap},
    Frame,
};
use crate::component::{Component, Action};
use crate::state::DashboardState;

pub struct LogsComponent<'a> {
    pub state: &'a DashboardState,
}

impl<'a> Component for LogsComponent<'a> {
    fn handle_key_event(&mut self, key: KeyEvent) -> Result<Action> {
        match key.code {
            KeyCode::Down | KeyCode::Char('j') => Ok(Action::SelectNext),
            KeyCode::Up | KeyCode::Char('k') => Ok(Action::SelectPrev),
             // Logs might want scrolling in the future, keeping generic for now
            _ => Ok(Action::None),
        }
    }

    fn render(&mut self, f: &mut Frame, area: Rect) {
        if self.state.agents.is_empty() {
            let text = Paragraph::new("No agents active.").block(Block::default().borders(Borders::ALL));
            f.render_widget(text, area);
            return;
        }

        let agent = match self.state.agents.get(self.state.selected_index) {
            Some(a) => a,
            None => return,
        };
        let logs = self.state.logs_cache.get(&agent.id).map(|s| s.as_str()).unwrap_or("Loading logs...");

        let p = Paragraph::new(logs)
            .block(Block::default().borders(Borders::ALL).title(format!("Logs: {}", agent.id)))
            .wrap(Wrap { trim: false }); 
        
        f.render_widget(p, area);
    }
}
