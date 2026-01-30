use anyhow::Result;
use crossterm::event::{KeyEvent, MouseEvent};
use ratatui::{Frame, layout::Rect};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Action {
    None,
    Render,
    Quit,
    SelectNext,
    SelectPrev,
    KillAgent,
}

pub trait Component {
    fn handle_key_event(&mut self, _key: KeyEvent) -> Result<Action> {
        Ok(Action::None)
    }

    #[allow(dead_code)]
    fn handle_mouse_event(&mut self, _mouse: MouseEvent) -> Result<Action> {
        Ok(Action::None)
    }

    fn render(&mut self, f: &mut Frame, area: Rect);
}
