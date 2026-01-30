use crate::state::DashboardState;
use std::sync::{Arc, RwLock};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Id {
    Overview,
    Logs,
    Controls,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Msg {
    SelectNext,
    SelectPrev,
    KillAgent(String),
    None,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd)]
pub enum UserEvent {
    Tick,
}

pub struct Model {
    pub state: Arc<RwLock<DashboardState>>,
    pub quit: bool,
    pub redraw: bool,
    pub tab_index: usize,
}

impl Model {
    pub fn new(state: Arc<RwLock<DashboardState>>) -> Self {
        Self {
            state,
            quit: false,
            redraw: true,
            tab_index: 0,
        }
    }
}
