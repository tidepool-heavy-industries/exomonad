use exomonad_core::mcp::ToolDefinition;
use tuirealm::terminal::{CrosstermTerminalAdapter, TerminalBridge};
use tuirealm::{Application, NoUserEvent, Update};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Id {
    ToolList,
    Form,
    Results,
    GlobalListener,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Msg {
    None,
    ToolSelected(String),
    ExecuteTool(String, serde_json::Value),
    SwitchFocus(Id),
    AppClose,
}

pub struct Model {
    pub app: Application<Id, Msg, NoUserEvent>,
    pub quit: bool,
    pub redo_render: bool,
    pub terminal: TerminalBridge<CrosstermTerminalAdapter>,
    pub tools: Vec<ToolDefinition>,
}

impl Model {
    pub fn new(tools: Vec<ToolDefinition>) -> Self {
        let app: Application<Id, Msg, NoUserEvent> = Application::init(
            tuirealm::EventListenerCfg::default()
                .crossterm_input_listener(std::time::Duration::from_millis(10), 3),
        );

        Self {
            app,
            quit: false,
            redo_render: true,
            terminal: TerminalBridge::init(
                CrosstermTerminalAdapter::new().expect("Failed to create terminal adapter"),
            )
            .expect("Failed to create terminal bridge"),
            tools,
        }
    }
}

impl Update<Msg> for Model {
    fn update(&mut self, msg: Option<Msg>) -> Option<Msg> {
        if let Some(msg) = msg {
            self.redo_render = true;
            match msg {
                Msg::None => None,
                Msg::AppClose => {
                    self.quit = true;
                    None
                }
                _ => None,
            }
        } else {
            None
        }
    }
}
