use tuirealm::command::{Cmd, CmdResult};
use tuirealm::props::{AttrValue, Attribute, Props};
use tuirealm::ratatui::layout::Rect;
use tuirealm::ratatui::style::{Color, Modifier, Style};
use tuirealm::ratatui::text::{Line, Span};
use tuirealm::ratatui::widgets::Paragraph;
use tuirealm::{Frame, MockComponent, State, StateValue};

/// Simple text display component
pub struct TextComponent {
    content: String,
    props: Props,
}

impl TextComponent {
    pub fn new(content: &str) -> Self {
        Self {
            content: content.to_string(),
            props: Props::default(),
        }
    }
}

impl MockComponent for TextComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect) {
        // Terminal theme colors
        
        // Style based on content type - Solarized-inspired
        let style = if self.content.starts_with("---") && self.content.ends_with("---") {
            // Group headers
            Style::default()
                .fg(Color::Blue)
                .add_modifier(Modifier::UNDERLINED)
        } else if self.content.contains('?') {
            // Questions - subtle cyan
            Style::default()
                .fg(Color::Cyan)
        } else if self.content.contains('!') {
            // Important text - yellow alert
            Style::default()
                .fg(Color::Yellow)
        } else {
            // Regular text
            Style::default().fg(Color::White)
        };
        
        // Transform group headers - clean and simple
        let display_text = if self.content.starts_with("---") && self.content.ends_with("---") {
            let label = self.content
                .trim_start_matches('-')
                .trim_end_matches('-')
                .trim();
            format!("── {} ──", label.to_uppercase())
        } else {
            self.content.clone()
        };
        
        let paragraph = Paragraph::new(Line::from(vec![
            Span::styled(display_text, style)
        ]))
        .alignment(tuirealm::ratatui::layout::Alignment::Left);
        
        frame.render_widget(paragraph, area);
    }
    
    fn query(&self, attr: Attribute) -> Option<AttrValue> {
        self.props.get(attr)
    }
    
    fn attr(&mut self, attr: Attribute, value: AttrValue) {
        self.props.set(attr, value);
    }
    
    fn state(&self) -> State {
        State::One(StateValue::String(self.content.clone()))
    }
    
    fn perform(&mut self, _cmd: Cmd) -> CmdResult {
        // Text components don't handle commands
        CmdResult::None
    }
}