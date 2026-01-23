use tuirealm::command::{Cmd, CmdResult};
use tuirealm::props::{AttrValue, Attribute, Props};
use tuirealm::ratatui::layout::Rect;
use tuirealm::ratatui::style::{Color, Style};
use tuirealm::ratatui::text::{Line, Span};
use tuirealm::ratatui::widgets::Paragraph;
use tuirealm::{Frame, MockComponent, State, StateValue};

/// Checkbox component
/// Shows: "[X] Label" or "[ ] Label"
pub struct CheckboxComponent {
    label: String,
    checked: bool,
    props: Props,
}

impl CheckboxComponent {
    pub fn new(label: &str, checked: bool) -> Self {
        Self {
            label: label.to_string(),
            checked,
            props: Props::default(),
        }
    }

    pub fn is_checked(&self) -> bool {
        self.checked
    }

    pub fn set_checked(&mut self, checked: bool) {
        self.checked = checked;
    }

    fn toggle(&mut self) -> CmdResult {
        self.checked = !self.checked;
        CmdResult::Changed(self.state())
    }
}

impl MockComponent for CheckboxComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect) {
        // Terminal theme colors

        // Check if this component has focus
        let has_focus = self
            .props
            .get(Attribute::Focus)
            .map(|v| matches!(v, AttrValue::Flag(true)))
            .unwrap_or(false);

        let mut spans = vec![];

        // Checkbox - Solarized-inspired
        let (checkbox_icon, icon_color) = if self.checked {
            ("[X]", Color::Green) // Green when checked
        } else {
            ("[ ]", Color::DarkGray) // Gray when unchecked
        };

        spans.push(Span::styled(checkbox_icon, Style::default().fg(icon_color)));

        spans.push(Span::raw(" "));

        // Label - Solarized-inspired
        let label_style = if has_focus {
            Style::default().fg(Color::Cyan)
        } else if self.checked {
            Style::default().fg(Color::Green)
        } else {
            Style::default().fg(Color::Blue)
        };

        spans.push(Span::styled(self.label.to_uppercase(), label_style));

        let paragraph = Paragraph::new(Line::from(spans));

        frame.render_widget(paragraph, area);
    }

    fn query(&self, attr: Attribute) -> Option<AttrValue> {
        self.props.get(attr)
    }

    fn attr(&mut self, attr: Attribute, value: AttrValue) {
        self.props.set(attr, value);
    }

    fn state(&self) -> State {
        State::One(StateValue::Bool(self.checked))
    }

    fn perform(&mut self, cmd: Cmd) -> CmdResult {
        match cmd {
            Cmd::Submit => self.toggle(),
            _ => CmdResult::None,
        }
    }
}
