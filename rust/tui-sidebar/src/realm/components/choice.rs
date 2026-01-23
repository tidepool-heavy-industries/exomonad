use tuirealm::command::{Cmd, CmdResult, Direction};
use tuirealm::props::{AttrValue, Attribute, Props};
use tuirealm::ratatui::layout::Rect;
use tuirealm::ratatui::style::{Color, Style};
use tuirealm::ratatui::text::{Line, Span};
use tuirealm::ratatui::widgets::Paragraph;
use tuirealm::{Frame, MockComponent, State, StateValue};

/// Single choice/radio component
/// Shows: "Label: > Option 1  Option 2  Option 3"
pub struct ChoiceComponent {
    label: String,
    options: Vec<String>,
    selected: usize,
    props: Props,
}

impl ChoiceComponent {
    pub fn new(label: &str, options: &[String], selected: usize) -> Self {
        Self {
            label: label.to_string(),
            options: options.to_vec(),
            selected: selected.min(options.len().saturating_sub(1)),
            props: Props::default(),
        }
    }

    pub fn get_selected(&self) -> usize {
        self.selected
    }

    pub fn get_selected_text(&self) -> Option<&str> {
        self.options.get(self.selected).map(|s| s.as_str())
    }

    pub fn set_selected(&mut self, index: usize) {
        if index < self.options.len() {
            self.selected = index;
        }
    }

    fn next_option(&mut self) -> CmdResult {
        if self.selected + 1 < self.options.len() {
            self.selected += 1;
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }

    fn prev_option(&mut self) -> CmdResult {
        if self.selected > 0 {
            self.selected -= 1;
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }
}

impl MockComponent for ChoiceComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect) {
        // Terminal theme colors

        // Check if this component has focus
        let has_focus = self
            .props
            .get(Attribute::Focus)
            .map(|v| matches!(v, AttrValue::Flag(true)))
            .unwrap_or(false);

        let mut spans = vec![];

        // Label - Solarized-inspired
        let label_style = if has_focus {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::Blue)
        };
        spans.push(Span::styled(
            format!("{}: ", self.label.to_uppercase()),
            label_style,
        ));

        // Options with color coding
        for (i, option) in self.options.iter().enumerate() {
            if i > 0 {
                spans.push(Span::raw("  "));
            }

            if i == self.selected {
                // Selected option - Green highlight
                spans.push(Span::styled(
                    format!("[{}]", option),
                    Style::default().fg(Color::Black).bg(Color::Green),
                ));
            } else {
                // Unselected options
                let option_style = Style::default().fg(Color::White);
                spans.push(Span::styled(format!(" {} ", option), option_style));
            }
        }

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
        State::One(StateValue::Usize(self.selected))
    }

    fn perform(&mut self, cmd: Cmd) -> CmdResult {
        match cmd {
            Cmd::Move(Direction::Right) => self.next_option(),
            Cmd::Move(Direction::Left) => self.prev_option(),
            _ => CmdResult::None,
        }
    }
}
