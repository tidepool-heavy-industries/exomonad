use tuirealm::command::{Cmd, CmdResult, Direction};
use tuirealm::props::{AttrValue, Attribute, Props};
use tuirealm::ratatui::layout::Rect;
use tuirealm::ratatui::style::{Color, Style};
use tuirealm::ratatui::text::{Line, Span};
use tuirealm::ratatui::widgets::{Block, Borders, Paragraph};
use tuirealm::{Frame, MockComponent, State, StateValue};

/// Slider component using text representation
/// Shows: "Label: [████████░░] 75/100"
pub struct SliderComponent {
    label: String,
    min: f32,
    max: f32,
    value: f32,
    props: Props,
}

impl SliderComponent {
    pub fn new(label: &str, min: f32, max: f32, value: f32) -> Self {
        Self {
            label: label.to_string(),
            min,
            max,
            value: value.clamp(min, max),
            props: Props::default(),
        }
    }

    pub fn get_value(&self) -> f32 {
        self.value
    }

    pub fn set_value(&mut self, value: f32) {
        self.value = value.clamp(self.min, self.max);
    }

    fn increment(&mut self) -> CmdResult {
        let step = (self.max - self.min) / 20.0; // 20 steps
        let new_value = (self.value + step).clamp(self.min, self.max);
        if new_value != self.value {
            self.value = new_value;
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }

    fn decrement(&mut self) -> CmdResult {
        let step = (self.max - self.min) / 20.0; // 20 steps
        let new_value = (self.value - step).clamp(self.min, self.max);
        if new_value != self.value {
            self.value = new_value;
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }
}

impl MockComponent for SliderComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect) {
        // Use terminal theme colors for better visibility

        // Check if this component has focus
        let has_focus = self
            .props
            .get(Attribute::Focus)
            .map(|v| matches!(v, AttrValue::Flag(true)))
            .unwrap_or(false);

        // Create gradient effect for the bar
        let bar_width = 20;
        let progress = (self.value - self.min) / (self.max - self.min);
        let filled = (progress * bar_width as f32) as usize;

        let mut spans = vec![];

        // Label - Solarized-inspired
        let label_style = if has_focus {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::Blue)
        };
        spans.push(Span::styled(
            format!("{}: [", self.label.to_uppercase()),
            label_style,
        ));

        // Render bar - BRUTAL BLOCKS
        for i in 0..bar_width {
            if i < filled {
                // Filled blocks in cyan
                spans.push(Span::styled("█", Style::default().fg(Color::Cyan)));
            } else {
                // Empty blocks darker
                spans.push(Span::styled("░", Style::default().fg(Color::DarkGray)));
            }
        }

        spans.push(Span::styled("] ", label_style));

        // Value display - subtle
        let value_style = Style::default().fg(Color::White);
        spans.push(Span::styled(
            format!("[{}]", self.value as i32),
            value_style,
        ));

        let paragraph =
            Paragraph::new(Line::from(spans)).block(Block::default().borders(Borders::NONE));

        frame.render_widget(paragraph, area);
    }

    fn query(&self, attr: Attribute) -> Option<AttrValue> {
        self.props.get(attr)
    }

    fn attr(&mut self, attr: Attribute, value: AttrValue) {
        self.props.set(attr, value);
    }

    fn state(&self) -> State {
        State::One(StateValue::F64(self.value as f64))
    }

    fn perform(&mut self, cmd: Cmd) -> CmdResult {
        match cmd {
            Cmd::Move(Direction::Right) => self.increment(),
            Cmd::Move(Direction::Left) => self.decrement(),
            _ => CmdResult::None,
        }
    }
}
