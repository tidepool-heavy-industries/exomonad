use tuirealm::command::{Cmd, CmdResult, Direction};
use tuirealm::props::{AttrValue, Attribute, Props};
use tuirealm::ratatui::layout::Rect;
use tuirealm::ratatui::style::{Color, Style};
use tuirealm::ratatui::text::{Line, Span, Text};
use tuirealm::ratatui::widgets::Paragraph;
use tuirealm::{Frame, MockComponent, State, StateValue};

/// Multiple selection component
/// Shows: "Label: [X] Option1  [ ] Option2  [X] Option3"
pub struct MultiselectComponent {
    label: String,
    options: Vec<String>,
    selections: Vec<bool>,
    focused_option: usize,
    props: Props,
}

impl MultiselectComponent {
    pub fn new(label: &str, options: &[String], selections: &[bool]) -> Self {
        let mut selections_vec = selections.to_vec();
        // Ensure selections vector matches options length
        selections_vec.resize(options.len(), false);

        Self {
            label: label.to_string(),
            options: options.to_vec(),
            selections: selections_vec,
            focused_option: 0,
            props: Props::default(),
        }
    }

    pub fn get_selections(&self) -> &[bool] {
        &self.selections
    }

    pub fn get_selected_indices(&self) -> Vec<usize> {
        self.selections
            .iter()
            .enumerate()
            .filter_map(|(i, &selected)| if selected { Some(i) } else { None })
            .collect()
    }

    pub fn set_selections(&mut self, selections: &[bool]) {
        self.selections = selections.to_vec();
        self.selections.resize(self.options.len(), false);
    }

    fn toggle_current(&mut self) -> CmdResult {
        if self.focused_option < self.selections.len() {
            self.selections[self.focused_option] = !self.selections[self.focused_option];
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }

    fn next_option(&mut self) -> CmdResult {
        if self.focused_option + 1 < self.options.len() {
            self.focused_option += 1;
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }

    fn prev_option(&mut self) -> CmdResult {
        if self.focused_option > 0 {
            self.focused_option -= 1;
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }
}

impl MockComponent for MultiselectComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect) {
        // Terminal theme colors

        // Check if this component has focus
        let has_focus = self
            .props
            .get(Attribute::Focus)
            .map(|v| matches!(v, AttrValue::Flag(true)))
            .unwrap_or(false);

        // Count selected items for header
        let selected_count = self.selections.iter().filter(|&&s| s).count();

        let mut lines = vec![];

        // Header - Solarized-inspired
        let header_style = if has_focus {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::Blue)
        };

        let count_style = Style::default().fg(Color::White);

        let header_spans = vec![
            Span::styled(self.label.to_uppercase(), header_style),
            Span::raw(" "),
            Span::styled(
                format!("[{}/{}]", selected_count, self.options.len()),
                count_style,
            ),
        ];
        lines.push(Line::from(header_spans));

        // Options with colorful checkboxes
        for (i, option) in self.options.iter().enumerate() {
            let mut option_spans = vec![];

            // Prefix for focused option
            let prefix = if i == self.focused_option {
                Span::styled("> ", Style::default().fg(Color::Cyan))
            } else {
                Span::raw("  ")
            };
            option_spans.push(prefix);

            // Checkbox - Solarized-inspired
            let (checkbox, checkbox_color) = if self.selections[i] {
                ("[X]", Color::Green)
            } else {
                ("[ ]", Color::DarkGray)
            };

            let checkbox_style = Style::default().fg(checkbox_color);

            option_spans.push(Span::styled(checkbox, checkbox_style));
            option_spans.push(Span::raw(" "));

            // Option text
            let text_style = if self.selections[i] {
                Style::default().fg(Color::Green)
            } else if i == self.focused_option {
                Style::default().fg(Color::Cyan)
            } else {
                Style::default().fg(Color::White)
            };

            option_spans.push(Span::styled(option, text_style));

            lines.push(Line::from(option_spans));
        }

        let paragraph = Paragraph::new(Text::from(lines));

        frame.render_widget(paragraph, area);
    }

    fn query(&self, attr: Attribute) -> Option<AttrValue> {
        self.props.get(attr)
    }

    fn attr(&mut self, attr: Attribute, value: AttrValue) {
        self.props.set(attr, value);
    }

    fn state(&self) -> State {
        State::Vec(
            self.selections
                .iter()
                .map(|&selected| StateValue::Bool(selected))
                .collect(),
        )
    }

    fn perform(&mut self, cmd: Cmd) -> CmdResult {
        match cmd {
            Cmd::Move(Direction::Down) => self.next_option(),
            Cmd::Move(Direction::Up) => self.prev_option(),
            Cmd::Submit => self.toggle_current(),
            _ => CmdResult::None,
        }
    }
}
