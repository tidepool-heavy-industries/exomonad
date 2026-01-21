use tuirealm::command::{Cmd, CmdResult};
use tuirealm::props::{AttrValue, Attribute, Props};
use tuirealm::ratatui::layout::Rect;
use tuirealm::ratatui::style::{Color, Style};
use tuirealm::ratatui::text::{Line, Span};
use tuirealm::ratatui::widgets::{Block, Borders, Paragraph};
use tuirealm::{Frame, MockComponent, State, StateValue};

/// Text input component
/// Shows: "Label: [current text...]"
pub struct TextboxComponent {
    label: String,
    text: String,
    placeholder: Option<String>,
    rows: u32,
    cursor_pos: usize,
    props: Props,
}

impl TextboxComponent {
    pub fn new(label: &str, text: &str, placeholder: Option<&str>, rows: Option<u32>) -> Self {
        Self {
            label: label.to_string(),
            text: text.to_string(),
            placeholder: placeholder.map(|s| s.to_string()),
            rows: rows.unwrap_or(1),
            cursor_pos: text.len(),
            props: Props::default(),
        }
    }
    
    pub fn get_text(&self) -> &str {
        &self.text
    }
    
    pub fn set_text(&mut self, text: &str) {
        self.text = text.to_string();
        self.cursor_pos = text.len();
    }
    
    fn insert_char(&mut self, c: char) -> CmdResult {
        self.text.insert(self.cursor_pos, c);
        self.cursor_pos += 1;
        CmdResult::Changed(self.state())
    }
    
    fn delete_char(&mut self) -> CmdResult {
        if self.cursor_pos > 0 {
            self.cursor_pos -= 1;
            self.text.remove(self.cursor_pos);
            CmdResult::Changed(self.state())
        } else {
            CmdResult::None
        }
    }
}

impl MockComponent for TextboxComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect) {
        // Use terminal theme colors
        
        // Check if this component has focus
        let has_focus = self.props.get(Attribute::Focus)
            .map(|v| matches!(v, AttrValue::Flag(true)))
            .unwrap_or(false);
        
        let is_placeholder = self.text.is_empty() && self.placeholder.is_some();
        let display_text = if self.text.is_empty() {
            self.placeholder.as_deref().unwrap_or("")
        } else {
            &self.text
        };
        
        // Build the text with cursor
        let mut spans = vec![];
        
        // Label - Solarized-inspired
        let label_style = if has_focus {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::Blue)
        };
        spans.push(Span::styled(format!("{}: [", self.label.to_uppercase()), label_style));
        
        // Text content with cursor
        if display_text.is_empty() && has_focus {
            // Show cursor as inverted space when empty
            spans.push(Span::styled(" ", Style::default().bg(Color::White).fg(Color::Black)));
        } else {
            let text_style = if is_placeholder {
                Style::default().fg(Color::DarkGray)
            } else {
                Style::default().fg(Color::White)
            };
            
            for (i, ch) in display_text.chars().enumerate() {
                if has_focus && i == self.cursor_pos && !is_placeholder {
                    // Inverted colors for cursor - subtle yellow
                    spans.push(Span::styled(
                        ch.to_string(),
                        Style::default().bg(Color::Yellow).fg(Color::Black)
                    ));
                } else {
                    spans.push(Span::styled(ch.to_string(), text_style));
                }
            }
            
            // If cursor is at the end (after all text)
            if has_focus && self.cursor_pos >= display_text.len() && !is_placeholder {
                spans.push(Span::styled(" ", Style::default().bg(Color::Yellow).fg(Color::Black)));
            }
        }
        
        // Closing bracket
        spans.push(Span::styled("]", label_style));
        
        let border_style = if has_focus {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::DarkGray)
        };
        
        let paragraph = Paragraph::new(Line::from(spans))
            .block(Block::default()
                .borders(Borders::BOTTOM)
                .border_style(border_style));
        
        frame.render_widget(paragraph, area);
    }
    
    fn query(&self, attr: Attribute) -> Option<AttrValue> {
        self.props.get(attr)
    }
    
    fn attr(&mut self, attr: Attribute, value: AttrValue) {
        self.props.set(attr, value);
    }
    
    fn state(&self) -> State {
        State::One(StateValue::String(self.text.clone()))
    }
    
    fn perform(&mut self, cmd: Cmd) -> CmdResult {
        match cmd {
            Cmd::Type(c) => self.insert_char(c),
            Cmd::Cancel => self.delete_char(),
            _ => CmdResult::None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cursor_position_with_spaces() {
        let mut textbox = TextboxComponent::new("Name", "", Some("Enter name"), None);
        
        // Type "Hello World" and verify cursor position
        textbox.perform(Cmd::Type('H'));
        textbox.perform(Cmd::Type('e'));
        textbox.perform(Cmd::Type('l'));
        textbox.perform(Cmd::Type('l'));
        textbox.perform(Cmd::Type('o'));
        assert_eq!(textbox.cursor_pos, 5);
        
        // Type a space - crucial test for our fix
        textbox.perform(Cmd::Type(' '));
        assert_eq!(textbox.cursor_pos, 6);
        assert_eq!(textbox.get_text(), "Hello ");
        
        textbox.perform(Cmd::Type('W'));
        textbox.perform(Cmd::Type('o'));
        textbox.perform(Cmd::Type('r'));
        textbox.perform(Cmd::Type('l'));
        textbox.perform(Cmd::Type('d'));
        assert_eq!(textbox.cursor_pos, 11);
        assert_eq!(textbox.get_text(), "Hello World");
    }
}