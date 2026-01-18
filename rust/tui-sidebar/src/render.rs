use ratatui::{
    layout::{Constraint, Direction, Layout as RatatuiLayout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Gauge, Paragraph},
    Frame,
};

use crate::protocol::{Element, Layout, UISpec};

/// Render the UI specification to the terminal.
///
/// Phase 1: Renders Text, Button, Input, Progress (Vertical layout only for simplicity)
/// Phase 2: Full recursive layout support (Horizontal, Split)
pub fn render_ui(frame: &mut Frame, spec: &UISpec, focus_idx: usize) {
    let area = frame.area();

    // Render layout recursively
    render_layout(frame, &spec.layout, area, focus_idx);
}

/// Render a layout node recursively.
fn render_layout(frame: &mut Frame, layout: &Layout, area: Rect, focus_idx: usize) {
    match layout {
        Layout::Vertical { elements } => {
            render_vertical(frame, elements, area, focus_idx);
        }
        Layout::Horizontal { elements } => {
            render_horizontal(frame, elements, area, focus_idx);
        }
        Layout::Split { left, right, ratio } => {
            render_split(frame, left, right, *ratio, area, focus_idx);
        }
    }
}

/// Render vertical stack of elements.
fn render_vertical(frame: &mut Frame, elements: &[Element], area: Rect, focus_idx: usize) {
    if elements.is_empty() {
        return;
    }

    // Calculate constraints (equal height for now, Phase 2: smart sizing)
    let constraints: Vec<Constraint> = elements
        .iter()
        .map(|_| Constraint::Length(3))
        .collect();

    let chunks = RatatuiLayout::default()
        .direction(Direction::Vertical)
        .constraints(constraints)
        .split(area);

    let mut focusable_idx = 0;
    for (idx, element) in elements.iter().enumerate() {
        if idx < chunks.len() {
            let is_focused = if is_focusable(element) {
                let focused = focusable_idx == focus_idx;
                focusable_idx += 1;
                focused
            } else {
                false
            };
            render_element(frame, element, chunks[idx], is_focused);
        }
    }
}

/// Render horizontal row of elements.
fn render_horizontal(frame: &mut Frame, elements: &[Element], area: Rect, focus_idx: usize) {
    if elements.is_empty() {
        return;
    }

    let constraints: Vec<Constraint> = elements
        .iter()
        .map(|_| Constraint::Percentage(100 / elements.len() as u16))
        .collect();

    let chunks = RatatuiLayout::default()
        .direction(Direction::Horizontal)
        .constraints(constraints)
        .split(area);

    let mut focusable_idx = 0;
    for (idx, element) in elements.iter().enumerate() {
        if idx < chunks.len() {
            let is_focused = if is_focusable(element) {
                let focused = focusable_idx == focus_idx;
                focusable_idx += 1;
                focused
            } else {
                false
            };
            render_element(frame, element, chunks[idx], is_focused);
        }
    }
}

/// Render split layout (left/right with ratio).
fn render_split(
    frame: &mut Frame,
    left: &Layout,
    right: &Layout,
    ratio: u8,
    area: Rect,
    focus_idx: usize,
) {
    let chunks = RatatuiLayout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(ratio as u16),
            Constraint::Percentage(100 - ratio as u16),
        ])
        .split(area);

    // Calculate how many focusable elements are in the left layout
    let left_focusable_count = count_focusable(left);

    // Render left side with original focus_idx
    render_layout(frame, left, chunks[0], focus_idx);

    // Render right side with offset focus_idx
    // If focus is in the right side, adjust the index
    if focus_idx >= left_focusable_count {
        render_layout(frame, right, chunks[1], focus_idx - left_focusable_count);
    } else {
        // Focus is in left side, right side gets impossible index (won't match)
        render_layout(frame, right, chunks[1], usize::MAX);
    }
}

/// Check if element is focusable.
fn is_focusable(element: &Element) -> bool {
    matches!(element, Element::Button { .. } | Element::Input { .. })
}

/// Count focusable elements in a layout.
fn count_focusable(layout: &Layout) -> usize {
    match layout {
        Layout::Vertical { elements } | Layout::Horizontal { elements } => {
            elements.iter().filter(|e| is_focusable(e)).count()
        }
        Layout::Split { left, right, .. } => {
            count_focusable(left) + count_focusable(right)
        }
    }
}

/// Render a single element.
fn render_element(frame: &mut Frame, element: &Element, area: Rect, is_focused: bool) {
    match element {
        Element::Text { content, .. } => {
            render_text(frame, content, area);
        }
        Element::Button { label, .. } => {
            render_button(frame, label, area, is_focused);
        }
        Element::Input { label, value, .. } => {
            render_input(frame, label, value, area, is_focused);
        }
        Element::Progress { label, value, max, .. } => {
            render_progress(frame, label, *value, *max, area);
        }
        // Phase 2: Table, Select, List
        _ => {
            render_placeholder(frame, "Unsupported element", area);
        }
    }
}

/// Render static text.
fn render_text(frame: &mut Frame, content: &str, area: Rect) {
    let paragraph = Paragraph::new(content)
        .block(Block::default().borders(Borders::NONE));

    frame.render_widget(paragraph, area);
}

/// Render button (focusable).
fn render_button(frame: &mut Frame, label: &str, area: Rect, is_focused: bool) {
    let style = if is_focused {
        Style::default()
            .fg(Color::Black)
            .bg(Color::Green)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default().fg(Color::White)
    };

    let button_text = format!("[ {} ]", label);
    let paragraph = Paragraph::new(button_text)
        .style(style)
        .block(Block::default().borders(Borders::ALL));

    frame.render_widget(paragraph, area);
}

/// Render input field (focusable).
///
/// Phase 1: Display only (no character editing)
/// Phase 3: Add cursor position, character accumulation
fn render_input(frame: &mut Frame, label: &str, value: &str, area: Rect, is_focused: bool) {
    let style = if is_focused {
        Style::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default()
    };

    let lines = vec![
        Line::from(label),
        Line::from(Span::styled(
            format!("> {}", value),
            style,
        )),
    ];

    let paragraph = Paragraph::new(lines)
        .block(Block::default().borders(Borders::ALL));

    frame.render_widget(paragraph, area);
}

/// Render progress bar.
fn render_progress(frame: &mut Frame, label: &str, value: u32, max: u32, area: Rect) {
    let ratio = if max > 0 {
        (value as f64 / max as f64).clamp(0.0, 1.0)
    } else {
        0.0
    };

    let gauge = Gauge::default()
        .block(Block::default().borders(Borders::ALL).title(label))
        .gauge_style(Style::default().fg(Color::Blue))
        .ratio(ratio);

    frame.render_widget(gauge, area);
}

/// Render placeholder for unsupported elements.
fn render_placeholder(frame: &mut Frame, msg: &str, area: Rect) {
    let paragraph = Paragraph::new(msg)
        .style(Style::default().fg(Color::Red))
        .block(Block::default().borders(Borders::ALL));

    frame.render_widget(paragraph, area);
}

/// Collect all focusable elements from layout.
///
/// Phase 1: Returns element IDs in render order.
/// Used by input handler to cycle focus.
pub fn collect_focusable_ids(layout: &Layout) -> Vec<String> {
    match layout {
        Layout::Vertical { elements } => collect_element_ids(elements),
        Layout::Horizontal { elements } => collect_element_ids(elements),
        Layout::Split { left, right, .. } => {
            let mut ids = collect_focusable_ids(left);
            ids.extend(collect_focusable_ids(right));
            ids
        }
    }
}

fn collect_element_ids(elements: &[Element]) -> Vec<String> {
    elements
        .iter()
        .filter_map(|e| match e {
            Element::Button { id, .. } => Some(id.clone()),
            Element::Input { id, .. } => Some(id.clone()),
            _ => None,  // Text, Progress not focusable
        })
        .collect()
}
