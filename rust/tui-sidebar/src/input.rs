use anyhow::Result;
use crossterm::event::{KeyCode, KeyEvent};
use ratatui::layout::{Constraint, Direction, Layout as RatatuiLayout, Rect};

use crate::protocol::{Element, Interaction, Layout};
use crate::ui_stack::UIStack;

/// Handle keyboard event and return interaction if completed.
///
/// Tab: Cycle focus
/// Enter: Trigger interaction (ButtonClicked, InputSubmitted)
///
/// Returns Some(Interaction) if action completed, None otherwise.
pub fn handle_key_event(
    key_event: KeyEvent,
    ui_stack: &UIStack,
    focus_idx: &mut usize,
) -> Result<Option<Interaction>> {
    let Some(spec) = ui_stack.current() else {
        return Ok(None);
    };

    match key_event.code {
        KeyCode::Tab | KeyCode::Down | KeyCode::Right => {
            // Cycle focus forward
            let focusable_count = count_focusable(&spec.layout);
            if focusable_count > 0 {
                *focus_idx = (*focus_idx + 1) % focusable_count;
            }
            Ok(None)
        }

        KeyCode::BackTab | KeyCode::Up | KeyCode::Left => {
            // Cycle focus backward
            let focusable_count = count_focusable(&spec.layout);
            if focusable_count > 0 {
                if *focus_idx == 0 {
                    *focus_idx = focusable_count - 1;
                } else {
                    *focus_idx -= 1;
                }
            }
            Ok(None)
        }

        KeyCode::Enter => {
            // Trigger interaction for focused element
            if let Some(element) = get_focusable_at_index(&spec.layout, *focus_idx) {
                let interaction = element_to_interaction(&spec.id, element);
                Ok(Some(interaction))
            } else {
                Ok(None)
            }
        }

        _ => Ok(None),
    }
}

/// Count focusable elements in layout.
fn count_focusable(layout: &Layout) -> usize {
    match layout {
        Layout::Vertical { elements } => count_focusable_elements(elements),
        Layout::Horizontal { elements } => count_focusable_elements(elements),
        Layout::Split { left, right, .. } => {
            count_focusable(left) + count_focusable(right)
        }
    }
}

fn count_focusable_elements(elements: &[Element]) -> usize {
    elements
        .iter()
        .filter(|e| is_focusable(e))
        .count()
}

/// Check if element is focusable.
fn is_focusable(element: &Element) -> bool {
    matches!(element, Element::Button { .. } | Element::Input { .. })
}

/// Get focusable element at given index (0-based).
fn get_focusable_at_index(layout: &Layout, idx: usize) -> Option<&Element> {
    let mut current_idx = 0;
    find_focusable_recursive(layout, idx, &mut current_idx)
}

fn find_focusable_recursive<'a>(
    layout: &'a Layout,
    target_idx: usize,
    current_idx: &mut usize,
) -> Option<&'a Element> {
    match layout {
        Layout::Vertical { elements } | Layout::Horizontal { elements } => {
            find_in_elements(elements, target_idx, current_idx)
        }
        Layout::Split { left, right, .. } => {
            if let Some(e) = find_focusable_recursive(left, target_idx, current_idx) {
                return Some(e);
            }
            find_focusable_recursive(right, target_idx, current_idx)
        }
    }
}

fn find_in_elements<'a>(
    elements: &'a [Element],
    target_idx: usize,
    current_idx: &mut usize,
) -> Option<&'a Element> {
    for element in elements {
        if is_focusable(element) {
            if *current_idx == target_idx {
                return Some(element);
            }
            *current_idx += 1;
        }
    }
    None
}

/// Convert element to interaction event.
fn element_to_interaction(ui_id: &str, element: &Element) -> Interaction {
    match element {
        Element::Button { id, .. } => Interaction::ButtonClicked {
            ui_id: ui_id.to_string(),
            button_id: id.clone(),
        },
        Element::Input { id, value, .. } => Interaction::InputSubmitted {
            ui_id: ui_id.to_string(),
            input_id: id.clone(),
            value: value.clone(),
        },
        _ => unreachable!("Non-focusable element should not reach here"),
    }
}

/// Handle mouse click and return interaction if a focusable element was clicked.
///
/// Returns Some((interaction, focus_idx)) if a focusable element was clicked.
pub fn handle_mouse_click(
    x: u16,
    y: u16,
    ui_stack: &UIStack,
    terminal_area: Rect,
) -> Result<Option<(Interaction, usize)>> {
    let Some(spec) = ui_stack.current() else {
        return Ok(None);
    };

    // Find which element was clicked and get both interaction and index
    if let Some((element, idx)) = find_clicked_element(&spec.layout, x, y, terminal_area) {
        let interaction = element_to_interaction(&spec.id, element);
        Ok(Some((interaction, idx)))
    } else {
        Ok(None)
    }
}

/// Find which focusable element contains the given coordinates.
/// Returns the element and its focusable index.
fn find_clicked_element(
    layout: &Layout,
    x: u16,
    y: u16,
    area: Rect,
) -> Option<(&Element, usize)> {
    let mut focusable_idx = 0;
    find_in_layout(layout, x, y, area, &mut focusable_idx)
}

fn find_in_layout<'a>(
    layout: &'a Layout,
    x: u16,
    y: u16,
    area: Rect,
    focusable_idx: &mut usize,
) -> Option<(&'a Element, usize)> {
    match layout {
        Layout::Vertical { elements } => {
            find_in_vertical(elements, x, y, area, focusable_idx)
        }
        Layout::Horizontal { elements } => {
            find_in_horizontal(elements, x, y, area, focusable_idx)
        }
        Layout::Split { left, right, ratio } => {
            find_in_split(left, right, *ratio, x, y, area, focusable_idx)
        }
    }
}

fn find_in_vertical<'a>(
    elements: &'a [Element],
    x: u16,
    y: u16,
    area: Rect,
    focusable_idx: &mut usize,
) -> Option<(&'a Element, usize)> {
    if elements.is_empty() {
        return None;
    }

    let constraints: Vec<Constraint> = elements.iter().map(|_| Constraint::Length(3)).collect();
    let chunks = RatatuiLayout::default()
        .direction(Direction::Vertical)
        .constraints(constraints)
        .split(area);

    for (idx, element) in elements.iter().enumerate() {
        if idx < chunks.len() {
            let rect = chunks[idx];
            if is_focusable(element) {
                if rect.x <= x && x < rect.x + rect.width && rect.y <= y && y < rect.y + rect.height
                {
                    return Some((element, *focusable_idx));
                }
                *focusable_idx += 1;
            }
        }
    }

    None
}

fn find_in_horizontal<'a>(
    elements: &'a [Element],
    x: u16,
    y: u16,
    area: Rect,
    focusable_idx: &mut usize,
) -> Option<(&'a Element, usize)> {
    if elements.is_empty() {
        return None;
    }

    let constraints: Vec<Constraint> = elements
        .iter()
        .map(|_| Constraint::Percentage(100 / elements.len() as u16))
        .collect();
    let chunks = RatatuiLayout::default()
        .direction(Direction::Horizontal)
        .constraints(constraints)
        .split(area);

    for (idx, element) in elements.iter().enumerate() {
        if idx < chunks.len() {
            let rect = chunks[idx];
            if is_focusable(element) {
                if rect.x <= x && x < rect.x + rect.width && rect.y <= y && y < rect.y + rect.height
                {
                    return Some((element, *focusable_idx));
                }
                *focusable_idx += 1;
            }
        }
    }

    None
}

fn find_in_split<'a>(
    left: &'a Layout,
    right: &'a Layout,
    ratio: u8,
    x: u16,
    y: u16,
    area: Rect,
    focusable_idx: &mut usize,
) -> Option<(&'a Element, usize)> {
    let chunks = RatatuiLayout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(ratio as u16),
            Constraint::Percentage(100 - ratio as u16),
        ])
        .split(area);

    // Try left side first
    if let Some(result) = find_in_layout(left, x, y, chunks[0], focusable_idx) {
        return Some(result);
    }

    // Try right side
    find_in_layout(right, x, y, chunks[1], focusable_idx)
}
