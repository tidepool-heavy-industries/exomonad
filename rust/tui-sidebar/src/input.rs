use anyhow::Result;
use crossterm::event::{KeyCode, KeyEvent};

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
        KeyCode::Tab => {
            // Cycle focus
            let focusable_count = count_focusable(&spec.layout);
            if focusable_count > 0 {
                *focus_idx = (*focus_idx + 1) % focusable_count;
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
