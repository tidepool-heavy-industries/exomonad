use std::collections::HashMap;

use crate::protocol::{ElementUpdate, UISpec, UIUpdate};

/// Stack of active UIs (supports nested dialogs).
pub struct UIStack {
    stack: Vec<StackEntry>,
}

/// Single entry in the UI stack.
struct StackEntry {
    spec: UISpec,
    /// Element state for dynamic updates (value, progress, etc.)
    /// Key: element ID, Value: current state
    element_state: HashMap<String, ElementState>,
}

/// Runtime state for elements (updated via UpdateUI).
#[allow(dead_code)]
#[derive(Debug, Clone)]
enum ElementState {
    Text(String),
    InputValue(String),
    Progress { value: u32, max: u32 },
    SelectOptions { options: Vec<String>, selected: Option<usize> },
}

impl UIStack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Push a new UI onto the stack.
    pub fn push(&mut self, spec: UISpec) {
        self.stack.push(StackEntry {
            spec,
            element_state: HashMap::new(),
        });
    }

    /// Pop the current UI from the stack.
    pub fn pop(&mut self) -> Option<UISpec> {
        self.stack.pop().map(|entry| entry.spec)
    }

    /// Get the current (top) UI.
    pub fn current(&self) -> Option<&UISpec> {
        self.stack.last().map(|entry| &entry.spec)
    }

    /// Check if the stack is empty.
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Update an element in the current UI.
    ///
    /// Stores the update in element_state. The renderer will apply these
    /// updates when drawing.
    ///
    /// Phase 1: Not used (no UpdateUI support yet)
    /// Phase 2/3: Will be used for progress bars, dynamic content
    #[allow(dead_code)]
    pub fn update(&mut self, update: UIUpdate) {
        if let Some(entry) = self.stack.last_mut() {
            if entry.spec.id != update.ui_id {
                // Update is for different UI (stale or invalid)
                return;
            }

            let state = match update.update {
                ElementUpdate::SetText { text } => ElementState::Text(text),
                ElementUpdate::SetValue { value } => ElementState::InputValue(value),
                ElementUpdate::SetProgress { value, max } => ElementState::Progress { value, max },
                ElementUpdate::SetOptions { options, selected } => {
                    ElementState::SelectOptions { options, selected }
                }
            };

            entry.element_state.insert(update.element_id, state);
        }
    }

    /// Get element state for rendering (if updated).
    ///
    /// Returns None if element has no runtime state (use spec defaults).
    #[allow(dead_code)]
    fn get_element_state(&self, element_id: &str) -> Option<&ElementState> {
        self.stack.last()?.element_state.get(element_id)
    }
}
