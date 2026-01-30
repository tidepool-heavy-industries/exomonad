use std::time::Instant;
use tuirealm::command::{Cmd, CmdResult, Direction};
use tuirealm::props::{BorderType, Props};
use tuirealm::ratatui::layout::{Constraint, Layout as RatatuiLayout, Rect};
use tuirealm::ratatui::widgets::{Block, Clear};
use tuirealm::{Frame, MockComponent, State, StateValue};

use crate::protocol::{
    Component, ElementValue, PopupDefinition, PopupResult, PopupState, VisibilityRule,
};

pub mod builder;
pub mod components;

/// Component type enum for identifying widget types
#[derive(Debug, Clone, PartialEq)]
pub enum ComponentType {
    Text,
    Slider,
    Checkbox,
    Textbox,
    Choice,
    Multiselect,
    Group,
}

/// Main TUI component that renders a popup definition
pub struct PopupComponent {
    definition: PopupDefinition,
    state: PopupState,
    focused_component: usize,
    components: Vec<ComponentWrapper>,
    props: Props,
    component_areas: Vec<Rect>, // Track render areas for mouse click detection
    visibility_rules: Vec<Option<VisibilityRule>>, // Per-component visibility rules
    start_time: Instant,        // Track when popup was created for time tracking
}

/// Wrapper for individual components with their metadata
pub struct ComponentWrapper {
    pub label: String,
    pub component: Box<dyn MockComponent>,
    pub component_type: ComponentType,
    pub focusable: bool,
    pub min_height: u16,
    pub is_multiselect: bool,
}

impl PopupComponent {
    pub fn new(definition: PopupDefinition) -> Self {
        let state = PopupState::new(&definition);
        let (components, _id_to_index, visibility_rules) =
            builder::build_components(&definition, &state);
        let component_areas = Vec::with_capacity(components.len());

        // Find first focusable and visible component
        let mut popup_component = Self {
            definition,
            state,
            focused_component: 0,
            components,
            props: Props::default(),
            component_areas,
            visibility_rules,
            start_time: Instant::now(), // Track popup creation time
        };

        // Set initial focus to first focusable visible component
        popup_component.focused_component = popup_component.find_first_focusable_visible();
        popup_component
    }

    /// Get the filtered form result (only visible, interactive components)
    pub fn get_filtered_result(&self) -> PopupResult {
        let mut result_values = serde_json::Map::new();

        for (index, component) in self.definition.components.iter().enumerate() {
            // Skip non-interactive components (Text and Group are display-only)
            if matches!(component, Component::Text { .. } | Component::Group { .. }) {
                continue;
            }

            // Skip hidden components based on visibility rules
            if !self.is_component_visible(index) {
                continue;
            }

            let id = component.id();

            // Only include visible, interactive components
            if let Some(value) = self.state.values.get(id) {
                let json_value = match (value, component) {
                    (ElementValue::Number(n), _) => serde_json::json!(n),
                    (ElementValue::Boolean(b), _) => serde_json::json!(b),
                    (ElementValue::Text(s), _) => serde_json::json!(s),
                    (ElementValue::Choice(idx), Component::Choice { options, .. }) => {
                        if let Some(selected) = options.get(*idx) {
                            serde_json::json!(selected)
                        } else {
                            serde_json::json!(null)
                        }
                    }
                    (
                        ElementValue::MultiChoice(selections),
                        Component::Multiselect { options, .. },
                    ) => {
                        let selected: Vec<&String> = options
                            .iter()
                            .enumerate()
                            .filter(|(i, _)| selections.get(*i).copied().unwrap_or(false))
                            .map(|(_, option)| option)
                            .collect();
                        serde_json::json!(selected)
                    }
                    _ => serde_json::json!(null),
                };
                result_values.insert(id.to_string(), json_value);
            }
        }

        PopupResult {
            button: self
                .state
                .button_clicked
                .clone()
                .unwrap_or_else(|| "decline".to_string()),
            values: serde_json::Value::Object(result_values),
            time_spent_seconds: Some(self.start_time.elapsed().as_secs_f64()),
        }
    }

    /// Check if the currently focused component is a textbox
    pub fn is_focused_textbox(&self) -> bool {
        self.components
            .get(self.focused_component)
            .map(|wrapper| wrapper.component_type == ComponentType::Textbox)
            .unwrap_or(false)
    }

    /// Check if a component is currently visible based on its visibility rule
    pub fn is_component_visible(&self, index: usize) -> bool {
        if let Some(Some(rule)) = self.visibility_rules.get(index) {
            self.evaluate_visibility_rule(rule)
        } else {
            true // No rule means always visible
        }
    }

    /// Evaluate a visibility rule against current state
    fn evaluate_visibility_rule(&self, rule: &VisibilityRule) -> bool {
        match rule {
            VisibilityRule::Checked(id) => self.state.get_boolean(id).unwrap_or(false),
            VisibilityRule::Equals(conditions) => conditions.iter().all(|(id, expected_value)| {
                if let Some(choice_index) = self.state.get_choice(id) {
                    if let Some(component) =
                        self.definition.components.iter().find(|c| c.id() == id)
                    {
                        if let Component::Choice { options, .. } = component {
                            return options
                                .get(choice_index)
                                .map(|actual| actual == expected_value)
                                .unwrap_or(false);
                        }
                    }
                }
                false
            }),
            VisibilityRule::GreaterThan { id, min_value } => {
                self.state.get_number(id).unwrap_or(0.0) >= *min_value
            }
            VisibilityRule::LessThan { id, max_value } => {
                self.state.get_number(id).unwrap_or(0.0) <= *max_value
            }
            VisibilityRule::CountEquals { id, exact_count } => {
                self.state
                    .get_multichoice(id)
                    .map(|s| s.iter().filter(|&&b| b).count() as u32)
                    .unwrap_or(0)
                    == *exact_count
            }
            VisibilityRule::CountGreaterThan { id, min_count } => {
                self.state
                    .get_multichoice(id)
                    .map(|s| s.iter().filter(|&&b| b).count() as u32)
                    .unwrap_or(0)
                    >= *min_count
            }
        }
    }

    /// Find first focusable and visible component
    fn find_first_focusable_visible(&self) -> usize {
        for (index, wrapper) in self.components.iter().enumerate() {
            if wrapper.focusable && self.is_component_visible(index) {
                return index;
            }
        }
        0 // Fallback to first component
    }

    /// Handle form submission
    pub fn submit(&mut self) -> PopupResult {
        self.sync_state();
        self.state.button_clicked = Some("submit".to_string());
        self.get_filtered_result()
    }

    /// Handle form cancellation
    pub fn cancel(&mut self) -> PopupResult {
        self.sync_state();
        self.state.button_clicked = Some("decline".to_string());
        self.get_filtered_result()
    }

    /// Move focus to next component
    fn focus_next(&mut self) -> CmdResult {
        let start = self.focused_component;
        loop {
            self.focused_component = (self.focused_component + 1) % self.components.len();
            if self.components[self.focused_component].focusable
                && self.is_component_visible(self.focused_component)
            {
                break;
            }
            if self.focused_component == start {
                break; // Avoid infinite loop if no focusable components
            }
        }
        CmdResult::Changed(self.state())
    }

    /// Move focus to previous component
    fn focus_prev(&mut self) -> CmdResult {
        let start = self.focused_component;
        loop {
            self.focused_component = if self.focused_component == 0 {
                self.components.len() - 1
            } else {
                self.focused_component - 1
            };
            if self.components[self.focused_component].focusable
                && self.is_component_visible(self.focused_component)
            {
                break;
            }
            if self.focused_component == start {
                break; // Avoid infinite loop if no focusable components
            }
        }
        CmdResult::Changed(self.state())
    }

    /// Update state from component values
    fn sync_state(&mut self) {
        for wrapper in &self.components {
            let state_value = wrapper.component.state();

            match state_value {
                State::One(StateValue::F64(value)) => {
                    // Slider component
                    self.state.set_number(&wrapper.label, value as f32);
                }
                State::One(StateValue::Bool(value)) => {
                    // Checkbox component
                    self.state.set_boolean(&wrapper.label, value);
                }
                State::One(StateValue::String(value)) => {
                    // Text input
                    self.state.set_text(&wrapper.label, value);
                }
                State::One(StateValue::Usize(value)) => {
                    // Choice component
                    self.state.set_choice(&wrapper.label, value);
                }
                State::Vec(values) => {
                    // Multiselect component
                    let selections: Vec<bool> = values
                        .iter()
                        .filter_map(|v| match v {
                            StateValue::Bool(b) => Some(*b),
                            _ => None,
                        })
                        .collect();
                    self.state.set_multichoice(&wrapper.label, selections);
                }
                _ => {}
            }
        }
    }

    /// Handle mouse click at the given relative coordinates
    pub fn handle_click(&mut self, x: u16, y: u16) {
        // Find which component was clicked
        if let Some(index) = self.get_component_at_position(x, y) {
            // Only handle clicks on visible components
            if !self.is_component_visible(index) {
                return;
            }

            // If clicking on a different component, change focus
            if index != self.focused_component && self.components[index].focusable {
                self.focused_component = index;
            }

            // Handle click action for the component
            if let Some(wrapper) = self.components.get_mut(index) {
                match wrapper.component_type {
                    ComponentType::Checkbox => {
                        // Toggle checkbox on click
                        wrapper.component.perform(Cmd::Submit);
                        self.sync_state();
                    }
                    ComponentType::Choice => {
                        // For choice, cycle through options
                        wrapper.component.perform(Cmd::Move(Direction::Right));
                        self.sync_state();
                    }
                    ComponentType::Multiselect => {
                        // Calculate which item in the list was clicked
                        if let Some(area) = self.component_areas.get(index) {
                            let relative_y = y.saturating_sub(area.y);
                            // If clicking within the list body (below the header), toggle current item
                            if relative_y > 0 {
                                wrapper.component.perform(Cmd::Submit);
                                self.sync_state();
                            }
                        }
                    }
                    ComponentType::Slider => {
                        // Just focus the slider - let arrow keys handle the value changes
                    }
                    _ => {}
                }
            }
        }
    }

    /// Find which component contains the given coordinates
    fn get_component_at_position(&self, x: u16, y: u16) -> Option<usize> {
        for (index, area) in self.component_areas.iter().enumerate() {
            if x >= area.x && x < area.x + area.width && y >= area.y && y < area.y + area.height {
                return Some(index);
            }
        }
        None
    }
}

impl MockComponent for PopupComponent {
    fn view(&mut self, frame: &mut Frame, area: Rect) {
        // Clear the area
        frame.render_widget(Clear, area);

        // Create bordered block for the popup
        let block = Block::default()
            .title(self.definition.title.as_str())
            .borders(tuirealm::ratatui::widgets::Borders::ALL)
            .border_type(BorderType::Rounded);

        let inner_area = block.inner(area);
        frame.render_widget(block, area);

        // Create vertical layout for visible components only
        if !self.components.is_empty() {
            // Collect visible component indices and their heights
            let visible_indices: Vec<usize> = (0..self.components.len())
                .filter(|i| self.is_component_visible(*i))
                .collect();

            if !visible_indices.is_empty() {
                let constraints: Vec<Constraint> = visible_indices
                    .iter()
                    .map(|i| Constraint::Length(self.components[*i].min_height))
                    .collect();

                let chunks = RatatuiLayout::default()
                    .direction(tuirealm::ratatui::layout::Direction::Vertical)
                    .margin(1)
                    .constraints(constraints)
                    .split(inner_area);

                // Reset component areas and resize to match all components
                self.component_areas = vec![Rect::default(); self.components.len()];

                // Store component areas for mouse detection, mapping visible chunks back to component indices
                for (chunk_index, component_index) in visible_indices.iter().enumerate() {
                    if chunk_index < chunks.len() {
                        self.component_areas[*component_index] = chunks[chunk_index];
                    }
                }

                // Render each visible component
                for (chunk_index, component_index) in visible_indices.iter().enumerate() {
                    if chunk_index < chunks.len() {
                        // Set focus state on component
                        let is_focused = *component_index == self.focused_component
                            && self.components[*component_index].focusable;
                        self.components[*component_index].component.attr(
                            tuirealm::props::Attribute::Focus,
                            tuirealm::props::AttrValue::Flag(is_focused),
                        );

                        // Render component
                        self.components[*component_index]
                            .component
                            .view(frame, chunks[chunk_index]);
                    }
                }
            }
        }
    }

    fn query(&self, attr: tuirealm::props::Attribute) -> Option<tuirealm::props::AttrValue> {
        self.props.get(attr)
    }

    fn attr(&mut self, attr: tuirealm::props::Attribute, value: tuirealm::props::AttrValue) {
        self.props.set(attr, value);
    }

    fn state(&self) -> State {
        State::One(StateValue::String(format!(
            "PopupComponent: {}",
            self.definition.title
        )))
    }

    fn perform(&mut self, cmd: Cmd) -> CmdResult {
        match cmd {
            // For Up/Down navigation, check if current component is multiselect
            // If it is, let it handle the navigation internally
            Cmd::Move(Direction::Down) | Cmd::Move(Direction::Up) => {
                if let Some(wrapper) = self.components.get(self.focused_component) {
                    // If this is a multiselect, let it handle arrow navigation
                    if wrapper.is_multiselect {
                        if let Some(wrapper) = self.components.get_mut(self.focused_component) {
                            let result = wrapper.component.perform(cmd);
                            self.sync_state();
                            if !matches!(result, CmdResult::None) {
                                return result;
                            }
                        }
                    }
                }
                // If not multiselect or it didn't handle it, do component navigation
                match cmd {
                    Cmd::Move(Direction::Down) => self.focus_next(),
                    Cmd::Move(Direction::Up) => self.focus_prev(),
                    _ => CmdResult::None,
                }
            }

            // Let focused component handle other commands
            _ => {
                if let Some(wrapper) = self.components.get_mut(self.focused_component) {
                    let result = wrapper.component.perform(cmd);
                    self.sync_state();

                    // After state changes, check if current focus is still visible
                    // If not, move to next visible focusable component
                    if !self.is_component_visible(self.focused_component) {
                        self.focused_component = self.find_first_focusable_visible();
                    }

                    result
                } else {
                    CmdResult::None
                }
            }
        }
    }
}
