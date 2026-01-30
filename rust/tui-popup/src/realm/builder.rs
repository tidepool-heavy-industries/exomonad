use super::{ComponentType, ComponentWrapper};
use crate::protocol::{Component, PopupDefinition, PopupState, VisibilityRule};
use crate::realm::components::*;

use std::collections::HashMap;
use tuirealm::MockComponent;

/// Build component tree from popup definition
/// Returns (components, id_to_index_map, visibility_rules)
pub fn build_components(
    definition: &PopupDefinition,
    state: &PopupState,
) -> (
    Vec<ComponentWrapper>,
    HashMap<String, usize>,
    Vec<Option<VisibilityRule>>,
) {
    let mut components = Vec::new();
    let mut id_to_index = HashMap::new();
    let mut visibility_rules = Vec::new();

    for (index, component) in definition.components.iter().enumerate() {
        id_to_index.insert(component.id().to_string(), index);
        visibility_rules.push(component.visible_when().cloned());

        let wrapper = build_component(component, state);
        components.push(wrapper);
    }

    (components, id_to_index, visibility_rules)
}

/// Build a single component from its definition
fn build_component(component: &Component, state: &PopupState) -> ComponentWrapper {
    match component {
        Component::Text { id, content, .. } => ComponentWrapper {
            label: id.clone(),
            component: create_text_component(content),
            component_type: ComponentType::Text,
            focusable: false,
            min_height: 1,
            is_multiselect: false,
        },

        Component::Slider {
            id,
            label,
            min,
            max,
            default,
            ..
        } => {
            let value = state.get_number(id).unwrap_or(*default);
            ComponentWrapper {
                label: id.clone(),
                component: create_slider_component(label, *min, *max, value),
                component_type: ComponentType::Slider,
                focusable: true,
                min_height: 2,
                is_multiselect: false,
            }
        }

        Component::Checkbox {
            id, label, default, ..
        } => {
            let checked = state.get_boolean(id).unwrap_or(*default);
            ComponentWrapper {
                label: id.clone(),
                component: create_checkbox_component(label, checked),
                component_type: ComponentType::Checkbox,
                focusable: true,
                min_height: 2,
                is_multiselect: false,
            }
        }

        Component::Textbox {
            id,
            label,
            placeholder,
            rows,
            ..
        } => {
            let text = state.get_text(id).unwrap_or("").to_string();
            let height = rows.unwrap_or(1) as u16 + 1; // +1 for label
            ComponentWrapper {
                label: id.clone(),
                component: create_textbox_component(label, &text, placeholder.as_deref(), *rows),
                component_type: ComponentType::Textbox,
                focusable: true,
                min_height: height,
                is_multiselect: false,
            }
        }

        Component::Choice {
            id,
            label,
            options,
            default,
            ..
        } => {
            let selected = state.get_choice(id).unwrap_or(default.unwrap_or(0));
            ComponentWrapper {
                label: id.clone(),
                component: create_choice_component(label, options, selected),
                component_type: ComponentType::Choice,
                focusable: true,
                min_height: 2,
                is_multiselect: false,
            }
        }

        Component::Multiselect {
            id, label, options, ..
        } => {
            let selections = state
                .get_multichoice(id)
                .map(|s| s.to_vec())
                .unwrap_or_else(|| vec![false; options.len()]);
            let height = (options.len() as u16 + 1).max(3); // +1 for label, minimum 3
            ComponentWrapper {
                label: id.clone(),
                component: create_multiselect_component(label, options, &selections),
                component_type: ComponentType::Multiselect,
                focusable: true,
                min_height: height,
                is_multiselect: true,
            }
        }

        Component::Group { id, label, .. } => ComponentWrapper {
            label: id.clone(),
            component: create_group_label(label),
            component_type: ComponentType::Group,
            focusable: false,
            min_height: 2,
            is_multiselect: false,
        },
    }
}

// Component creation functions (to be implemented in components/ modules)
fn create_text_component(content: &str) -> Box<dyn MockComponent> {
    Box::new(text::TextComponent::new(content))
}

fn create_slider_component(label: &str, min: f32, max: f32, value: f32) -> Box<dyn MockComponent> {
    Box::new(slider::SliderComponent::new(label, min, max, value))
}

fn create_checkbox_component(label: &str, checked: bool) -> Box<dyn MockComponent> {
    Box::new(checkbox::CheckboxComponent::new(label, checked))
}

fn create_textbox_component(
    label: &str,
    text: &str,
    placeholder: Option<&str>,
    rows: Option<u32>,
) -> Box<dyn MockComponent> {
    Box::new(textbox::TextboxComponent::new(
        label,
        text,
        placeholder,
        rows,
    ))
}

fn create_choice_component(
    label: &str,
    options: &[String],
    selected: usize,
) -> Box<dyn MockComponent> {
    Box::new(choice::ChoiceComponent::new(label, options, selected))
}

fn create_multiselect_component(
    label: &str,
    options: &[String],
    selections: &[bool],
) -> Box<dyn MockComponent> {
    Box::new(multiselect::MultiselectComponent::new(
        label, options, selections,
    ))
}

fn create_group_label(label: &str) -> Box<dyn MockComponent> {
    Box::new(text::TextComponent::new(&format!("--- {} ---", label)))
}
