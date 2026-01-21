use serde::{Deserialize, Serialize};

// ══════════════════════════════════════════════════════════════
// MESSAGES FROM HASKELL TO RUST
// ══════════════════════════════════════════════════════════════

/// Messages sent from Haskell tui-interpreter to Rust tui-sidebar.
///
/// Note: There is no PopUI message. The Rust TUI sidebar pops its UI stack
/// automatically after sending an Interaction. For multi-step UIs, Haskell
/// just sends another PushUI - the previous UI was already popped.
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum TUIMessage {
    PushUI { spec: UISpec },
    ReplaceUI { spec: UISpec },
    UpdateUI { update: UIUpdate },
}

// ══════════════════════════════════════════════════════════════
// UI SPECIFICATION
// ══════════════════════════════════════════════════════════════

/// Complete UI specification sent to the TUI sidebar.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UISpec {
    pub id: String,
    pub layout: Layout,
}

/// Recursive layout structure for UI elements.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Layout {
    Vertical { elements: Vec<Element> },
    Horizontal { elements: Vec<Element> },
    Split {
        left: Box<Layout>,
        right: Box<Layout>,
        ratio: u8,  // 0-100
    },
}

/// UI elements (widgets).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Element {
    Text {
        id: String,
        content: String,
    },
    Input {
        id: String,
        label: String,
        value: String,
    },
    Button {
        id: String,
        label: String,
    },
    Table {
        id: String,
        columns: Vec<String>,
        rows: Vec<Vec<String>>,
    },
    Select {
        id: String,
        label: String,
        options: Vec<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        selected: Option<usize>,
    },
    List {
        id: String,
        items: Vec<String>,
    },
    Progress {
        id: String,
        label: String,
        value: u32,
        max: u32,
    },
}

// ══════════════════════════════════════════════════════════════
// DYNAMIC UPDATES
// ══════════════════════════════════════════════════════════════

/// Update message for modifying existing UI elements.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UIUpdate {
    pub ui_id: String,
    pub element_id: String,
    pub update: ElementUpdate,
}

/// Specific update operations for elements.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ElementUpdate {
    SetText {
        text: String,
    },
    SetValue {
        value: String,
    },
    SetProgress {
        value: u32,
        max: u32,
    },
    SetOptions {
        options: Vec<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        selected: Option<usize>,
    },
}

// ══════════════════════════════════════════════════════════════
// USER INTERACTIONS (RUST TO HASKELL)
// ══════════════════════════════════════════════════════════════

/// User interaction events from the TUI sidebar.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Interaction {
    ButtonClicked {
        ui_id: String,
        button_id: String,
    },
    InputSubmitted {
        ui_id: String,
        input_id: String,
        value: String,
    },
    InputChanged {
        ui_id: String,
        input_id: String,
        value: String,
    },
    TableRowSelected {
        ui_id: String,
        table_id: String,
        row: usize,
    },
    SelectionChanged {
        ui_id: String,
        select_id: String,
        index: usize,
    },
}
