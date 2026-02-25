//! Reusable web components for A2A interfaces

pub mod streaming;
pub mod task_viewer;

pub use streaming::create_sse_stream;
pub use task_viewer::{MessageView, TaskView};
