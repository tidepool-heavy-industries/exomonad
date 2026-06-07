//! Minimal config read for `exo init` (node-mode root bootstrap).
//!
//! Classic `exomonad` owns the full `Config` (17 fields, role/wasm/companions/…). The node-mode
//! root bootstrap needs exactly two of them — `tmux_session` (the session name to attach) and
//! `model` (the root agent's `--model` flag) — so this reads only those, with the same file
//! precedence (local over global) and the same session-name sanitization classic uses. It walks up
//! from CWD for `.exo/config.toml` / `.exo/config.local.toml`; everything else falls back to a
//! default so a config-less project still boots.

use serde::Deserialize;
use std::path::{Path, PathBuf};

/// The two config fields the node-mode root bootstrap reads. (`serde(default)` on the raw form
/// ignores every other field, so this stays in sync with classic config files without naming them.)
#[derive(Deserialize, Default)]
struct RawInit {
    tmux_session: Option<String>,
    model: Option<String>,
}

/// Resolved node-mode init config.
pub struct InitConfig {
    pub tmux_session: String,
    pub model: Option<String>,
}

/// Discover the node-mode init config by merging `.exo/config.local.toml` over `.exo/config.toml`,
/// searching upward from CWD. `tmux_session`: local > global > project-dir name, sanitized.
pub fn discover() -> InitConfig {
    let project_root = find_project_root();

    let local = load_raw(&project_root.join(".exo/config.local.toml"));
    let global = load_raw(&project_root.join(".exo/config.toml"));

    let tmux_session = local
        .tmux_session
        .or(global.tmux_session)
        .unwrap_or_else(|| {
            project_root
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("exomonad")
                .to_string()
        });
    let tmux_session = sanitize_session_name(tmux_session);

    let model = local.model.or(global.model);

    InitConfig {
        tmux_session,
        model,
    }
}

fn load_raw(path: &Path) -> RawInit {
    std::fs::read_to_string(path)
        .ok()
        .and_then(|c| toml::from_str(&c).ok())
        .unwrap_or_default()
}

/// Walk up from CWD to the project root containing `.exo/config.toml` (or any `.exo/`); fall back
/// to CWD. Mirrors classic config discovery.
fn find_project_root() -> PathBuf {
    let start = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let mut current = start.as_path();
    loop {
        if current.join(".exo/config.toml").exists() || current.join(".exo").is_dir() {
            return current.to_path_buf();
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => return start,
        }
    }
}

/// Sanitize a tmux session name: replace `.` with `_` (dots break tmux targets), max 36 chars.
fn sanitize_session_name(name: String) -> String {
    name.replace('.', "_").chars().take(36).collect()
}
