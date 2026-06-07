//! Minimal config read for `exo init` (node-mode root bootstrap).
//!
//! Classic `exomonad` owns the full `Config` (17 fields, role/wasm/companions/…). The node-mode
//! root bootstrap needs a handful of them — `tmux_session` (the session name to attach), `model`
//! (the root agent's `--model` flag), and the child-launch policy knobs (`yolo`, `wrap_nix`) that
//! flow down the tree via the root's papers — so this reads only those, with the same file
//! precedence (local over global) and the same session-name sanitization classic uses. It walks up
//! from CWD for `.exo/config.toml` / `.exo/config.local.toml`; everything else falls back to a
//! default so a config-less project still boots.

use exo_caps::NodePapers;
use serde::Deserialize;
use std::path::{Path, PathBuf};

/// The config fields the node-mode root bootstrap reads. (`serde(default)` on the raw form
/// ignores every other field, so this stays in sync with classic config files without naming them.)
#[derive(Deserialize, Default)]
struct RawInit {
    tmux_session: Option<String>,
    model: Option<String>,
    /// Child-launch policy stamped onto the root's papers and inherited down the whole tree
    /// (`own_launch_policy`). Absent ⇒ the behavior-preserving [`NodePapers`] defaults.
    yolo: Option<bool>,
    wrap_nix: Option<bool>,
}

/// Resolved node-mode init config.
pub struct InitConfig {
    pub tmux_session: String,
    pub model: Option<String>,
    /// Child-launch policy for the root node's papers (inherited down the tree). Defaulted to the
    /// behavior-preserving [`NodePapers`] defaults when unset in config.
    pub yolo: bool,
    pub wrap_nix: bool,
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
    let yolo = local
        .yolo
        .or(global.yolo)
        .unwrap_or(NodePapers::DEFAULT_YOLO);
    let wrap_nix = local
        .wrap_nix
        .or(global.wrap_nix)
        .unwrap_or(NodePapers::DEFAULT_WRAP_NIX);

    InitConfig {
        tmux_session,
        model,
        yolo,
        wrap_nix,
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
