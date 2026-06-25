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
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// A per-role launch profile from config — the convenient, file-based way to set a role's
/// proxy-backed brain (e.g. the reviewer → Kimi via `claude-code-proxy`) instead of exporting
/// `EXO_<ROLE>_*` env vars by hand. Each table key is a role name (e.g. `reviewer`); `discover`
/// translates `[launch_profile.<role>]` → `EXO_<ROLE_UPPER>_{BASE_URL,MODEL,AUTH_TOKEN,LABEL}`,
/// which `exo init` embeds in the root launch and the tree propagates + resolves as usual. A real
/// API key can still be kept out of the file by setting that one env var in the shell (env wins).
#[derive(Deserialize, Default)]
struct LaunchProfileRaw {
    base_url: Option<String>,
    model: Option<String>,
    auth_token: Option<String>,
    label: Option<String>,
}

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
    /// `[launch_profile.<role>]` tables, keyed by role name.
    launch_profile: Option<BTreeMap<String, LaunchProfileRaw>>,
}

/// Resolved node-mode init config.
pub struct InitConfig {
    pub tmux_session: String,
    pub model: Option<String>,
    /// Child-launch policy for the root node's papers (inherited down the tree). Defaulted to the
    /// behavior-preserving [`NodePapers`] defaults when unset in config.
    pub yolo: bool,
    pub wrap_nix: bool,
    /// Launch-profile env vars (`EXO_<ROLE>_*`) derived from `[launch_profile.<role>]`, ready for
    /// `exo init` to embed in the root launch. Empty ⇒ no profiled roles. A matching shell env var
    /// overrides a config value (so a secret key needn't live in the file).
    pub profile_env: Vec<(String, String)>,
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

    // Merge the per-role profile tables (a role defined in local fully overrides the same role in
    // global), then flatten to `EXO_<ROLE_UPPER>_<FIELD>` env vars.
    let mut profiles = global.launch_profile.unwrap_or_default();
    profiles.extend(local.launch_profile.unwrap_or_default());
    let profile_env = flatten_profiles(profiles);

    InitConfig {
        tmux_session,
        model,
        yolo,
        wrap_nix,
        profile_env,
    }
}

/// Flatten `[launch_profile.<role>]` tables to `EXO_<ROLE_UPPER>_<FIELD>` env-var pairs (only
/// present fields). The `<ROLE_UPPER>` prefix matches each role's
/// `RoleKind::launch_profile_env_prefix` (e.g. `reviewer` → `EXO_REVIEWER`).
fn flatten_profiles(profiles: BTreeMap<String, LaunchProfileRaw>) -> Vec<(String, String)> {
    profiles
        .into_iter()
        .flat_map(|(role, p)| {
            let prefix = format!("EXO_{}", role.to_uppercase());
            [
                ("BASE_URL", p.base_url),
                ("MODEL", p.model),
                ("AUTH_TOKEN", p.auth_token),
                ("LABEL", p.label),
            ]
            .into_iter()
            .filter_map(move |(field, val)| val.map(|v| (format!("{prefix}_{field}"), v)))
        })
        .collect()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reviewer_profile_flattens_to_exo_reviewer_env() {
        // The shape a user would write for the Kimi-via-proxy reviewer.
        let toml = r#"
            [launch_profile.reviewer]
            base_url = "http://localhost:18765"
            model = "kimi-for-coding"
            auth_token = "unused"
            label = "kimi"
        "#;
        let raw: RawInit = toml::from_str(toml).unwrap();
        let mut env = flatten_profiles(raw.launch_profile.unwrap());
        env.sort();
        assert_eq!(
            env,
            vec![
                ("EXO_REVIEWER_AUTH_TOKEN".to_string(), "unused".to_string()),
                (
                    "EXO_REVIEWER_BASE_URL".to_string(),
                    "http://localhost:18765".to_string()
                ),
                ("EXO_REVIEWER_LABEL".to_string(), "kimi".to_string()),
                (
                    "EXO_REVIEWER_MODEL".to_string(),
                    "kimi-for-coding".to_string()
                ),
            ]
        );
    }

    #[test]
    fn partial_profile_emits_only_present_fields() {
        let toml = r#"
            [launch_profile.worker]
            model = "kimi-for-coding"
        "#;
        let raw: RawInit = toml::from_str(toml).unwrap();
        let env = flatten_profiles(raw.launch_profile.unwrap());
        // role name uppercased into the prefix; only the set field is emitted.
        assert_eq!(
            env,
            vec![(
                "EXO_WORKER_MODEL".to_string(),
                "kimi-for-coding".to_string()
            )]
        );
    }

    #[test]
    fn no_profile_table_is_empty() {
        let raw: RawInit = toml::from_str("tmux_session = \"x\"").unwrap();
        assert!(flatten_profiles(raw.launch_profile.unwrap_or_default()).is_empty());
    }
}
