//! Minimal config read for `exo init` (node-mode root bootstrap).
//!
//! Classic `exomonad` owns the full `Config` (17 fields, role/wasm/companions/…). The node-mode
//! root bootstrap needs a handful of them — `tmux_session` (the session name to attach), `model`
//! (the root agent's `--model` flag), and the child-launch policy knobs (`yolo`, `wrap_nix`) that
//! flow down the tree via the root's papers — so this reads only those, with the same file
//! precedence (local over global) and the same session-name sanitization classic uses. It walks up
//! from CWD for `.exo/config.toml` / `.exo/config.local.toml`; everything else falls back to a
//! default so a config-less project still boots.

use anyhow::Context;
use exo_caps::NodePapers;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// A per-role launch profile from config — the convenient, file-based way to set a role's
/// proxy-backed brain (e.g. the reviewer → Kimi via `claude-code-proxy`) instead of exporting
/// `EXO_<ROLE>_*` env vars by hand. `discover` translates each profile →
/// `EXO_<ROLE_UPPER>_{BASE_URL,MODEL,AUTH_TOKEN,LABEL}`, which `exo init` embeds in the root launch
/// and the tree propagates + resolves as usual. A real API key can still be kept out of the file by
/// setting that one env var in the shell (env wins); `auth_token` is optional (a local proxy holds
/// the OAuth — the runtime supplies a placeholder when none is given).
#[derive(Deserialize, Default)]
struct LaunchProfileRaw {
    base_url: Option<String>,
    model: Option<String>,
    auth_token: Option<String>,
    label: Option<String>,
}

/// A profile entry is either a **named-brain shorthand** (`reviewer = "kimi"`) resolved from the
/// built-in registry, or the **full table** (`[launch_profile.reviewer]` with explicit fields) for a
/// custom/unknown backend. Untagged: a bare string parses as `Named`, a table as `Full`.
#[derive(Deserialize)]
#[serde(untagged)]
enum LaunchProfileEntry {
    Named(String),
    Full(LaunchProfileRaw),
}

/// Expand a named-brain shorthand to its profile fields. This is the **one place a vendor is named**
/// — domain config sugar; the runtime / `exo-caps` seam stays backend-agnostic. The default
/// `base_url` is [`claude-code-proxy`](https://github.com/raine/claude-code-proxy)'s default port;
/// use the full-table form to override it. `auth_token` is `None` — the proxy holds the real OAuth.
fn named_brain(name: &str) -> Option<LaunchProfileRaw> {
    match name {
        "kimi" => Some(LaunchProfileRaw {
            base_url: Some("http://localhost:18765".into()),
            model: Some("kimi-for-coding".into()),
            auth_token: None,
            label: Some("kimi".into()),
        }),
        _ => None,
    }
}

/// The known named brains, for diagnostics (an unknown name is a loud error, never a silent
/// fall-through to the default model).
const KNOWN_BRAINS: &[&str] = &["kimi"];

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
    /// Whether `submit_branch` spawns a reviewer at all, inherited down the tree the same way as
    /// `yolo`/`wrap_nix`. Absent ⇒ [`NodePapers::DEFAULT_REVIEW_ENABLED`] (`false` — reviewers are
    /// opt-in, not a fully-cooked default).
    review_enabled: Option<bool>,
    /// `[launch_profile.<role>]` profiles, keyed by role name. Each value is a named-brain
    /// shorthand string or a full table (see [`LaunchProfileEntry`]).
    launch_profile: Option<BTreeMap<String, LaunchProfileEntry>>,
    /// Cgroup confinement: start the swarm's tmux SERVER on a dedicated socket under a wrapper
    /// (`swarm-run`), so every pane the swarm ever creates structurally inherits the slice (panes
    /// are forked by the tmux SERVER, not the client). Absent ⇒ `false` (today's behavior).
    confine: Option<bool>,
    /// The wrapper command that launches its argv inside the cgroup slice. Absent ⇒ `"swarm-run"`.
    confine_wrapper: Option<String>,
    /// The dedicated tmux socket name the confined server listens on. Absent ⇒ derived from the
    /// resolved `tmux_session` (`"exo-{tmux_session}"`, lowercased).
    confine_socket: Option<String>,
}

/// Resolved node-mode init config.
pub struct InitConfig {
    pub tmux_session: String,
    pub model: Option<String>,
    /// Child-launch policy for the root node's papers (inherited down the tree). Defaulted to the
    /// behavior-preserving [`NodePapers`] defaults when unset in config.
    pub yolo: bool,
    pub wrap_nix: bool,
    /// Whether `submit_branch` spawns reviewers, for the root's papers (inherited down the tree).
    /// Defaults to `false` (opt-in) when unset in config.
    pub review_enabled: bool,
    /// Launch-profile env vars (`EXO_<ROLE>_*`) derived from `[launch_profile.<role>]`, ready for
    /// `exo init` to embed in the root launch. Empty ⇒ no profiled roles. A matching shell env var
    /// overrides a config value (so a secret key needn't live in the file).
    pub profile_env: Vec<(String, String)>,
    /// Whether `exo init` should confine the swarm's tmux server to a cgroup slice. Defaults to
    /// `false` when unset in config.
    pub confine: bool,
    /// The wrapper command used to launch the tmux server inside the slice. Defaults to
    /// `"swarm-run"` when unset in config.
    pub confine_wrapper: String,
    /// The dedicated tmux socket name for the confined server. Defaults to
    /// `"exo-{tmux_session}"` (lowercased) when unset in config.
    pub confine_socket: String,
}

/// Discover the node-mode init config by merging `.exo/config.local.toml` over `.exo/config.toml`,
/// searching upward from CWD. `tmux_session`: local > global > project-dir name, sanitized.
///
/// A missing config file defaults silently (a config-less project still boots); a config file that
/// exists but fails to read or parse is a loud error — a typo'd `.exo/config.toml` must not silently
/// drop `review_enabled`/`yolo`/`wrap_nix` by masquerading as "absent".
pub fn discover() -> anyhow::Result<InitConfig> {
    let project_root = find_project_root();

    let local = load_raw(&project_root.join(".exo/config.local.toml"))?;
    let global = load_raw(&project_root.join(".exo/config.toml"))?;

    // Pulled out before `local`/`global` are consumed field-by-field below.
    let confine_local = confine_of(&local);
    let confine_global = confine_of(&global);

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

    let (confine, confine_wrapper, confine_socket) =
        resolve_confine(confine_local, confine_global, &tmux_session);

    let model = local.model.or(global.model);
    let yolo = local
        .yolo
        .or(global.yolo)
        .unwrap_or(NodePapers::DEFAULT_YOLO);
    let wrap_nix = local
        .wrap_nix
        .or(global.wrap_nix)
        .unwrap_or(NodePapers::DEFAULT_WRAP_NIX);
    let review_enabled = local
        .review_enabled
        .or(global.review_enabled)
        .unwrap_or(NodePapers::DEFAULT_REVIEW_ENABLED);

    // Merge the per-role profile tables (a role defined in local fully overrides the same role in
    // global), then flatten to `EXO_<ROLE_UPPER>_<FIELD>` env vars.
    let mut profiles = global.launch_profile.unwrap_or_default();
    profiles.extend(local.launch_profile.unwrap_or_default());
    let profile_env = flatten_profiles(profiles);

    Ok(InitConfig {
        tmux_session,
        model,
        yolo,
        wrap_nix,
        review_enabled,
        profile_env,
        confine,
        confine_wrapper,
        confine_socket,
    })
}

/// Resolve `confine`/`confine_wrapper`/`confine_socket`, merging local over global (matching
/// `yolo`/`wrap_nix`/`review_enabled`), with the socket defaulted from the already-resolved
/// `tmux_session` when neither file sets it.
type RawConfine = (Option<bool>, Option<String>, Option<String>);

/// Pull the three confine-related fields out of a [`RawInit`] as a [`RawConfine`] tuple, cloning
/// so the caller keeps ownership of the rest of `raw` (needed at the `discover()` call site, where
/// `local`/`global` are consumed field-by-field afterward; harmless duplication for tests).
fn confine_of(raw: &RawInit) -> RawConfine {
    (
        raw.confine,
        raw.confine_wrapper.clone(),
        raw.confine_socket.clone(),
    )
}

fn resolve_confine(
    local: RawConfine,
    global: RawConfine,
    tmux_session: &str,
) -> (bool, String, String) {
    let (l_confine, l_wrapper, l_socket) = local;
    let (g_confine, g_wrapper, g_socket) = global;
    let confine = l_confine.or(g_confine).unwrap_or(false);
    let confine_wrapper = l_wrapper
        .or(g_wrapper)
        .unwrap_or_else(|| "swarm-run".to_string());
    let confine_socket = l_socket
        .or(g_socket)
        .unwrap_or_else(|| format!("exo-{tmux_session}").to_lowercase());
    (confine, confine_wrapper, confine_socket)
}

/// Flatten each `[launch_profile.<role>]` profile to `EXO_<ROLE_UPPER>_<FIELD>` env-var pairs (only
/// present fields). The `<ROLE_UPPER>` prefix matches each role's
/// `RoleKind::launch_profile_env_prefix` (e.g. `reviewer` → `EXO_REVIEWER`). A `Named` shorthand is
/// expanded via [`named_brain`]; an **unknown** brain name is a loud `eprintln!` at init and that
/// role's profile is skipped (it stays the default model) — never a silent fall-through.
fn flatten_profiles(profiles: BTreeMap<String, LaunchProfileEntry>) -> Vec<(String, String)> {
    let mut env = Vec::new();
    for (role, entry) in profiles {
        let resolved = match entry {
            LaunchProfileEntry::Named(name) => match named_brain(&name) {
                Some(p) => p,
                None => {
                    eprintln!(
                        "exo: unknown launch-profile brain {name:?} for role {role:?}; known brains: \
                         {KNOWN_BRAINS:?}. This role will NOT be redirected (stays the default model)."
                    );
                    continue;
                }
            },
            LaunchProfileEntry::Full(raw) => raw,
        };
        let prefix = format!("EXO_{}", role.to_uppercase());
        for (field, val) in [
            ("BASE_URL", resolved.base_url),
            ("MODEL", resolved.model),
            ("AUTH_TOKEN", resolved.auth_token),
            ("LABEL", resolved.label),
        ] {
            if let Some(v) = val {
                env.push((format!("{prefix}_{field}"), v));
            }
        }
    }
    env
}

/// Read + parse one config file. Missing ⇒ defaults (a config-less project still boots, silently).
/// Present but unreadable (permissions, IO) or unparseable (a TOML syntax error) ⇒ loud error — this
/// must never be conflated with "no config", or a typo'd file would silently drop every field it sets.
fn load_raw(path: &Path) -> anyhow::Result<RawInit> {
    let contents = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(RawInit::default()),
        Err(e) => return Err(e).with_context(|| format!("reading config file {}", path.display())),
    };
    toml::from_str(&contents).with_context(|| format!("parsing config file {}", path.display()))
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

    #[test]
    fn named_brain_shorthand_expands_without_auth_token() {
        // The clean form: `reviewer = "kimi"` under `[launch_profile]`.
        let toml = r#"
            [launch_profile]
            reviewer = "kimi"
        "#;
        let raw: RawInit = toml::from_str(toml).unwrap();
        let mut env = flatten_profiles(raw.launch_profile.unwrap());
        env.sort();
        // No EXO_REVIEWER_AUTH_TOKEN — the proxy holds the OAuth, the runtime supplies a placeholder.
        assert_eq!(
            env,
            vec![
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
    fn shorthand_and_full_table_mix_in_one_file() {
        let toml = r#"
            [launch_profile]
            reviewer = "kimi"

            [launch_profile.worker]
            base_url = "http://localhost:9999"
            model = "custom"
        "#;
        let raw: RawInit = toml::from_str(toml).unwrap();
        let env = flatten_profiles(raw.launch_profile.unwrap());
        assert!(env.contains(&(
            "EXO_REVIEWER_MODEL".to_string(),
            "kimi-for-coding".to_string()
        )));
        assert!(env.contains(&("EXO_WORKER_MODEL".to_string(), "custom".to_string())));
    }

    #[test]
    fn confine_absent_defaults_to_false_and_defaults() {
        let raw = RawInit::default();
        let (confine, wrapper, socket) =
            resolve_confine(confine_of(&raw), confine_of(&raw), "myproj");
        assert!(!confine);
        assert_eq!(wrapper, "swarm-run");
        assert_eq!(socket, "exo-myproj");
    }

    #[test]
    fn confine_set_parses_from_toml() {
        let toml = r#"
            confine = true
            confine_wrapper = "custom-wrap"
            confine_socket = "MySocket"
        "#;
        let raw: RawInit = toml::from_str(toml).unwrap();
        let empty = RawInit::default();
        let (confine, wrapper, socket) =
            resolve_confine(confine_of(&raw), confine_of(&empty), "myproj");
        assert!(confine);
        assert_eq!(wrapper, "custom-wrap");
        assert_eq!(socket, "MySocket");
    }

    #[test]
    fn confine_local_overrides_global() {
        let global: RawInit =
            toml::from_str("confine = false\nconfine_wrapper = \"global-wrap\"").unwrap();
        let local: RawInit = toml::from_str("confine = true").unwrap();
        let (confine, wrapper, socket) =
            resolve_confine(confine_of(&local), confine_of(&global), "myproj");
        assert!(
            confine,
            "local confine=true must win over global confine=false"
        );
        assert_eq!(
            wrapper, "global-wrap",
            "wrapper unset locally should fall through to global"
        );
        assert_eq!(socket, "exo-myproj");
    }

    #[test]
    fn unknown_brain_is_skipped_not_fatal() {
        let toml = r#"
            [launch_profile]
            reviewer = "gpt9000"
        "#;
        let raw: RawInit = toml::from_str(toml).unwrap();
        // Unknown brain → no env emitted for that role (and a loud eprintln, not a panic).
        assert!(flatten_profiles(raw.launch_profile.unwrap()).is_empty());
    }
}
