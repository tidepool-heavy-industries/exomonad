//! Secret loading from ~/.exomonad/secrets.
//!
//! Secrets are stored in a simple KEY=value format (like .env files).
//! Single canonical location: ~/.exomonad/secrets

use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

/// Loaded secrets from ~/.exomonad/secrets file.
#[derive(Debug, Clone, Default)]
pub struct Secrets {
    values: HashMap<String, String>,
}

impl Secrets {
    /// Load secrets from ~/.exomonad/secrets.
    ///
    /// Falls back to environment variables if file doesn't exist.
    pub fn load() -> Self {
        let home = std::env::var("HOME").unwrap_or_default();
        let secrets_path = PathBuf::from(home).join(".exomonad/secrets");

        if let Ok(content) = std::fs::read_to_string(&secrets_path) {
            tracing::info!(path = %secrets_path.display(), "Loaded secrets");
            Self {
                values: parse_env_file(&content),
            }
        } else {
            tracing::debug!(
                path = %secrets_path.display(),
                "No secrets file, using env vars"
            );
            Self::default()
        }
    }

    /// Get a secret value, falling back to environment variable.
    pub fn get(&self, key: &str) -> Option<String> {
        self.values
            .get(key)
            .cloned()
            .or_else(|| std::env::var(key).ok())
    }

    /// Get GITHUB_TOKEN.
    pub fn github_token(&self) -> Option<String> {
        self.get("GITHUB_TOKEN")
    }

    /// Get ANTHROPIC_API_KEY.
    pub fn anthropic_api_key(&self) -> Option<String> {
        self.get("ANTHROPIC_API_KEY")
    }
}

/// Parse a .env-style file into key-value pairs.
fn parse_env_file(content: &str) -> HashMap<String, String> {
    content
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            !trimmed.is_empty() && !trimmed.starts_with('#')
        })
        .filter_map(|line| {
            let mut parts = line.splitn(2, '=');
            let key = parts.next()?.trim();
            let value = parts.next()?.trim();
            // Strip optional quotes
            let value = value
                .strip_prefix('"')
                .and_then(|v| v.strip_suffix('"'))
                .unwrap_or(value);
            Some((key.to_string(), value.to_string()))
        })
        .collect()
}

// --- Host Functions ---

#[derive(Deserialize)]
struct GetSecretInput {
    key: String,
}

#[derive(Serialize)]
struct GetSecretOutput {
    value: Option<String>,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "payload")]
enum HostResult<T> {
    Success(T),
    Error(HostError),
}

#[derive(Serialize)]
struct HostError {
    message: String,
}

impl<T> From<Result<T, anyhow::Error>> for HostResult<T> {
    fn from(res: Result<T, anyhow::Error>) -> Self {
        match res {
            Ok(val) => HostResult::Success(val),
            Err(e) => HostResult::Error(HostError {
                message: e.to_string(),
            }),
        }
    }
}

fn get_input<T: serde::de::DeserializeOwned>(
    plugin: &mut CurrentPlugin,
    val: Val,
) -> Result<T, Error> {
    let handle = plugin
        .memory_from_val(&val)
        .ok_or_else(|| Error::msg("Invalid memory handle in input"))?;
    let bytes = plugin.memory_bytes(handle)?;
    Ok(serde_json::from_slice(bytes)?)
}

fn set_output<T: Serialize>(plugin: &mut CurrentPlugin, data: &T) -> Result<Val, Error> {
    let json = serde_json::to_vec(data)?;
    let handle = plugin.memory_new(json)?;
    Ok(plugin.memory_to_val(handle))
}

pub fn register_host_functions(service: Arc<Secrets>) -> Vec<Function> {
    vec![Function::new(
        "secrets_get",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        secrets_get,
    )
    .with_namespace("env")]
}

fn secrets_get(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<Secrets>>,
) -> Result<(), Error> {
    let input: GetSecretInput = get_input(plugin, inputs[0].clone())?;
    let service_arc = user_data.get()?;
    let service = service_arc
        .lock()
        .map_err(|_| Error::msg("Poisoned lock"))?;

    let value = service.get(&input.key);
    let output: HostResult<GetSecretOutput> = Ok(GetSecretOutput { value }).into();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_env_file() {
        let content = r#"
# Comment
GITHUB_TOKEN=ghp_123
ANTHROPIC_API_KEY=sk-ant-456

# Another comment
EMPTY=
QUOTED="value with spaces"
"#;

        let parsed = parse_env_file(content);
        assert_eq!(parsed.get("GITHUB_TOKEN"), Some(&"ghp_123".to_string()));
        assert_eq!(
            parsed.get("ANTHROPIC_API_KEY"),
            Some(&"sk-ant-456".to_string())
        );
        assert_eq!(parsed.get("EMPTY"), Some(&"".to_string()));
        assert_eq!(parsed.get("QUOTED"), Some(&"value with spaces".to_string()));
    }

    #[test]
    fn test_secrets_fallback_to_env() {
        let secrets = Secrets::default();
        // This will check env var, which may or may not be set
        // Just verify it doesn't panic
        let _ = secrets.get("GITHUB_TOKEN");
    }
}
