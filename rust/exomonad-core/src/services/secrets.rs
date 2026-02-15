//! Secret loading from ~/.exomonad/secrets.
//!
//! Secrets are stored in a simple KEY=value format (like .env files).
//! Single canonical location: ~/.exomonad/secrets

use std::collections::HashMap;
use std::path::PathBuf;

/// Loaded secrets from ~/.exomonad/secrets file.
#[derive(Debug, Clone, Default)]
pub struct Secrets {
    values: HashMap<String, String>,
}

impl Secrets {
    /// Load secrets from ~/.exomonad/secrets.
    ///
    /// Falls back to environment variables if file doesn't exist.
    /// Also exports loaded secrets to environment so host functions can access them.
    pub fn load() -> Self {
        let home = std::env::var("HOME").unwrap_or_default();
        let secrets_path = PathBuf::from(home).join(".exomonad/secrets");

        if let Ok(content) = std::fs::read_to_string(&secrets_path) {
            tracing::info!(path = %secrets_path.display(), "Loaded secrets");
            let values = parse_env_file(&content);

            // Export secrets to environment so host functions can access them
            // Only set if not already present (env var takes precedence)
            for (key, value) in &values {
                if std::env::var(key).is_err() {
                    std::env::set_var(key, value);
                    tracing::debug!(key = %key, "Exported secret to environment");
                }
            }

            Self { values }
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
