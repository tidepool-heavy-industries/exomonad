//! Key-value storage effect handler for the `kv.*` namespace.
//!
//! Stores values as JSON files under `.exo/kv/{key}.json`.
//! Keys are validated to contain only alphanumeric characters, underscores, and hyphens.

use crate::effects::{dispatch_kv_effect, EffectError, EffectHandler, EffectResult, KvEffects};
use async_trait::async_trait;
use exomonad_proto::effects::kv::*;
use std::path::PathBuf;

/// Key-value storage effect handler.
///
/// Persists values as individual JSON files under `.exo/kv/` in the project directory.
pub struct KvHandler {
    project_dir: PathBuf,
}

impl KvHandler {
    /// Create a new KV handler rooted at the given project directory.
    pub fn new(project_dir: PathBuf) -> Self {
        Self { project_dir }
    }

    /// Path to the KV storage directory.
    fn kv_dir(&self) -> PathBuf {
        self.project_dir.join(".exo").join("kv")
    }

    /// Path to a specific key's file.
    fn key_path(&self, key: &str) -> PathBuf {
        self.kv_dir().join(format!("{}.json", key))
    }

    /// Validate a key: alphanumeric, underscore, hyphen only.
    fn validate_key(key: &str) -> EffectResult<()> {
        if key.is_empty() {
            return Err(EffectError::invalid_input("Key must not be empty"));
        }
        if !key
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
        {
            return Err(EffectError::invalid_input(format!(
                "Key must contain only alphanumeric characters, underscores, and hyphens: {:?}",
                key
            )));
        }
        Ok(())
    }
}

#[async_trait]
impl EffectHandler for KvHandler {
    fn namespace(&self) -> &str {
        "kv"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_kv_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl KvEffects for KvHandler {
    async fn get(&self, req: GetRequest) -> EffectResult<GetResponse> {
        Self::validate_key(&req.key)?;

        let path = self.key_path(&req.key);
        tracing::info!(key = %req.key, path = %path.display(), "kv.get");

        match tokio::fs::read_to_string(&path).await {
            Ok(value) => {
                tracing::info!(key = %req.key, value_len = value.len(), "kv.get: found");
                Ok(GetResponse { found: true, value })
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                tracing::info!(key = %req.key, "kv.get: not found");
                Ok(GetResponse {
                    found: false,
                    value: String::new(),
                })
            }
            Err(e) => {
                tracing::error!(key = %req.key, error = %e, "kv.get: read failed");
                Err(EffectError::custom("kv_error", e.to_string()))
            }
        }
    }

    async fn set(&self, req: SetRequest) -> EffectResult<SetResponse> {
        Self::validate_key(&req.key)?;

        let dir = self.kv_dir();
        let path = self.key_path(&req.key);
        tracing::info!(key = %req.key, value_len = req.value.len(), path = %path.display(), "kv.set");

        // Ensure directory exists
        tokio::fs::create_dir_all(&dir).await.map_err(|e| {
            tracing::error!(dir = %dir.display(), error = %e, "kv.set: failed to create directory");
            EffectError::custom("kv_error", format!("Failed to create kv directory: {}", e))
        })?;

        // Write atomically: write to temp file, then rename
        let tmp_path = path.with_extension("json.tmp");
        tokio::fs::write(&tmp_path, &req.value).await.map_err(|e| {
            tracing::error!(path = %tmp_path.display(), error = %e, "kv.set: write failed");
            EffectError::custom("kv_error", format!("Failed to write kv file: {}", e))
        })?;

        tokio::fs::rename(&tmp_path, &path).await.map_err(|e| {
            tracing::error!(
                from = %tmp_path.display(),
                to = %path.display(),
                error = %e,
                "kv.set: rename failed"
            );
            EffectError::custom("kv_error", format!("Failed to rename kv file: {}", e))
        })?;

        tracing::info!(key = %req.key, "kv.set: success");
        Ok(SetResponse { success: true })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_key_valid() {
        assert!(KvHandler::validate_key("my-key").is_ok());
        assert!(KvHandler::validate_key("my_key").is_ok());
        assert!(KvHandler::validate_key("key123").is_ok());
        assert!(KvHandler::validate_key("a").is_ok());
        assert!(KvHandler::validate_key("ABC-def_123").is_ok());
    }

    #[test]
    fn test_validate_key_invalid() {
        assert!(KvHandler::validate_key("").is_err());
        assert!(KvHandler::validate_key("../etc/passwd").is_err());
        assert!(KvHandler::validate_key("key with spaces").is_err());
        assert!(KvHandler::validate_key("key/slash").is_err());
        assert!(KvHandler::validate_key("key.dot").is_err());
        assert!(KvHandler::validate_key("key\0null").is_err());
    }

    #[tokio::test]
    async fn test_get_missing_key() {
        let dir = tempfile::tempdir().unwrap();
        let handler = KvHandler::new(dir.path().to_path_buf());

        let resp = handler
            .get(GetRequest {
                key: "nonexistent".into(),
            })
            .await
            .unwrap();

        assert!(!resp.found);
        assert!(resp.value.is_empty());
    }

    #[tokio::test]
    async fn test_set_then_get() {
        let dir = tempfile::tempdir().unwrap();
        let handler = KvHandler::new(dir.path().to_path_buf());

        let set_resp = handler
            .set(SetRequest {
                key: "test-key".into(),
                value: "hello world".into(),
            })
            .await
            .unwrap();
        assert!(set_resp.success);

        let get_resp = handler
            .get(GetRequest {
                key: "test-key".into(),
            })
            .await
            .unwrap();
        assert!(get_resp.found);
        assert_eq!(get_resp.value, "hello world");
    }

    #[tokio::test]
    async fn test_set_overwrites() {
        let dir = tempfile::tempdir().unwrap();
        let handler = KvHandler::new(dir.path().to_path_buf());

        handler
            .set(SetRequest {
                key: "key".into(),
                value: "v1".into(),
            })
            .await
            .unwrap();

        handler
            .set(SetRequest {
                key: "key".into(),
                value: "v2".into(),
            })
            .await
            .unwrap();

        let resp = handler.get(GetRequest { key: "key".into() }).await.unwrap();
        assert_eq!(resp.value, "v2");
    }

    #[tokio::test]
    async fn test_invalid_key_rejected() {
        let dir = tempfile::tempdir().unwrap();
        let handler = KvHandler::new(dir.path().to_path_buf());

        let err = handler
            .get(GetRequest {
                key: "../bad".into(),
            })
            .await
            .unwrap_err();

        assert!(matches!(err, EffectError::InvalidInput { .. }));

        let err = handler
            .set(SetRequest {
                key: "bad/key".into(),
                value: "val".into(),
            })
            .await
            .unwrap_err();

        assert!(matches!(err, EffectError::InvalidInput { .. }));
    }
}
