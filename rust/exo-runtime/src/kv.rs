//! `impl Kv for Runtime` — small file-backed key/value (hook allowlists, etc.).
//!
//! **Leaf R3.** Trivial: a `kv/` dir under `self.working_dir`, one file per key
//! (sanitize the key to a safe filename). `get` = read-or-`None`; `set` = `write_atomic`.
//! Adapt exomonad-core `KvHandler` semantics. Use `tokio::fs`, never blocking IO.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Kv, KvError};

#[async_trait]
impl Kv for Runtime {
    async fn get(&self, key: &str) -> Result<Option<String>, KvError> {
        let encoded = encode_key(key);
        let path = self.working_dir().join("kv").join(encoded);

        match tokio::fs::read_to_string(&path).await {
            Ok(content) => Ok(Some(content)),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(KvError::Failed {
                op: "get",
                key: key.to_string(),
                detail: e.to_string(),
            }),
        }
    }

    async fn set(&self, key: &str, value: &str) -> Result<(), KvError> {
        let encoded = encode_key(key);
        let path = self.working_dir().join("kv").join(&encoded);
        crate::util::atomic_write(&path, value.as_bytes())
            .await
            .map_err(|e| KvError::Failed {
                op: "set",
                key: key.to_string(),
                detail: e.to_string(),
            })
    }
}

fn encode_key(key: &str) -> String {
    key.as_bytes()
        .iter()
        .map(|b| format!("{:02x}", b))
        .collect()
}
