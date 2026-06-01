//! `impl Kv for Runtime` — small file-backed key/value (hook allowlists, etc.).
//!
//! **Leaf R3.** Trivial: a `kv/` dir under `self.working_dir`, one file per key
//! (sanitize the key to a safe filename). `get` = read-or-`None`; `set` = `write_atomic`.
//! Adapt exomonad-core `KvHandler` semantics. Use `tokio::fs`, never blocking IO.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Kv, KvError};
use std::sync::atomic::{AtomicU64, Ordering};

static COUNTER: AtomicU64 = AtomicU64::new(0);

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
        let kv_dir = self.working_dir().join("kv");
        tokio::fs::create_dir_all(&kv_dir).await.map_err(|e| KvError::Failed {
            op: "set (create_dir_all)",
            key: key.to_string(),
            detail: e.to_string(),
        })?;

        let encoded = encode_key(key);
        let path = kv_dir.join(&encoded);
        let id = COUNTER.fetch_add(1, Ordering::Relaxed);
        let tmp_path = kv_dir.join(format!("{}.{}.{}.tmp", encoded, std::process::id(), id));

        tokio::fs::write(&tmp_path, value).await.map_err(|e| KvError::Failed {
            op: "set (write tmp)",
            key: key.to_string(),
            detail: e.to_string(),
        })?;

        tokio::fs::rename(&tmp_path, path).await.map_err(|e| KvError::Failed {
            op: "set (rename)",
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
