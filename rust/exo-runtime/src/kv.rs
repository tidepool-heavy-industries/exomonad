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
    async fn get(&self, _key: &str) -> Result<Option<String>, KvError> {
        todo!("R3: read kv/<key> under self.working_dir; missing file => Ok(None)")
    }

    async fn set(&self, _key: &str, _value: &str) -> Result<(), KvError> {
        todo!("R3: temp+rename write of kv/<key> under self.working_dir")
    }
}
