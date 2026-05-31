//! `Kv` capability — small file-backed key/value (e.g. hook allowlists read by
//! `pre_tool_use`). Signatures firm up in Wave 1.

use async_trait::async_trait;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum KvError {
    #[error("kv {op} failed for key {key:?}: {detail}")]
    Failed {
        op: &'static str,
        key: String,
        detail: String,
    },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[async_trait]
pub trait Kv {
    async fn get(&self, key: &str) -> Result<Option<String>, KvError>;
    async fn set(&self, key: &str, value: &str) -> Result<(), KvError>;
}
