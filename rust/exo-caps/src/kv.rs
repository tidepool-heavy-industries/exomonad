//! `Kv` capability — small file-backed key/value (e.g. hook allowlists read by
//! `pre_tool_use`). Signatures firm up in Wave 1.

use crate::error::CapResult;
use async_trait::async_trait;

#[async_trait]
pub trait Kv {
    async fn get(&self, key: &str) -> CapResult<Option<String>>;
    async fn set(&self, key: &str, value: &str) -> CapResult<()>;
}
