//! `Process` capability — ad-hoc command execution. Signatures firm up in Wave 1.

use crate::error::CapResult;
use async_trait::async_trait;

#[derive(Debug, Clone)]
pub struct Output {
    pub status: i32,
    pub stdout: String,
    pub stderr: String,
}

#[async_trait]
pub trait Process {
    async fn run(&self, program: &str, args: &[String]) -> CapResult<Output>;
}
