// Example code that deserializes and serializes the model.
// extern crate serde;
// #[macro_use]
// extern crate serde_derive;
// extern crate serde_json;
//
// use generated_module::ExoMonadTypes;
//
// fn main() {
//     let json = r#"{"answer": 42}"#;
//     let model: ExoMonadTypes = serde_json::from_str(&json).unwrap();
// }

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub type Common = Option<serde_json::Value>;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExoMonadTypes {
    pub effect: Effect,

    pub effect_result: EffectResult,

    pub worktree_info: Option<WorktreeInfo>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Effect {
    pub kind: EffectKind,

    pub payload: Payload,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum EffectKind {
    #[serde(rename = "DockerExec")]
    DockerExec,

    #[serde(rename = "GitGetBranch")]
    GitGetBranch,

    #[serde(rename = "GitGetWorktree")]
    GitGetWorktree,

    #[serde(rename = "GitHubListIssues")]
    GitHubListIssues,

    Log,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Payload {
    pub fields: Option<HashMap<String, String>>,

    pub level: Option<String>,

    pub message: Option<String>,

    pub working_dir: Option<String>,

    pub filter: Option<IssueFilter>,

    pub repo: Option<Repo>,

    pub command: Option<Vec<String>>,

    pub container_id: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct IssueFilter {
    pub assignee: Option<String>,

    pub labels: Option<Vec<String>>,

    pub state: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Repo {
    pub name: String,

    pub owner: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EffectResult {
    pub kind: EffectResultKind,

    pub payload: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum EffectResultKind {
    Error,

    Success,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WorktreeInfo {
    pub branch: String,

    pub commit: String,

    pub path: String,
}
