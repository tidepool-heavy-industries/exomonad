use anyhow::{Context, Result};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::process::Stdio;
use std::sync::Arc;
use tokio::process::Command;

// ============================================================================
// Types (match Haskell Explore.hs)
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub line: i32,
    pub character: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    pub uri: String,
    pub range: Range,
    pub context: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct AstGrepInput {
    pub language: String,
    pub pattern: String,
    pub path: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct LspQueryInput {
    pub path: String,
    pub position: Position,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ReadFileRangeInput {
    pub path: String,
    #[serde(rename = "start_line")]
    pub start_line: i32,
    #[serde(rename = "end_line")]
    pub end_line: i32,
}

#[derive(Debug, Clone, Serialize)]
pub struct ReadFileRangeOutput {
    pub content: String,
    #[serde(rename = "start_line")]
    pub start_line: i32,
    #[serde(rename = "end_line")]
    pub end_line: i32,
}

// Internal types for parsing sg output
#[derive(Deserialize)]
struct SgMatch {
    range: SgRange,
    file: String,
    text: String,
}

#[derive(Deserialize)]
struct SgRange {
    start: SgPosition,
    end: SgPosition,
}

#[derive(Deserialize)]
struct SgPosition {
    line: i32,
    column: i32,
}

// ============================================================================
// Service
// ============================================================================

pub struct ExplorationService {}

impl ExplorationService {
    pub fn new() -> Self {
        Self {}
    }

    pub async fn ast_grep(&self, input: &AstGrepInput) -> Result<Vec<Location>> {
        let mut cmd = Command::new("sg");
        cmd.arg("run")
            .arg("--pattern")
            .arg(&input.pattern)
            .arg("--lang")
            .arg(&input.language)
            .arg("--json=stream");

        if !input.path.is_empty() && input.path != "." {
            cmd.arg(&input.path);
        }

        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

        let output = cmd.output().await.context("Failed to run sg")?;

        // sg returns 1 if no matches found, so we don't treat !success as fatal immediately
        if !output.status.success() && !output.stdout.is_empty() {
            // If stdout has content, we try to parse it.
            // If it's an error message, json parsing will likely fail or return empty.
        }

        let stdout = String::from_utf8(output.stdout).context("sg output not utf8")?;
        let mut locations = Vec::new();

        for line in stdout.lines() {
            if line.trim().is_empty() {
                continue;
            }
            // Parse each line as a JSON object
            let m: SgMatch = serde_json::from_str(line).context("Failed to parse sg output")?;

            locations.push(Location {
                uri: m.file,
                range: Range {
                    start: Position {
                        line: m.range.start.line,
                        character: m.range.start.column,
                    },
                    end: Position {
                        line: m.range.end.line,
                        character: m.range.end.column,
                    },
                },
                context: Some(m.text),
            });
        }

        Ok(locations)
    }

    pub async fn lsp_references(&self, _input: &LspQueryInput) -> Result<Vec<Location>> {
        // Stub: This would query the LSP server.
        // Requires integration with the LSP client architecture.
        Ok(vec![])
    }

    pub async fn lsp_definition(&self, _input: &LspQueryInput) -> Result<Vec<Location>> {
        // Stub
        Ok(vec![])
    }

    pub async fn lsp_hover(&self, _input: &LspQueryInput) -> Result<Option<String>> {
        // Stub
        Ok(None)
    }

    pub async fn read_file_range(&self, input: &ReadFileRangeInput) -> Result<ReadFileRangeOutput> {
        let path = std::path::Path::new(&input.path);
        if !path.exists() {
            return Ok(ReadFileRangeOutput {
                content: String::new(),
                start_line: input.start_line,
                end_line: input.end_line,
            });
        }

        let content = tokio::fs::read_to_string(path).await?;
        let lines: Vec<&str> = content.lines().collect();

        let start = input.start_line.max(0) as usize;
        let end = input.end_line.max(0) as usize;

        let safe_end = std::cmp::min(end, lines.len());
        let safe_start = std::cmp::min(start, safe_end);

        let selected_lines = if safe_start < lines.len() {
            lines[safe_start..safe_end].join("\n")
        } else {
            String::new()
        };

        Ok(ReadFileRangeOutput {
            content: selected_lines,
            start_line: input.start_line,
            end_line: input.end_line,
        })
    }
}

impl Default for ExplorationService {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Host Functions
// ============================================================================

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

impl<T> From<Result<T>> for HostResult<T> {
    fn from(res: Result<T>) -> Self {
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

fn block_on<F: std::future::Future>(future: F) -> Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => Err(Error::msg("No Tokio runtime available")),
    }
}

pub fn register_host_functions(service: Arc<ExplorationService>) -> Vec<Function> {
    vec![
        Function::new(
            "explore_ast_grep",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            |plugin: &mut CurrentPlugin,
             inputs: &[Val],
             outputs: &mut [Val],
             user_data: UserData<Arc<ExplorationService>>|
             -> Result<(), Error> {
                let input: AstGrepInput = get_input(plugin, inputs[0].clone())?;
                let service_mutex = user_data.get()?;
                let service = service_mutex
                    .lock()
                    .map_err(|_| Error::msg("Poisoned lock"))?;
                let result = block_on(service.ast_grep(&input))?;
                outputs[0] = set_output(plugin, &HostResult::from(result))?;
                Ok(())
            },
        )
        .with_namespace("env"),
        Function::new(
            "explore_lsp_references",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            |plugin: &mut CurrentPlugin,
             inputs: &[Val],
             outputs: &mut [Val],
             user_data: UserData<Arc<ExplorationService>>|
             -> Result<(), Error> {
                let input: LspQueryInput = get_input(plugin, inputs[0].clone())?;
                let service_mutex = user_data.get()?;
                let service = service_mutex
                    .lock()
                    .map_err(|_| Error::msg("Poisoned lock"))?;
                let result = block_on(service.lsp_references(&input))?;
                outputs[0] = set_output(plugin, &HostResult::from(result))?;
                Ok(())
            },
        )
        .with_namespace("env"),
        Function::new(
            "explore_lsp_definition",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            |plugin: &mut CurrentPlugin,
             inputs: &[Val],
             outputs: &mut [Val],
             user_data: UserData<Arc<ExplorationService>>|
             -> Result<(), Error> {
                let input: LspQueryInput = get_input(plugin, inputs[0].clone())?;
                let service_mutex = user_data.get()?;
                let service = service_mutex
                    .lock()
                    .map_err(|_| Error::msg("Poisoned lock"))?;
                let result = block_on(service.lsp_definition(&input))?;
                outputs[0] = set_output(plugin, &HostResult::from(result))?;
                Ok(())
            },
        )
        .with_namespace("env"),
        Function::new(
            "explore_lsp_hover",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            |plugin: &mut CurrentPlugin,
             inputs: &[Val],
             outputs: &mut [Val],
             user_data: UserData<Arc<ExplorationService>>|
             -> Result<(), Error> {
                let input: LspQueryInput = get_input(plugin, inputs[0].clone())?;
                let service_mutex = user_data.get()?;
                let service = service_mutex
                    .lock()
                    .map_err(|_| Error::msg("Poisoned lock"))?;
                let result = block_on(service.lsp_hover(&input))?;
                outputs[0] = set_output(plugin, &HostResult::from(result))?;
                Ok(())
            },
        )
        .with_namespace("env"),
        Function::new(
            "explore_read_file_range",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            |plugin: &mut CurrentPlugin,
             inputs: &[Val],
             outputs: &mut [Val],
             user_data: UserData<Arc<ExplorationService>>|
             -> Result<(), Error> {
                let input: ReadFileRangeInput = get_input(plugin, inputs[0].clone())?;
                let service_mutex = user_data.get()?;
                let service = service_mutex
                    .lock()
                    .map_err(|_| Error::msg("Poisoned lock"))?;
                let result = block_on(service.read_file_range(&input))?;
                outputs[0] = set_output(plugin, &HostResult::from(result))?;
                Ok(())
            },
        )
        .with_namespace("env"),
    ]
}
