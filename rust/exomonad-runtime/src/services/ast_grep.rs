use anyhow::{Context, Result};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::io::Write;
use std::process::Command;
use std::sync::Arc;
use tempfile::NamedTempFile;

#[derive(Clone)]
pub struct AstGrepService;

impl AstGrepService {
    pub fn new() -> Self {
        Self
    }

    pub fn scan(&self, rule_yaml: &str, scope: &str) -> Result<String> {
        let mut rule_file = NamedTempFile::new()?;
        write!(rule_file, "{}", rule_yaml)?;

        // sg scan --rule <file> <scope> --json
        let output = Command::new("sg")
            .arg("scan")
            .arg("--rule")
            .arg(rule_file.path())
            .arg(scope)
            .arg("--json")
            .output()
            .context("Failed to execute ast-grep scan")?;

        if !output.status.success() {
            // ast-grep returns non-zero on error.
            // Note: It returns 0 if matches found, 0 if no matches found (usually),
            // unless --error-on-no-matches is set.
            // But if there is a syntax error in rule, it writes to stderr and exits non-zero.
            let err = String::from_utf8_lossy(&output.stderr);
            if !err.is_empty() {
                return Err(anyhow::anyhow!("ast-grep scan failed: {}", err));
            }
        }

        let json_out = String::from_utf8(output.stdout)?;
        Ok(json_out)
    }

    pub fn apply(&self, rule_yaml: &str, scope: &str) -> Result<()> {
        let mut rule_file = NamedTempFile::new()?;
        write!(rule_file, "{}", rule_yaml)?;

        // sg scan --rule <file> <scope> --update-all
        let output = Command::new("sg")
            .arg("scan")
            .arg("--rule")
            .arg(rule_file.path())
            .arg(scope)
            .arg("--update-all")
            .output()
            .context("Failed to execute ast-grep apply")?;

        if !output.status.success() {
            let err = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow::anyhow!("ast-grep apply failed: {}", err));
        }
        Ok(())
    }
}

// --- Host Functions ---

#[derive(Deserialize)]
struct ScanInput {
    rule: String,
    scope: String,
}

#[derive(Serialize)]
struct ScanOutput {
    json: String,
}

#[derive(Deserialize)]
struct ApplyInput {
    rule: String,
    scope: String,
}

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

pub fn register_host_functions(service: Arc<AstGrepService>) -> Vec<Function> {
    vec![
        Function::new(
            "ast_grep_scan",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            ast_grep_scan,
        )
        .with_namespace("env"),
        Function::new(
            "ast_grep_apply",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service),
            ast_grep_apply,
        )
        .with_namespace("env"),
    ]
}

fn ast_grep_scan(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<AstGrepService>>,
) -> Result<(), Error> {
    let input: ScanInput = get_input(plugin, inputs[0].clone())?;
    let service_arc = user_data.get()?;
    let service = service_arc
        .lock()
        .map_err(|_| Error::msg("Poisoned lock"))?;

    let result = service.scan(&input.rule, &input.scope);
    let output: HostResult<ScanOutput> = result.map(|json| ScanOutput { json }).into();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

fn ast_grep_apply(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<AstGrepService>>,
) -> Result<(), Error> {
    let input: ApplyInput = get_input(plugin, inputs[0].clone())?;
    let service_arc = user_data.get()?;
    let service = service_arc
        .lock()
        .map_err(|_| Error::msg("Poisoned lock"))?;

    let result = service.apply(&input.rule, &input.scope);
    let output: HostResult<()> = result.into();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}
