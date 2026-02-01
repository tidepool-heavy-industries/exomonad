use anyhow::Result;
use exomonad_services::anthropic::AnthropicService;
use exomonad_services::ExternalService;
use exomonad_shared::protocol::{ChatMessage, ServiceRequest, ServiceResponse, StopReason, Usage};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

// ============================================================================ 
// Types
// ============================================================================ 

#[derive(Debug, Clone, Deserialize)]
pub struct LlmCompleteInput {
    pub model: String,
    pub messages: Vec<ChatMessage>,
    #[serde(rename = "max_tokens")]
    pub max_tokens: u32,
    pub system: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct LlmCompleteOutput {
    pub content: String,
    pub stop_reason: String,
    pub usage: Usage,
}

// ============================================================================ 
// Service
// ============================================================================ 

pub struct LlmService {
    anthropic: Option<AnthropicService>,
}

impl LlmService {
    pub fn new() -> Self {
        let anthropic = AnthropicService::from_env().ok();
        Self { anthropic }
    }

    pub async fn complete(&self, input: &LlmCompleteInput) -> Result<LlmCompleteOutput> {
        let service = self
            .anthropic
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Anthropic service not available"))?;

        let req = ServiceRequest::AnthropicChat {
            model: input.model.clone(),
            messages: input.messages.clone(),
            max_tokens: input.max_tokens,
            tools: None,
            system: input.system.clone(),
            thinking: None,
        };

        match service.call(req).await {
            Ok(ServiceResponse::AnthropicChat {
                content,
                stop_reason,
                usage,
            }) => {
                // For now, we only support text content in the explorer
                let text_content = content
                    .iter()
                    .filter(|block| block.block_type == "text")
                    .map(|block| block.text.as_deref().unwrap_or(""))
                    .collect::<Vec<_>>()
                    .join("\n");

                let stop_reason_str = match stop_reason {
                    StopReason::EndTurn => "end_turn",
                    StopReason::MaxTokens => "max_tokens",
                    StopReason::StopSequence => "stop_sequence",
                    StopReason::ToolUse => "tool_use",
                }
                .to_string();

                Ok(LlmCompleteOutput {
                    content: text_content,
                    stop_reason: stop_reason_str,
                    usage,
                })
            }
            Ok(_) => Err(anyhow::anyhow!("Unexpected response type")),
            Err(e) => Err(anyhow::anyhow!("Anthropic error: {}", e)),
        }
    }
}

impl Default for LlmService {
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

pub fn register_host_functions(service: Arc<LlmService>) -> Vec<Function> {
    vec![Function::new(
        "llm_complete",
        [ValType::I64],
        [ValType::I64],
        UserData::new(service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<LlmService>>|
         -> Result<(), Error> {
            let input: LlmCompleteInput = get_input(plugin, inputs[0].clone())?;
            let service_arc = user_data.get()?;
            let service = service_arc
                .lock()
                .map_err(|_| Error::msg("Poisoned lock"))?;
            let result = block_on(service.complete(&input))?;
            outputs[0] = set_output(plugin, &HostResult::from(result))?;
            Ok(())
        },
    )
    .with_namespace("env")]
}
