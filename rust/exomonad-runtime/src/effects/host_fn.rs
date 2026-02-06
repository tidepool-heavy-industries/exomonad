//! Host function for WASM to yield effects to extension handlers.
//!
//! This module provides `yield_effect`, the single entry point for WASM guests
//! to invoke extension effects. Unlike builtin host functions (git_get_branch, etc.),
//! this function routes to user-registered effect handlers based on namespace.

use super::{EffectError, EffectRegistry};
use crate::common::{ErrorCode, FFIError, FFIResult};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;

/// Request structure for yield_effect host function.
///
/// The WASM guest serializes this to JSON and passes it to the host.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EffectRequest {
    /// Full effect type including namespace (e.g., "egregore.emit_signal").
    #[serde(rename = "$type")]
    pub effect_type: String,

    /// Effect-specific payload (everything except $type).
    #[serde(flatten)]
    pub payload: Value,
}

/// Convert EffectError to FFIError for compatibility with existing error handling.
impl From<EffectError> for FFIError {
    fn from(err: EffectError) -> Self {
        match err {
            EffectError::NotFound { resource } => FFIError {
                message: format!("Not found: {}", resource),
                code: ErrorCode::NotFound,
                context: None,
                suggestion: None,
            },
            EffectError::InvalidInput { message } => FFIError {
                message,
                code: ErrorCode::InvalidInput,
                context: None,
                suggestion: None,
            },
            EffectError::NetworkError { message } => FFIError {
                message,
                code: ErrorCode::NetworkError,
                context: None,
                suggestion: None,
            },
            EffectError::PermissionDenied { message } => FFIError {
                message,
                code: ErrorCode::NotAuthenticated,
                context: None,
                suggestion: None,
            },
            EffectError::Timeout { message } => FFIError {
                message,
                code: ErrorCode::Timeout,
                context: None,
                suggestion: None,
            },
            EffectError::Custom { code, message, .. } => FFIError {
                message: format!("[{}] {}", code, message),
                code: ErrorCode::InternalError,
                context: None,
                suggestion: None,
            },
        }
    }
}

/// User data for the yield_effect host function.
///
/// Contains the effect registry for dispatching effects to handlers.
pub struct YieldEffectContext {
    pub registry: Arc<EffectRegistry>,
}

/// Create the yield_effect host function.
///
/// This function is registered with the Extism plugin and called by WASM guests
/// to invoke extension effects.
///
/// # Protocol
///
/// 1. WASM guest calls `yield_effect(ptr)` where ptr points to JSON `EffectRequest`
/// 2. Host deserializes request, extracts effect_type and payload
/// 3. Host dispatches to registry by namespace prefix
/// 4. Handler executes async, returns result
/// 5. Host serializes `FFIResult<Value>` and returns ptr to WASM
///
/// # Example
///
/// ```rust,ignore
/// let registry = Arc::new(EffectRegistry::new());
/// let ctx = YieldEffectContext {
///     registry: registry.clone(),
/// };
/// let host_fn = yield_effect_host_fn(ctx);
/// // Register with Extism plugin builder
/// ```
pub fn yield_effect_host_fn(context: YieldEffectContext) -> Function {
    Function::new(
        "yield_effect",
        [ValType::I64],
        [ValType::I64],
        UserData::new(context),
        yield_effect_impl,
    )
}

fn block_on<F: std::future::Future>(future: F) -> Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => Err(Error::msg(
            "No Tokio runtime available for async effect dispatch",
        )),
    }
}

/// Implementation of the yield_effect host function.
fn yield_effect_impl(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<YieldEffectContext>,
) -> Result<(), Error> {
    let _span = tracing::info_span!("host_function", function = "yield_effect").entered();

    if inputs.is_empty() {
        return Err(Error::msg("yield_effect: expected input argument"));
    }

    // Parse the effect request using Json wrapper
    let Json(request): Json<EffectRequest> = plugin.memory_get_val(&inputs[0])?;

    tracing::debug!(
        effect_type = %request.effect_type,
        "yield_effect: dispatching"
    );

    // Get context and dispatch
    let ctx = user_data.get()?;
    let ctx_lock = ctx.lock().map_err(|_| Error::msg("Poisoned lock"))?;

    // Block on the async dispatch (we're in a sync host function)
    let result = block_on(
        ctx_lock
            .registry
            .dispatch(&request.effect_type, request.payload),
    )?;

    // Convert to FFIResult for consistency with other host functions
    let response: FFIResult<Value> = match result {
        Ok(value) => FFIResult::Success(value),
        Err(err) => FFIResult::Error(err.into()),
    };

    // Set output using Json wrapper
    plugin.memory_set_val(&mut outputs[0], Json(response))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_effect_request_parse() {
        let json = r#"{"$type": "test.do_thing", "arg1": "value1", "arg2": 42}"#;
        let req: EffectRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.effect_type, "test.do_thing");
        assert_eq!(req.payload["arg1"], "value1");
        assert_eq!(req.payload["arg2"], 42);
    }

    #[test]
    fn test_effect_error_to_ffi() {
        let effect_err = EffectError::not_found("resource/123");
        let ffi_err: FFIError = effect_err.into();
        assert_eq!(ffi_err.code, ErrorCode::NotFound);
        assert!(ffi_err.message.contains("123"));
    }
}
