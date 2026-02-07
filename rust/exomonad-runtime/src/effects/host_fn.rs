//! Host function for WASM to yield effects via protobuf binary encoding.
//!
//! The WASM guest sends an `EffectEnvelope` (protobuf) containing the effect type
//! and payload bytes. The host dispatches to the appropriate handler and returns
//! an `EffectResponse` (protobuf) with either payload bytes or an error.

use super::{EffectError, EffectRegistry};
use exomonad_proto::effects::error as proto_error;
use exomonad_proto::effects::{EffectEnvelope, EffectResponse};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use prost::Message;
use std::sync::Arc;

/// Convert runtime EffectError to proto EffectError.
fn to_proto_error(err: &EffectError) -> proto_error::EffectError {
    use proto_error::effect_error::Kind;

    let kind = match err {
        EffectError::NotFound { resource } => Kind::NotFound(proto_error::NotFound {
            resource: resource.clone(),
        }),
        EffectError::InvalidInput { message } => Kind::InvalidInput(proto_error::InvalidInput {
            message: message.clone(),
        }),
        EffectError::NetworkError { message } => Kind::NetworkError(proto_error::NetworkError {
            message: message.clone(),
        }),
        EffectError::PermissionDenied { message } => {
            Kind::PermissionDenied(proto_error::PermissionDenied {
                message: message.clone(),
            })
        }
        EffectError::Timeout { message } => Kind::Timeout(proto_error::Timeout {
            message: message.clone(),
        }),
        EffectError::Custom { code, message, .. } => Kind::Custom(proto_error::Custom {
            code: code.clone(),
            message: message.clone(),
            data: Vec::new(),
        }),
    };

    proto_error::EffectError { kind: Some(kind) }
}

/// User data for the yield_effect host function.
pub struct YieldEffectContext {
    pub registry: Arc<EffectRegistry>,
}

/// Create the yield_effect host function.
///
/// # Protocol
///
/// 1. WASM guest calls `yield_effect(ptr)` where ptr points to protobuf `EffectEnvelope`
/// 2. Host decodes envelope, extracts effect_type and payload bytes
/// 3. Host dispatches to registry by namespace prefix
/// 4. Handler processes binary payload, returns binary response
/// 5. Host wraps in `EffectResponse` and returns protobuf bytes to WASM
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

    // Read raw bytes from WASM memory
    let input_bytes = plugin.memory_get_val::<Vec<u8>>(&inputs[0])?;

    // Decode the envelope
    let envelope = EffectEnvelope::decode(input_bytes.as_slice())
        .map_err(|e| Error::msg(format!("Failed to decode EffectEnvelope: {}", e)))?;

    tracing::debug!(
        effect_type = %envelope.effect_type,
        payload_bytes = envelope.payload.len(),
        "yield_effect: dispatching"
    );

    // Get context and dispatch
    let ctx = user_data.get()?;
    let ctx_lock = ctx.lock().map_err(|_| Error::msg("Poisoned lock"))?;

    // Block on the async dispatch
    let result = block_on(
        ctx_lock
            .registry
            .dispatch(&envelope.effect_type, &envelope.payload),
    )?;

    // Build EffectResponse
    let response = match result {
        Ok(payload) => EffectResponse {
            result: Some(exomonad_proto::effects::error::effect_response::Result::Payload(payload)),
        },
        Err(err) => EffectResponse {
            result: Some(exomonad_proto::effects::error::effect_response::Result::Error(
                to_proto_error(&err),
            )),
        },
    };

    // Encode and return
    let response_bytes = response.encode_to_vec();
    plugin.memory_set_val(&mut outputs[0], response_bytes)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_envelope_roundtrip() {
        let envelope = EffectEnvelope {
            effect_type: "git.get_branch".to_string(),
            payload: vec![10, 1, 46], // protobuf for working_dir = "."
        };

        let bytes = envelope.encode_to_vec();
        let decoded = EffectEnvelope::decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded.effect_type, "git.get_branch");
        assert_eq!(decoded.payload, vec![10, 1, 46]);
    }

    #[test]
    fn test_error_response_roundtrip() {
        let err = EffectError::not_found("resource/123");
        let proto_err = to_proto_error(&err);

        let response = EffectResponse {
            result: Some(exomonad_proto::effects::error::effect_response::Result::Error(proto_err)),
        };

        let bytes = response.encode_to_vec();
        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();

        match decoded.result {
            Some(exomonad_proto::effects::error::effect_response::Result::Error(e)) => {
                match e.kind {
                    Some(proto_error::effect_error::Kind::NotFound(nf)) => {
                        assert!(nf.resource.contains("123"));
                    }
                    _ => panic!("Expected NotFound"),
                }
            }
            _ => panic!("Expected Error response"),
        }
    }
}
