//! Host function for WASM to yield effects via protobuf binary encoding.
//!
//! The WASM guest sends an `EffectEnvelope` (protobuf) containing the effect type
//! and payload bytes. The host dispatches to the appropriate handler and returns
//! an `EffectResponse` (protobuf) with either payload bytes or an error.

use super::{EffectContext, EffectError, EffectRegistry};
use exomonad_proto::effects::error as proto_error;
use exomonad_proto::effects::{EffectEnvelope, EffectResponse};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use prost::Message;
use std::sync::Arc;

/// Convert runtime EffectError to proto EffectError.
pub fn to_proto_error(err: &EffectError) -> proto_error::EffectError {
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
        EffectError::Custom {
            code,
            message,
            data,
        } => Kind::Custom(proto_error::Custom {
            code: code.clone(),
            message: message.clone(),
            data: data
                .as_ref()
                .and_then(|v| serde_json::to_vec(v).ok())
                .unwrap_or_default(),
        }),
    };

    proto_error::EffectError { kind: Some(kind) }
}

/// User data for the yield_effect host function.
pub struct YieldEffectContext {
    /// Effect registry for dispatching semantic effects.
    pub registry: Arc<EffectRegistry>,
    /// Agent identity, baked in at plugin construction time.
    pub ctx: EffectContext,
}

/// Create the yield_effect host function.
///
/// This host function is registered with the Extism WASM plugin and
/// serves as the single entry point for all semantic effects (git, fs, etc.).
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
    .with_namespace("env")
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

    eprintln!(
        "[yield_effect] dispatching: {} ({} bytes payload)",
        envelope.effect_type,
        envelope.payload.len()
    );

    // Clone Arc<EffectRegistry> and EffectContext out of the mutex, then drop the lock.
    // This prevents holding the UserData mutex across the async dispatch.
    let (registry, effect_ctx) = {
        let ctx = user_data.get()?;
        let guard = ctx.lock().map_err(|_| Error::msg("Poisoned lock"))?;
        (guard.registry.clone(), guard.ctx.clone())
    };

    let result =
        block_on(registry.dispatch(&envelope.effect_type, &envelope.payload, &effect_ctx))?;

    match &result {
        Ok(payload) => tracing::debug!(
            effect_type = %envelope.effect_type,
            response_bytes = payload.len(),
            "yield_effect: dispatch succeeded"
        ),
        Err(err) => tracing::error!(
            effect_type = %envelope.effect_type,
            error = %err,
            "yield_effect: dispatch failed"
        ),
    }

    // Build EffectResponse
    let response = match result {
        Ok(payload) => EffectResponse {
            result: Some(exomonad_proto::effects::error::effect_response::Result::Payload(payload)),
        },
        Err(err) => EffectResponse {
            result: Some(
                exomonad_proto::effects::error::effect_response::Result::Error(to_proto_error(
                    &err,
                )),
            ),
        },
    };

    // Encode and return
    let response_bytes = response.encode_to_vec();
    tracing::debug!(
        response_len = response_bytes.len(),
        first_bytes = ?&response_bytes[..response_bytes.len().min(32)],
        "yield_effect: encoding response"
    );
    plugin.memory_set_val(&mut outputs[0], response_bytes)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use exomonad_proto::effects::error::effect_response::Result as ResponseResult;

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
            result: Some(ResponseResult::Error(proto_err)),
        };

        let bytes = response.encode_to_vec();
        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();

        match decoded.result {
            Some(ResponseResult::Error(e)) => match e.kind {
                Some(proto_error::effect_error::Kind::NotFound(nf)) => {
                    assert!(nf.resource.contains("123"));
                }
                _ => panic!("Expected NotFound"),
            },
            _ => panic!("Expected Error response"),
        }
    }

    #[test]
    fn test_payload_response_binary_roundtrip() {
        let payload = vec![1, 2, 3, 4, 5];
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(payload.clone())),
        };

        let bytes = response.encode_to_vec();
        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();

        match decoded.result {
            Some(ResponseResult::Payload(p)) => assert_eq!(p, payload),
            _ => panic!("Expected Payload response, got {:?}", decoded.result),
        }
    }

    #[test]
    fn test_empty_payload_response() {
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(vec![])),
        };

        let bytes = response.encode_to_vec();
        assert!(
            !bytes.is_empty(),
            "Empty payload response must still produce non-empty wire bytes"
        );

        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();

        match decoded.result {
            Some(ResponseResult::Payload(p)) => assert!(p.is_empty()),
            _ => panic!("Expected Payload response, got {:?}", decoded.result),
        }
    }

    #[test]
    fn test_spawn_response_in_envelope() {
        use exomonad_proto::effects::agent::{AgentInfo, SpawnResponse};

        let agent_info = AgentInfo {
            id: "gh-123-claude".into(),
            issue: "123".into(),
            worktree_path: "/tmp/worktrees/gh-123".into(),
            branch_name: "gh-123/fix-bug".into(),
            agent_type: 1, // CLAUDE
            role: 1,       // DEV
            status: 1,     // RUNNING
            zellij_tab: "123-fix-bug".into(),
            error: String::new(),
            pr_number: 0,
            pr_url: String::new(),
            topology: 1, // WORKTREE_PER_AGENT
        };

        let spawn_resp = SpawnResponse {
            agent: Some(agent_info),
        };
        let inner_bytes = spawn_resp.encode_to_vec();

        let response = EffectResponse {
            result: Some(ResponseResult::Payload(inner_bytes.clone())),
        };

        let wire_bytes = response.encode_to_vec();
        let decoded = EffectResponse::decode(wire_bytes.as_slice()).unwrap();

        match decoded.result {
            Some(ResponseResult::Payload(p)) => {
                let decoded_spawn = SpawnResponse::decode(p.as_slice()).unwrap();
                let agent = decoded_spawn.agent.unwrap();
                assert_eq!(agent.id, "gh-123-claude");
                assert_eq!(agent.issue, "123");
                assert_eq!(agent.branch_name, "gh-123/fix-bug");
                assert_eq!(agent.status, 1);
            }
            _ => panic!("Expected Payload response"),
        }
    }

    #[test]
    fn test_error_response_all_variants() {
        let variants: Vec<EffectError> = vec![
            EffectError::not_found("missing/resource"),
            EffectError::invalid_input("bad field"),
            EffectError::network_error("connection refused"),
            EffectError::permission_denied("no access"),
            EffectError::timeout("30s exceeded"),
            EffectError::custom("custom.code", "custom msg"),
        ];

        for err in &variants {
            let proto_err = to_proto_error(err);
            let response = EffectResponse {
                result: Some(ResponseResult::Error(proto_err)),
            };

            let bytes = response.encode_to_vec();
            let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();

            match (&decoded.result, err) {
                (Some(ResponseResult::Error(e)), EffectError::NotFound { resource }) => {
                    match &e.kind {
                        Some(proto_error::effect_error::Kind::NotFound(nf)) => {
                            assert_eq!(&nf.resource, resource);
                        }
                        other => panic!("Expected NotFound, got {:?}", other),
                    }
                }
                (Some(ResponseResult::Error(e)), EffectError::InvalidInput { message }) => {
                    match &e.kind {
                        Some(proto_error::effect_error::Kind::InvalidInput(ii)) => {
                            assert_eq!(&ii.message, message);
                        }
                        other => panic!("Expected InvalidInput, got {:?}", other),
                    }
                }
                (Some(ResponseResult::Error(e)), EffectError::NetworkError { message }) => {
                    match &e.kind {
                        Some(proto_error::effect_error::Kind::NetworkError(ne)) => {
                            assert_eq!(&ne.message, message);
                        }
                        other => panic!("Expected NetworkError, got {:?}", other),
                    }
                }
                (Some(ResponseResult::Error(e)), EffectError::PermissionDenied { message }) => {
                    match &e.kind {
                        Some(proto_error::effect_error::Kind::PermissionDenied(pd)) => {
                            assert_eq!(&pd.message, message);
                        }
                        other => panic!("Expected PermissionDenied, got {:?}", other),
                    }
                }
                (Some(ResponseResult::Error(e)), EffectError::Timeout { message }) => {
                    match &e.kind {
                        Some(proto_error::effect_error::Kind::Timeout(t)) => {
                            assert_eq!(&t.message, message);
                        }
                        other => panic!("Expected Timeout, got {:?}", other),
                    }
                }
                (Some(ResponseResult::Error(e)), EffectError::Custom { code, message, .. }) => {
                    match &e.kind {
                        Some(proto_error::effect_error::Kind::Custom(c)) => {
                            assert_eq!(&c.code, code);
                            assert_eq!(&c.message, message);
                        }
                        other => panic!("Expected Custom, got {:?}", other),
                    }
                }
                (result, err) => panic!("Mismatch: result={:?}, err={:?}", result, err),
            }
        }
    }

    /// Verify raw wire bytes for a payload response match protobuf spec.
    ///
    /// For `EffectResponse { payload: b"hello" }`:
    /// - Field 1, wire type 2 (LEN) → tag byte = (1 << 3) | 2 = 0x0a
    /// - Varint length of "hello" = 5
    /// - Payload bytes: [104, 101, 108, 108, 111]
    #[test]
    fn test_response_byte_inspection() {
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(b"hello".to_vec())),
        };

        let bytes = response.encode_to_vec();

        assert_eq!(bytes[0], 0x0a, "Field 1 LEN tag");
        assert_eq!(bytes[1], 5, "Varint length of 'hello'");
        assert_eq!(&bytes[2..7], b"hello", "Payload bytes");
        assert_eq!(bytes.len(), 7, "Total encoded length");
    }
}
