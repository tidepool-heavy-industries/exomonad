use crate::common::{GitBranch, IssueRef, Role, SessionId};
use crate::effects::error::{effect_error, effect_response};
use crate::effects::{
    Custom, EffectEnvelope, EffectError, EffectResponse, InvalidInput, NetworkError, NotFound,
    PermissionDenied, Timeout,
};
use crate::ffi::{self, ErrorCode, ErrorContext, FfiError, FfiResult};
use proptest::prelude::*;
use prost::Message;

// ============================================================================
// Strategies for Common types
// ============================================================================

fn arb_role() -> impl Strategy<Value = Role> {
    prop_oneof![
        Just(Role::Unspecified),
        Just(Role::Dev),
        Just(Role::Tl),
        Just(Role::Pm),
        Just(Role::Reviewer),
    ]
}

fn arb_session_id() -> impl Strategy<Value = SessionId> {
    any::<String>().prop_map(|value| SessionId { value })
}

fn arb_git_branch() -> impl Strategy<Value = GitBranch> {
    any::<String>().prop_map(|name| GitBranch { name })
}

fn arb_issue_ref() -> impl Strategy<Value = IssueRef> {
    (any::<String>(), any::<String>(), any::<i32>()).prop_map(|(owner, repo, number)| IssueRef {
        owner,
        repo,
        number,
    })
}

// ============================================================================
// Strategies for Effect types (Protobuf Binary)
// ============================================================================

fn arb_not_found() -> impl Strategy<Value = NotFound> {
    any::<String>().prop_map(|resource| NotFound { resource })
}

fn arb_invalid_input() -> impl Strategy<Value = InvalidInput> {
    any::<String>().prop_map(|message| InvalidInput { message })
}

fn arb_network_error() -> impl Strategy<Value = NetworkError> {
    any::<String>().prop_map(|message| NetworkError { message })
}

fn arb_permission_denied() -> impl Strategy<Value = PermissionDenied> {
    any::<String>().prop_map(|message| PermissionDenied { message })
}

fn arb_timeout() -> impl Strategy<Value = Timeout> {
    any::<String>().prop_map(|message| Timeout { message })
}

fn arb_custom() -> impl Strategy<Value = Custom> {
    (any::<String>(), any::<String>(), any::<Vec<u8>>()).prop_map(|(code, message, data)| Custom {
        code,
        message,
        data,
    })
}

fn arb_effect_error() -> impl Strategy<Value = EffectError> {
    prop_oneof![
        arb_not_found().prop_map(|v| EffectError {
            kind: Some(effect_error::Kind::NotFound(v))
        }),
        arb_invalid_input().prop_map(|v| EffectError {
            kind: Some(effect_error::Kind::InvalidInput(v))
        }),
        arb_network_error().prop_map(|v| EffectError {
            kind: Some(effect_error::Kind::NetworkError(v))
        }),
        arb_permission_denied().prop_map(|v| EffectError {
            kind: Some(effect_error::Kind::PermissionDenied(v))
        }),
        arb_timeout().prop_map(|v| EffectError {
            kind: Some(effect_error::Kind::Timeout(v))
        }),
        arb_custom().prop_map(|v| EffectError {
            kind: Some(effect_error::Kind::Custom(v))
        }),
    ]
}

fn arb_effect_envelope() -> impl Strategy<Value = EffectEnvelope> {
    (any::<String>(), any::<Vec<u8>>()).prop_map(|(effect_type, payload)| EffectEnvelope {
        effect_type,
        payload,
    })
}

fn arb_effect_response() -> impl Strategy<Value = EffectResponse> {
    prop_oneof![
        any::<Vec<u8>>().prop_map(|v| EffectResponse {
            result: Some(effect_response::Result::Payload(v))
        }),
        arb_effect_error().prop_map(|v| EffectResponse {
            result: Some(effect_response::Result::Error(v))
        }),
    ]
}

// ============================================================================
// Strategies for FFI types (JSON)
// ============================================================================

fn arb_error_code() -> impl Strategy<Value = ErrorCode> {
    prop_oneof![
        Just(ErrorCode::Unspecified),
        Just(ErrorCode::NotFound),
        Just(ErrorCode::NotAuthenticated),
        Just(ErrorCode::GitError),
        Just(ErrorCode::IoError),
        Just(ErrorCode::NetworkError),
        Just(ErrorCode::InvalidInput),
        Just(ErrorCode::InternalError),
        Just(ErrorCode::Timeout),
        Just(ErrorCode::AlreadyExists),
    ]
}

fn arb_error_context() -> impl Strategy<Value = ErrorContext> {
    (
        any::<String>(),
        any::<i32>(),
        any::<String>(),
        any::<String>(),
        any::<String>(),
        any::<String>(),
    )
        .prop_map(
            |(command, exit_code, stderr, stdout, file_path, working_dir)| ErrorContext {
                command,
                exit_code,
                stderr,
                stdout,
                file_path,
                working_dir,
            },
        )
}

fn arb_ffi_error() -> impl Strategy<Value = FfiError> {
    (
        any::<String>(),
        arb_error_code(),
        prop::option::weighted(0.8, arb_error_context()),
        any::<String>(),
    )
        .prop_map(|(message, code, context, suggestion)| FfiError {
            message,
            code: code as i32,
            context,
            suggestion,
        })
}

fn arb_ffi_result() -> impl Strategy<Value = FfiResult> {
    prop_oneof![
        any::<Vec<u8>>().prop_map(|v| FfiResult {
            result: Some(ffi::ffi_result::Result::SuccessPayload(v))
        }),
        arb_ffi_error().prop_map(|v| FfiResult {
            result: Some(ffi::ffi_result::Result::Error(v))
        }),
    ]
}

// ============================================================================
// Roundtrip Tests
// ============================================================================

proptest! {
    #[test]
    fn test_role_json_roundtrip(role in arb_role()) {
        let json = serde_json::to_string(&role).unwrap();
        let decoded: Role = serde_json::from_str(&json).unwrap();
        assert_eq!(role, decoded);
    }

    #[test]
    fn test_session_id_roundtrip(sid in arb_session_id()) {
        let json = serde_json::to_string(&sid).unwrap();
        let decoded: SessionId = serde_json::from_str(&json).unwrap();
        assert_eq!(sid, decoded);
    }

    #[test]
    fn test_git_branch_roundtrip(branch in arb_git_branch()) {
        let json = serde_json::to_string(&branch).unwrap();
        let decoded: GitBranch = serde_json::from_str(&json).unwrap();
        assert_eq!(branch, decoded);
    }

    #[test]
    fn test_issue_ref_roundtrip(issue in arb_issue_ref()) {
        let json = serde_json::to_string(&issue).unwrap();
        let decoded: IssueRef = serde_json::from_str(&json).unwrap();
        assert_eq!(issue, decoded);
    }

    #[test]
    fn test_effect_envelope_roundtrip(envelope in arb_effect_envelope()) {
        let mut buf = Vec::new();
        envelope.encode(&mut buf).unwrap();
        let decoded = EffectEnvelope::decode(&buf[..]).unwrap();
        assert_eq!(envelope, decoded);
    }

    #[test]
    fn test_effect_error_roundtrip(error in arb_effect_error()) {
        let mut buf = Vec::new();
        error.encode(&mut buf).unwrap();
        let decoded = EffectError::decode(&buf[..]).unwrap();
        assert_eq!(error, decoded);
    }

    #[test]
    fn test_effect_response_roundtrip(response in arb_effect_response()) {
        let mut buf = Vec::new();
        response.encode(&mut buf).unwrap();
        let decoded = EffectResponse::decode(&buf[..]).unwrap();
        assert_eq!(response, decoded);
    }

    #[test]
    fn test_ffi_error_json_roundtrip(error in arb_ffi_error()) {
        let json = serde_json::to_string(&error).unwrap();
        let decoded: FfiError = serde_json::from_str(&json).unwrap();
        assert_eq!(error, decoded);
    }

    #[test]
    fn test_ffi_error_proto_roundtrip(error in arb_ffi_error()) {
        let mut buf = Vec::new();
        error.encode(&mut buf).unwrap();
        let decoded = FfiError::decode(&buf[..]).unwrap();
        assert_eq!(error, decoded);
    }

    #[test]
    fn test_ffi_result_json_roundtrip(res in arb_ffi_result()) {
        let json = serde_json::to_string(&res).unwrap();
        let decoded: FfiResult = serde_json::from_str(&json).unwrap();
        assert_eq!(res, decoded);
    }

    #[test]
    fn test_ffi_result_proto_roundtrip(res in arb_ffi_result()) {
        let mut buf = Vec::new();
        res.encode(&mut buf).unwrap();
        let decoded = FfiResult::decode(&buf[..]).unwrap();
        assert_eq!(res, decoded);
    }
}
