//! Wire format compatibility tests.
//!
//! These tests verify that the proto-generated types serialize to JSON in a format
//! that's compatible with the existing manually-defined types in exomonad-shared.

use exomonad_proto::ffi::{ErrorCode, ErrorContext, FfiError};

/// Test that ErrorCode serializes to snake_case JSON strings.
#[test]
fn error_code_json_format() {
    // Proto-generated ErrorCode should serialize to snake_case
    assert_eq!(
        serde_json::to_string(&ErrorCode::NotFound).unwrap(),
        "\"not_found\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::NotAuthenticated).unwrap(),
        "\"not_authenticated\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::GitError).unwrap(),
        "\"git_error\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::IoError).unwrap(),
        "\"io_error\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::NetworkError).unwrap(),
        "\"network_error\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::InvalidInput).unwrap(),
        "\"invalid_input\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::InternalError).unwrap(),
        "\"internal_error\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::Timeout).unwrap(),
        "\"timeout\""
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::AlreadyExists).unwrap(),
        "\"already_exists\""
    );
}

/// Test that ErrorCode deserializes from snake_case JSON strings.
#[test]
fn error_code_json_parse() {
    assert_eq!(
        serde_json::from_str::<ErrorCode>("\"not_found\"").unwrap(),
        ErrorCode::NotFound
    );
    assert_eq!(
        serde_json::from_str::<ErrorCode>("\"internal_error\"").unwrap(),
        ErrorCode::InternalError
    );
}

/// Test that ErrorContext serializes correctly, omitting None fields.
#[test]
fn error_context_json_format() {
    let ctx = ErrorContext {
        command: Some("git status".into()),
        exit_code: Some(1),
        stderr: Some("error: not a git repository".into()),
        stdout: None,
        file_path: None,
        working_dir: Some("/tmp/test".into()),
    };

    let json = serde_json::to_value(&ctx).unwrap();

    // Verify fields that are set
    assert_eq!(json["command"], "git status");
    assert_eq!(json["exit_code"], 1);
    assert_eq!(json["stderr"], "error: not a git repository");
    assert_eq!(json["working_dir"], "/tmp/test");

    // Verify None fields are omitted
    assert!(!json.as_object().unwrap().contains_key("stdout"));
    assert!(!json.as_object().unwrap().contains_key("file_path"));
}

/// Test that FfiError serializes correctly.
#[test]
fn ffi_error_json_format() {
    let err = FfiError {
        message: "File not found".into(),
        code: ErrorCode::NotFound as i32,
        context: None,
        suggestion: Some("Check that the file exists".into()),
    };

    let json = serde_json::to_value(&err).unwrap();

    assert_eq!(json["message"], "File not found");
    // Note: code is serialized as integer in prost-generated code
    assert_eq!(json["code"], 1);
    assert_eq!(json["suggestion"], "Check that the file exists");
    assert!(!json.as_object().unwrap().contains_key("context"));
}

/// Test roundtrip serialization for FfiError.
#[test]
fn ffi_error_roundtrip() {
    let original = FfiError {
        message: "Git operation failed".into(),
        code: ErrorCode::GitError as i32,
        context: Some(ErrorContext {
            command: Some("git push".into()),
            exit_code: Some(128),
            stderr: Some("fatal: rejected".into()),
            stdout: None,
            file_path: None,
            working_dir: Some("/repo".into()),
        }),
        suggestion: Some("Pull before pushing".into()),
    };

    let json = serde_json::to_string(&original).unwrap();
    let parsed: FfiError = serde_json::from_str(&json).unwrap();

    assert_eq!(original, parsed);
}
