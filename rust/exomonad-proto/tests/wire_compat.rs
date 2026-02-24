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

/// Test that ErrorContext serializes correctly, omitting empty/default fields.
#[test]
fn error_context_json_format() {
    let ctx = ErrorContext {
        command: "git status".into(),
        exit_code: 1,
        stderr: "error: not a git repository".into(),
        stdout: String::new(),
        file_path: String::new(),
        working_dir: "/tmp/test".into(),
    };

    let json = serde_json::to_value(&ctx).unwrap();

    // Verify fields that are set
    assert_eq!(json["command"], "git status");
    assert_eq!(json["exit_code"], 1);
    assert_eq!(json["stderr"], "error: not a git repository");
    assert_eq!(json["working_dir"], "/tmp/test");

    // Verify empty fields are omitted (skip_serializing_if)
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
        suggestion: "Check that the file exists".into(),
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
            command: "git push".into(),
            exit_code: 128,
            stderr: "fatal: rejected".into(),
            stdout: String::new(),
            file_path: String::new(),
            working_dir: "/repo".into(),
        }),
        suggestion: "Pull before pushing".into(),
    };

    let json = serde_json::to_string(&original).unwrap();
    let parsed: FfiError = serde_json::from_str(&json).unwrap();

    assert_eq!(original, parsed);
}

// ============================================================================
// Protobuf binary roundtrip tests (effects system)
// ============================================================================
//
// These tests verify that protobuf binary encoding via prost roundtrips
// correctly. The hex bytes from these tests serve as cross-language reference
// values for the Haskell proto3-suite decoder tests.

#[cfg(feature = "effects")]
mod binary {
    use exomonad_proto::effects::error::{
        self as proto_error, EffectEnvelope, EffectError, EffectResponse,
        effect_response::Result as ResponseResult,
    };
    use prost::Message;

    // ========================================================================
    // Envelope
    // ========================================================================

    #[test]
    fn effect_envelope_binary_roundtrip() {
        let envelope = EffectEnvelope {
            effect_type: "git.get_branch".into(),
            payload: vec![10, 1, 46], // GetBranchRequest { working_dir: "." }
        };

        let bytes = envelope.encode_to_vec();
        let decoded = EffectEnvelope::decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded.effect_type, "git.get_branch");
        assert_eq!(decoded.payload, vec![10, 1, 46]);
    }

    // ========================================================================
    // EffectResponse: payload variants
    // ========================================================================

    #[test]
    fn effect_response_payload_binary() {
        let inner = b"response-payload-bytes";
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(inner.to_vec())),
        };

        let bytes = response.encode_to_vec();
        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();

        match decoded.result {
            Some(ResponseResult::Payload(p)) => assert_eq!(p, inner),
            other => panic!("Expected Payload, got {:?}", other),
        }
    }

    #[test]
    fn effect_response_error_binary() {
        let response = EffectResponse {
            result: Some(ResponseResult::Error(EffectError {
                kind: Some(proto_error::effect_error::Kind::NotFound(
                    proto_error::NotFound {
                        resource: "handler/unknown".into(),
                    },
                )),
            })),
        };

        let bytes = response.encode_to_vec();
        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();

        match decoded.result {
            Some(ResponseResult::Error(e)) => match e.kind {
                Some(proto_error::effect_error::Kind::NotFound(nf)) => {
                    assert_eq!(nf.resource, "handler/unknown");
                }
                other => panic!("Expected NotFound, got {:?}", other),
            },
            other => panic!("Expected Error, got {:?}", other),
        }
    }

    // ========================================================================
    // Agent effects
    // ========================================================================

    #[test]
    fn spawn_request_binary_roundtrip() {
        use exomonad_proto::effects::agent::SpawnRequest;

        let req = SpawnRequest {
            issue: "433".into(),
            owner: "anthropics".into(),
            repo: "exomonad".into(),
            agent_type: 1, // CLAUDE
            role: 1,       // DEV
            worktree_dir: ".exo/worktrees".into(),
            subrepo: String::new(),
            topology: 0,
            base_branch: String::new(),
            context: String::new(),
        };

        let bytes = req.encode_to_vec();
        let decoded = SpawnRequest::decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded.issue, "433");
        assert_eq!(decoded.owner, "anthropics");
        assert_eq!(decoded.repo, "exomonad");
        assert_eq!(decoded.agent_type, 1);
        assert_eq!(decoded.role, 1);
        assert_eq!(decoded.worktree_dir, ".exo/worktrees");
    }

    #[test]
    fn spawn_response_binary_roundtrip() {
        use exomonad_proto::effects::agent::{AgentInfo, SpawnResponse};

        let resp = SpawnResponse {
            agent: Some(AgentInfo {
                id: "gh-433-claude".into(),
                issue: "433".into(),
                worktree_path: "/tmp/worktrees/gh-433-fix-build-claude".into(),
                branch_name: "gh-433/fix-build".into(),
                agent_type: 1, // CLAUDE
                role: 1,       // DEV
                status: 1,     // RUNNING
                zellij_tab: "433-fix-build".into(),
                error: String::new(),
                pr_number: 0,
                pr_url: String::new(),
                topology: 0,
            }),
        };

        let bytes = resp.encode_to_vec();
        let decoded = SpawnResponse::decode(bytes.as_slice()).unwrap();

        let agent = decoded.agent.unwrap();
        assert_eq!(agent.id, "gh-433-claude");
        assert_eq!(agent.issue, "433");
        assert_eq!(
            agent.worktree_path,
            "/tmp/worktrees/gh-433-fix-build-claude"
        );
        assert_eq!(agent.branch_name, "gh-433/fix-build");
        assert_eq!(agent.agent_type, 1);
        assert_eq!(agent.role, 1);
        assert_eq!(agent.status, 1);
        assert_eq!(agent.zellij_tab, "433-fix-build");
    }

    #[test]
    fn spawn_batch_response_binary() {
        use exomonad_proto::effects::agent::{AgentInfo, SpawnBatchResponse};

        let resp = SpawnBatchResponse {
            agents: vec![
                AgentInfo {
                    id: "gh-1-claude".into(),
                    issue: "1".into(),
                    worktree_path: "/w/1".into(),
                    branch_name: "gh-1/a".into(),
                    agent_type: 1,
                    role: 1,
                    status: 1,
                    zellij_tab: "1-a".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 0,
                },
                AgentInfo {
                    id: "gh-2-gemini".into(),
                    issue: "2".into(),
                    worktree_path: "/w/2".into(),
                    branch_name: "gh-2/b".into(),
                    agent_type: 2, // GEMINI
                    role: 1,
                    status: 1,
                    zellij_tab: "2-b".into(),
                    error: String::new(),
                    pr_number: 0,
                    pr_url: String::new(),
                    topology: 0,
                },
            ],
            errors: vec!["issue 3: not found".into()],
        };

        let bytes = resp.encode_to_vec();
        let decoded = SpawnBatchResponse::decode(bytes.as_slice()).unwrap();

        assert_eq!(decoded.agents.len(), 2);
        assert_eq!(decoded.agents[0].id, "gh-1-claude");
        assert_eq!(decoded.agents[1].agent_type, 2);
        assert_eq!(decoded.errors, vec!["issue 3: not found"]);
    }

    #[test]
    fn list_response_binary_roundtrip() {
        use exomonad_proto::effects::agent::{AgentInfo, ListResponse};

        let resp = ListResponse {
            agents: vec![AgentInfo {
                id: "gh-10-claude".into(),
                issue: "10".into(),
                worktree_path: "/worktrees/10".into(),
                branch_name: "gh-10/feature".into(),
                agent_type: 1,
                role: 2,   // TL
                status: 2, // STOPPED
                zellij_tab: String::new(),
                error: String::new(),
                pr_number: 42,
                pr_url: "https://github.com/org/repo/pull/42".into(),
                topology: 0,
            }],
        };

        let bytes = resp.encode_to_vec();
        let decoded = ListResponse::decode(bytes.as_slice()).unwrap();

        let agent = &decoded.agents[0];
        assert_eq!(agent.pr_number, 42);
        assert_eq!(agent.pr_url, "https://github.com/org/repo/pull/42");
        assert_eq!(agent.status, 2);
    }

    #[test]
    fn cleanup_response_binary_roundtrip() {
        use exomonad_proto::effects::agent::CleanupResponse;

        let resp = CleanupResponse {
            success: true,
            error: String::new(),
        };

        let bytes = resp.encode_to_vec();
        let decoded = CleanupResponse::decode(bytes.as_slice()).unwrap();

        assert!(decoded.success);
        assert!(decoded.error.is_empty());
    }

    // ========================================================================
    // Git effects
    // ========================================================================

    #[test]
    fn git_get_branch_binary_roundtrip() {
        use exomonad_proto::effects::git::{GetBranchRequest, GetBranchResponse};

        let req = GetBranchRequest {
            working_dir: ".".into(),
        };
        let bytes = req.encode_to_vec();
        let decoded = GetBranchRequest::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.working_dir, ".");

        let resp = GetBranchResponse {
            branch: "main".into(),
            detached: false,
        };
        let bytes = resp.encode_to_vec();
        let decoded = GetBranchResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.branch, "main");
        assert!(!decoded.detached);
    }

    // ========================================================================
    // GitHub effects
    // ========================================================================

    #[test]
    fn github_get_issue_binary_roundtrip() {
        use exomonad_proto::effects::github::{GetIssueRequest, GetIssueResponse, Issue};

        let req = GetIssueRequest {
            owner: "anthropics".into(),
            repo: "exomonad".into(),
            number: 433,
            include_comments: true,
        };
        let bytes = req.encode_to_vec();
        let decoded = GetIssueRequest::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.number, 433);
        assert!(decoded.include_comments);

        let resp = GetIssueResponse {
            issue: Some(Issue {
                number: 433,
                title: "Fix FFI decode".into(),
                body: "The EffectResponse decode fails".into(),
                state: 1, // OPEN
                author: None,
                labels: vec![],
                created_at: 1700000000,
                updated_at: 1700001000,
                comments_count: 3,
            }),
            comments: vec![],
        };
        let bytes = resp.encode_to_vec();
        let decoded = GetIssueResponse::decode(bytes.as_slice()).unwrap();
        let issue = decoded.issue.unwrap();
        assert_eq!(issue.number, 433);
        assert_eq!(issue.title, "Fix FFI decode");
    }

    // ========================================================================
    // Log effects
    // ========================================================================

    #[test]
    fn log_info_binary_roundtrip() {
        use exomonad_proto::effects::log::{InfoRequest, LogResponse};

        let req = InfoRequest {
            message: "Agent started".into(),
            fields: b"{\"issue\":433}".to_vec(),
        };
        let bytes = req.encode_to_vec();
        let decoded = InfoRequest::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.message, "Agent started");
        assert_eq!(decoded.fields, b"{\"issue\":433}");

        let resp = LogResponse { success: true };
        let bytes = resp.encode_to_vec();
        let decoded = LogResponse::decode(bytes.as_slice()).unwrap();
        assert!(decoded.success);
    }

    // ========================================================================
    // Filesystem effects
    // ========================================================================

    #[test]
    fn fs_read_file_binary_roundtrip() {
        use exomonad_proto::effects::fs::{ReadFileRequest, ReadFileResponse};

        let req = ReadFileRequest {
            path: "/tmp/test.txt".into(),
            max_bytes: 1024,
            offset: 0,
        };
        let bytes = req.encode_to_vec();
        let decoded = ReadFileRequest::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.path, "/tmp/test.txt");
        assert_eq!(decoded.max_bytes, 1024);

        let resp = ReadFileResponse {
            content: "hello world".into(),
            bytes_read: 11,
            truncated: false,
            total_size: 11,
        };
        let bytes = resp.encode_to_vec();
        let decoded = ReadFileResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.content, "hello world");
        assert_eq!(decoded.bytes_read, 11);
    }

    // ========================================================================
    // Varint boundary edge cases
    // ========================================================================

    /// Test that payloads crossing varint length boundaries encode/decode correctly.
    ///
    /// Protobuf varints: 0-127 = 1 byte, 128-16383 = 2 bytes, 16384+ = 3 bytes.
    /// EffectResponse wraps the payload in a LEN field, so the varint encodes
    /// the payload length. If the varint encoding differs between prost and
    /// proto3-suite, payloads near these boundaries would fail.
    #[test]
    fn varint_boundary_127_bytes() {
        // Exactly 127 bytes → 1-byte varint length
        let payload = vec![0x42u8; 127];
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(payload.clone())),
        };
        let bytes = response.encode_to_vec();
        // tag(1 byte) + varint_len(1 byte) + payload(127 bytes) = 129
        assert_eq!(
            bytes.len(),
            129,
            "127-byte payload should produce 129 wire bytes"
        );
        assert_eq!(bytes[0], 0x0a, "Field 1 LEN tag");
        assert_eq!(bytes[1], 127, "1-byte varint for length 127");

        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();
        match decoded.result {
            Some(ResponseResult::Payload(p)) => assert_eq!(p, payload),
            other => panic!("Expected Payload, got {:?}", other),
        }
    }

    #[test]
    fn varint_boundary_128_bytes() {
        // Exactly 128 bytes → 2-byte varint length (first multi-byte varint)
        let payload = vec![0x42u8; 128];
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(payload.clone())),
        };
        let bytes = response.encode_to_vec();
        // tag(1) + varint_len(2) + payload(128) = 131
        assert_eq!(
            bytes.len(),
            131,
            "128-byte payload should produce 131 wire bytes"
        );
        assert_eq!(bytes[0], 0x0a, "Field 1 LEN tag");
        // 128 as varint = [0x80, 0x01]
        assert_eq!(bytes[1], 0x80, "First byte of 2-byte varint");
        assert_eq!(bytes[2], 0x01, "Second byte of 2-byte varint");

        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();
        match decoded.result {
            Some(ResponseResult::Payload(p)) => assert_eq!(p, payload),
            other => panic!("Expected Payload, got {:?}", other),
        }
    }

    #[test]
    fn varint_boundary_16384_bytes() {
        // Exactly 16384 bytes → 3-byte varint length
        let payload = vec![0x42u8; 16384];
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(payload.clone())),
        };
        let bytes = response.encode_to_vec();
        // tag(1) + varint_len(3) + payload(16384) = 16388
        assert_eq!(
            bytes.len(),
            16388,
            "16384-byte payload should produce 16388 wire bytes"
        );
        assert_eq!(bytes[0], 0x0a, "Field 1 LEN tag");
        // 16384 as varint = [0x80, 0x80, 0x01]
        assert_eq!(bytes[1], 0x80, "First byte of 3-byte varint");
        assert_eq!(bytes[2], 0x80, "Second byte of 3-byte varint");
        assert_eq!(bytes[3], 0x01, "Third byte of 3-byte varint");

        let decoded = EffectResponse::decode(bytes.as_slice()).unwrap();
        match decoded.result {
            Some(ResponseResult::Payload(p)) => assert_eq!(p.len(), 16384),
            other => panic!("Expected Payload, got {:?}", other),
        }
    }

    // ========================================================================
    // Comprehensive response type roundtrips (all FFI boundary types)
    // ========================================================================

    #[test]
    fn git_get_status_binary_roundtrip() {
        use exomonad_proto::effects::git::{GetStatusRequest, GetStatusResponse};

        let req = GetStatusRequest {
            working_dir: "/repo".into(),
        };
        let bytes = req.encode_to_vec();
        assert_eq!(
            GetStatusRequest::decode(bytes.as_slice())
                .unwrap()
                .working_dir,
            "/repo"
        );

        let resp = GetStatusResponse {
            dirty_files: vec!["src/main.rs".into(), "Cargo.toml".into()],
            staged_files: vec!["README.md".into()],
            untracked_files: vec![],
        };
        let bytes = resp.encode_to_vec();
        let decoded = GetStatusResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.dirty_files.len(), 2);
        assert_eq!(decoded.staged_files, vec!["README.md"]);
        assert!(decoded.untracked_files.is_empty());
    }

    #[test]
    fn git_get_commits_binary_roundtrip() {
        use exomonad_proto::effects::git::{Commit, GetCommitsResponse};

        let resp = GetCommitsResponse {
            commits: vec![Commit {
                sha: "abc123def456".into(),
                short_sha: "abc123d".into(),
                message: "fix: resolve FFI decode error".into(),
                author: "dev".into(),
                author_email: "dev@example.com".into(),
                timestamp: 1700000000,
            }],
        };
        let bytes = resp.encode_to_vec();
        let decoded = GetCommitsResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.commits[0].sha, "abc123def456");
        assert_eq!(decoded.commits[0].timestamp, 1700000000);
    }

    #[test]
    fn git_has_unpushed_binary_roundtrip() {
        use exomonad_proto::effects::git::HasUnpushedCommitsResponse;

        let resp = HasUnpushedCommitsResponse {
            has_unpushed: true,
            count: 3,
        };
        let bytes = resp.encode_to_vec();
        let decoded = HasUnpushedCommitsResponse::decode(bytes.as_slice()).unwrap();
        assert!(decoded.has_unpushed);
        assert_eq!(decoded.count, 3);
    }

    #[test]
    fn git_get_repo_info_binary_roundtrip() {
        use exomonad_proto::effects::git::GetRepoInfoResponse;

        let resp = GetRepoInfoResponse {
            branch: "main".into(),
            owner: "anthropics".into(),
            name: "exomonad".into(),
        };
        let bytes = resp.encode_to_vec();
        let decoded = GetRepoInfoResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.owner, "anthropics");
        assert_eq!(decoded.name, "exomonad");
    }

    #[test]
    fn github_list_issues_binary_roundtrip() {
        use exomonad_proto::effects::github::{Issue, Label, ListIssuesResponse};

        let resp = ListIssuesResponse {
            issues: vec![Issue {
                number: 1,
                title: "Bug".into(),
                body: "Description".into(),
                state: 1,
                author: None,
                labels: vec![Label {
                    name: "bug".into(),
                    color: "d73a4a".into(),
                    description: "Something isn't working".into(),
                }],
                created_at: 1700000000,
                updated_at: 1700001000,
                comments_count: 0,
            }],
        };
        let bytes = resp.encode_to_vec();
        let decoded = ListIssuesResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.issues[0].title, "Bug");
        assert_eq!(decoded.issues[0].labels[0].name, "bug");
        assert_eq!(decoded.issues[0].labels[0].color, "d73a4a");
    }

    #[test]
    fn agent_cleanup_batch_binary_roundtrip() {
        use exomonad_proto::effects::agent::CleanupBatchResponse;

        let resp = CleanupBatchResponse {
            cleaned: vec!["1".into(), "2".into()],
            failed: vec!["3".into()],
            errors: vec!["issue 3: worktree has changes".into()],
        };
        let bytes = resp.encode_to_vec();
        let decoded = CleanupBatchResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.cleaned, vec!["1", "2"]);
        assert_eq!(decoded.failed, vec!["3"]);
    }

    #[test]
    fn log_emit_event_binary_roundtrip() {
        use exomonad_proto::effects::log::EmitEventResponse;

        let resp = EmitEventResponse {
            event_id: "evt-abc-123".into(),
        };
        let bytes = resp.encode_to_vec();
        let decoded = EmitEventResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.event_id, "evt-abc-123");
    }

    #[test]
    fn fs_write_file_binary_roundtrip() {
        use exomonad_proto::effects::fs::WriteFileResponse;

        let resp = WriteFileResponse {
            bytes_written: 1024,
            path: "/tmp/out.txt".into(),
        };
        let bytes = resp.encode_to_vec();
        let decoded = WriteFileResponse::decode(bytes.as_slice()).unwrap();
        assert_eq!(decoded.bytes_written, 1024);
        assert_eq!(decoded.path, "/tmp/out.txt");
    }

    // ========================================================================
    // Hex byte extraction for cross-language tests
    // ========================================================================

    /// Produces reference hex bytes for the Haskell decode tests.
    ///
    /// Run with `cargo test -p exomonad-proto --features effects -- --nocapture cross_language_hex`
    /// to see the hex output, then paste into Haskell test literals.
    #[test]
    fn cross_language_hex_reference() {
        use exomonad_proto::effects::agent::{AgentInfo, SpawnBatchResponse, SpawnResponse};
        use exomonad_proto::effects::git::GetBranchResponse;
        use exomonad_proto::effects::log::LogResponse;

        println!("\n=== Cross-Language Hex Reference Bytes ===\n");

        // 1. EffectResponse with simple payload
        let simple = EffectResponse {
            result: Some(ResponseResult::Payload(b"hello".to_vec())),
        };
        print_hex("EffectResponse(Payload(hello))", &simple.encode_to_vec());

        // 2. EffectResponse wrapping GetBranchResponse
        let branch_resp = GetBranchResponse {
            branch: "main".into(),
            detached: false,
        };
        let branch_inner = branch_resp.encode_to_vec();
        let branch_wrapped = EffectResponse {
            result: Some(ResponseResult::Payload(branch_inner.clone())),
        };
        print_hex("GetBranchResponse(main)", &branch_inner);
        print_hex(
            "EffectResponse(Payload(GetBranchResponse))",
            &branch_wrapped.encode_to_vec(),
        );

        // 3. EffectResponse wrapping LogResponse
        let log_resp = LogResponse { success: true };
        let log_inner = log_resp.encode_to_vec();
        let log_wrapped = EffectResponse {
            result: Some(ResponseResult::Payload(log_inner.clone())),
        };
        print_hex("LogResponse(true)", &log_inner);
        print_hex(
            "EffectResponse(Payload(LogResponse))",
            &log_wrapped.encode_to_vec(),
        );

        // 4. EffectResponse wrapping SpawnResponse with full AgentInfo
        let spawn_resp = SpawnResponse {
            agent: Some(AgentInfo {
                id: "a1".into(),
                issue: "1".into(),
                worktree_path: "/w".into(),
                branch_name: "b".into(),
                agent_type: 1,
                role: 1,
                status: 1,
                zellij_tab: "t".into(),
                error: String::new(),
                pr_number: 0,
                pr_url: String::new(),
                topology: 0,
            }),
        };
        let spawn_inner = spawn_resp.encode_to_vec();
        let spawn_wrapped = EffectResponse {
            result: Some(ResponseResult::Payload(spawn_inner.clone())),
        };
        print_hex("SpawnResponse(a1)", &spawn_inner);
        print_hex(
            "EffectResponse(Payload(SpawnResponse))",
            &spawn_wrapped.encode_to_vec(),
        );

        // 5. EffectResponse with NotFound error
        let error_resp = EffectResponse {
            result: Some(ResponseResult::Error(EffectError {
                kind: Some(proto_error::effect_error::Kind::NotFound(
                    proto_error::NotFound {
                        resource: "test".into(),
                    },
                )),
            })),
        };
        print_hex(
            "EffectResponse(Error(NotFound(test)))",
            &error_resp.encode_to_vec(),
        );

        // 6. EffectResponse with Custom error (common in agent handler failures)
        let custom_err = EffectResponse {
            result: Some(ResponseResult::Error(EffectError {
                kind: Some(proto_error::effect_error::Kind::Custom(
                    proto_error::Custom {
                        code: "agent_error".into(),
                        message: "spawn timed out".into(),
                        data: vec![],
                    },
                )),
            })),
        };
        print_hex(
            "EffectResponse(Error(Custom(agent_error)))",
            &custom_err.encode_to_vec(),
        );

        // 7. SpawnBatchResponse with agents + errors
        let batch = SpawnBatchResponse {
            agents: vec![AgentInfo {
                id: "gh-1-claude".into(),
                issue: "1".into(),
                worktree_path: "/w/1".into(),
                branch_name: "gh-1/a".into(),
                agent_type: 1,
                role: 1,
                status: 1,
                zellij_tab: "1-a".into(),
                error: String::new(),
                pr_number: 0,
                pr_url: String::new(),
                topology: 0,
            }],
            errors: vec!["issue 2: failed".into()],
        };
        let batch_inner = batch.encode_to_vec();
        let batch_wrapped = EffectResponse {
            result: Some(ResponseResult::Payload(batch_inner.clone())),
        };
        print_hex("SpawnBatchResponse(1 agent, 1 error)", &batch_inner);
        print_hex(
            "EffectResponse(Payload(SpawnBatchResponse))",
            &batch_wrapped.encode_to_vec(),
        );

        // 8. EffectEnvelope
        let envelope = EffectEnvelope {
            effect_type: "agent.spawn".into(),
            payload: vec![10, 1, 49],
        };
        print_hex("EffectEnvelope(agent.spawn)", &envelope.encode_to_vec());

        // 9. Varint boundary: 128-byte payload (2-byte varint)
        let boundary = EffectResponse {
            result: Some(ResponseResult::Payload(vec![0x42; 128])),
        };
        let boundary_bytes = boundary.encode_to_vec();
        print_hex("EffectResponse(Payload(128 bytes))", &boundary_bytes[..5]); // Just first 5 bytes
        println!("  total_len={}", boundary_bytes.len());

        // 10. Large error string in SpawnBatchResponse (reproduces production decode failure)
        // Error string is ~377 bytes, causing 2-byte varint in both inner and outer messages
        let long_error = "Issue 539: git worktree add failed: Preparing worktree (checking out 'gh-539/improve-stop-hook-error-messages-with-specific-com-gemini')\nfatal: '/Users/inannamalick/hangars/exomonad/repo/.exo/worktrees/gh-539-improve-stop-hook-error-messages-with-specific-com-gemini' is a missing but already registered worktree;\nuse 'git worktree prune' to remove stale worktree entries\n";
        let large_batch = SpawnBatchResponse {
            agents: vec![],
            errors: vec![long_error.into()],
        };
        let large_inner = large_batch.encode_to_vec();
        let large_wrapped = EffectResponse {
            result: Some(ResponseResult::Payload(large_inner.clone())),
        };
        let large_bytes = large_wrapped.encode_to_vec();
        print_hex("SpawnBatchResponse(large error)", &large_inner);
        print_hex(
            "EffectResponse(Payload(large SpawnBatchResponse))",
            &large_bytes[..32],
        );
        println!(
            "  inner_len={} outer_len={}",
            large_inner.len(),
            large_bytes.len()
        );

        println!("\n=== End Reference Bytes ===\n");
    }

    /// Verify that a large SpawnBatchResponse (with long error string) roundtrips correctly.
    /// Reproduces the production decode failure where varint parsing failed at offset 349.
    #[test]
    fn spawn_batch_large_error_roundtrip() {
        use exomonad_proto::effects::agent::SpawnBatchResponse;

        let long_error = "Issue 539: git worktree add failed: Preparing worktree (checking out 'gh-539/improve-stop-hook-error-messages-with-specific-com-gemini')\nfatal: '/Users/inannamalick/hangars/exomonad/repo/.exo/worktrees/gh-539-improve-stop-hook-error-messages-with-specific-com-gemini' is a missing but already registered worktree;\nuse 'git worktree prune' to remove stale worktree entries\n";

        let resp = SpawnBatchResponse {
            agents: vec![],
            errors: vec![long_error.into()],
        };

        // Inner roundtrip
        let inner_bytes = resp.encode_to_vec();
        let decoded = SpawnBatchResponse::decode(inner_bytes.as_slice()).unwrap();
        assert_eq!(decoded.errors.len(), 1);
        assert_eq!(decoded.errors[0], long_error);

        // Wrapped in EffectResponse (the actual wire format)
        let wrapped = EffectResponse {
            result: Some(ResponseResult::Payload(inner_bytes)),
        };
        let outer_bytes = wrapped.encode_to_vec();

        let outer_decoded = EffectResponse::decode(outer_bytes.as_slice()).unwrap();
        match outer_decoded.result {
            Some(ResponseResult::Payload(payload)) => {
                let inner_decoded = SpawnBatchResponse::decode(payload.as_slice()).unwrap();
                assert_eq!(inner_decoded.errors[0], long_error);
            }
            other => panic!("Expected Payload, got {:?}", other),
        }
    }

    /// Verify empty payload encoding for cross-language test.
    #[test]
    fn empty_payload_hex_reference() {
        let response = EffectResponse {
            result: Some(ResponseResult::Payload(vec![])),
        };
        let bytes = response.encode_to_vec();
        // Empty bytes in oneof: tag(0x0a) + len(0x00)
        assert_eq!(bytes, vec![0x0a, 0x00], "Empty payload wire bytes");
    }

    fn print_hex(label: &str, bytes: &[u8]) {
        let hex: String = bytes
            .iter()
            .map(|b| format!("0x{:02x}", b))
            .collect::<Vec<_>>()
            .join(", ");
        println!("{label}: [{hex}]");
    }
}

// ============================================================================
// JSON wire format tests (core types with serde)
// ============================================================================

#[cfg(feature = "full")]
mod json_full {
    // ========================================================================
    // Common types (JSON wire format)
    // ========================================================================

    #[test]
    fn role_json_format() {
        use exomonad_proto::common::Role;

        assert_eq!(serde_json::to_string(&Role::Dev).unwrap(), "\"dev\"");
        assert_eq!(serde_json::to_string(&Role::Tl).unwrap(), "\"tl\"");
        assert_eq!(serde_json::to_string(&Role::Pm).unwrap(), "\"pm\"");
        assert_eq!(
            serde_json::to_string(&Role::Reviewer).unwrap(),
            "\"reviewer\""
        );
        assert_eq!(
            serde_json::to_string(&Role::Unspecified).unwrap(),
            "\"unspecified\""
        );
    }

    #[test]
    fn tool_permission_json_format() {
        use exomonad_proto::common::ToolPermission;

        assert_eq!(
            serde_json::to_string(&ToolPermission::Allow).unwrap(),
            "\"allow\""
        );
        assert_eq!(
            serde_json::to_string(&ToolPermission::Deny).unwrap(),
            "\"deny\""
        );
        assert_eq!(
            serde_json::to_string(&ToolPermission::Ask).unwrap(),
            "\"ask\""
        );
        assert_eq!(
            serde_json::to_string(&ToolPermission::Skip).unwrap(),
            "\"skip\""
        );
    }

    #[test]
    fn session_id_json_roundtrip() {
        use exomonad_proto::common::SessionId;

        let original = SessionId {
            value: "sess-abc-123".into(),
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: SessionId = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn git_branch_json_roundtrip() {
        use exomonad_proto::common::GitBranch;

        let original = GitBranch {
            name: "main".into(),
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: GitBranch = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn git_commit_json_roundtrip() {
        use exomonad_proto::common::GitCommit;

        let original = GitCommit {
            sha: "abc123def456".into(),
            message: "fix: resolve bug".into(),
            author: "dev".into(),
            timestamp_unix: 1700000000,
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: GitCommit = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn issue_ref_json_roundtrip() {
        use exomonad_proto::common::IssueRef;

        let original = IssueRef {
            owner: "org".into(),
            repo: "repo".into(),
            number: 42,
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: IssueRef = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    // ========================================================================
    // Hook types (JSON wire format)
    // ========================================================================

    #[test]
    fn hook_type_json_format() {
        use exomonad_proto::hook::HookType;

        assert_eq!(
            serde_json::to_string(&HookType::PreToolUse).unwrap(),
            "\"pre_tool_use\""
        );
        assert_eq!(
            serde_json::to_string(&HookType::SessionStart).unwrap(),
            "\"session_start\""
        );
        assert_eq!(
            serde_json::to_string(&HookType::SubagentStop).unwrap(),
            "\"subagent_stop\""
        );
    }

    #[test]
    fn stop_decision_json_format() {
        use exomonad_proto::hook::StopDecision;

        assert_eq!(
            serde_json::to_string(&StopDecision::Allow).unwrap(),
            "\"allow\""
        );
        assert_eq!(
            serde_json::to_string(&StopDecision::Block).unwrap(),
            "\"block\""
        );
        assert_eq!(
            serde_json::to_string(&StopDecision::Ask).unwrap(),
            "\"ask\""
        );
    }

    #[test]
    fn pre_tool_use_input_json_roundtrip() {
        use exomonad_proto::hook::PreToolUseInput;

        let original = PreToolUseInput {
            tool_name: "Write".into(),
            tool_input: "{}".into(),
            tool_use_id: "tu-1".into(),
            session_id: None,
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: PreToolUseInput = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn hook_output_json_roundtrip() {
        use exomonad_proto::hook::HookOutput;

        let original = HookOutput {
            r#continue: true,
            stop_reason: String::new(),
            suppress_output: false,
            system_message: String::new(),
            output: None,
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: HookOutput = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    // ========================================================================
    // Agent types (exomonad/ — JSON wire format)
    // ========================================================================

    #[test]
    fn agent_type_json_format() {
        use exomonad_proto::agent::AgentType;

        assert_eq!(
            serde_json::to_string(&AgentType::Claude).unwrap(),
            "\"claude\""
        );
        assert_eq!(
            serde_json::to_string(&AgentType::Gemini).unwrap(),
            "\"gemini\""
        );
    }

    #[test]
    fn agent_status_json_format() {
        use exomonad_proto::agent::AgentStatus;

        assert_eq!(
            serde_json::to_string(&AgentStatus::Running).unwrap(),
            "\"running\""
        );
        assert_eq!(
            serde_json::to_string(&AgentStatus::Stopped).unwrap(),
            "\"stopped\""
        );
        assert_eq!(
            serde_json::to_string(&AgentStatus::Failed).unwrap(),
            "\"failed\""
        );
        assert_eq!(
            serde_json::to_string(&AgentStatus::Waiting).unwrap(),
            "\"waiting\""
        );
    }

    #[test]
    fn spawn_options_json_roundtrip() {
        use exomonad_proto::agent::SpawnOptions;

        let original = SpawnOptions {
            agent_type: 1, // CLAUDE
            role: 2,       // TL
            initial_prompt: "implement feature X".into(),
            branch_prefix: "gh-42".into(),
            worktree_name: "feature-x".into(),
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: SpawnOptions = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn agent_info_json_roundtrip() {
        use exomonad_proto::agent::AgentInfo;

        let original = AgentInfo {
            worktree_path: "/tmp/worktrees/feature-x".into(),
            branch_name: "gh-42/feature-x".into(),
            agent_type: 1,
            role: 2,
            status: 1,
            zellij_tab: "42-feature-x".into(),
            error: String::new(),
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: AgentInfo = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn worktree_info_json_roundtrip() {
        use exomonad_proto::agent::WorktreeInfo;

        let original = WorktreeInfo {
            path: "/home/dev/project/.exo/worktrees/feature".into(),
            branch: "main.feature".into(),
            head_commit: "abc123def456".into(),
            is_dirty: true,
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: WorktreeInfo = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    // ========================================================================
    // Popup types (JSON wire format)
    // ========================================================================

    #[test]
    fn popup_definition_json_roundtrip() {
        use exomonad_proto::popup::{PopupComponent, PopupDefinition, TextInput, popup_component};

        let original = PopupDefinition {
            id: "popup-1".into(),
            title: "Enter details".into(),
            components: vec![PopupComponent {
                id: "name-input".into(),
                component: Some(popup_component::Component::TextInput(TextInput {
                    label: "Name".into(),
                    placeholder: "Enter your name".into(),
                    default_value: String::new(),
                    required: true,
                    max_length: 100,
                })),
            }],
            submit_label: "Submit".into(),
            cancel_label: "Cancel".into(),
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: PopupDefinition = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn form_submission_json_roundtrip() {
        use exomonad_proto::popup::FormSubmission;
        use std::collections::HashMap;

        let mut values = HashMap::new();
        values.insert("name-input".to_string(), "Alice".to_string());
        values.insert("email-input".to_string(), "alice@example.com".to_string());

        let original = FormSubmission {
            popup_id: "popup-1".into(),
            values,
            cancelled: false,
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: FormSubmission = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn popup_error_json_roundtrip() {
        use exomonad_proto::popup::PopupError;

        let original = PopupError {
            message: "Invalid input".into(),
            component_id: "name-input".into(),
        };
        let json = serde_json::to_string(&original).unwrap();
        let parsed: PopupError = serde_json::from_str(&json).unwrap();
        assert_eq!(original, parsed);
    }
}
