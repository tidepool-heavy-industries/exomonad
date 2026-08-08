use exo::review::{Finding, ReviewSystem, Severity};
use exo::roles::ExoRole;
use exo::tools::merge::MergeArgs;
use exo::tools::submit::SubmitBranchArgs;
use exo::tools::tree::TreeArgs;
use exo::tools::verdict::VerdictArgs;
use exo_caps::Branch;
use serde_json::json;

fn assert_roundtrip<T>(val: &T) -> T
where
    T: serde::Serialize + for<'de> serde::Deserialize<'de> + std::fmt::Debug,
{
    let json = serde_json::to_string(val).expect("failed to serialize");
    let back: T = serde_json::from_str(&json).expect("failed to deserialize");
    back
}

#[test]
fn test_exo_role_roundtrip() {
    for role in [
        ExoRole::Root,
        ExoRole::Tl,
        ExoRole::Dev,
        ExoRole::Worker,
        ExoRole::Reviewer,
    ] {
        let back = assert_roundtrip(&role);
        assert_eq!(role, back);
    }

    // Pin wire form
    assert_eq!(serde_json::to_value(ExoRole::Dev).unwrap(), json!("dev"));
}

#[test]
fn test_review_system_roundtrip() {
    let branch = Branch::new("main.dev-0".into()).unwrap();
    let variants = [
        ReviewSystem::Reviewed {
            branch: branch.clone(),
            sha: "abc".into(),
            summary: "ok".into(),
            findings: vec![Finding {
                file: "f".into(),
                line: Some(1),
                severity: Severity::Error,
                body: "b".into(),
                suggestion: Some("s".into()),
            }],
        },
        ReviewSystem::ReviewAborted {
            reason: "timeout".into(),
        },
    ];
    for v in variants {
        let back = assert_roundtrip(&v);
        assert_eq!(v, back);
    }
}

#[test]
fn test_review_system_wire_pinning() {
    let reviewed: ReviewSystem = serde_json::from_str(
        r#"{"type":"reviewed","branch":"b","sha":"s","summary":"ok","findings":[]}"#,
    )
    .unwrap();
    assert!(matches!(reviewed, ReviewSystem::Reviewed { .. }));
}

#[test]
fn test_submit_branch_args_roundtrip() {
    let args = SubmitBranchArgs {
        note: "my work".into(),
        ..Default::default()
    };
    let back = assert_roundtrip(&args);
    assert_eq!(args.note, back.note);
    assert_eq!(
        args.dangerously_skip_reviewer,
        back.dangerously_skip_reviewer
    );
}

#[test]
fn test_merge_args_roundtrip() {
    let args = MergeArgs {
        branch: "b1".into(),
        child: Some("c1".into()),
        gate: None,
    };
    let back = assert_roundtrip(&args);
    assert_eq!(args.branch, back.branch);
    assert_eq!(args.child, back.child);
}

#[test]
fn test_verdict_args_roundtrip() {
    let args = VerdictArgs {
        branch: "b".into(),
        sha: "s".into(),
        summary: "ok".into(),
        findings: vec![],
    };
    let back = assert_roundtrip(&args);
    assert_eq!(args.branch, back.branch);
    assert_eq!(args.sha, back.sha);
    assert_eq!(args.summary, back.summary);
}

#[test]
fn test_tree_args_defaults_to_all_false_from_empty_object() {
    let args: TreeArgs = serde_json::from_str("{}").unwrap();
    assert!(!args.all);
}

#[test]
fn test_tree_args_roundtrip() {
    let args = TreeArgs { all: true };
    let back = assert_roundtrip(&args);
    assert_eq!(args.all, back.all);
}
