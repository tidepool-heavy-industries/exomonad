use exo::roles::ExoRole;
use exo::review::ReviewSystem;
use exo::tools::submit::SubmitBranchArgs;
use exo::tools::merge::MergeArgs;
use exo::tools::verdict::{VerdictArgs, Decision};
use exo::tools::tree::TreeArgs;
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
    assert_eq!(serde_json::to_value(&ExoRole::Dev).unwrap(), json!("dev"));
}

#[test]
fn test_review_system_roundtrip() {
    let branch = Branch::new("main.dev-0".into()).unwrap();
    let variants = [
        ReviewSystem::ReviewApproved {
            branch: branch.clone(),
            sha: "abc".into(),
        },
        ReviewSystem::ReviewDenied {
            branch: branch.clone(),
            sha: "abc".into(),
            message: "fix".into(),
        },
        ReviewSystem::ReviewChanges {
            branch: branch.clone(),
            sha: "abc".into(),
            changes_branch: Branch::new("rev.patch".into()).unwrap(),
            message: "fixed".into(),
        },
        ReviewSystem::ReviewAborted { reason: "timeout".into() },
    ];
    for v in variants {
        let back = assert_roundtrip(&v);
        assert_eq!(v, back);
    }
}

#[test]
fn test_review_system_wire_pinning() {
    let approved: ReviewSystem = serde_json::from_str(r#"{"type":"review_approved","branch":"b","sha":"s"}"#).unwrap();
    assert!(matches!(approved, ReviewSystem::ReviewApproved { .. }));

    let denied: ReviewSystem = serde_json::from_str(r#"{"type":"review_denied","branch":"b","sha":"s","message":"m"}"#).unwrap();
    assert!(matches!(denied, ReviewSystem::ReviewDenied { .. }));
}

#[test]
fn test_submit_branch_args_roundtrip() {
    let args = SubmitBranchArgs {
        note: "my work".into(),
        dangerously_skip_reviewer: false,
    };
    let back = assert_roundtrip(&args);
    assert_eq!(args.note, back.note);
    assert_eq!(args.dangerously_skip_reviewer, back.dangerously_skip_reviewer);
}

#[test]
fn test_merge_args_roundtrip() {
    let args = MergeArgs {
        branch: "b1".into(),
        child: Some("c1".into()),
    };
    let back = assert_roundtrip(&args);
    assert_eq!(args.branch, back.branch);
    assert_eq!(args.child, back.child);
}

#[test]
fn test_verdict_args_roundtrip() {
    let args = VerdictArgs {
        decision: Decision::Approve,
        branch: "b".into(),
        sha: "s".into(),
        message: "ok".into(),
        changes_branch: None,
    };
    let back = assert_roundtrip(&args);
    assert!(matches!(back.decision, Decision::Approve));
    assert_eq!(args.branch, back.branch);
    assert_eq!(args.sha, back.sha);
    assert_eq!(args.message, back.message);
}

#[test]
fn test_tree_args_roundtrip() {
    let args = TreeArgs {};
    let _back = assert_roundtrip(&args);
}
