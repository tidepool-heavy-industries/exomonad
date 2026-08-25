//! `request_review` tool — flip a child's review gate ON for its next `submit_branch`.
//!
//! Unlike `amend_boundary` (parent-side bookkeeping only), this tool writes the CHILD'S OWN
//! papers (`.exo/worktrees/{child}/.exo/node.json`, relative to THIS node's own worktree) —
//! parent-authored infra that the child's `submit_branch` reads at call time to decide whether
//! to spawn a reviewer. One-way: flips review ON only, never back off (see [`RequestReview`]).

use crate::tools::broadcast::read_own_children;
use crate::tools::messaging::wake_note;
use exo_caps::{
    fold_children, Addressee, AgentName, Bus, CapError, CapResult, ChildState, Fs, Message,
    MessageBody, MessageKind, NodePapers, Summary,
};
use schemars::JsonSchema;
use serde::Deserialize;
use std::path::PathBuf;

use exo_framework::{Tool, ToolOutput};

/// Where a worktree child's own birth papers live, relative to the SPAWNING node's cwd — the
/// same layout `spawn_dev`/`fork_wave` create the child's worktree under.
fn child_papers_path(child: &AgentName) -> PathBuf {
    PathBuf::from(format!(".exo/worktrees/{}/.exo/node.json", child.as_str()))
}

/// Arguments for the `request_review` tool.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct RequestReviewArgs {
    /// The child (by name) whose review gate to enable.
    pub child: String,
}

/// The `request_review` tool.
pub struct RequestReview;

#[async_trait::async_trait]
impl<R: Fs + Bus + Send + Sync> Tool<R> for RequestReview {
    const NAME: &'static str = "request_review";
    const DESCRIPTION: &'static str =
        "Turn a child's review gate ON for its NEXT `submit_branch` — a mid-flight flip for a \
         child that was spawned with reviews off (or inheriting them off). ONE-WAY: this can \
         only enable review, never disable it. Refuses on an unknown or terminal \
         (reaped/died) child, naming the state. Idempotent: flipping an already-on gate is a \
         no-op success, reported as such.";
    type Args = RequestReviewArgs;

    async fn run(ctx: &R, args: RequestReviewArgs) -> CapResult<ToolOutput> {
        let name = AgentName::new(args.child)?;

        let records = read_own_children(ctx).await;
        let children = fold_children(&records);
        let child = children.get(&name).ok_or_else(|| {
            CapError::invalid(
                "request_review",
                format!("no child named `{}` in this node's ledger", name.as_str()),
            )
        })?;
        if child.state.is_terminal() {
            let state = match &child.state {
                ChildState::Reaped => "reaped",
                ChildState::Died => "died",
                ChildState::Live | ChildState::Submitted { .. } => {
                    unreachable!("is_terminal implies Reaped or Died")
                }
            };
            return Err(CapError::invalid(
                "request_review",
                format!("child `{}` is {state} — nothing to flip", name.as_str()),
            ));
        }

        let path = child_papers_path(&name);
        let bytes = ctx.read(&path).await.map_err(|e| {
            CapError::invalid(
                "request_review",
                format!(
                    "could not read {}: {e} — is `{}` a worktree child that has finished \
                     booting?",
                    path.display(),
                    name.as_str()
                ),
            )
        })?;
        let mut papers: NodePapers =
            serde_json::from_slice(&bytes).map_err(|e| CapError::Json {
                context: format!("{}", path.display()),
                source: e,
            })?;

        let already_on = papers.review_enabled;
        papers.review_enabled = true;
        let out_bytes = serde_json::to_vec(&papers).map_err(|e| CapError::Json {
            context: format!("{}", path.display()),
            source: e,
        })?;
        ctx.write_atomic(&path, &out_bytes).await?;

        let to = Addressee::Child(name.clone());
        let msg = Message {
            text: MessageBody::new(
                "review gate enabled by your parent — your next submit_branch will spawn a \
                 reviewer"
                    .to_string(),
            )?,
            summary: Summary::new("review enabled".to_string())?,
            kind: MessageKind::Chat,
            reply_to: None,
        };
        ctx.deliver(to.clone(), msg).await?;

        let mut text = if already_on {
            format!("review already enabled for {} (no change)", name.as_str())
        } else {
            format!("review enabled for {}", name.as_str())
        };
        if let Some(note) = wake_note(ctx.wake_status(&to).await) {
            text.push('\n');
            text.push_str(note);
        }

        Ok(ToolOutput::text(text))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_caps::{ChildKind, ChildRecord, InboxPath, PaneId};

    fn spawned_line(name: &str) -> String {
        serde_json::to_string(&ChildRecord::Spawned {
            child: AgentName::new(name.into()).unwrap(),
            kind: ChildKind::Worktree,
            pane: PaneId::new("%1".into()).unwrap(),
            inbox: InboxPath::new("/tmp/x.jsonl".into()),
            model_label: None,
            model: None,
            directives_hash: None,
        })
        .unwrap()
    }

    fn seed_children(mock: &MockRuntime, lines: Vec<String>) {
        mock.files.lock().unwrap().insert(
            ".exo/children.jsonl".to_string(),
            lines.join("\n").into_bytes(),
        );
    }

    fn seed_papers(mock: &MockRuntime, child: &str, review_enabled: bool) {
        let json = format!(
            r#"{{"path":["root","{child}"],"branch":"root.{child}","role":"dev","pane":"%2",
                "parent_inbox":null,"review_enabled":{review_enabled}}}"#
        );
        mock.files.lock().unwrap().insert(
            format!(".exo/worktrees/{child}/.exo/node.json"),
            json.into_bytes(),
        );
    }

    #[tokio::test]
    async fn flips_the_papers_field_and_sends_the_message() {
        let mock = MockRuntime::default();
        seed_children(&mock, vec![spawned_line("dev-1")]);
        seed_papers(&mock, "dev-1", false);

        let out = RequestReview::run(
            &mock,
            RequestReviewArgs {
                child: "dev-1".into(),
            },
        )
        .await
        .unwrap();
        assert_eq!(out.text, "review enabled for dev-1");

        let bytes = mock
            .files
            .lock()
            .unwrap()
            .get(".exo/worktrees/dev-1/.exo/node.json")
            .cloned()
            .unwrap();
        let papers: NodePapers = serde_json::from_slice(&bytes).unwrap();
        assert!(papers.review_enabled);

        let calls = mock.calls_made();
        assert!(calls.iter().any(|c| matches!(
            c,
            Call::BusDeliver { to, msg }
                if to == &Addressee::Child(AgentName::new("dev-1".into()).unwrap())
                    && msg.text.as_str().contains("review gate enabled")
        )));
    }

    #[tokio::test]
    async fn already_on_is_idempotent_ok() {
        let mock = MockRuntime::default();
        seed_children(&mock, vec![spawned_line("dev-1")]);
        seed_papers(&mock, "dev-1", true);

        let out = RequestReview::run(
            &mock,
            RequestReviewArgs {
                child: "dev-1".into(),
            },
        )
        .await
        .unwrap();
        assert_eq!(out.text, "review already enabled for dev-1 (no change)");
    }

    #[tokio::test]
    async fn unknown_child_is_refused() {
        let mock = MockRuntime::default();
        let err = RequestReview::run(
            &mock,
            RequestReviewArgs {
                child: "ghost".into(),
            },
        )
        .await
        .unwrap_err();
        assert!(err.to_string().contains("no child named `ghost`"));
    }

    #[tokio::test]
    async fn terminal_child_is_refused_naming_the_state() {
        let mock = MockRuntime::default();
        seed_children(
            &mock,
            vec![
                spawned_line("dev-1"),
                serde_json::to_string(&ChildRecord::Died {
                    child: AgentName::new("dev-1".into()).unwrap(),
                    pane: PaneId::new("%1".into()).unwrap(),
                    at: None,
                })
                .unwrap(),
            ],
        );

        let err = RequestReview::run(
            &mock,
            RequestReviewArgs {
                child: "dev-1".into(),
            },
        )
        .await
        .unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("dev-1"));
        assert!(msg.contains("died"));
    }

    #[tokio::test]
    async fn corrupt_papers_is_a_loud_error() {
        let mock = MockRuntime::default();
        seed_children(&mock, vec![spawned_line("dev-1")]);
        mock.files.lock().unwrap().insert(
            ".exo/worktrees/dev-1/.exo/node.json".to_string(),
            b"not valid json".to_vec(),
        );

        assert!(RequestReview::run(
            &mock,
            RequestReviewArgs {
                child: "dev-1".into(),
            },
        )
        .await
        .is_err());
    }

    #[tokio::test]
    async fn missing_papers_file_is_a_loud_error() {
        let mock = MockRuntime::default();
        seed_children(&mock, vec![spawned_line("dev-1")]);

        assert!(RequestReview::run(
            &mock,
            RequestReviewArgs {
                child: "dev-1".into(),
            },
        )
        .await
        .is_err());
    }

    #[tokio::test]
    async fn submitted_child_is_not_terminal_and_can_be_flipped() {
        let mock = MockRuntime::default();
        seed_children(
            &mock,
            vec![
                spawned_line("dev-1"),
                serde_json::to_string(&ChildRecord::Submitted {
                    child: AgentName::new("dev-1".into()).unwrap(),
                    branch: exo_caps::Branch::new("root.dev-1".into()).unwrap(),
                    sha: "deadbeef".into(),
                    reviewed: false,
                    at: None,
                })
                .unwrap(),
            ],
        );
        seed_papers(&mock, "dev-1", false);

        assert!(RequestReview::run(
            &mock,
            RequestReviewArgs {
                child: "dev-1".into(),
            },
        )
        .await
        .is_ok());
    }
}
