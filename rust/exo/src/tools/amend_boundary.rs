//! `amend_boundary` tool — fix a wrong file boundary recorded for a child at spawn time.
//!
//! Parent-side bookkeeping ONLY: reads/writes `.exo/boundaries/{child}.json` in THIS node's own
//! worktree (never the child's — see `rust/exo/CLAUDE.md` § Fold-time file boundary). `merge`
//! reads that file fresh at fold time, so an amendment takes effect with zero merge-side changes.
//! Amends an EXISTING list only: a child spawned without `file_boundary` has no boundary file to
//! amend, and this tool refuses rather than authoring enforcement that was never there.

use crate::boundary::{boundary_path, read_boundary, FileBoundary};
use crate::tools::broadcast::read_own_children;
use crate::tools::messaging::wake_note;
use exo_caps::{
    fold_children, Addressee, AgentName, Bus, CapError, CapResult, ChildState, Fs, Message,
    MessageBody, MessageKind, Summary,
};
use schemars::JsonSchema;
use serde::Deserialize;
use serde_json::json;

use exo_framework::{Tool, ToolOutput};

/// Arguments for the `amend_boundary` tool.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct AmendBoundaryArgs {
    /// The child (by name) whose recorded file boundary to amend.
    pub child: String,
    /// The new allowed-paths list — a FULL REPLACE of the old one, not a merge. Must be
    /// non-empty (see the tool description for the empty-list case).
    pub allowed: Vec<String>,
}

/// The `amend_boundary` tool.
pub struct AmendBoundary;

#[async_trait::async_trait]
impl<R: Fs + Bus + Send + Sync> Tool<R> for AmendBoundary {
    const NAME: &'static str = "amend_boundary";
    const DESCRIPTION: &'static str =
        "Fix a wrong file boundary recorded for a child at spawn time — a full replace of its \
         allowed-paths list. PARENT-SIDE bookkeeping only (`.exo/boundaries/{child}.json` in \
         YOUR OWN worktree, never the child's); `merge` reads the file fresh at fold time, so \
         the amendment takes effect with no other changes needed. Amends an EXISTING boundary \
         only — refuses loudly if the child was never spawned with a `file_boundary` in the \
         first place (this tool fixes a recorded list, it does not add enforcement after the \
         fact). `allowed` must be non-empty — for the rare case of genuinely wanting a child to \
         touch nothing, use `merge`'s `boundary_override` at fold time instead. Refuses on an \
         unknown or terminal (reaped/died) child, naming the state.";
    type Args = AmendBoundaryArgs;

    async fn run(ctx: &R, args: AmendBoundaryArgs) -> CapResult<ToolOutput> {
        let name = AgentName::new(args.child)?;

        let records = read_own_children(ctx).await;
        let children = fold_children(&records);
        let child = children.get(&name).ok_or_else(|| {
            CapError::invalid(
                "amend_boundary",
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
                "amend_boundary",
                format!("child `{}` is {state} — nothing to amend", name.as_str()),
            ));
        }

        if args.allowed.is_empty() {
            return Err(CapError::invalid(
                "amend_boundary",
                "allowed must be non-empty — an empty list would mean \"touch nothing\", almost \
                 certainly a mistake; for the rare case of genuinely wanting that, use merge's \
                 boundary_override at fold time instead",
            ));
        }

        let existing = read_boundary(ctx, &name).await?.ok_or_else(|| {
            CapError::invalid(
                "amend_boundary",
                format!(
                    "no boundary was recorded for {} at spawn; amend fixes an existing list, it \
                     does not add enforcement after the fact",
                    name.as_str()
                ),
            )
        })?;

        let new_boundary = FileBoundary {
            allowed: args.allowed,
        };
        let path = boundary_path(name.as_str());
        let bytes = serde_json::to_vec(&new_boundary).map_err(|e| CapError::Json {
            context: format!("{}", path.display()),
            source: e,
        })?;
        ctx.write_atomic(&path, &bytes).await?;

        let to = Addressee::Child(name.clone());
        let msg = Message {
            text: MessageBody::new(format!(
                "your recorded file boundary was amended by your parent — allowed paths are \
                 now: {}",
                new_boundary.allowed.join(", ")
            ))?,
            summary: Summary::new("boundary amended".to_string())?,
            kind: MessageKind::Chat,
            reply_to: None,
        };
        ctx.deliver(to.clone(), msg).await?;

        let mut text = format!(
            "amended boundary for {}: [{}] -> [{}]",
            name.as_str(),
            existing.allowed.join(", "),
            new_boundary.allowed.join(", ")
        );
        if let Some(note) = wake_note(ctx.wake_status(&to).await) {
            text.push('\n');
            text.push_str(note);
        }

        Ok(ToolOutput::with_data(
            text,
            json!({
                "child": name.as_str(),
                "old": existing.allowed,
                "new": new_boundary.allowed,
            }),
        ))
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

    fn seed_boundary(mock: &MockRuntime, child: &str, allowed: Vec<&str>) {
        mock.files.lock().unwrap().insert(
            boundary_path(child).display().to_string(),
            serde_json::to_vec(&FileBoundary {
                allowed: allowed.into_iter().map(str::to_string).collect(),
            })
            .unwrap(),
        );
    }

    #[tokio::test]
    async fn replaces_the_list_and_notifies() {
        let mock = MockRuntime::default();
        seed_children(&mock, vec![spawned_line("dev-1")]);
        seed_boundary(&mock, "dev-1", vec!["rust/exo/src/tools/spawn.rs"]);

        let out = AmendBoundary::run(
            &mock,
            AmendBoundaryArgs {
                child: "dev-1".into(),
                allowed: vec!["rust/exo/src/tools".into()],
            },
        )
        .await
        .unwrap();
        assert!(out.text.contains("rust/exo/src/tools/spawn.rs"));
        assert!(out.text.contains("rust/exo/src/tools"));

        let bytes = mock
            .files
            .lock()
            .unwrap()
            .get(&boundary_path("dev-1").display().to_string())
            .cloned()
            .unwrap();
        let boundary: FileBoundary = serde_json::from_slice(&bytes).unwrap();
        assert_eq!(boundary.allowed, vec!["rust/exo/src/tools"]);

        let data = out.data.unwrap();
        assert_eq!(data["old"][0], "rust/exo/src/tools/spawn.rs");
        assert_eq!(data["new"][0], "rust/exo/src/tools");

        let calls = mock.calls_made();
        assert!(calls.iter().any(|c| matches!(
            c,
            Call::BusDeliver { to, msg }
                if to == &Addressee::Child(AgentName::new("dev-1".into()).unwrap())
                    && msg.text.as_str().contains("boundary was amended")
        )));
    }

    #[tokio::test]
    async fn refuses_unknown_child() {
        let mock = MockRuntime::default();
        let err = AmendBoundary::run(
            &mock,
            AmendBoundaryArgs {
                child: "ghost".into(),
                allowed: vec!["a".into()],
            },
        )
        .await
        .unwrap_err();
        assert!(err.to_string().contains("no child named `ghost`"));
    }

    #[tokio::test]
    async fn refuses_terminal_child() {
        let mock = MockRuntime::default();
        seed_children(
            &mock,
            vec![
                spawned_line("dev-1"),
                serde_json::to_string(&ChildRecord::Reaped {
                    child: AgentName::new("dev-1".into()).unwrap(),
                    at: None,
                })
                .unwrap(),
            ],
        );
        seed_boundary(&mock, "dev-1", vec!["a"]);

        let err = AmendBoundary::run(
            &mock,
            AmendBoundaryArgs {
                child: "dev-1".into(),
                allowed: vec!["b".into()],
            },
        )
        .await
        .unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("dev-1"));
        assert!(msg.contains("reaped"));
    }

    #[tokio::test]
    async fn refuses_empty_allowed() {
        let mock = MockRuntime::default();
        seed_children(&mock, vec![spawned_line("dev-1")]);
        seed_boundary(&mock, "dev-1", vec!["a"]);

        let err = AmendBoundary::run(
            &mock,
            AmendBoundaryArgs {
                child: "dev-1".into(),
                allowed: vec![],
            },
        )
        .await
        .unwrap_err();
        assert!(err.to_string().contains("boundary_override"));
    }

    #[tokio::test]
    async fn refuses_missing_prior_boundary() {
        let mock = MockRuntime::default();
        seed_children(&mock, vec![spawned_line("dev-1")]);

        let err = AmendBoundary::run(
            &mock,
            AmendBoundaryArgs {
                child: "dev-1".into(),
                allowed: vec!["a".into()],
            },
        )
        .await
        .unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("no boundary was recorded"));
        assert!(msg.contains("does not add enforcement"));
    }

    #[tokio::test]
    async fn write_failure_surfaces_as_error() {
        let mock = MockRuntime::failing("write_atomic");
        seed_children(&mock, vec![spawned_line("dev-1")]);
        seed_boundary(&mock, "dev-1", vec!["a"]);

        let res = AmendBoundary::run(
            &mock,
            AmendBoundaryArgs {
                child: "dev-1".into(),
                allowed: vec!["b".into()],
            },
        )
        .await;
        assert!(res.is_err());
    }

    #[tokio::test]
    async fn submitted_child_can_be_amended() {
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
        seed_boundary(&mock, "dev-1", vec!["a"]);

        assert!(AmendBoundary::run(
            &mock,
            AmendBoundaryArgs {
                child: "dev-1".into(),
                allowed: vec!["b".into()],
            },
        )
        .await
        .is_ok());
    }
}
