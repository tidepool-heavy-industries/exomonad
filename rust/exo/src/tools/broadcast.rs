//! `broadcast` tool — flat fan-out of one message to every LIVE direct child, over the same
//! [`Bus`] path [`send_message`](crate::tools::messaging::SendMessage) uses. No role filtering,
//! no fanciness: every live child gets the same text, one `Bus::deliver` call each.

use exo_caps::{
    fold_children, Addressee, Bus, CapError, CapResult, ChildRecord, ChildState, Fs, Message,
    MessageBody, MessageKind, Summary,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::path::Path;

use exo_framework::{Tool, ToolOutput};

use crate::tools::messaging::wake_note;

/// Arguments for the `broadcast` tool.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct BroadcastArgs {
    /// The message body delivered to every live direct child. Must be non-empty.
    pub text: String,
}

/// The `broadcast` tool.
pub struct Broadcast;

/// One child's delivery outcome, surfaced in `data.results`.
#[derive(Debug, Clone, Serialize)]
struct DeliveryResult {
    child: String,
    status: String,
}

/// Read + tolerantly parse this node's own `.exo/children.jsonl` directly via the `Fs` cap —
/// mirrors `doctor::read_root_child_records`'s discipline (a malformed line is skipped and
/// warned about, never fatal to the rest of the ledger). A missing ledger means no children yet.
async fn read_own_children<C: Fs>(ctx: &C) -> Vec<ChildRecord> {
    let path = Path::new(".exo/children.jsonl");
    let bytes = match ctx.read(path).await {
        Ok(b) => b,
        Err(exo_caps::FsError::At { source, .. } | exo_caps::FsError::Io(source))
            if source.kind() == std::io::ErrorKind::NotFound =>
        {
            return Vec::new();
        }
        Err(e) => {
            tracing::warn!("broadcast: failed to read children ledger: {e}");
            return Vec::new();
        }
    };
    let mut records = Vec::new();
    for line in bytes.split(|&b| b == b'\n') {
        if line.is_empty() {
            continue;
        }
        match serde_json::from_slice::<ChildRecord>(line) {
            Ok(r) => records.push(r),
            Err(e) => tracing::warn!("broadcast: skipping malformed children.jsonl line: {e}"),
        }
    }
    records
}

#[async_trait::async_trait]
impl<R: Bus + Send + Sync> Tool<R> for Broadcast {
    const NAME: &'static str = "broadcast";
    const DESCRIPTION: &'static str =
        "Send the same message to every one of your LIVE direct children — flat fan-out, no \
         role filtering, no addressing. Same delivery path as `send_message`, one `Bus::deliver` \
         per child. Skips terminal (reaped/died) and submitted-but-not-yet-merged children. \
         Returns a per-child delivered/error result, with the same wake-channel note \
         `send_message` gives when a recipient hasn't armed its listener yet. No live children is \
         not an error — you get back \"no live children\".";
    type Args = BroadcastArgs;

    async fn run(ctx: &R, args: BroadcastArgs) -> CapResult<ToolOutput> {
        if args.text.trim().is_empty() {
            return Err(CapError::invalid("broadcast", "text must be non-empty"));
        }

        let records = read_own_children(ctx).await;
        let live: Vec<_> = fold_children(&records)
            .into_values()
            .filter(|c| matches!(c.state, ChildState::Live))
            .collect();

        if live.is_empty() {
            return Ok(ToolOutput::with_data(
                "no live children".to_string(),
                json!({ "results": [] }),
            ));
        }

        let mut lines = Vec::with_capacity(live.len());
        let mut results = Vec::with_capacity(live.len());
        for child in &live {
            let to = Addressee::Child(child.name.clone());
            let msg = Message {
                text: MessageBody::new(args.text.clone())?,
                summary: Summary::new("broadcast".to_string())?,
                kind: MessageKind::Chat,
                reply_to: None,
            };
            let status = match ctx.deliver(to.clone(), msg).await {
                Ok(()) => match wake_note(ctx.wake_status(&to).await) {
                    None => "delivered".to_string(),
                    Some(note) => format!("delivered ({note})"),
                },
                Err(e) => format!("error: {e}"),
            };
            lines.push(format!("{}: {}", child.name.as_str(), status));
            results.push(DeliveryResult {
                child: child.name.as_str().to_string(),
                status,
            });
        }

        Ok(ToolOutput::with_data(
            lines.join("\n"),
            json!({ "results": results }),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_caps::{AgentName, ChildKind, InboxPath, PaneId, WakeStatus};

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

    #[tokio::test]
    async fn test_broadcast_rejects_empty_text() {
        let mock = MockRuntime::default();
        let res = Broadcast::run(
            &mock,
            BroadcastArgs {
                text: "   ".to_string(),
            },
        )
        .await;
        assert!(res.is_err());
    }

    #[tokio::test]
    async fn test_broadcast_no_children_is_ok() {
        let mock = MockRuntime::default();
        let out = Broadcast::run(
            &mock,
            BroadcastArgs {
                text: "hello".to_string(),
            },
        )
        .await
        .unwrap();
        assert_eq!(out.text, "no live children");
        assert_eq!(out.data.unwrap()["results"], json!([]));
    }

    #[tokio::test]
    async fn test_broadcast_fans_out_to_exactly_live_children() {
        let mock = MockRuntime::default();
        seed_children(
            &mock,
            vec![
                spawned_line("a"),
                spawned_line("b"),
                spawned_line("c"),
                serde_json::to_string(&ChildRecord::Reaped {
                    child: AgentName::new("c".into()).unwrap(),
                    at: None,
                })
                .unwrap(),
                serde_json::to_string(&ChildRecord::Submitted {
                    child: AgentName::new("b".into()).unwrap(),
                    branch: exo_caps::Branch::new("root.b".into()).unwrap(),
                    sha: "deadbeef".into(),
                    reviewed: false,
                    at: None,
                })
                .unwrap(),
            ],
        );

        let out = Broadcast::run(
            &mock,
            BroadcastArgs {
                text: "status check".to_string(),
            },
        )
        .await
        .unwrap();

        // Only "a" is Live: "b" was submitted, "c" was reaped.
        let calls = mock.calls_made();
        let delivered: Vec<_> = calls
            .iter()
            .filter_map(|c| match c {
                Call::BusDeliver { to, .. } => Some(to.clone()),
                _ => None,
            })
            .collect();
        assert_eq!(
            delivered,
            vec![Addressee::Child(AgentName::new("a".into()).unwrap())]
        );

        let data = out.data.unwrap();
        assert_eq!(data["results"].as_array().unwrap().len(), 1);
        assert_eq!(data["results"][0]["child"], "a");
        assert_eq!(data["results"][0]["status"], "delivered");
    }

    #[tokio::test]
    async fn test_broadcast_reports_per_child_wake_note() {
        let mock = MockRuntime {
            wake_status: WakeStatus::NotListening,
            ..MockRuntime::default()
        };
        seed_children(&mock, vec![spawned_line("a")]);

        let out = Broadcast::run(
            &mock,
            BroadcastArgs {
                text: "status check".to_string(),
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("a: delivered ("));
        assert!(out.text.contains("no active listener"));
    }

    #[tokio::test]
    async fn test_broadcast_reports_delivery_error() {
        let mock = MockRuntime::failing("deliver");
        seed_children(&mock, vec![spawned_line("a")]);

        let out = Broadcast::run(
            &mock,
            BroadcastArgs {
                text: "status check".to_string(),
            },
        )
        .await
        .unwrap();

        assert!(out.text.contains("a: error:"));
        let data = out.data.unwrap();
        assert!(data["results"][0]["status"]
            .as_str()
            .unwrap()
            .starts_with("error:"));
    }
}
