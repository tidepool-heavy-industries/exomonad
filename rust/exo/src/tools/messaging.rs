//! **P1 leaf.** `notify_parent` + `send_message` — the messaging tools, over the [`Bus`]
//! cap (port from `teams-mcp`). Each is a type with an `Args` (derive `Deserialize +
//! JsonSchema`), a generic-over-caps `run<C: Bus>(ctx, args) -> CapResult<ToolOutput>`, and a
//! hand-written `Tool<R>` adapter. Ships mock-cap unit tests (assert `Bus::deliver` recorded
//! with the right `Addressee`/`Message`) in this file.

use exo_caps::{
    Addressee, AgentName, Bus, CapResult, ControlKind, Message, MessageBody, MessageKind, Summary,
    WakeStatus,
};
use exo_framework::{Tool, ToolOutput};
use schemars::JsonSchema;
use serde::Deserialize;

/// Sender-side wake-channel note appended to a message tool's response — the "your recipient
/// can't hear yet" signal lands where someone can act on it, at the moment they act. `None`
/// (a live listener) keeps the output clean.
pub(crate) fn wake_note(status: WakeStatus) -> Option<&'static str> {
    match status {
        WakeStatus::Listening => None,
        WakeStatus::NotListening => Some(
            "⚠ recipient has no active listener — the message is queued durably and delivers \
             when they arm (or re-arm) their wake monitor",
        ),
        WakeStatus::Unknown => Some(
            "note: recipient wake status unknown (no fresh status snapshot — a just-spawned node \
             may not have armed its monitor yet); the message is queued durably either way",
        ),
    }
}

/// `"delivered"`, plus the wake note when the recipient's listener isn't confirmed live.
pub(crate) async fn delivered_output<C: Bus + Sync>(ctx: &C, to: &Addressee) -> ToolOutput {
    match wake_note(ctx.wake_status(to).await) {
        None => ToolOutput::text("delivered"),
        Some(note) => ToolOutput::text(format!("delivered\n{note}")),
    }
}

/// A tool to notify the parent agent.
pub struct NotifyParent;

#[derive(Deserialize, JsonSchema)]
pub struct NotifyParentArgs {
    /// The message body.
    pub text: String,
    /// A short one-line preview/summary.
    pub summary: String,
    /// The kind of message (defaults to chat).
    #[serde(default)]
    pub kind: ToolMessageKind,
    /// The id of the message you are replying to, taken from the `id:` field in the header of a
    /// message that was delivered to you. Renders as `re:` on the recipient's side. Omit for a
    /// message that is not a reply.
    #[serde(default)]
    pub reply_to: Option<String>,
}

#[async_trait::async_trait]
impl<R: Bus + Send + Sync> Tool<R> for NotifyParent {
    const NAME: &'static str = "notify_parent";
    const DESCRIPTION: &'static str =
        "Send a status or failure update to your parent. This is NOT the done-signal — when your \
         branch is committed and ready to merge, use `submit_branch` instead. Use this for \
         progress notes or to escalate a failure you can't resolve.";
    type Args = NotifyParentArgs;

    async fn run(ctx: &R, args: NotifyParentArgs) -> CapResult<ToolOutput> {
        let msg = Message {
            text: MessageBody::new(args.text)?,
            summary: Summary::new(args.summary)?,
            kind: args.kind.into(),
            reply_to: args.reply_to,
        };
        ctx.deliver(Addressee::Parent, msg).await?;
        Ok(delivered_output(ctx, &Addressee::Parent).await)
    }
}

/// A tool to send a message to a child agent.
pub struct SendMessage;

#[derive(Deserialize, JsonSchema)]
pub struct SendMessageArgs {
    /// The name of the child agent to send the message to.
    pub to: String,
    /// The message body.
    pub text: String,
    /// A short one-line preview/summary.
    pub summary: String,
    /// The kind of message (defaults to chat).
    #[serde(default)]
    pub kind: ToolMessageKind,
    /// The id of the message you are replying to, taken from the `id:` field in the header of a
    /// message that was delivered to you. Renders as `re:` on the recipient's side. Omit for a
    /// message that is not a reply.
    #[serde(default)]
    pub reply_to: Option<String>,
}

/// The kind of message to send, porting the [`MessageKind`] vocabulary to a
/// schema-friendly form.
#[derive(Deserialize, JsonSchema, Default)]
#[serde(rename_all = "snake_case")]
pub enum ToolMessageKind {
    /// A standard peer-to-peer chat message.
    #[default]
    Chat,
    /// A world event notification.
    Event,
    /// A lifecycle control message. `force` defaults to `false` (cooperative: defers with an
    /// "are you sure" if the target has live children); `force:true` cascades a subtree teardown.
    Shutdown {
        grace_ms: u32,
        #[serde(default)]
        force: bool,
    },
}

impl From<ToolMessageKind> for MessageKind {
    fn from(k: ToolMessageKind) -> Self {
        match k {
            ToolMessageKind::Chat => MessageKind::Chat,
            ToolMessageKind::Event => MessageKind::Event,
            ToolMessageKind::Shutdown { grace_ms, force } => {
                MessageKind::Control(ControlKind::Shutdown { grace_ms, force })
            }
        }
    }
}

#[async_trait::async_trait]
impl<R: Bus + Send + Sync> Tool<R> for SendMessage {
    const NAME: &'static str = "send_message";
    const DESCRIPTION: &'static str =
        "Send a message to one of your children (a tree-edge: any direct child). \
         For messaging your parent use `notify_parent`.";
    type Args = SendMessageArgs;

    async fn run(ctx: &R, args: SendMessageArgs) -> CapResult<ToolOutput> {
        let to = Addressee::Child(AgentName::new(args.to)?);
        let msg = Message {
            text: MessageBody::new(args.text)?,
            summary: Summary::new(args.summary)?,
            kind: args.kind.into(),
            reply_to: args.reply_to,
        };
        ctx.deliver(to.clone(), msg).await?;
        Ok(delivered_output(ctx, &to).await)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_framework::tool;
    use serde_json::json;

    #[tokio::test]
    async fn test_notify_parent() {
        let mock = MockRuntime::default();
        let args = json!({
            "text": "Hello parent",
            "summary": "Greeting"
        });

        let res = tool(NotifyParent)
            .call(&mock, args)
            .await
            .expect("tool call failed");
        assert_eq!(res, json!({ "text": "delivered" }));

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(to, &Addressee::Parent);
            assert_eq!(msg.text.as_str(), "Hello parent");
            assert_eq!(msg.summary.as_str(), "Greeting");
            assert_eq!(msg.kind, MessageKind::Chat);
            assert_eq!(msg.reply_to, None);
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }

    #[tokio::test]
    async fn test_notify_parent_warns_when_recipient_not_listening() {
        let mock = MockRuntime {
            wake_status: WakeStatus::NotListening,
            ..MockRuntime::default()
        };
        let args = json!({
            "text": "Hello parent",
            "summary": "Greeting"
        });

        let res = tool(NotifyParent)
            .call(&mock, args)
            .await
            .expect("tool call failed");
        let text = res["text"].as_str().unwrap();
        assert!(text.starts_with("delivered"), "delivery still succeeds");
        assert!(
            text.contains("⚠ recipient has no active listener"),
            "sender must see the wake warning: {text}"
        );
    }

    #[tokio::test]
    async fn test_notify_parent_soft_note_when_status_unknown() {
        let mock = MockRuntime {
            wake_status: WakeStatus::Unknown,
            ..MockRuntime::default()
        };
        let args = json!({ "text": "hi", "summary": "hi" });

        let res = tool(NotifyParent)
            .call(&mock, args)
            .await
            .expect("tool call failed");
        let text = res["text"].as_str().unwrap();
        assert!(text.contains("wake status unknown"), "{text}");
        assert!(!text.contains('⚠'), "unknown is phrased softly: {text}");
    }

    #[tokio::test]
    async fn test_notify_parent_reply_to() {
        let mock = MockRuntime::default();
        let args = json!({
            "text": "Answering your question",
            "summary": "Answer",
            "reply_to": "11111111-2222-3333-4444-555555555555"
        });

        tool(NotifyParent)
            .call(&mock, args)
            .await
            .expect("tool call failed");

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(to, &Addressee::Parent);
            assert_eq!(
                msg.reply_to,
                Some("11111111-2222-3333-4444-555555555555".to_string())
            );
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }

    #[tokio::test]
    async fn test_send_message_child() {
        let mock = MockRuntime::default();
        let args = json!({
            "to": "worker-1",
            "text": "Hello worker",
            "summary": "Greeting"
        });

        tool(SendMessage)
            .call(&mock, args)
            .await
            .expect("tool call failed");

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(
                to,
                &Addressee::Child(AgentName::new("worker-1".into()).unwrap())
            );
            assert_eq!(msg.text.as_str(), "Hello worker");
            assert_eq!(msg.summary.as_str(), "Greeting");
            assert_eq!(msg.kind, MessageKind::Chat);
            assert_eq!(msg.reply_to, None);
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }

    #[tokio::test]
    async fn test_send_message_reply_to() {
        let mock = MockRuntime::default();
        let args = json!({
            "to": "worker-1",
            "text": "Answering your question",
            "summary": "Answer",
            "reply_to": "66666666-7777-8888-9999-000000000000"
        });

        tool(SendMessage)
            .call(&mock, args)
            .await
            .expect("tool call failed");

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(
                to,
                &Addressee::Child(AgentName::new("worker-1".into()).unwrap())
            );
            assert_eq!(
                msg.reply_to,
                Some("66666666-7777-8888-9999-000000000000".to_string())
            );
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }

    #[tokio::test]
    async fn test_send_message_shutdown() {
        let mock = MockRuntime::default();
        let args = json!({
            "to": "child-1",
            "text": "finish and exit",
            "summary": "shutdown",
            "kind": { "shutdown": { "grace_ms": 5000 } }
        });

        tool(SendMessage)
            .call(&mock, args)
            .await
            .expect("tool call failed");

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(
                to,
                &Addressee::Child(AgentName::new("child-1".into()).unwrap())
            );
            assert_eq!(
                msg.kind,
                MessageKind::Control(ControlKind::Shutdown {
                    grace_ms: 5000,
                    force: false
                })
            );
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }
}
