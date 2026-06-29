//! **P1 leaf.** `notify_parent` + `send_message` — the messaging tools, over the [`Bus`]
//! cap (port from `teams-mcp`). Each is a type with an `Args` (derive `Deserialize +
//! JsonSchema`), a generic-over-caps `run<C: Bus>(ctx, args) -> CapResult<ToolOutput>`, and a
//! hand-written `Tool<R>` adapter. Ships mock-cap unit tests (assert `Bus::deliver` recorded
//! with the right `Addressee`/`Message`) in this file.

use exo_caps::{
    Addressee, AgentName, Bus, CapResult, ControlKind, Message, MessageBody, MessageKind, Summary,
};
use exo_framework::{Tool, ToolOutput};
use schemars::JsonSchema;
use serde::Deserialize;

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
        };
        ctx.deliver(Addressee::Parent, msg).await?;
        Ok(ToolOutput::text("delivered"))
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
        };
        ctx.deliver(to, msg).await?;
        Ok(ToolOutput::text("delivered"))
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

        let res = tool(NotifyParent).call(&mock, args).await.expect("tool call failed");
        assert_eq!(res, json!({ "text": "delivered" }));

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(to, &Addressee::Parent);
            assert_eq!(msg.text.as_str(), "Hello parent");
            assert_eq!(msg.summary.as_str(), "Greeting");
            assert_eq!(msg.kind, MessageKind::Chat);
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

        tool(SendMessage).call(&mock, args).await.expect("tool call failed");

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

        tool(SendMessage).call(&mock, args).await.expect("tool call failed");

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
