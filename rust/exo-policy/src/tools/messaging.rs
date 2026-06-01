//! **P1 leaf.** `notify_parent` + `send_message` — the messaging tools, over the [`Bus`]
//! cap (port from `teams-mcp`). Each is a type with an `Args` (derive `Deserialize +
//! JsonSchema`), a generic-over-caps `run<C: Bus>(ctx, args) -> CapResult<ToolOutput>`, and a
//! hand-written `Tool<R>` adapter. Ships mock-cap unit tests (assert `Bus::deliver` recorded
//! with the right `Addressee`/`Message`) in this file. See `docs/design/swarm/04-policy.md`.

use crate::tool::{ok_json, parse, schema_json, Tool, ToolOutput};
use exo_caps::{Addressee, AgentName, Bus, CapResult, Message, MessageBody, MessageKind, Summary};
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
}

impl NotifyParent {
    /// The typed logic: builds a [`Message`] and delivers it to [`Addressee::Parent`].
    pub async fn run<C: Bus>(ctx: &C, args: NotifyParentArgs) -> CapResult<ToolOutput> {
        let msg = Message {
            text: MessageBody::new(args.text)?,
            summary: Summary::new(args.summary)?,
            kind: MessageKind::Chat,
        };
        ctx.deliver(Addressee::Parent, msg).await?;
        Ok(ToolOutput::text("delivered"))
    }
}

#[async_trait::async_trait]
impl<R: Bus + Send + Sync> Tool<R> for NotifyParent {
    fn name(&self) -> &str {
        "notify_parent"
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(NotifyParentArgs))
    }
    async fn call(&self, ctx: &R, j: serde_json::Value) -> CapResult<serde_json::Value> {
        ok_json(Self::run(ctx, parse(j)?).await?)
    }
}

/// A tool to send a message to a child agent.
pub struct SendMessage;

#[derive(Deserialize, JsonSchema)]
pub struct SendMessageArgs {
    /// The recipient child and its kind (inline or worktree).
    pub to: ChildTarget,
    /// The message body.
    pub text: String,
    /// A short one-line preview/summary.
    pub summary: String,
}

/// Selects an [`Addressee`] child variant.
#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ChildTarget {
    /// A worker spawned in the parent's worktree.
    Inline(String),
    /// A child spawned in its own worktree.
    Worktree(String),
}

impl SendMessage {
    /// The typed logic: builds a [`Message`] and delivers it to the specified child.
    pub async fn run<C: Bus>(ctx: &C, args: SendMessageArgs) -> CapResult<ToolOutput> {
        let to = match args.to {
            ChildTarget::Inline(name) => Addressee::InlineChild(AgentName::new(name)?),
            ChildTarget::Worktree(name) => Addressee::WorktreeChild(AgentName::new(name)?),
        };
        let msg = Message {
            text: MessageBody::new(args.text)?,
            summary: Summary::new(args.summary)?,
            kind: MessageKind::Chat,
        };
        ctx.deliver(to, msg).await?;
        Ok(ToolOutput::text("delivered"))
    }
}

#[async_trait::async_trait]
impl<R: Bus + Send + Sync> Tool<R> for SendMessage {
    fn name(&self) -> &str {
        "send_message"
    }
    fn schema(&self) -> serde_json::Value {
        schema_json(schemars::schema_for!(SendMessageArgs))
    }
    async fn call(&self, ctx: &R, j: serde_json::Value) -> CapResult<serde_json::Value> {
        ok_json(Self::run(ctx, parse(j)?).await?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use serde_json::json;

    #[tokio::test]
    async fn test_notify_parent() {
        let mock = MockRuntime::default();
        let tool = NotifyParent;
        let args = json!({
            "text": "Hello parent",
            "summary": "Greeting"
        });

        let res = tool.call(&mock, args).await.expect("tool call failed");
        assert_eq!(res, json!({ "text": "delivered" }));

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(to, &Addressee::Parent);
            assert_eq!(msg.text.as_str(), "Hello parent");
            assert_eq!(msg.summary.as_str(), "Greeting");
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }

    #[tokio::test]
    async fn test_send_message_inline() {
        let mock = MockRuntime::default();
        let tool = SendMessage;
        let args = json!({
            "to": { "inline": "worker-1" },
            "text": "Hello worker",
            "summary": "Greeting"
        });

        tool.call(&mock, args).await.expect("tool call failed");

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(
                to,
                &Addressee::InlineChild(AgentName::new("worker-1".into()).unwrap())
            );
            assert_eq!(msg.text.as_str(), "Hello worker");
            assert_eq!(msg.summary.as_str(), "Greeting");
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }

    #[tokio::test]
    async fn test_send_message_worktree() {
        let mock = MockRuntime::default();
        let tool = SendMessage;
        let args = json!({
            "to": { "worktree": "child-1" },
            "text": "Hello child",
            "summary": "Greeting"
        });

        tool.call(&mock, args).await.expect("tool call failed");

        let calls = mock.calls_made();
        assert_eq!(calls.len(), 1);
        if let Call::BusDeliver { to, msg } = &calls[0] {
            assert_eq!(
                to,
                &Addressee::WorktreeChild(AgentName::new("child-1".into()).unwrap())
            );
            assert_eq!(msg.text.as_str(), "Hello child");
            assert_eq!(msg.summary.as_str(), "Greeting");
        } else {
            panic!("expected BusDeliver call, got {:?}", calls[0]);
        }
    }
}
