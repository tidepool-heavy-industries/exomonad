//! **N2c — parent-side Teams→Bus bridge.**
//!
//! A spawned child is a *synthetic* member of the parent's team: no CC InboxPoller services
//! it (the child's own session never joined that team as that member). So a native
//! `teams-mcp send` to a child lands in the child's Teams inbox and would go nowhere.
//!
//! The PARENT (which spawned the child and registered it as a synthetic member) watches its
//! children's Teams inboxes and forwards new messages into each child's **Bus** inbox — so
//! they flow through the child's normal inbound → dispatch → paste path. One delivery path;
//! the Teams inbox is just an on-ramp to the Bus.
//!
//! This never loops: dispatch's last-hop *pastes* for synthetic members (never re-writes their
//! Teams inbox), so a forwarded message terminates at the child's pane.

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use exo_caps::{
    Addressee, Bus, ChildKind, ChildRecord, Log, Message, MessageBody, MessageKind, Summary,
};
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::mpsc;

use crate::bootstrap::NodeContext;
use crate::error::{NodeError, NodeResult};

/// Watch this node's children's Teams inboxes and forward new messages into their Bus inboxes.
/// No-op (returns) if this node leads no team — only a team lead has synthetic-member children.
pub async fn bridge_children(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let team = match exo_scry::resolve_by_pane(ctx.own_pane.as_str()) {
        Ok(Some(t)) => t.team.0,
        _ => return Ok(()), // not in a team → nothing to bridge
    };

    let home = std::env::var("HOME").map_err(|_| NodeError::MissingContext("HOME"))?;
    let inboxes_dir = Path::new(&home)
        .join(".claude/teams")
        .join(&team)
        .join("inboxes");
    // Ensure the dir exists so the watcher has something to watch (CC creates it lazily).
    let _ = std::fs::create_dir_all(&inboxes_dir);

    let ledger = ctx.runtime.working_dir().join(".exo/children.jsonl");
    let mut cursors: HashMap<String, usize> = HashMap::new();

    let (tx, mut rx) = mpsc::channel(100);
    let mut watcher = RecommendedWatcher::new(
        move |res: notify::Result<Event>| {
            if let Ok(event) = res {
                if event.kind.is_modify() || event.kind.is_create() {
                    let _ = tx.blocking_send(());
                }
            }
        },
        Config::default(),
    )
    .map_err(std::io::Error::other)?;
    watcher
        .watch(&inboxes_dir, RecursiveMode::NonRecursive)
        .map_err(std::io::Error::other)?;

    forward_all(&ctx, &team, &ledger, &mut cursors).await;
    while rx.recv().await.is_some() {
        while rx.try_recv().is_ok() {}
        forward_all(&ctx, &team, &ledger, &mut cursors).await;
    }
    Ok(())
}

async fn forward_all(
    ctx: &Arc<NodeContext>,
    team: &str,
    ledger: &Path,
    cursors: &mut HashMap<String, usize>,
) {
    for (name, kind) in read_children(ledger) {
        let msgs = match exo_scry::inbox::read_inbox(team, name.as_str()) {
            Ok(m) => m,
            Err(_) => continue, // inbox not created yet / unreadable
        };
        let cursor = cursors.entry(name.as_str().to_string()).or_insert(0);
        if *cursor > msgs.len() {
            *cursor = 0; // inbox shrank/rotated — re-read
        }
        for msg in msgs.iter().skip(*cursor) {
            let body = match MessageBody::new(msg.text.clone()) {
                Ok(b) => b,
                Err(e) => {
                    Log::error(
                        &*ctx.runtime,
                        &format!("teams-bridge: dropping oversized msg to {}: {e}", name.as_str()),
                    );
                    continue;
                }
            };
            let summary_src = if msg.summary.is_empty() {
                msg.text.chars().take(80).collect::<String>()
            } else {
                msg.summary.clone()
            };
            let summary = Summary::new(summary_src)
                .unwrap_or_else(|_| Summary::new("(message)".to_string()).unwrap());
            let addressee = match kind {
                ChildKind::Inline => Addressee::InlineChild(name.clone()),
                ChildKind::Worktree => Addressee::WorktreeChild(name.clone()),
            };
            let forwarded = Message {
                text: body,
                summary,
                kind: MessageKind::Chat,
            };
            if let Err(e) = Bus::deliver(&*ctx.runtime, addressee, forwarded).await {
                Log::error(
                    &*ctx.runtime,
                    &format!("teams-bridge: forward to {} failed: {e}", name.as_str()),
                );
            }
        }
        *cursor = msgs.len();
    }
}

/// Read the parent-local child ledger and fold it to the current child set (name + kind).
fn read_children(ledger: &Path) -> Vec<(exo_caps::AgentName, ChildKind)> {
    let content = match std::fs::read_to_string(ledger) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };
    let records: Vec<ChildRecord> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .filter_map(|l| serde_json::from_str(l).ok())
        .collect();
    exo_caps::fold_children(&records)
        .into_values()
        .map(|c| (c.name, c.kind))
        .collect()
}
