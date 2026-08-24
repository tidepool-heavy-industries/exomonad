//! `tree` tool — show the caller's place in the swarm: its subtree (folded recursively from the
//! `.exo/children.jsonl` ledgers) + its parent, with a per-node pane-liveness proxy.
//!
//! A thin shim over the [`Topology`] cap: the runtime owns identity + IO and does the ledger
//! walk + tmux probe; this tool just surfaces the structured view. Served by Root/Tl (the roles
//! that have children).
//!
//! **Tombstones.** A child's folded [`ChildState`] is `Live`, `Submitted`, `Reaped`, or `Died`.
//! `Reaped`/`Died` are terminal — the child is gone and its recorded pane id may have been
//! recycled by tmux onto a different, live agent, so this tool never probes a terminal node's
//! pane or status file. The default view hides routine `Reaped` history (torn down deliberately
//! by this very node) but always shows `Died` (unacknowledged — the child may hold unmerged
//! work); `all: true` shows everything.

use exo_caps::{
    paths::status_path, CapError, CapResult, ChildState, Fs, NodeStatus, Topology, TreeNode,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use exo_framework::{Tool, ToolOutput};

/// Arguments for the `tree` tool. `all: true` shows every node including routine reaped
/// tombstones; the default hides them (a `Died` child is never hidden — see module docs).
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
pub struct TreeArgs {
    #[serde(default)]
    pub all: bool,
}

/// The `tree` tool.
pub struct Tree;

/// The cgroup slice `exo init`'s `confine=true` places the swarm's tmux server in — see
/// `rust/exo/src/init.rs`'s `CONFINE_SLICE`. Duplicated as a literal (not shared via a common
/// const) since `tree` and `init` don't otherwise depend on each other.
const CONFINE_SLICE: &str = "swarm.slice";

/// Pure classification: does this `/proc/self/cgroup` content place the process in `slice`?
/// Split out for unit testing without touching `/proc`.
fn cgroup_content_in_slice(cgroup_content: &str, slice: &str) -> bool {
    cgroup_content.contains(slice)
}

/// The caller's own directives bundle, as needed to judge a child's `directives_hash` against it.
/// A load failure degrades to `Unknown` (never an error) — see module docs.
enum CallerDirectives {
    /// The caller has a bundle with this hash.
    Some(String),
    /// The caller has no standing directives.
    None,
    /// The caller's own bundle failed to load; render children's hashes unlabeled.
    Unknown,
}

impl CallerDirectives {
    /// The status bit for a child that carries `hash`, judged against the caller's own bundle.
    fn status_bit(&self, hash: &str) -> String {
        let hash8 = &hash[..hash.len().min(8)];
        match self {
            CallerDirectives::Some(caller_hash) if caller_hash == hash => "directives:ok".into(),
            CallerDirectives::Some(_) => format!("directives:stale({hash8})"),
            CallerDirectives::None | CallerDirectives::Unknown => format!("directives:{hash8}"),
        }
    }
}

impl Tree {
    /// Prune `Reaped` nodes (and their subtrees) out of the rendered tree unless `all` is set.
    /// Returns the pruned copy; `hidden` is incremented once per `Reaped` node dropped.
    fn prune(node: &TreeNode, all: bool, hidden: &mut usize) -> TreeNode {
        let children = node
            .children
            .iter()
            .filter_map(|c| {
                if !all && matches!(c.state, Some(ChildState::Reaped)) {
                    *hidden += 1;
                    None
                } else {
                    Some(Self::prune(c, all, hidden))
                }
            })
            .collect();
        TreeNode {
            name: node.name.clone(),
            kind: node.kind,
            pane: node.pane.clone(),
            pane_alive: node.pane_alive,
            state: node.state.clone(),
            model: node.model.clone(),
            model_label: node.model_label.clone(),
            directives_hash: node.directives_hash.clone(),
            children,
        }
    }

    async fn collect_status<C: Fs>(
        ctx: &C,
        node: &TreeNode,
        home: &Path,
        run_id: &str,
        map: &mut HashMap<String, NodeStatus>,
    ) {
        // A terminal node's recorded pane id may have been recycled by tmux onto a different
        // live agent — never look up its status file.
        let is_terminal = matches!(&node.state, Some(s) if s.is_terminal());
        if !is_terminal {
            let path = status_path(home, run_id, &node.pane);
            if let Ok(bytes) = ctx.read(&path).await {
                if let Ok(status) = serde_json::from_slice::<NodeStatus>(&bytes) {
                    map.insert(node.name.as_str().to_string(), status);
                }
            }
        }
        for child in &node.children {
            // Manual recursion for async
            Box::pin(Self::collect_status(ctx, child, home, run_id, map)).await;
        }
    }

    fn format_node(
        node: &TreeNode,
        depth: usize,
        status_map: &HashMap<String, NodeStatus>,
        parent_status: Option<&NodeStatus>,
        caller_directives: &CallerDirectives,
        out: &mut String,
    ) {
        use std::fmt::Write;
        let indent = "  ".repeat(depth);
        let is_terminal = matches!(&node.state, Some(s) if s.is_terminal());

        if is_terminal {
            // Tombstones get NO liveness bracket, NO busy bit, NO status-file lookup, NO
            // directives bit — the pane id may have been recycled onto a different, live agent.
            let state_word = match &node.state {
                Some(ChildState::Died) => "died",
                Some(ChildState::Reaped) => "reaped",
                _ => unreachable!("is_terminal implies Died or Reaped"),
            };
            writeln!(
                out,
                "{indent}• {} ({}) - {state_word}",
                node.name.as_str(),
                node.pane.as_str()
            )
            .unwrap();
            for child in &node.children {
                Self::format_node(child, depth + 1, status_map, None, caller_directives, out);
            }
            return;
        }

        let liveness = if node.pane_alive { "alive" } else { "dead" };
        let mut status_bits: Vec<String> = Vec::new();

        // 0. Model label (e.g. "kimi") for a node on a non-default brain via a launch profile.
        if let Some(label) = &node.model_label {
            status_bits.push(label.clone());
        }

        // 0b. The effective launch model (e.g. "sonnet").
        if let Some(model) = &node.model {
            status_bits.push(format!("model:{model}"));
        }

        // 0c. A submitted-but-not-yet-merged child: still running, still gets its liveness
        // bracket below, but also carries the pending-merge state.
        if let Some(ChildState::Submitted { sha, reviewed }) = &node.state {
            let sha8 = &sha[..sha.len().min(8)];
            let unreviewed = if *reviewed { "" } else { ", unreviewed" };
            status_bits.push(format!("submitted @ {sha8}, awaiting merge{unreviewed}"));
        }

        // 0d. Directives audit — did this child get the memo? Skipped entirely when it was
        // spawned without directives (`directives_hash: None`).
        if let Some(hash) = &node.directives_hash {
            status_bits.push(caller_directives.status_bit(hash));
        }

        // 1. Busy bit (from parent's view)
        if let Some(ps) = parent_status {
            if let Some(cs) = ps.children.iter().find(|c| c.name == node.name.as_str()) {
                status_bits.push(if cs.busy { "busy" } else { "idle" }.to_string());
            }
        }

        // 2. Branch, wake channel & Shutdown (from node's own view)
        let mut shutdown_flag = "";
        if let Some(s) = status_map.get(node.name.as_str()) {
            status_bits.push(s.branch.clone());
            // Is the node's `exo listen` monitor armed? `wake:-` means messages to it are
            // queuing (cursor-pinned) until it arms/re-arms.
            status_bits.push(if s.listener_connected {
                "wake:listen".to_string()
            } else {
                "wake:-".to_string()
            });
            if s.shutdown_pending {
                shutdown_flag = " [SHUTDOWN PENDING]";
            }
        }

        let extra = if status_bits.is_empty() {
            String::new()
        } else {
            format!(" — {}", status_bits.join("; "))
        };

        writeln!(
            out,
            "{indent}• {} ({}) [{liveness}]{extra}{shutdown_flag}",
            node.name.as_str(),
            node.pane.as_str()
        )
        .unwrap();

        let my_status = status_map.get(node.name.as_str());
        for child in &node.children {
            Self::format_node(
                child,
                depth + 1,
                status_map,
                my_status,
                caller_directives,
                out,
            );
        }
    }

    fn enrich_json(
        val: &mut serde_json::Value,
        status_map: &HashMap<String, NodeStatus>,
        parent_status: Option<&NodeStatus>,
    ) {
        if let Some(obj) = val.as_object_mut() {
            let name = obj
                .get("name")
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();

            // Add node's own status
            if let Some(status) = status_map.get(&name) {
                if let Ok(val) = serde_json::to_value(status) {
                    obj.insert("status".to_string(), val);
                }
            }

            // Add busy bit from parent
            if let Some(ps) = parent_status {
                if let Some(cs) = ps.children.iter().find(|c| c.name == name) {
                    obj.insert("busy".to_string(), serde_json::Value::Bool(cs.busy));
                }
            }

            let my_status = status_map.get(&name);
            if let Some(children) = obj.get_mut("children").and_then(|v| v.as_array_mut()) {
                for child in children {
                    Self::enrich_json(child, status_map, my_status);
                }
            }
        }
    }
}

/// Count `(total, alive)` over a subtree, inclusive of the node itself. A terminal node is NEVER
/// counted alive, regardless of its (possibly stale/recycled) `pane_alive` bit.
fn count(node: &TreeNode) -> (usize, usize) {
    let self_alive = match &node.state {
        Some(s) if s.is_terminal() => 0,
        _ => usize::from(node.pane_alive),
    };
    node.children.iter().fold((1, self_alive), |(t, a), c| {
        let (ct, ca) = count(c);
        (t + ct, a + ca)
    })
}

#[async_trait::async_trait]
impl<R: Topology + Fs + Send + Sync> Tool<R> for Tree {
    const NAME: &'static str = "tree";
    const DESCRIPTION: &'static str =
        "Show your place in the swarm tree: your subtree (every descendant, folded from the \
         on-disk ledgers) plus your parent, with a per-node liveness flag (`pane_alive` = the \
         node's tmux pane still exists; never shown for a tombstoned node — its pane id may have \
         been recycled onto a different, live agent). Default view: Live, Submitted, and Died \
         children are shown; routine Reaped history is hidden (pass `all: true` to see \
         everything, including reaped nodes). A Submitted child is still running, awaiting your \
         `merge`. Read-only.";
    type Args = TreeArgs;

    async fn run(ctx: &R, args: TreeArgs) -> CapResult<ToolOutput> {
        let view = ctx.topology().await?;
        let mut hidden = 0usize;
        let pruned_node = Self::prune(&view.node, args.all, &mut hidden);
        let (total, alive) = count(&pruned_node);

        let home = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("."));
        let run_id = std::env::var("EXOMONAD_SWARM_RUN_ID").unwrap_or_default();

        let mut status_map = HashMap::new();
        Tree::collect_status(ctx, &pruned_node, &home, &run_id, &mut status_map).await;

        // "Did everyone get the memo?" — load our own current directives bundle once, so each
        // child's recorded `directives_hash` can be judged against it. A load failure degrades to
        // showing children's bare hashes with a one-line warning — never an error (see module docs).
        let caller_directives = match crate::directives::load_directives(ctx).await {
            Ok(d) => match d.hash() {
                Some(hash) => CallerDirectives::Some(hash),
                None => CallerDirectives::None,
            },
            Err(e) => {
                tracing::warn!(
                    "tree: failed to load own directives, degrading to bare child hashes: {e}"
                );
                CallerDirectives::Unknown
            }
        };

        let mut text = String::new();
        use std::fmt::Write;
        // Self-check: this sidecar IS a session descendant, so its own cgroup is a valid proxy
        // for the whole session's confinement. Only checked (and only ever prints anything) when
        // `exo init` believed confinement was verified at boot — an unconfigured host stays silent.
        if std::env::var("EXO_CONFINED").is_ok() {
            let confined = std::fs::read_to_string("/proc/self/cgroup")
                .map(|c| cgroup_content_in_slice(&c, CONFINE_SLICE))
                .unwrap_or(false);
            if !confined {
                writeln!(
                    &mut text,
                    "⚠ UNCONFINED — confine=true was set but this process is not in {CONFINE_SLICE}"
                )
                .unwrap();
            }
        }
        if let Some(parent) = &view.parent {
            writeln!(&mut text, "parent: {parent}").unwrap();
        } else {
            writeln!(&mut text, "parent: none (root)").unwrap();
        }
        if matches!(caller_directives, CallerDirectives::Unknown) {
            writeln!(
                &mut text,
                "⚠ could not load own directives — child directives hashes shown unlabeled"
            )
            .unwrap();
        }
        Tree::format_node(
            &pruned_node,
            0,
            &status_map,
            None,
            &caller_directives,
            &mut text,
        );

        let pruned_view = exo_caps::TopologyView {
            node: pruned_node,
            parent: view.parent.clone(),
            path: view.path.clone(),
        };
        let mut data = serde_json::to_value(&pruned_view).map_err(|e| CapError::Json {
            context: "tree topology view".into(),
            source: e,
        })?;

        // Enrich data with status
        if let Some(node_val) = data.get_mut("node") {
            Tree::enrich_json(node_val, &status_map, None);
        }

        // Add summary line at the bottom
        let mut summary_line = format!("{total} node(s) in subtree, {alive} alive");
        if hidden > 0 {
            write!(
                &mut summary_line,
                " ({hidden} reaped hidden - pass all:true)"
            )
            .unwrap();
        }
        writeln!(&mut text, "\n{summary_line}").unwrap();

        Ok(ToolOutput::with_data(text, data))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockRuntime;
    use exo_framework::Tool;

    #[test]
    fn cgroup_content_matches_slice_present() {
        let content =
            "0::/user.slice/user-1000.slice/user@1000.service/swarm.slice/run-p1-i2.scope\n";
        assert!(cgroup_content_in_slice(content, "swarm.slice"));
    }

    #[test]
    fn cgroup_content_no_match_when_slice_absent() {
        let content = "0::/user.slice/user-1000.slice/user@1000.service/app.slice/foo.scope\n";
        assert!(!cgroup_content_in_slice(content, "swarm.slice"));
    }

    #[tokio::test]
    async fn test_tree_returns_view() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();

        // summary mentions self + parent
        assert!(out.text.contains("mock"));
        assert!(out.text.contains("mock-parent"));

        // structured data round-trips back to the view (self + one child of each state)
        let data = out.data.expect("tree returns structured data");
        let view: exo_caps::TopologyView = serde_json::from_value(data).unwrap();
        assert_eq!(view.node.name.as_str(), "mock");
        assert_eq!(view.parent.as_deref(), Some("mock-parent"));
        assert_eq!(view.node.children.len(), 4);
        assert_eq!(view.node.children[0].name.as_str(), "child-a");
    }

    #[tokio::test]
    async fn default_view_hides_reaped_but_shows_died() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs::default()).await.unwrap();

        let names: Vec<String> = out
            .text
            .lines()
            .filter(|l| l.contains('•'))
            .map(|l| l.to_string())
            .collect();
        assert!(
            !names.iter().any(|l| l.contains("child-reaped")),
            "reaped child must be hidden by default: {names:?}"
        );
        assert!(
            names.iter().any(|l| l.contains("child-died")),
            "died child must always be shown: {names:?}"
        );

        let data = out.data.expect("tree returns structured data");
        let view: exo_caps::TopologyView = serde_json::from_value(data).unwrap();
        assert_eq!(
            view.node.children.len(),
            3,
            "reaped child pruned from data too"
        );
    }

    #[tokio::test]
    async fn default_view_reports_hidden_count() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs::default()).await.unwrap();
        assert!(
            out.text.contains("(1 reaped hidden - pass all:true)"),
            "text: {}",
            out.text
        );
    }

    #[tokio::test]
    async fn all_true_shows_everything_with_no_hidden_line() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        assert!(out.text.contains("child-reaped"));
        assert!(!out.text.contains("reaped hidden"));
    }

    #[tokio::test]
    async fn submitted_node_renders_sha_prefix_and_awaiting_merge() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs::default()).await.unwrap();
        assert!(out.text.contains("submitted @ abcdef12, awaiting merge"));
        assert!(out.text.contains("unreviewed"));
    }

    #[tokio::test]
    async fn tombstoned_nodes_have_no_liveness_bracket() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        let died_line = out
            .text
            .lines()
            .find(|l| l.contains("child-died"))
            .expect("died child rendered");
        assert!(!died_line.contains("[alive]"));
        assert!(!died_line.contains("[dead]"));
        assert!(died_line.contains("- died"));

        let reaped_line = out
            .text
            .lines()
            .find(|l| l.contains("child-reaped"))
            .expect("reaped child rendered with all:true");
        assert!(!reaped_line.contains("[alive]"));
        assert!(!reaped_line.contains("[dead]"));
        assert!(reaped_line.contains("- reaped"));

        // Live and Submitted keep their bracket.
        let live_line = out.text.lines().find(|l| l.contains("child-a")).unwrap();
        assert!(live_line.contains("[alive]") || live_line.contains("[dead]"));
        let submitted_line = out
            .text
            .lines()
            .find(|l| l.contains("child-submitted"))
            .unwrap();
        assert!(submitted_line.contains("[alive]") || submitted_line.contains("[dead]"));
    }

    #[tokio::test]
    async fn model_bit_shown_when_set() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs::default()).await.unwrap();
        assert!(out.text.contains("model:sonnet"));
    }

    #[tokio::test]
    async fn collect_status_skips_tombstoned_panes() {
        // A status file keyed on the DIED child's pane (%4) — planted directly to prove
        // `collect_status` never reads it: tmux recycles pane ids, so a status file keyed by a
        // tombstone's pane may actually belong to a different, live agent today.
        let mock = MockRuntime::default();
        let home = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("."));
        let run_id = std::env::var("EXOMONAD_SWARM_RUN_ID").unwrap_or_default();
        let path = status_path(&home, &run_id, &exo_caps::PaneId::new("%4".into()).unwrap());
        let bogus_status = serde_json::json!({
            "node": ["mock", "some-other-live-agent"],
            "kind": "dev",
            "branch": "tombstone-alias-branch",
            "shutdown_pending": false,
            "children": [],
            "ts": "2026-01-01T00:00:00Z",
        });
        mock.files.lock().unwrap().insert(
            path.display().to_string(),
            serde_json::to_vec(&bogus_status).unwrap(),
        );

        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        assert!(
            !out.text.contains("tombstone-alias-branch"),
            "a tombstone's pane-keyed status file must never be read: {}",
            out.text
        );
    }

    #[tokio::test]
    async fn alive_count_excludes_tombstones() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        // self(mock) + child-a + child-submitted are non-terminal and pane_alive=true = 3 alive.
        // child-reaped / child-died are terminal (forced not-alive) despite pane_alive=true in the mock.
        assert!(
            out.text.contains("5 node(s) in subtree, 3 alive"),
            "text: {}",
            out.text
        );
    }

    /// Caller has no directives of its own (the default mock) — a child with a recorded
    /// `directives_hash` renders the bare, informational hash form, never ok/stale.
    #[tokio::test]
    async fn directives_bare_hash_when_caller_has_none() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        let a_line = out.text.lines().find(|l| l.contains("child-a (")).unwrap();
        assert!(a_line.contains("directives:deadbeef"), "line: {a_line}");
        assert!(!a_line.contains("directives:ok"));
        assert!(!a_line.contains("stale"));
    }

    /// A child whose `directives_hash` was never recorded (spawned without directives) gets no
    /// directives status bit at all.
    #[tokio::test]
    async fn no_directives_bit_when_child_has_no_hash() {
        let mock = MockRuntime::default();
        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        let died_line = out.text.lines().find(|l| l.contains("child-died")).unwrap();
        assert!(!died_line.contains("directives:"));
    }

    /// When the caller's own directives bundle matches a child's recorded hash, the child renders
    /// `directives:ok`; when it differs, `directives:stale(hash8)`.
    #[tokio::test]
    async fn directives_status_ok_and_stale_against_caller_bundle() {
        let mock = MockRuntime::default();
        mock.dirs.lock().unwrap().insert(
            crate::directives::DIRECTIVES_DIR.to_string(),
            vec![std::path::PathBuf::from(".exo/directives/d.md")],
        );
        mock.files.lock().unwrap().insert(
            ".exo/directives/d.md".to_string(),
            b"directive body".to_vec(),
        );

        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();

        let submitted_line = out
            .text
            .lines()
            .find(|l| l.contains("child-submitted"))
            .unwrap();
        assert!(
            submitted_line.contains("directives:ok"),
            "line: {submitted_line}"
        );

        let a_line = out.text.lines().find(|l| l.contains("child-a (")).unwrap();
        assert!(
            a_line.contains("directives:stale(deadbeef)"),
            "line: {a_line}"
        );
    }

    /// A load failure on the caller's own directives degrades to bare child hashes plus a
    /// one-line warning — the tool call itself must never fail.
    #[tokio::test]
    async fn directives_load_error_degrades_with_warning() {
        let mock = MockRuntime::default();
        // A directory entry with no corresponding file content — `load_directives` errs loudly
        // reading it (see `directives::missing_file_is_loud_error`).
        mock.dirs.lock().unwrap().insert(
            crate::directives::DIRECTIVES_DIR.to_string(),
            vec![std::path::PathBuf::from(".exo/directives/missing.md")],
        );

        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        assert!(
            out.text.contains("could not load own directives"),
            "text: {}",
            out.text
        );
        let a_line = out.text.lines().find(|l| l.contains("child-a (")).unwrap();
        assert!(a_line.contains("directives:deadbeef"), "line: {a_line}");
    }

    /// Terminal nodes never render a directives bit even when the caller has a matching or
    /// stale bundle — tombstones get no status bits at all.
    #[tokio::test]
    async fn terminal_nodes_never_render_directives_bit() {
        let mock = MockRuntime::default();
        mock.dirs.lock().unwrap().insert(
            crate::directives::DIRECTIVES_DIR.to_string(),
            vec![std::path::PathBuf::from(".exo/directives/d.md")],
        );
        mock.files.lock().unwrap().insert(
            ".exo/directives/d.md".to_string(),
            b"directive body".to_vec(),
        );
        let out = Tree::run(&mock, TreeArgs { all: true }).await.unwrap();
        for name in ["child-reaped", "child-died"] {
            let line = out.text.lines().find(|l| l.contains(name)).unwrap();
            assert!(!line.contains("directives:"), "line: {line}");
        }
    }
}
