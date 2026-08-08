//! Structured **receipts** — what a submitting node actually did, in typed form, carried on the
//! `[READY]` message it hands its parent.
//!
//! A `[READY]` used to be prose: the submitter said "did the thing" and the parent believed it.
//! Receipts make the claim checkable — the commit the work was *verified at*, the exact commands
//! run, whatever counts the node wants to hand up, and where it knowingly departed from its spec.
//!
//! The load-bearing part is the **transfer proof**. A node tests at one commit and submits at
//! another; rebases, follow-up fixes and "one more small thing" all land in that gap, and the
//! parent has no way to see it. [`TransferProof`] closes it: it names both commits, enumerates
//! what landed between them ([`Git::commits_between`](exo_caps::Git::commits_between)) and says
//! whether any of it touches the diff the parent is about to merge. When the tested commit can't
//! be resolved at all, that is a [`TransferProof::Unverifiable`] — stated loudly, never dropped.
//!
//! Receipts ride the message **text** (and the tool's `data`) only. They are deliberately NOT part
//! of `Lifecycle::Submitted`: `exo-caps` sits below this crate and cannot name a domain type.
//!
//! Everything here is pure — no caps, no IO. `submit_branch` gathers the facts; this module only
//! shapes and renders them.

use exo_caps::CommitFiles;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Hard ceiling on any single receipt string as it arrives from the agent. Over this is a LOUD
/// rejection at the tool boundary, never a silent trim — an agent that pastes a build log into a
/// receipt must be told, not quietly edited.
pub const MAX_FIELD_BYTES: usize = 2 * 1024;

/// Hard ceiling on the fully rendered block. The parent-bound [`MessageBody`](exo_caps::MessageBody)
/// caps at 4 KiB and *errors* on overflow, so the rendered receipts must leave room for the
/// `[READY]` line and the submitter's note.
pub const MAX_RENDERED_BYTES: usize = 2 * 1024;

/// Per-string budget inside the rendered block. Longer strings are truncated with a visible `…`.
pub const MAX_STRING_RENDER_BYTES: usize = 200;

/// Rendered-list caps. Overflow renders a visible `(+N more)`, never a silent cut.
pub const MAX_VERIFY_COMMANDS: usize = 8;
/// See [`MAX_VERIFY_COMMANDS`].
pub const MAX_METRICS: usize = 12;
/// See [`MAX_VERIFY_COMMANDS`].
pub const MAX_DEVIATIONS: usize = 8;
/// See [`MAX_VERIFY_COMMANDS`].
pub const MAX_FILES: usize = 20;

/// What the submitting node claims it did — the typed half of a `[READY]`.
///
/// Every field is optional/defaulted: receipts are an upgrade to the submit path, not a new
/// mandatory ceremony. Fields are typed (no free-form JSON) so the tool schema inlines cleanly and
/// an agent can't smuggle an unbounded blob through a `Value`.
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
pub struct Receipts {
    /// Full or short sha the submitter last ran its verification at. Compared against `HEAD` to
    /// produce the [`TransferProof`] — the single most useful field here.
    #[serde(default)]
    pub commit_tested: Option<String>,
    /// The exact verification commands actually run, verbatim (e.g. `cargo test -p exo --lib`).
    /// Not what you intended to run — what you ran.
    #[serde(default)]
    pub verify_commands_run: Vec<String>,
    /// Counts, durations, sizes — whatever this node wants to hand up, as `{label, value}` pairs.
    #[serde(default)]
    pub metrics: Vec<LabeledValue>,
    /// Where the work knowingly departed from its spec, one per item.
    #[serde(default)]
    pub deviations: Vec<String>,
}

/// One `{label, value}` receipt datum — typed-but-flexible, deliberately not free-form JSON.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct LabeledValue {
    /// What was measured (e.g. `tests passed`).
    pub label: String,
    /// The measurement, rendered by the agent (e.g. `412`, `1m41s`).
    pub value: String,
}

/// What moved between the commit the submitter verified at and the commit it is handing up.
///
/// Internal to `submit_branch` — built from [`Git::commits_between`](exo_caps::Git::commits_between)
/// and rendered into the parent-bound message. The enum (rather than a struct of optionals) is what
/// makes the unverifiable case impossible to drop on the floor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TransferProof {
    /// The verification commit *is* `HEAD` — nothing moved. The strongest receipt available.
    AtHead {
        /// The sha both the test and the submit sit on.
        sha: String,
    },
    /// Commits landed between the tested commit and `HEAD`.
    Moved {
        /// The sha as the submitter reported it (short or full).
        tested: String,
        /// The sha actually being submitted.
        head: String,
        /// The commits in `tested..HEAD`, newest first, with the files each touched.
        commits: Vec<CommitFiles>,
        /// Files touched *both* by those commits and by the diff the parent will merge.
        /// `None` when no diff base resolved (so the question can't be answered);
        /// `Some(empty)` is the real, reassuring answer "none of it overlaps".
        overlap: Option<Vec<String>>,
    },
    /// The tested sha could not be checked against `HEAD` at all (bad sha, git error). The parent
    /// must be told plainly that this is an **untested transfer** rather than shown a reassuring
    /// blank.
    Unverifiable {
        /// The sha as the submitter reported it.
        tested: String,
        /// The sha being submitted.
        head: String,
        /// Why it couldn't be resolved — surfaced verbatim so it's debuggable.
        reason: String,
    },
}

/// Clip `s` to [`MAX_STRING_RENDER_BYTES`] bytes, on a char boundary, with a trailing `…` when
/// truncated. Used for every rendered string so a truncation is never silent.
fn clip(s: &str) -> String {
    if s.len() <= MAX_STRING_RENDER_BYTES {
        return s.to_string();
    }
    let mut end = MAX_STRING_RENDER_BYTES;
    while !s.is_char_boundary(end) {
        end -= 1;
    }
    format!("{}…", &s[..end])
}

/// Render a list of strings, each individually clipped ([`clip`]), joined by `sep`, capped at
/// `max` shown items with a trailing `(+N more)` when the list overflows. Used for every rendered
/// list (verify commands, metrics, deviations, file lists) so a truncated list is never silent.
fn render_clipped_list(items: &[String], max: usize, sep: &str) -> String {
    let shown: Vec<String> = items.iter().take(max).map(|s| clip(s)).collect();
    let mut out = shown.join(sep);
    if items.len() > max {
        if !out.is_empty() {
            out.push(' ');
        }
        out.push_str(&format!("(+{} more)", items.len() - max));
    }
    out
}

/// The union of every commit's touched files, sorted and deduped.
fn union_files(commits: &[CommitFiles]) -> Vec<String> {
    let mut files: Vec<String> = commits
        .iter()
        .flat_map(|c| c.files.iter().cloned())
        .collect();
    files.sort();
    files.dedup();
    files
}

fn render_transfer_proof(p: &TransferProof) -> String {
    match p {
        TransferProof::AtHead { sha } => format!("tested@HEAD {}", clip(sha)),
        TransferProof::Moved {
            tested,
            head,
            commits,
            overlap,
        } => {
            let files = union_files(commits);
            let files_str = render_clipped_list(&files, MAX_FILES, ", ");
            let overlap_str = match overlap {
                None => "(no diff base resolved — overlap unknown)".to_string(),
                Some(ov) if ov.is_empty() => "none overlap your diff".to_string(),
                Some(ov) => format!(
                    "{} overlap your diff: {}",
                    ov.len(),
                    render_clipped_list(ov, MAX_FILES, ", ")
                ),
            };
            format!(
                "tested at {}, submitting {} — {} commits between; files touched: {}; {}",
                clip(tested),
                clip(head),
                commits.len(),
                files_str,
                overlap_str,
            )
        }
        TransferProof::Unverifiable {
            tested,
            head,
            reason,
        } => format!(
            "tested-at commit {} could not be verified against HEAD {} — treat as untested \
             transfer ({})",
            clip(tested),
            clip(head),
            clip(reason),
        ),
    }
}

/// Render the compact receipts block for the parent-bound message text.
///
/// Pure and total: deliberately truncating, and every truncation leaves a visible marker (`…` for
/// an over-long string, `(+N more)` for a clipped list) so a reader can always tell the difference
/// between "that's all there was" and "there was more". Returns an empty string when there is
/// nothing to say (empty receipts and no proof).
///
/// The caller is responsible for rejecting oversized input *before* calling this
/// ([`MAX_FIELD_BYTES`]) and for refusing a result over [`MAX_RENDERED_BYTES`].
pub fn render_receipts_summary(r: &Receipts, proof: Option<&TransferProof>) -> String {
    let mut sections: Vec<String> = Vec::new();

    if !r.verify_commands_run.is_empty() {
        sections.push(format!(
            "  ran: {}",
            render_clipped_list(&r.verify_commands_run, MAX_VERIFY_COMMANDS, " | ")
        ));
    }
    if !r.metrics.is_empty() {
        let metric_strs: Vec<String> = r
            .metrics
            .iter()
            .map(|m| format!("{}={}", m.label, m.value))
            .collect();
        sections.push(format!(
            "  metrics: {}",
            render_clipped_list(&metric_strs, MAX_METRICS, " | ")
        ));
    }
    if !r.deviations.is_empty() {
        sections.push(format!(
            "  deviations: {}",
            render_clipped_list(&r.deviations, MAX_DEVIATIONS, " | ")
        ));
    }
    if let Some(p) = proof {
        sections.push(format!("  transfer proof: {}", render_transfer_proof(p)));
    }

    if sections.is_empty() {
        return String::new();
    }
    format!("receipts:\n{}", sections.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_receipts_no_proof_renders_empty() {
        let r = Receipts::default();
        assert_eq!(render_receipts_summary(&r, None), "");
    }

    #[test]
    fn overlong_string_is_clipped_with_marker() {
        let long = "x".repeat(500);
        let r = Receipts {
            deviations: vec![long.clone()],
            ..Default::default()
        };
        let out = render_receipts_summary(&r, None);
        assert!(out.contains('…'), "missing truncation marker: {out}");
        assert!(!out.contains(&long), "long string was not clipped: {out}");
    }

    #[test]
    fn verify_commands_over_cap_render_plus_n_more() {
        let cmds: Vec<String> = (0..MAX_VERIFY_COMMANDS + 3)
            .map(|i| format!("cmd{i}"))
            .collect();
        let r = Receipts {
            verify_commands_run: cmds,
            ..Default::default()
        };
        let out = render_receipts_summary(&r, None);
        assert!(out.contains("(+3 more)"), "wrong overflow count: {out}");
    }

    #[test]
    fn at_head_renders_marker() {
        let p = TransferProof::AtHead {
            sha: "deadbeef".into(),
        };
        let out = render_receipts_summary(&Receipts::default(), Some(&p));
        assert!(out.contains("tested@HEAD"), "{out}");
    }

    #[test]
    fn moved_renders_commits_between_marker() {
        let p = TransferProof::Moved {
            tested: "abc1234".into(),
            head: "def5678".into(),
            commits: vec![CommitFiles {
                sha: "c1".into(),
                files: vec!["a.rs".into()],
            }],
            overlap: Some(vec!["a.rs".into()]),
        };
        let out = render_receipts_summary(&Receipts::default(), Some(&p));
        assert!(out.contains("commits between"), "{out}");
    }

    #[test]
    fn unverifiable_renders_untested_transfer_marker() {
        let p = TransferProof::Unverifiable {
            tested: "abc".into(),
            head: "def".into(),
            reason: "bad sha".into(),
        };
        let out = render_receipts_summary(&Receipts::default(), Some(&p));
        assert!(out.contains("treat as untested transfer"), "{out}");
    }

    #[test]
    fn moved_overlap_empty_vs_unknown() {
        let moved = |overlap| TransferProof::Moved {
            tested: "abc".into(),
            head: "def".into(),
            commits: vec![],
            overlap,
        };
        let out_empty = render_receipts_summary(&Receipts::default(), Some(&moved(Some(vec![]))));
        assert!(out_empty.contains("none overlap your diff"), "{out_empty}");
        let out_unknown = render_receipts_summary(&Receipts::default(), Some(&moved(None)));
        assert!(out_unknown.contains("overlap unknown"), "{out_unknown}");
    }

    #[test]
    fn multibyte_utf8_clips_without_panic() {
        let long = "é".repeat(300);
        let r = Receipts {
            deviations: vec![long],
            ..Default::default()
        };
        let out = render_receipts_summary(&r, None);
        assert!(out.contains('…'), "{out}");
    }
}
