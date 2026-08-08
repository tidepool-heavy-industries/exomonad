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

/// Render the compact receipts block for the parent-bound message text.
///
/// Pure and total: deliberately truncating, and every truncation leaves a visible marker (`…` for
/// an over-long string, `(+N more)` for a clipped list) so a reader can always tell the difference
/// between "that's all there was" and "there was more". Returns an empty string when there is
/// nothing to say (empty receipts and no proof).
///
/// The caller is responsible for rejecting oversized input *before* calling this
/// ([`MAX_FIELD_BYTES`]) and for refusing a result over [`MAX_RENDERED_BYTES`].
pub fn render_receipts_summary(_r: &Receipts, _proof: Option<&TransferProof>) -> String {
    todo!("render the compact receipts block: verify commands, metrics, deviations, transfer proof")
}
