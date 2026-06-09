# 2026-06 Orchestration Tree — executing the month's remainder as one swarm

> **⚠️ SPAWNED AGENTS: THIS FILE IS NOT YOUR TASK.** If you are a leaf/worker/reviewer, your
> task is ONLY what your spawn spec says. Do not implement anything from this file. Do not
> read further. TLs: read ONLY the charter section your spawn prompt names.

**What this is:** the self-contained runbook for the ROOT session to execute the open work in
[`2026-06-v2-hardening-month.md`](2026-06-v2-hardening-month.md) as a recursive tree — Opus
TLs decompose, Gemini leaves implement, the review gate converges. Written to survive a
session recreate: a fresh root executes this with no prior conversation. Task *content* lives
in the month plan (task IDs) and `docs/decisions/exo-authoring-dsl.md` (the DSL design);
this doc owns *sequencing, decomposition, and merge discipline*.

**Prerequisite (human):** P0.2 — `just install-all-dev` + session recreate, so live sidecars
run the `stop_reviewer`/ReviewAborted fix. Do not spawn review-gated children before this.

---

## Re-sequencing: DSL lands FIRST

The month plan ordered hardening (W1/W2) before the stretch DSL work. That ordering is
**superseded**: T3.7's design is accepted and it reshapes the exact files the trust tasks
touch. Landing trust tasks first would write them against a surface T3.7 immediately deletes.

| Task | Files it touches | T3.7 collision |
|---|---|---|
| T1.1 | spawn.rs, review.rs, testing.rs | Phase A converts spawn.rs adapters |
| T1.2 | gates.rs (`stop_notify`), submit.rs | Phase B *deletes* `stop_notify` → becomes a new **stop observer** |
| T1.3 / T1.4 | review.rs / config.rs + submit.rs | Phase A converts submit.rs |
| T2.3 | gates.rs (`stop` dirty-gate) | Phase B decomposes `stop` → becomes a change to **`require_clean_tree`** |
| T3.2 | testing.rs + seam.rs | Phase B rewrites seam.rs RoleDef literals |
| T4.3 | outbound.rs | Phase A renames `dyn Tool` there |

So: **root wave 1 = DSL + the disjoint ops work in parallel; trust tasks follow, written
against the new DSL** (several get simpler: T1.2 is just a new observer in the Dev/Worker
pipelines; T2.3 is a `require_clean_tree` edit).

---

## The tree

```
root (hooksock) — decompose, spawn, idle, merge. NEVER implements.
│
├─ WAVE 1 (parallel)
│  ├─ dsl   (Claude TL) ─ T3.7 impl: Phase A → Phase B → integrate+docs
│  │   ├─ wave A: 1 Gemini leaf (tool conversions)
│  │   └─ wave B: 2 Gemini leaves ∥ (exo crate / exo-node crate)
│  └─ ops   (Claude TL) ─ disjoint operational work
│      ├─ wave 1: T2.1, T2.2a(worker), T4.1, T4.2   (4 leaves ∥)
│      └─ wave 2: T2.4, T2.2b, T4.4                 (3 leaves ∥)
│
├─ WAVE 2 (after dsl folds; ops may still run)
│  ├─ trust (Claude TL) ─ review-gate trust, against the new DSL
│  │   ├─ wave 1: T1.1, T1.4, T2.3                  (3 leaves ∥)
│  │   ├─ wave 2: T1.2, T1.3, T1.5                  (3 leaves ∥)
│  │   └─ wave 3: T1.6 (design call at scaffold, then 1 leaf)
│  ├─ seam-audit (Claude leaf, direct)  ─ T3.6 (now audits the post-DSL surface too)
│  └─ mock-macro (Gemini leaf, direct)  ─ T3.2 (seam.rs has settled)
│
├─ WAVE 3 (after wave 2 folds)
│  └─ rmcp  (Claude child, solo — no sub-leaves) ─ T4.3, riskiest, long leash
│
└─ WAVE 4 (close-out)
   ├─ docs-trueing (Claude leaf) ─ T4.5
   └─ T4.6 retro — root + human
```

Max fan-out at any node ≤ 4. Root sees O(TLs), never O(leaves).

---

## Root runbook

**Bootstrap (post-recreate):** create the team your SessionStart hook names (TeamCreate)
before anything else. Sanity: `exo doctor` (expect clean), `git log --oneline -3` on
hooksock (expect the design-doc commit `d0a46e1e` or later).

1. **Spawn wave 1**: `fork_wave` with two children — `dsl` and `ops` — each prompt: *"Read
   your charter: plans/2026-06-orchestration-tree.md § your name, then the references it
   names. Execute scaffold-fork-converge per the charter. This plan file's other sections
   are not your task."* Then **idle**.
2. **On `[READY]` from `dsl`**: scope-check the fold (`git diff --name-only $(git merge-base
   HEAD <branch>) <branch>` — expect only the Phase A/B file inventory from the decision doc
   + docs), `merge`, then `cargo test --workspace`. This fold is the foundation for wave 2 —
   verify before proceeding.
3. **Spawn wave 2**: `fork_wave` → `trust`; `fork_wave` → `seam-audit` (Claude, solo);
   `spawn_gemini` → `mock-macro` (T3.2 spec: month plan lines + full macro sketch — TL-write
   it self-contained; boundary: `rust/exo/src/testing.rs`, `rust/exo-framework/tests/seam.rs`).
   Idle.
4. **On `[READY]` from `ops` / `trust` / leaves**: scope-check, `merge`, workspace build
   after each fold (cross-crate rule). Merge order within a day: smaller fold first.
5. **Spawn wave 3**: `rmcp` (T4.3) as a solo Claude child. Its bar: wire-compatible
   `initialize`/`tools/list`/`tools/call`; verify = existing E2E + live-session smoke. Fold
   LAST among anything in flight.
6. **Wave 4**: `docs-trueing` (T4.5, Claude leaf — every v2 CLAUDE.md gaps section trued,
   `.claude/rules/exomonad.md` updated for T1.4/T1.6 surface changes). Then T4.6 retro with
   the human: fold outcomes into memory + next-month seed.

**Root discipline** (root.md protocol applies): after spawning, END THE TURN. Messages
arrive between turns. Never touch a child worktree, never checkout another branch, never
hand-fix a leaf's code — re-spec or escalate. Until T1.5 lands, the manual scope-check on
every fold is mandatory. If a `merge` hits conflicts it fails hard — do NOT raw-`git merge`;
re-task the child to rebase (`teardown-order` and `merge-conflict-skips-teardown` lessons
apply if cleanup is needed: kill pane FIRST, nested reviewers before parents).

**Known live risks while managing:** until T2.2b lands, a Gemini leaf hitting provider quota
dies/stalls silently — if a leaf goes quiet way past its size, `tmux capture-pane -t <pane>
-p -S -300` before assuming idle. Reviewers can also stall if anything pre-deploy lingers
(the ReviewAborted fix needs the P0.2 restart). A reviewer that printed its verdict as prose
needs a tmux-paste poke.

---

## Charter: `dsl` (TL) — T3.7 implementation

**Read first:** `docs/decisions/exo-authoring-dsl.md` (the ENTIRE design + implementation
map — exact signatures, pinned fold semantics, verified per-file site lists). Also
`rust/exo-framework/CLAUDE.md`, `rust/exo/CLAUDE.md`. The design is settled — zero design
latitude except where the doc explicitly flags a choice.

**Wave A — the Tool flip (wire-identical).**
- *Scaffold (TL-written, one commit, workspace GREEN):* `exo-framework/src/tool.rs` — rename
  `Tool` → `ErasedTool` **workspace-wide** (incl. `dyn Tool` sites the doc lists:
  outbound.rs, framework roles.rs:26, seam.rs, seam_proof.rs — rename only, stays green);
  add typed `Tool`, `Adapter`, `tool()` per the doc; rewrite the tool.rs header doctrine.
  Verify: `cargo test --workspace`.
- *Fork (1 Gemini leaf):* convert the 9 tool files (adapter deleted, inherent `run` + statics
  → `impl Tool<R>`), roster arms `Box::new(X)` → `tool(X)`, the 3 messaging erased-call test
  rewrites (messaging.rs:165/190/218). Full-code spec — lift the shapes and site list
  straight from the decision doc. Boundary: `rust/exo/src/tools/*.rs`, `rust/exo/src/roles.rs`.
  Verify: `cargo test -p exo && cargo check --workspace`.
- *Converge:* merge on [READY]; workspace test.

**Wave B — hook pipelines.**
- *Scaffold (TL-written, one commit):* `exo-framework/src/roles.rs` (aliases `PreToolRule`/
  `StopGate`/`StopObserver`, `RoleDef` Vec fields + `session_start` fn unchanged, the two
  fold methods with the doc's pinned semantics), `exo-framework/Cargo.toml` + `tracing`,
  and `exo-framework/tests/seam.rs` updated (literals + invocations) so **`cargo test -p
  exo-framework` is green**. NOTE: the workspace goes red here (exo, exo-node literals) until
  the leaves land — expected; no pre-merge check scripts exist to trip on it.
- *Fork (2 Gemini leaves, parallel, file-disjoint):*
  - **B1 — exo crate:** gates.rs decomposed per the doc's map (gates return `CapResult`,
    observers `()`; monoliths deleted; tests re-targeted at `role_def(...).run_stop(...)`;
    structural Gemini test `rd.stop.is_empty()` + behavioral dirty-mock test; assert the
    flagged git-error delta), roles.rs roster + tests, `exo/tests/seam_proof.rs` literal +
    vec-element asserts. Verify: `cargo test -p exo -p exo-framework` (NOT workspace — the
    sibling crate is red until both leaves fold; the TL integration commit restores it).
  - **B2 — exo-node:** hook.rs:110/129 → `run_pre_tool_use`/`run_stop` + its 3 test-module
    literals, hooksock/server.rs:142/175, test_support.rs:76-83, tests/common/mod.rs:112-117.
    Verify: `cargo test -p exo-node`.
- *Converge + integrate (TL commit):* workspace green; decide + implement the in-process
  Stop-arm question the doc flags (hook.rs:128-137: add agent-type shaping or document
  socket-only); fix-or-loudly-document the Gemini Modify-drop (server.rs:162-165).

**Wave C — docs + submit:** update `exo/CLAUDE.md` (Shape rows, "The gates" section, Roles
table, gap bullet), `exo-framework/CLAUDE.md`, module headers, `rust/CLAUDE.md` one-liners,
month-plan T3.7 → DONE. Then `submit_branch`.

**Leaf spec anti-patterns (put FIRST in every spec, plus the month plan's standing rules):**
do NOT redesign anything — the trait/fn signatures in your spec are exact; do NOT delete
fns the spec doesn't name (only the 4 monoliths + 9 adapters die); do NOT touch
`exo-caps` or `exo-runtime`; `Modify`-threading and observer semantics are pinned — copy
the spec's fold/stage bodies.

---

## Charter: `ops` (TL) — drift, signals, lifecycle, disjoint tests

**Read first:** month plan §Week 2 + the named task entries (T2.1, T2.2a/b, T2.4, T4.1,
T4.2, T4.4) — they carry files, pre-decided judgment calls, and verify commands. Specs are
TL-written, self-contained; the month plan is never handed to a leaf.

- *Wave 1 (4 leaves ∥, all file-disjoint):* **T2.1** version stamp (build.rs + boot log +
  doctor drift check); **T2.2a** worker (research probe, report via `notify_parent`
  `message`, NO code); **T4.1** birth E2E (tests/e2e, follow `tests/e2e/CLAUDE.md` exactly);
  **T4.2** exo-scry portable-rung test seam.
- *Wave 2 (3 leaves ∥):* **T2.4** doctor lingering-resource sweep (builds on T2.1's doctor
  changes; teardown-order anti-patterns are load-bearing — copy them from the task entry);
  **T2.2b** budget-failure signal (spec written FROM T2.2a's report; likely liveness.rs +
  inbound.rs reap path); **T4.4** cut `ActiveTeam.me` (after T4.2 — same crate, avoid
  parallel edits).
- *Boundary note:* your subtree must NOT touch `exo/src/{gates,roles}.rs`, `exo/src/tools/`,
  `exo-framework/`, or exo-node's hook.rs/hooksock/outbound/test_support — the `dsl` subtree
  owns those this wave. T2.2b's inbound.rs reap path is yours and disjoint.
- T2.3 is NOT yours — it moved to `trust` (it edits a fn that only exists post-DSL).

---

## Charter: `trust` (TL) — review-gate trust, against the NEW DSL

**Spawn timing:** root spawns this only after the `dsl` fold is merged. **Read first:**
month plan §Week 1 task entries + `docs/decisions/exo-authoring-dsl.md` §authoring-surface
(you are writing stages/observers, not monolith edits) + `rust/exo/src/review.rs`.

- *Wave 1 (3 leaves ∥, review.rs single-writer per wave):* **T1.1** boundary persist +
  reviewer scope section (spawn.rs + review.rs + testing.rs); **T1.4** review on/off config
  (config.rs + the spawn call site in submit.rs/review.rs — coordinate: T1.1 owns review.rs
  this wave, so if T1.4's gate lands in review.rs, sequence it behind T1.1 instead);
  **T2.3** untracked-only unwedge — now an edit to `require_clean_tree` (+ `Git::is_clean`
  split if needed: cap + Runtime + mock + seam stub, per the task entry).
- *Wave 2 (3 leaves ∥):* **T1.2** unsubmitted-work signal — now a NEW stop observer
  (`nudge_unsubmitted`-style, `()` return, KV-deduped) wired into Dev/Worker
  `stop_observers` + the `submitted:{branch}` flag in submit.rs; the #20426 anti-pattern is
  now structural but keep it in the spec anyway; **T1.3** reviewer rubric line; **T1.5**
  merge-tool scope report (depends on T1.1's KV key — removes root's manual scope-check).
- *Wave 3:* **T1.6** back-channel — make the design call at scaffold time per the task
  entry (read review.rs spawn topology; tree-edge invariant is load-bearing), then one leaf.

---

## Solo children (root-spawned, no sub-leaves)

- **`seam-audit`** (Claude, T3.6): read-only audit + doc patch; bar-not-bytes spec. Now also
  covers the post-DSL `exo-framework` surface.
- **`mock-macro`** (Gemini, T3.2): `macro_rules!` stub generator; boundary testing.rs +
  seam.rs; spec includes a concrete macro sketch (full-code — this is mechanical).
- **`rmcp`** (Claude, T4.3): wave 3, solo, wire-compat bar, fold last.
- **`docs-trueing`** (Claude, T4.5): wave 4, after everything folds.

---

## Done criteria (the whole tree)

- Month plan tasks T1.1–T1.6, T2.1–T2.4, T3.2, T3.6, T3.7, T4.1–T4.5 all ✅ with their
  verify commands green; `cargo test --workspace` green on hooksock.
- The roster reads per the decision doc's authoring surface; `rd.stop.is_empty()` holds for
  every Gemini role (structural #20426).
- `exo doctor` clean: no orphaned worktrees/panes from the swarm itself (the tree must not
  leave the mess T2.4 detects).
- T4.6 retro: outcomes folded into memory + next-month seed drafted.
