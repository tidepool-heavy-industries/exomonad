# 2026-06 Orchestration Tree — executing the month's remainder as one swarm

> **⚠️ SPAWNED AGENTS: THIS FILE IS NOT YOUR TASK.** If you are a leaf/worker/reviewer, your
> task is ONLY what your spawn spec says. Do not implement anything from this file. Do not
> read further. TLs: read ONLY the charter section your spawn prompt names.

**What this is:** the self-contained runbook for the ROOT session to execute the open work in
[`2026-06-v2-hardening-month.md`](2026-06-v2-hardening-month.md) as a recursive tree — Opus
TLs decompose, Gemini leaves implement, the review gate converges. Written to survive a
session recreate: a fresh root executes this with no prior conversation. Task *content* lives
in the month plan (task IDs) and `docs/decisions/exo-authoring-dsl.md` (the DSL design);
this doc owns *sequencing, decomposition, and merge discipline*. Reviewed adversarially
2026-06-09 (3 reviewers: correctness, topology, DSL-ambition); their accepted findings are
folded in below.

---

## Wave 0 — DONE (landed on hooksock before the deploy, so the deploy carries them)

- **P0.1** ✅ landed (`0d12c92b` — Log-cap removal + logging-hygiene pass). Working tree clean.
- **T1.3** ✅ landed 2026-06-09 (review round): intent-cue rubric line in BOTH
  `protocol.rs::REVIEWER` and submit.rs's `review_task`, with prompt-content tests. Every
  review in this run benefits. T1.3 is REMOVED from the `trust` charter.
- **Stale-rules banner** ✅: `.claude/rules/exomonad.md` (auto-loaded by every agent) now
  carries a v2 banner overriding its Classic claims (file_pr/Copilot/fork_session-default).
  Full trueing stays T4.5.

**Prerequisite (human): P0.2** — `just install-all-dev` + session recreate, so live sidecars
run the ReviewAborted fix (confirmed committed: `gates.rs` `stop_reviewer` +
`verdict_produced` flag) AND the T1.3 rubric. Do not spawn review-gated children before
this. While recreating: `.exo/config.toml` says `tmux_session = "EXOMONAD"` but the prior
live session was `EXOMONAD-exp` — reconcile.

---

## Re-sequencing: DSL lands FIRST

The month plan ordered hardening (W1/W2) before the stretch DSL work. That ordering is
**superseded**: T3.7's design is accepted and it reshapes the exact files the trust tasks
touch. Landing trust tasks first would write them against a surface T3.7 immediately deletes.

| Task | Files it touches | T3.7 collision |
|---|---|---|
| T1.1 | spawn.rs, review.rs, testing.rs | Phase A converts spawn.rs adapters |
| T1.2 | gates.rs (`stop_notify`), submit.rs | Phase B *deletes* `stop_notify` → becomes a new **stop observer** |
| T1.4 | config.rs + submit.rs/review.rs | Phase A converts submit.rs |
| T2.3 | gates.rs (`stop` dirty-gate) | Phase B decomposes `stop` → becomes a **`require_clean_tree`** edit (in `exo_framework::stages` post-design) |
| T3.2 | testing.rs + seam.rs | Phase B rewrites seam.rs RoleDef literals; **also collides with T2.3's possible cap split** → T3.2 runs in trust wave 2 |
| T4.3 | outbound.rs | Phase A renames `dyn Tool` there |

Known sacrifice (accepted consciously): most of the run executes under the *unhardened*
review gate. Mitigations: T1.3 landed in wave 0 (every review gets it via P0.2); a **mid-run
redeploy** after the trust fold (below) gives the late waves the hardened gate.

---

## The tree

```
root (hooksock) — decompose, spawn, idle, merge. NEVER implements (wave-0-class
│                 prologue edits and integration bookkeeping excepted).
│
├─ WAVE 1 (parallel)
│  ├─ dsl   (Claude TL) ─ T3.7 impl: Phase A → Phase B → integrate+docs
│  │   ├─ wave A: 1 Gemini leaf (9 adapter conversions across 6 files)
│  │   └─ wave B: 2 Gemini leaves ∥ (exo crate / exo-node crate)
│  └─ ops   (Claude TL) ─ disjoint operational work (≤3 Gemini in flight incl. reviewers)
│      ├─ wave 1a: T2.2a(worker), T4.2          (2 ∥ — quota-staggered)
│      ├─ wave 1b: T2.1, T4.4                   (2 ∥; T4.4 after T4.2: same crate)
│      └─ wave 2:  T2.4, T2.2b, T4.1            (3 ∥; T4.1 needs the TL's v2-harness scaffold)
│
├─ WAVE 2 (after BOTH wave-1 TLs fold — quota stagger; trust is written against the new DSL)
│  └─ trust (Claude TL) ─ review-gate trust
│      ├─ wave 1: T1.1, T1.4, T2.3              (3 ∥; review.rs single-writer → T1.4's
│      │                                          call-site half sequences behind T1.1 if needed)
│      ├─ wave 2: T1.2, T1.5, T3.2              (3 ∥)
│      └─ wave 3: T1.6 — the elective DEPTH-3 chain (see charter)
│
├─ CHECKPOINT: mid-run redeploy (after trust folds): `just install-all-dev` (NO recreate).
│  New spawns now get the hardened gate. Immediately run `exo doctor` and watch T2.1's
│  fresh drift check flag every surviving sidecar as stale — in-run validation of T2.1.
│
├─ WAVE 3 (parallel, post-redeploy)
│  ├─ rmcp       (Claude child, solo) ─ T4.3, riskiest, fold last. Optional: one
│  │             spawn_worker research probe (rmcp-crate API survey) before coding.
│  ├─ seam-audit (Claude leaf, solo)  ─ T3.6 (after trust: audits T2.3's possible cap
│  │             split + the wave-C seam rename as landed facts, not races)
│  └─ gradient   (Claude leaf, solo)  ─ MG.1 model-gradient forking v1 (added 2026-06-10,
│                post-review insertion). Wave 3 because trust's T1.1 touches spawn.rs (same
│                file as ForkChildArgs.model) and the surface is post-DSL by then. Design:
│                docs/decisions/model-gradient-forking.md.
│
└─ WAVE 4 (close-out)
   ├─ docs-trueing (Claude leaf) ─ T4.5
   ├─ FINAL deploy + live smoke (human-coordinated): `just install-all-dev` + recreate +
   │  rmcp wire smoke + one live converge round — the DSL hook pipelines and T2.2b's
   │  quota signal are never live-validated otherwise. Done is declared AFTER this.
   └─ T4.6 retro — root + human, from the run log + corpora (below)
```

Max fan-out at any node ≤ 4. Root sees O(TLs), never O(leaves). `ops` and `trust` stay
flat by review verdict: ops' sub-clusters would make the parent a pure relay; trust's tasks
interlock on shared files (review.rs/gates.rs/submit.rs) and tree-edge-only messaging makes
sibling sub-TLs sharing files strictly worse.

---

## Root runbook

**Bootstrap (post-recreate):**
1. Create the team your SessionStart hook names (TeamCreate) before anything else.
2. Record `$EXOMONAD_SWARM_RUN_ID` and create `plans/2026-06-run-log.md` (committed; root
   may write it — integration-commit-class). Header: run_id, binary SHA, date.
3. `exo doctor` — it WILL report ~40 pre-existing stale worktrees (prior sessions; several
   orphaned dirs not even git-registered). **Record the count as baseline in the run log; do
   NOT clean** (untracked dirs are signal; T2.4 is the cleanup task). The done-criterion
   "doctor clean" means *no NEW mess attributable to this run*.
4. Config facts: `.exo/config.toml` has `yolo = true` and **no `model` field** — child model
   is the CLI default, "Opus TLs" is otherwise unenforced. Set the CLI default accordingly
   before spawning if it matters (it does: TLs should be Opus).
5. `git log --oneline -3` on hooksock — expect the wave-0 commits.

**Run-log discipline (every fold, ~1 line each):** wall-clock spawn→`[READY]`; review round
count; scope-check result (clean / out-of-boundary files); pokes (prose-verdict pastes,
capture-pane interventions); suspected quota stalls WITH pane-capture evidence (doubles as
T2.2a/b corroboration data). After the FIRST dsl-leaf approve: check the sidecar log
(`<project>/.exo/logs/sidecar/<run_id>/`) confirms `[READY]` escalated with **no LLM turn**
— cheap one-time validation that the structural gate survived the deploy.

**Per-fold sequence (every `[READY]`):**
1. **Harvest the review corpus FIRST**: copy `<child-worktree>/.exo/reviews/*.json` →
   `<project-root>/.exo/review-archive/<run_id>/` — the log self-destructs with the
   worktree at merge (it resolves against the submitter's cwd; durable fix rides T1.1).
2. Scope-check: `git diff --name-only $(git merge-base HEAD <branch>) <branch>` against the
   charter's boundary. Out-of-scope ⇒ judge, don't auto-reject (cross-crate caller changes
   can be legitimate — but log it).
3. `merge` (the tool — NEVER raw `git merge`), then `cargo test --workspace` (cross-crate
   rule), then `exo doctor` (new-orphan check), then the run-log line.
4. Weight the manual scope-check MORE on TL folds, not less — a TL's multi-wave branch is
   one giant diff reviewed by one Gemini reviewer; the per-leaf reviews are the real gate,
   the TL-fold review is the weak link.

**Wave sequence:** spawn wave 1 (`fork_wave` → `dsl`, `ops`; prompts: *"Read your charter:
plans/2026-06-orchestration-tree.md § your name, then the references it names. Execute
scaffold-fork-converge per the charter. Other sections of that file are not your task."*)
→ idle. Both folded → spawn `trust` → idle. Trust folded → **mid-run redeploy checkpoint**
→ spawn `rmcp` + `seam-audit` + `gradient` → idle. Folded → wave 4.

**Failure paths:**
- *Leaf silent past its size*: `tmux capture-pane -t <pane> -p -S -300` before assuming
  idle. Quota death (pre-T2.2b, and pre-redeploy even after it lands) is silent.
- *Leaf fails 3×* (`[FAILED: id]`): TL re-decomposes or escalates to root; root re-specs —
  never hand-fixes.
- *Reviewer stall*: post-P0.2 the ReviewAborted path fires loud; a reviewer that printed
  its verdict as prose gets a tmux-paste poke ("invoke the tool for real").
- *TL wedge* (silent past expected wall): capture its window; poke; else kill + re-fork a
  fresh TL — its leaves' branches survive on disk, the new TL resumes from "merge what's
  submitted".
- *Dead dsl wave-B leaf*: respawn FRESH from the wave-B scaffold commit (re-spec, new
  worktree) — never repair in place. The workspace-red window makes B1 the run's single
  point of failure; concentrate capture-pane vigilance on dsl during wave 1.
- *Aborting a subtree*: cooperative shutdown first, then `force:true` cascade — NEVER
  manual tmux kills (orphans worktrees; violates teardown order). Log the outcome — forced
  cascade at depth is otherwise-untested coverage, an incident is data.
- *`merge` conflict*: it fails hard (no teardown). Re-task the CHILD to rebase onto the
  current parent branch and re-submit; if hand-teardown is ever needed: kill pane FIRST,
  worktree second, nested reviewers before parents.

---

## Charter: `dsl` (TL) — T3.7 implementation

**Read first:** `docs/decisions/exo-authoring-dsl.md` IN FULL — design + round-2 amendments
(stages contrib module, RoleSpec, riders) + implementation map with exact signatures and
verified site lists. Also `rust/exo-framework/CLAUDE.md`, `rust/exo/CLAUDE.md`. The design
is settled — zero design latitude except where the doc explicitly flags a choice.

**Wave A — the Tool flip (wire-identical).**
- *Scaffold (TL-written, one commit, workspace GREEN):* `exo-framework/src/tool.rs` — rename
  `Tool` → `ErasedTool` **workspace-wide** (incl. the `dyn Tool` sites the doc lists —
  rename only, stays green); add typed `Tool`, `Adapter`, `tool()` per the doc; rewrite the
  header doctrine. Verify: `cargo test --workspace`.
- *Fork (1 Gemini leaf):* convert the **9 adapters across the 6 `tools/*.rs` files**
  (adapter deleted, inherent `run` + statics → `impl Tool<R>`), roster arms `Box::new(X)` →
  `tool(X)`, the 3 messaging erased-call test rewrites. Full-code spec lifted from the
  decision doc. Boundary: `rust/exo/src/tools/*.rs`, `rust/exo/src/roles.rs`.
  Verify: `cargo test -p exo && cargo check --workspace`.
- *Converge:* merge on [READY]; workspace test.

**Wave B — hook pipelines + the round-2 riders.**
- *Scaffold (TL-written, one commit):* `exo-framework/src/roles.rs` (aliases, `RoleDef` Vec
  fields + unchanged `session_start` fn, the two fold methods), **`exo-framework/src/stages.rs`**
  (`deny_git_add_all`, `require_clean_tree`, `announce_idle` — TL-written deliberately: it
  concentrates the behavior-sensitive decomposition in Opus-written code and shrinks B1),
  module rustdoc stage-authoring example, `Cargo.toml` + `tracing`, and
  `exo-framework/tests/seam.rs` updated so **`cargo test -p exo-framework` is green**. The
  workspace goes red here until the leaves land — expected; no pre-merge check scripts
  exist to trip on it.
- *Fork (2 Gemini leaves ∥, file-disjoint):*
  - **B1 — exo crate:** gates.rs shrinks to `abort_if_no_verdict` + `session_start` +
    framework-stage imports; roles.rs roster → pipelines **plus the riders: `RoleSpec`
    co-location, `role_str`↔serde conformance test, `all()` totality canary**; tests
    re-targeted at `role_def(...).run_stop(...)`; structural Gemini test + behavioral
    dirty-mock test; seam_proof.rs literal + vec-element asserts.
    Verify: `cargo test -p exo -p exo-framework` (NOT workspace — sibling crate red).
  - **B2 — exo-node:** hook.rs:110/129 → fold methods + its 3 test-module literals,
    hooksock/server.rs:142/175, test_support.rs, tests/common. Verify: `cargo test -p exo-node`.
- *Converge + integrate (TL commit):* workspace green; the in-process Stop-arm decision
  (hook.rs:128-137: shaping or documented socket-only); fix-or-document the Gemini
  Modify-drop (server.rs:162-165).

**Wave C — docs + riders + submit:** the cookbook table in `exo/CLAUDE.md`; framework
CLAUDE.md `stages` row + amended doctrine line; module headers; `rust/CLAUDE.md`;
month-plan T3.7 → DONE; **the `read_reviews`/`persist_reviews` → `read_state`/`persist_state`
seam rename** (~6 sites, mechanical). Then `submit_branch`.

**Leaf spec anti-patterns (FIRST in every spec, plus the month plan's standing rules):**
do NOT redesign anything — signatures are exact; do NOT delete fns the spec doesn't name
(only the 4 monoliths + 9 adapters die); do NOT touch `exo-caps`/`exo-runtime`; fold/stage
bodies are pinned — copy them from the spec.

---

## Charter: `ops` (TL) — drift, signals, lifecycle, disjoint tests

**Read first:** month plan §Week 2 + the named entries (T2.1, T2.2a/b, T2.4, T4.1, T4.2,
T4.4). Specs are TL-written, self-contained; the month plan is never handed to a leaf.
**Quota cap: ≤3 Gemini in flight (leaves + reviewers) in your subtree at any time** — hence
the staggered waves.

- *Wave 1a (2 ∥):* **T2.2a** worker (research probe, report via `notify_parent` `message`,
  NO code); **T4.2** exo-scry portable-rung test seam.
- *Wave 1b (2 ∥):* **T2.1** version stamp + doctor drift check; **T4.4** cut
  `ActiveTeam.me` (after T4.2 — same crate).
- *Wave 2 (3 ∥):* **T2.4** doctor sweep (builds on T2.1's doctor diff; teardown-order
  anti-patterns from the task entry are load-bearing); **T2.2b** budget signal (spec written
  FROM T2.2a's report); **T4.1** birth E2E — **CAUTION (review finding): `tests/e2e/` is a
  CLASSIC harness** (`run.sh` does `exomonad init`, WASM, interactive attach). A v2 case
  needs a non-interactive `exo init` runner. YOU scaffold the v2 runner skeleton (TL commit,
  modeled on the existing harness conventions but scripted/assertable); the leaf fills the
  assertions (papers, inbox path, `children.jsonl` Spawned+Started, pane live, reclaim). Do
  NOT tell the leaf to "follow tests/e2e/CLAUDE.md exactly" — your spec overrides it.
- *Boundary:* do NOT touch `exo/src/{gates,roles,testing}.rs`, `exo/src/tools/`,
  `exo-framework/`, or exo-node's hook.rs/hooksock/outbound/test_support/tests-common —
  the `dsl` subtree owns those this wave. T2.2b's liveness.rs + inbound.rs reap path is
  yours and disjoint. T2.3 is NOT yours (trust owns it — it edits post-DSL stage code).

---

## Charter: `trust` (TL) — review-gate trust, against the NEW DSL

**Spawn timing:** after BOTH wave-1 TLs fold. **Read first:** month plan §Week 1 entries +
`docs/decisions/exo-authoring-dsl.md` §authoring-surface (you write stages/observers, not
monolith edits) + `rust/exo/src/review.rs`. T1.3 is DONE (wave 0) — not yours.

- *Wave 1 (3 ∥):* **T1.1** boundary persist + reviewer scope section — **spec addition
  (review finding): also anchor the review-log path at the project root** (today
  `.exo/reviews/{safe}.json` resolves against the submitter's worktree cwd and dies at
  reclaim; mirror the sidecar-log path scheme). **T1.4** review on/off config (if its gate
  lands in review.rs, sequence behind T1.1 — review.rs is single-writer per wave). **T2.3**
  untracked-only unwedge — an edit to `require_clean_tree` in `exo_framework::stages`
  (+ `Git::is_clean` split if needed: cap + Runtime + mock + seam stub).
- *Wave 2 (3 ∥):* **T1.2** unsubmitted-work signal — a NEW stop observer (`()` return,
  KV-deduped) in Dev/Worker `stop_observers` + the `submitted:{branch}` flag in submit.rs;
  #20426 is structural now but keep the anti-pattern in the spec anyway. **T1.5** merge-tool
  scope report (T1.1's KV key; retires root's manual scope-check). **T3.2** mock-stub
  macro (moved here by review: collides with T2.3's possible testing.rs/seam.rs touch, so
  it runs the wave AFTER).
- *Wave 3 — T1.6, the elective depth-3 chain (a deliberate dogfooding trade, record it in
  your report):* `fork_wave` ONE Claude child **with `fork_session: true`** (it genuinely
  wants your accumulated wave-1/2 context for the design call: where does the reviewer hang
  in the tree — tree-edge invariant is load-bearing). The child decides, then spawns ONE
  Gemini leaf for the mechanical half, folds it, submits to you. This chain is the run's
  only natural exercise of: fork_session at depth, Claude↔Claude teamout at depth ≥2, and
  multi-hop ChildIdle/merge. If it wedges, that's exactly the data the retro wants.

---

## Solo children (root-spawned)

- **`seam-audit`** (Claude, T3.6, wave 3): read-only audit + doc patch; now also audits the
  post-DSL framework surface, T2.3's cap split, and the wave-C seam rename as landed facts.
- **`rmcp`** (Claude, T4.3, wave 3): solo; wire-compat bar; verify =
  `cargo test -p exo-node --test converge` (the converge test is a cargo integration test,
  NOT `tests/e2e/`) — the live wire smoke happens at the FINAL deploy step. Optional
  spawn_worker probe for rmcp-crate API survey first.
- **`gradient`** (Claude, MG.1, wave 3; added 2026-06-10 post-review): per-child `model` on
  `fork_wave` — the monotone descent lattice. Read
  `docs/decisions/model-gradient-forking.md` IN FULL (design settled, zero latitude; its
  [Implementation map] is the step list, [Tests] the verify bar). Static enum v1 — per-node
  schema rendering is explicitly OUT (post-T3.7 follow-up). Boundary: `rust/exo-caps/src/`,
  `rust/exo/src/{tools/spawn.rs,spawn.rs}`, `rust/exo-runtime/src/spawner.rs`,
  `rust/exomonad-shared/src/services/agent_control/{types.rs,launch.rs}`,
  `.exo/roles/devswarm/context/model-transition.md` (new). Verify:
  `cargo test --workspace`. Its live probe rides the FINAL deploy smoke (one sonnet
  downshift fork; see the record's [Validation]).
- **`docs-trueing`** (Claude, T4.5, wave 4): after everything folds.

---

## Done criteria (the whole tree)

- Month plan tasks T1.1–T1.6, T2.1–T2.4, T3.2, T3.6, T3.7, T4.1–T4.5 — plus MG.1 (added
  2026-06-10) — all ✅ with their verify commands green; `cargo test --workspace` green on
  hooksock.
- The roster reads per the decision doc; `rd.stop.is_empty()` holds for every Gemini role.
- **The final deploy + live smoke has run** (wave 4): recreate, rmcp wire smoke, one live
  converge round. Source-level green alone does not close the month.
- `exo doctor` shows no NEW mess vs the bootstrap baseline.
- `plans/2026-06-run-log.md` + `.exo/review-archive/<run_id>/` + sidecar logs exist as the
  retro corpus; T4.6 evaluates the SYSTEM from them, not just the tasks.
