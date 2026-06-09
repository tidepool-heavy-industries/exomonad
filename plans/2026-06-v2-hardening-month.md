# v2 Node-Mode: Hardening + Generics Month (2026-06-09 → 2026-07-07)

> **⚠️ SPAWNED AGENTS: THIS FILE IS NOT YOUR TASK.** If you are a leaf/worker/reviewer, your
> task is ONLY what your spawn spec says. Do not implement anything from this file. Do not
> read further.

Scope: the v2 node-mode stack only (`exo-caps`, `exo-framework`, `exo`, `exo-node`,
`exo-runtime`, `exo-scry`, `claude-teams-bridge`). Classic (`exomonad-core`/`exomonad`) is
out of scope entirely. `exomonad-shared` is touched only where a v2 task forces it.

**Month shape:** weeks 1–2 buy operational trust (review gate, deployment drift, failure
signals); week 3 is the refactor centerpiece (cap supertraits + engine-goes-generic),
scaffold-first; week 4 trues tests and docs against the new foundation. Hardening lands
first so the refactor waves run under a gate we trust.

**Implementer key:** `G` = Gemini leaf (one narrow task, judgment pre-decided, full-code
spec where mechanical). `C` = Claude leaf (judgment work; bar-not-bytes spec). `TL` =
root/TL writes it directly (scaffold commits only). Sizes: S (≤½ day), M (1 day), L
(scaffold + multi-leaf wave).

**Standing spec rules (every spawn spec inherits these as anti-patterns):**
- ONLY modify the files listed in the spec. Do not delete code that "looks dead" after your change.
- `cargo fmt -p <crate>` only — never bare `cargo fmt`.
- `git add <specific files>` — never `git add .`/`-A`.
- Reports go in the `message` argument of the `notify_parent` MCP tool (not `tasks_completed`, not a bash command).
- Verify = the exact commands in the spec, plus `cargo check --workspace` when the task touches a trait or a caller in another crate.
- This plan file is not your task (see header).

**Standing merge rules (TL side, until T1.5 lands):**
- Scope-check every fold: `git diff --name-only $(git merge-base HEAD <branch>) <branch>` against the spec's boundary.
- Build the whole workspace after merging anything that touches a cross-crate caller.

---

## Week 0 — Prologue (before wave 1; no leaves)

- **P0.1 — Land the in-flight hooksock logging diff.** Already reviewed clean; includes the
  Log-cap removal. Fold in the stale "Wave-0 scaffold" banner fix in
  `rust/exo-caps/src/lib.rs:7-10` (the seam is long past Wave 1). `TL`, S.
- **P0.2 — Deploy + restart.** `just install-all-dev` and recreate the session so running
  sidecars pick up the `stop_reviewer`/ReviewAborted fix that is currently
  source-only. This is the live instance of the drift problem T2.1 solves. `TL`, S.

---

## Week 1 — Review-gate trust

The review gate is the convergence backbone; every gap in it is a per-merge TL tax.
Wave 1 tasks are independent; wave 2 builds on wave 1's merged state.

### Wave 1 (parallel)

- **T1.1 — Persist the spawn boundary; surface it to the reviewer.** `G`, M.
  - At spawn, write the spec's `boundary` (file/glob list) to KV: key `boundary:{child_agent_name}`, value JSON array. At review-request build time, read it and inject a "scope" section into the reviewer's context: the boundary + instruction to flag any changed file outside it as a structured finding (not an auto-reject).
  - Files: `rust/exo/src/tools/spawn.rs` (write), `rust/exo/src/review.rs` (read + inject), `rust/exo/src/testing.rs` (MockRuntime KV already exists).
  - Pre-decided: KV is the storage (not papers — papers are engine-owned and immutable); missing boundary key ⇒ review proceeds without a scope section (older children), never an error.
  - Verify: `cargo test -p exo`, new unit test that a built review prompt contains the boundary block.
- **T1.2 — Unsubmitted-work signal at dev stop.** `G`, M.
  - Known edge (validated 2026-06-05): devs commit but never `submit_branch`; the clean-gate passes and the work silently stalls. In `stop_notify`: if `fork_point()` shows the branch ahead of base AND no `submitted:{branch}` KV flag (set by `submit_branch`), (a) deliver a one-shot synthetic self-nudge "you have unsubmitted commits — call submit_branch" (KV-deduped), (b) tag the `ChildIdle` summary with `unsubmitted-commits` so the parent sees it.
  - Files: `rust/exo/src/gates.rs`, `rust/exo/src/tools/submit.rs` (set flag), tests in `rust/exo/src/gates.rs`.
  - Anti-pattern (load-bearing): NEVER block a Gemini at stop (gemini-cli #20426). This is signal-only.
  - Verify: `cargo test -p exo`; unit tests for all four (ahead × flag) cases.
- **T1.3 — Reviewer intent-cue anchoring fix.** `G`, S.
  - Known failure: reviewer rubber-stamps real bugs when the diff is labeled "throwaway/probe/WIP". Add an explicit rubric line to the reviewer protocol/prompt: intent labels in code or commit messages do not lower the review bar; review every diff as production code.
  - Files: reviewer prompt construction in `rust/exo/src/review.rs` and/or the reviewer role protocol in `rust/exo/src/roles.rs`.
  - Verify: `cargo test -p exo`; prompt-content unit test.
- **T1.4 — Review on/off config.** `G`, S.
  - `.exo/config.toml`: `[review] enabled = true` (default true; absent table ⇒ enabled). When disabled, `submit_branch` skips reviewer spawn and notifies the parent directly (the pre-review behavior).
  - Files: `rust/exo/src/config.rs`, the reviewer-spawn call site in `rust/exo/src/tools/submit.rs` or `review.rs`.
  - Pre-decided: global bool only — no per-role/per-depth policy this month.
  - Verify: `cargo test -p exo` with a config-parse test for present/absent/false.

### Wave 2 (after wave 1 folds)

- **T1.5 — Merge-tool scope report.** `G`, S. Depends: T1.1.
  - In the `merge` tool: before folding, `git diff --name-only` from fork-point and compare against the `boundary:{child}` KV. Out-of-scope files do NOT block the merge — they are listed loudly in the tool's result text ("out-of-boundary: …"). Removes the standing manual scope-check rule above.
  - Files: `rust/exo/src/tools/merge.rs`.
  - Verify: `cargo test -p exo` with a mocked-KV unit test.
- **T1.6 — Submitter→reviewer back-channel.** `C`, M. Depends: design call below.
  - Gap note: two-way colleague reply needs `send_message` on dev. OPEN DESIGN CALL: `Addressee` is tree-edges only (a load-bearing invariant) — a dev↔reviewer edge depends on where the reviewer actually hangs in the tree. Scaffold-time decision (TL reads `review.rs` spawn topology first): if reviewer is the dev's child, it's already an edge (grant dev `send_message` scoped to children); if it's the parent's child, relay through the parent rather than breaking the invariant.
  - Files: TBD at scaffold; likely `rust/exo/src/roles.rs` (tool matrix) + `rust/exo/src/tools/messaging.rs`.

---

## Week 2 — Operational trust (drift, signals, lifecycle)

### Wave 3 (parallel)

- **T2.1 — Version stamp + drift detection.** `G`, M.
  - Embed the build's git SHA at compile time (build script sets `EXO_BUILD_SHA`; `option_env!` read, "unknown" fallback). Sidecar logs it at boot AND writes it to `<project>/.exo/logs/sidecar/<run_id>/<branch>.version`. `exo doctor` gains a drift check: compare each live run's version files against the current binary's SHA; report stale sidecars by name.
  - Files: `rust/exo/build.rs` (new), `rust/exo/src/main.rs` or `rust/exo-node` boot path, `rust/exo/src/doctor.rs`.
  - Pre-decided: report-only (doctor never auto-restarts sessions).
  - Verify: `cargo test -p exo`; `exo doctor` manual run in a live session.
- **T2.2a — Token-budget failure: research probe.** `G` worker (ephemeral), S.
  - Determine the *observable* signal when a Gemini leaf exhausts provider quota mid-task: pane output patterns, process exit code, gemini-cli behavior (cite gemini-cli source/issues). Deliverable: a report (in `notify_parent` `message`) with exact match patterns + reproduction notes. NO code changes.
- **T2.2b — Token-budget failure: distinct signal.** `G`, M. Depends: T2.2a report.
  - Implement detection per T2.2a (likely: liveness/reap path inspects pane tail on unexpected child death) and emit a distinct parent notification `[FAILED: budget]` instead of a generic idle/death. Model fallback (re-spawn with different model) is OUT of scope this month — signal only.
  - Files: TBD from T2.2a (likely `rust/exo-runtime/src/liveness.rs`, `rust/exo-node/src/inbound.rs` reap path).
- **T2.3 — Stop dirty-gate: untracked-only unwedge.** `G`, S.
  - Gap: the `stop` dirty-gate can wedge an agent holding untracked artifacts it won't commit. Pre-decided split: modified/staged tracked files ⇒ block as today; untracked-only ⇒ allow exit, but list the untracked paths in the `ChildIdle` summary so the parent sees them.
  - Files: `rust/exo/src/gates.rs`, `rust/exo-runtime/src/git.rs` if `is_clean` needs a tracked/untracked split (new cap method ⇒ also `rust/exo/src/testing.rs` mock + `rust/exo-framework/tests/seam.rs` stub).
  - Verify: `cargo test -p exo -p exo-runtime`; `cargo check --workspace`.
- **T2.4 — Doctor: lingering-resource sweep.** `G`, M.
  - Extend `exo doctor`: detect (a) worktrees whose pane is dead but dir remains (the known dirty/nested-reclaim leak), (b) orphaned panes with no worktree, (c) stale inbox files for dead runs. `--fix` applies teardown in the safe order: **kill pane FIRST, then worktree remove, nested children before parents**.
  - Files: `rust/exo/src/doctor.rs`, `rust/exo-runtime/src/topology.rs` (read-only reuse).
  - Anti-pattern (load-bearing): never `git worktree remove` a live pane's tree — kill-pane first; never touch tracked-dirty worktrees under `--fix` without listing them as "needs manual review".
  - Verify: `cargo test -p exo`; manual run against a deliberately-orphaned fixture session.

---

## Week 3 — Refactor centerpiece (scaffold-first)

Two sequential scaffold-led waves. Each: TL commits the scaffold (new trait shape, compiles
with stubs/defaults), leaves migrate one crate each, TL integrates. Per the standing rule:
whole-workspace build after every fold — these waves are cross-crate by nature.

### Wave 4 — Cap inter-dependencies (supertraits)

- **T3.0 — Scaffold (TL).** Composite caps gain primitive-cap supertraits per the queued
  design: `Spawner: Tmux + Git + Fs`, `Bus: Fs`, `Topology: Tmux + Fs` (exact bounds
  finalized at scaffold against real impl usage — bounds reflect what the runtime impls
  already reach for, composites must not re-implement primitive domains). Scaffold commit =
  new bounds in `rust/exo-caps/src/{spawner,bus,topology}.rs` + doc updates + whatever
  default-stub plumbing keeps `cargo check --workspace` green.
- **T3.1 — Migrate exo-runtime impls.** `G`, M. Mechanical: satisfy the new bounds, delete
  any now-duplicated primitive logic inside composite impls (spec lists each duplication
  explicitly — found at scaffold time).
- **T3.2 — Mock/test-stub generator.** `G`, M. The mock tax is 4 sites per cap-signature
  change (trait, Runtime, MockRuntime, seam.rs). Add a `macro_rules!` default-unimplemented
  stub generator used by `rust/exo/src/testing.rs` and `rust/exo-framework/tests/seam.rs`
  so future signature changes touch 2 sites, not 4. Migrate both files onto it.
- **T3.3 — Bounds ripple.** `G`, S. Update `exo-framework::PolicyCaps` and any `exo` tool
  bounds that can now *narrow* (a tool needing `Spawner` no longer also names `Tmux`).
  Spec enumerates exact signatures (TL greps at scaffold time).

### Wave 5 — Engine goes generic (D::System)

- **T3.4 — Scaffold (TL).** Move `ReviewVerdict` out of `exo-caps` into the `exo` domain
  (`rust/exo/src/review.rs`); the inbound Domain arm and `SystemCtx`/`handle_system` become
  fully generic over `D::System` (the wiring already exists transitionally —
  `rust/exo-node/src/inbound.rs:252` marks the spot). exo-caps ends the wave with zero
  review vocabulary.
- **T3.5 — Migrate exo-node + exo call sites.** `G`, M. Mechanical relocation per scaffold.
- **T3.6 — Seam audit.** `C`, S. One Claude leaf reads the post-refactor `exo-caps` public
  API and confirms: no domain vocabulary left in the engine crates, docs match the new
  trait graph, every gap note still true. Output: a findings report + doc patch.

### Stretch (only if waves 4–5 fold clean by Friday)

- **T3.7 — RoleDef authoring polish.** `C`, L. The deferred builder/trait reshape of
  `RoleDef<R>` toward the "Rust DSL" north star (big framework, minimal domain crate).
  Judgment-heavy; if it slips, it's the natural centerpiece of *next* month.

---

## Week 4 — Tests, ports, trueing

### Wave 6 (parallel)

- **T4.1 — Birth E2E test.** `G`, M. `birth` has zero automated coverage (needs live
  tmux+git — the E2E harness has both). Add `tests/e2e` case: spawn a child →
  assert papers written, inbox path derived, `children.jsonl` `Spawned`+`Started`, pane
  live → teardown → assert reclaim. Follow `tests/e2e/CLAUDE.md` conventions exactly.
- **T4.2 — Portable-resolution fallback test.** `G`, S. The `resolve_via_transcript` rung
  has effectively never run (Linux `resolve_self` always wins). Add an env-var test seam
  (`EXO_SCRY_FORCE_PORTABLE=1` ⇒ skip `resolve_self`) + a test exercising the portable
  rung on Linux. Non-Linux support itself stays out of scope (backlog).
  - Files: `rust/exo-scry/src/`, test alongside.
- **T4.3 — outbound → rmcp migration.** `C`, L. Replace the hand-rolled JSON-RPC loop in
  `rust/exo-node/src/outbound.rs` with the `rmcp` crate (already a workspace dep via
  Shoal framing). Wire-compatible: `initialize`/`tools/list`/`tools/call` must behave
  identically from Claude/Gemini's side. Verify: existing converge E2E + a live session
  smoke test. Riskiest task of the month — schedule first, fold last.
- **T4.4 — Cut vestigial `ActiveTeam.me`.** `G`, S. Always-`None`, kept "for a future
  path" — dead-code rule says cut; trivially restorable from history.
  - Files: `rust/exo-scry/src/` + any destructuring call sites.

### Wave 7 (close-out)

- **T4.5 — Docs trueing pass.** `C`, M. Every v2 crate CLAUDE.md "Gaps / not-yet" section
  rewritten against post-month reality; the `v2-node-mode-open-gaps` style drift this plan
  was built from is the failure mode being prevented. Also update
  `.claude/rules/exomonad.md` if tool/role surfaces changed (T1.4, T1.6).
- **T4.6 — Month retro + next-month seed (TL).** Fold outcomes, update memory, draft the
  next plan's seed (likely: RoleDef DSL if T3.7 slipped, model fallback on `[FAILED:
  budget]`, non-Linux portability).

---

## Backlog (explicitly out this month)

- Non-Linux portable cwd reader (macOS native delivery).
- `ChildKind::Standalone` as a distinct representation.
- Porting classic's richer `pre_tool_use` antipattern set + PII rewrite.
- Model fallback / auto-respawn on budget failure (T2.2b is signal-only).
- Forced-teardown wrap-up pass (commit/stash before kill) — needs a design decision
  (auto-commit conflicts with the stale-index Gemini failure mode); revisit with retro.
- Shoal agent expansion.

## Dependency graph (waves)

```
P0.1 P0.2 ──► W1 [T1.1 T1.2 T1.3 T1.4] ──► W2 [T1.5 T1.6]
                                       └──► W3 [T2.1 T2.2a→T2.2b T2.3 T2.4]
W3 ──► W4 [T3.0 → T3.1 T3.2 T3.3] ──► W5 [T3.4 → T3.5 T3.6] (─► T3.7 stretch)
W5 ──► W6 [T4.1 T4.2 T4.3 T4.4] ──► W7 [T4.5 T4.6]
```
