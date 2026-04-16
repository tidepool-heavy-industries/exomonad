# Post-Wave Copilot Feedback Audit (2026-04-16)

This document consolidates Copilot review feedback from the 15 PRs merged during the 2026-04-16 fixup wave. Due to a poller state-machine limitation, `COMMENTED` reviews with inline feedback were treated as "no review," leading to several actionable suggestions being bypassed during the wave.

## PR #854: Remove unused Haskell prompt code

**Copilot Summary Excerpt:**
> Removes an unused Haskell WASM guest SDK module (`ExoMonad.Guest.Prompt`) and cleans up related exports/docs so the SDK surface matches actual usage.

**Inline Comments:**
- **EXOMONAD_DSL_LLM.md:597**: `ExoMonad.Guest.Prompt` is removed from the SDK/module index above, but the “Key Constraints” section later in this doc still mentions a “WASM Prompt <> bug” and refers to `Prompt`’s `Semigroup` instance.
  - **Verdict:** `action-needed` (outdated documentation reference).

---

## PR #855: Remove flexible JSON parsing in merge_pr

**Copilot Summary Excerpt:**
> Removes Haskell-side “flexible” JSON scalar parsing for `merge_pr` tool arguments, on the assumption that Rust-side MCP harness compatibility coercion now normalizes those values before they reach WASM.

**Inline Comments:**
- **haskell/wasm-guest/src/ExoMonad/Guest/Tools/MergePR.hs:68**: `mprPrNumber` and `mprForce` now rely on Aeson’s default `Int`/`Bool` decoding... which rejects harness-style scalar strings. The Rust boundary coercion only converts scalar strings when `x-exomonad-harness-compat: true` is set.
  - **Verdict:** `action-needed` (potential regression for harnesses not sending the compatibility header).

---

## PR #856: Schema-directed coercion for MCP tool arguments

**Copilot Summary Excerpt:**
> Implements schema-directed coercion for MCP tool arguments so that string-encoded JSON values are only coerced when the tool’s input JSON Schema indicates they should be.

**Inline Comments:**
- **rust/exomonad-core/src/mcp/harness_compat.rs:96**: `schema_type_is` treats JSON Schema `type: [..]` as an OR... That means `coerce_harness_value_with_schema` will coerce string values for schemas that explicitly allow strings.
  - **Verdict:** `action-needed` (false positive coercion for union types).
- **rust/exomonad-core/src/mcp/harness_compat.rs:69**: For `schema.type == \"integer\"`, the code currently coerces any JSON number (including decimals/exponent forms)... That can break downstream deserialization/validation.
  - **Verdict:** `action-needed` (integer coercion too broad).
- **rust/exomonad/src/serve.rs:716**: `call_tool` now calls `get_tool_definitions` on every tool invocation to fetch the schema... MATERIALly increase latency under load.
  - **Verdict:** `action-needed` (performance/caching).

---

## PR #857: Refactor tmux session handling

**Copilot Summary Excerpt:**
> Refactors tmux session handling to avoid direct `std::env::var("EXOMONAD_TMUX_SESSION")` lookups in service code by threading a `TmuxIpc` handle.

**Inline Comments:**
- **rust/exomonad-core/src/services/file_pr.rs:167**: `file_pr_async` is a public service API; adding a required `tmux_ipc: &TmuxIpc` parameter is a breaking signature change.
  - **Verdict:** `informational` (intended internal refactor, but breaking if external use).
- **rust/exomonad-core/src/services/file_pr.rs:249**: `tmux_events::emit_event` currently ignores its `session` argument... threading `tmux_ipc.session_name()` here has no functional effect.
  - **Verdict:** `action-needed` (no-op plumbing).
- **rust/exomonad-core/src/services/copilot_review.rs:208**: `wait_for_copilot_review` is `pub` and now requires `tmux_ipc`, which is a breaking API change.
  - **Verdict:** `informational` (similar to #857-1).
- **rust/exomonad-core/src/services/copilot_review.rs:239**: `emit_event` ignores session argument... passing `tmux_ipc.session_name()` here doesn’t change behavior.
  - **Verdict:** `already-addressed-by-other-PR` (addressed in later cleanups).
- **rust/exomonad-core/src/services/copilot_review.rs:275**: Same issue in the 0-comment review path: `emit_event` ignores the session argument.
  - **Verdict:** `already-addressed-by-other-PR`.

---

## PR #858: GC for stale agent directories

**Copilot Summary Excerpt:**
> Implements garbage collection for stale `.exo/agents/{name}/` directories during `exomonad init`.

**Inline Comments:**
- **rust/exomonad/src/init.rs:499**: `gc_stale_agents()` relies on `self.tmux()`... but `AgentControlService::new()` leaves `tmux_session` as `None`... any agent dir with a `routing.json` will be pruned as “dead tmux target”.
  - **Verdict:** `action-needed` (HIGH RISK: unintentional pruning of live agents).
- **rust/exomonad-core/src/services/agent_control/cleanup.rs:82**: failures to obtain a tmux client/session are treated as `is_live = false`, which triggers deletion... a misconfiguration would cause live agent state to be pruned.
  - **Verdict:** `action-needed` (Fail-open logic required).
- **rust/exomonad-core/src/services/agent_control/cleanup.rs:16**: `GcStats` is defined inside the private module... returning it from a public method triggers the `private_interfaces` lint.
  - **Verdict:** `action-needed` (Visibility bug).

---

## PR #860: Persist multiple agent identity keys (aliases)

**Copilot Summary Excerpt:**
> Persists multiple agent identity keys (e.g., birth-branch, slug) into `config.json` so Tier-2 resolution can still find the correct entry after restart.

**Inline Comments:**
- **rust/claude-teams-bridge/src/registry.rs:285**: `persist_config` rebuilds members solely from the *current* in-memory keys. If a member already has aliases on disk and only the primary key gets registered first, this will erase the persisted aliases.
  - **Verdict:** `action-needed` (Data loss on partial registration).
- **rust/claude-teams-bridge/src/registry.rs:281**: `grouped_in_memory` is a `HashMap`... produces a nondeterministic member ordering in `config.json`.
  - **Verdict:** `action-needed` (Config stability).
- **rust/claude-teams-bridge/tests/integration.rs:1636**: Missing test for preserving pre-existing `aliases` from disk when only a subset of keys are re-registered.
  - **Verdict:** `action-needed` (Test gap).

---

## PR #861: Claude session and supervisor routing persistence

**Copilot Summary Excerpt:**
> Makes Claude session and supervisor routing state survive server restarts by persisting both registries’ data into each agent’s `identity.json`.

**Inline Comments:**
- **rust/exomonad-core/src/services/agent_resolver.rs:218**: `update_record()` holds the `records` write lock across multiple awaited filesystem operations. This blocks all concurrent `get/all/register` calls.
  - **Verdict:** `action-needed` (Concurrency bottleneck).
- **rust/exomonad-core/src/services/supervisor_registry.rs:111**: `deregister()` also awaits while holding the `inner` mutex guard.
  - **Verdict:** `action-needed` (Async lock contention).
- **rust/exomonad/src/serve.rs:947**: Warming the registries via `register()` will also trigger the persistence paths... causing unnecessary identity.json rewrites on every server start.
  - **Verdict:** `action-needed` (Unnecessary I/O).
- **rust/exomonad-core/src/services/claude_session_registry.rs:39**: `register()` holds mutex guard while awaiting `resolver.update_record()`... additionally always treats slug aliases as AgentName for persistence.
  - **Verdict:** `action-needed` (Lock contention + incorrect persistence key).
- **rust/exomonad-core/src/services/supervisor_registry.rs:59**: `register()` performs `await` calls while holding the `inner` mutex guard... calling `resolver.all()` inside the loop clones entire identity set.
  - **Verdict:** `action-needed` (O(N^2) complexity + lock contention).
- **rust/exomonad-core/src/services/supervisor_registry.rs:76**: The disk fallback path clones all identity records via `resolver.all()` on every cache miss.
  - **Verdict:** `action-needed` (Performance bottleneck).

---

## PR #862: Drift cleanup for tmux targets and ACP registry

**Copilot Summary Excerpt:**
> Improves message delivery robustness by detecting and cleaning up drift caused by dead tmux targets and stale ACP registry entries.

**Inline Comments:**
- **rust/exomonad-core/src/services/tmux_ipc.rs:729**: `TmuxIpc::target_alive` checks liveness by running `tmux list-panes -t {session}:{target}`... For pane IDs (`%3`), `list-panes` expects a target window/session.
  - **Verdict:** `action-needed` (Incorrect liveness check for panes).
- **rust/exomonad-core/src/services/delivery.rs:18**: `resolve_and_deliver_to_lead()` currently treats only `DeliveryResult::Failed` as a failure... `StaleRouting` would incorrectly be reported as success.
  - **Verdict:** `action-needed` (Failure signaling bug).
- **rust/exomonad-core/src/services/delivery.rs:632**: ACP purge logic relies on string-matching the `Debug` representation of the error. This is brittle.
  - **Verdict:** `action-needed` (Brittle error handling).
- **rust/exomonad-core/src/services/delivery.rs: ...**: Missing tests for pane ID targets (`%N`) in routing liveness path.
  - **Verdict:** `action-needed` (Test gap).

---

## PR #863: Schema-directed coercion for MCP (Follow-up)

**Copilot Summary Excerpt:**
> Switching from heuristic JSON-string decoding to schema-directed coercion when a tool’s JSON Schema is available.

**Inline Comments:**
- **rust/exomonad/src/serve.rs:716**: Extra WASM round-trip per tool call (same as #856).
  - **Verdict:** `already-addressed-by-other-PR` (if PR 869/867 roll up).
- **rust/exomonad-core/src/mcp/harness_compat.rs:97**: Union types reintroduce false positives (same as #856).
  - **Verdict:** `action-needed`.
- **rust/exomonad-core/src/mcp/harness_compat.rs:73**: Integer coercion violates schema intent (same as #856).
  - **Verdict:** `action-needed`.
- **rust/exomonad-core/src/mcp/harness_compat.rs:46**: `anyOf`/`oneOf` mentioned in docs but not implemented.
  - **Verdict:** `action-needed` (Implementation/Doc mismatch).

---

## PR #864: Sub-TL Claude Teams registration

**Copilot Summary Excerpt:**
> Fixes sub-TL Claude Teams registration so the child sub-TL is associated with its own inbox name.

**Inline Comments:**
- *None.*
  - **Verdict:** `informational`.

---

## PR #865: GC for stale agent directories (Follow-up)

**Copilot Summary Excerpt:**
> Adds garbage-collection of stale agent directories during `exomonad init`.

**Inline Comments:**
- **rust/exomonad-core/src/services/agent_control/cleanup.rs: ...**: `RoutingInfo::read_from_dir` errors treated as \"no routing.json\", falling back to orphan age check... can leave broken agent dirs indefinitely.
  - **Verdict:** `action-needed` (Pruning logic refinement).
- **rust/exomonad-core/src/services/agent_control/cleanup.rs:540**: GC tests early-return when tmux isn’t available... undermines the “full test coverage” claim.
  - **Verdict:** `informational` (Environment dependency).
- **rust/exomonad-core/src/services/agent_control/cleanup.rs:606**: “Tmux unavailable” test case isn't actually exercising tmux-unavailable behavior.
  - **Verdict:** `action-needed` (Test fix).
- **rust/exomonad-core/src/services/agent_control/cleanup.rs:71**: Separate tmux CLI invocation per agent dir... can add noticeable latency.
  - **Verdict:** `action-needed` (O(N) latency optimization).

---

## PR #866: Cleanups (Roll-up)

**Copilot Summary Excerpt:**
> Bundles cleanups: removing dead Haskell prompt code, tightening `merge_pr` parsing, and eliminating `EXOMONAD_TMUX_SESSION` reads.

**Inline Comments:**
- **haskell/wasm-guest/src/ExoMonad/Guest/Tools/MergePR.hs:68**: Scalar coercion gating behind header will break clients not sending it (same as #855).
  - **Verdict:** `action-needed` (Compatibility risk).

---

## PR #867: Eliminate Drift (Roll-up)

**Copilot Summary Excerpt:**
> Eliminates “drift” between in-memory registries and disk by persisting Claude session + supervisor routing + team aliases.

**Inline Comments:**
- **rust/exomonad-core/src/services/claude_session_registry.rs:41**: The guard meant to “avoid persisting slug aliases” doesn’t actually do that.
  - **Verdict:** `action-needed` (Logic bug).

---

## PR #868: Tmux target resolution (stable IDs)

**Copilot Summary Excerpt:**
> Fixes tmux target resolution when callers pass stable global IDs (`%N` pane IDs / `@N` window IDs).

**Inline Comments:**
- **rust/exomonad-core/src/services/tmux_ipc.rs:476**: `inject_input` docs/comments still state target is always session-qualified (outdated).
  - **Verdict:** `informational` (Doc fix).
- **rust/exomonad-core/src/services/tmux_ipc.rs:976**: Test comment is misleading after the fix.
  - **Verdict:** `informational` (Comment fix).
- **rust/exomonad-core/src/services/tmux_ipc.rs:969**: Tests return without skip message when tmux is unavailable.
  - **Verdict:** `informational`.
- **rust/exomonad-core/src/services/tmux_ipc.rs:988**: Same issue: test returns without skip message.
  - **Verdict:** `informational`.

---

## PR #869: Synthetic member model + dotted window resolution

**Copilot Summary Excerpt:**
> Populate Claude Teams synthetic member “model” based on runtime agent type, and harden tmux input injection against dotted window names.

**Inline Comments:**
- **rust/exomonad-core/src/services/tmux_ipc.rs:146**: `qualify_target` ignores `$N` targets, which will get session-qualified to an invalid target.
  - **Verdict:** `action-needed` (Incomplete global ID handling).
- **rust/exomonad-core/src/services/tmux_ipc.rs:182**: `resolve_target` handles `session:` incorrectly for dotted window names.
  - **Verdict:** `action-needed` (Bug in dotted window resolution).
- **rust/exomonad-core/src/services/tmux_ipc.rs:184**: `resolve_target` bails when a `.` target doesn't match a window name, instead of falling back.
  - **Verdict:** `action-needed` (Incorrect fallback logic).
