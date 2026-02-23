# ExoMonad v1 Polish Plan

Target: open-source-ready. Wall clock: 1-2 nights of agent swarms.

## Principles

1. **Tests first, then refactor.** Write tests against current behavior, then use them to guide structural changes.
2. **Messaging is the primitive.** All inter-agent communication flows through one effect.
3. **Continuations are the architecture.** freer-simple was chosen for this. Popup proves it works.
4. **Speculative polish swarm.** Spawn workers broadly, merge good PRs, close bad ones.
5. **Plan for success.** Don't hedge — build toward features working. If something is blocked, research the blocker, don't descope.
6. **Maximize exomonad utilization.** Every wave exercises spawn tools.

---

## Wave 0: Baseline (parallel, ~30 min)

### 0A. cargo fmt + clippy (direct)

TL runs `cargo fmt --all && cargo clippy --workspace --fix` on main. Clean baseline before spawning.

### 0B. Init sequencing (worker)

Rewrite `run_init()` to sequence: start server tab → poll health check → start TL tab.

**Verification:** `exomonad init --recreate` → MCP connects first try, SessionStart hook gets additionalContext.

### 0C. Small fixes (worker)

- `expect()` → `Result` in popup.rs, github.rs, anthropic.rs
- Rename `docker.rs` → `command.rs`
- Move `egregore.proto` to `proto/experimental/`
- Replace "tidepool" in test fixtures

**Verification:** `cargo test --workspace`, `cargo clippy --workspace`.

---

## Wave 1: Messaging Unification (Claude subtree, ~3 hours)

**GATE: Phase 2 must land and be verified before spawning agents in later waves.** notify_parent is the heartbeat of the push-based system.

### Phase 1: Wire up existing tools

- Add to roles: TL gets GetAgentMessages + AnswerQuestion; Dev/Worker get Note + Question
- Basic send/receive verified through server

### Phase 2: Migrate notify_parent

- notify_parent emits Completion message through messaging effect
- Server stores message AND triggers Zellij injection (existing delivery)
- Same behavior, now queryable

### Phase 3: Enrichment

- StatusUpdate messages mid-task
- TL queries all child messages
- Question blocks worker until TL answers

**Agent type:** Claude subtree.

---

## Wave 2: Test Infrastructure (2 subtrees, ~3 hours)

### 2A. Integration test harness (Claude subtree)

- `tests/integration_harness.rs` — starts server, health check, runs tests, teardown
- `just test-integration`
- Fix broken integration tests

**This is a hard gate for 2B and 5B.**

### 2B. agent_control.rs: tests then refactor (Claude subtree, after 2A)

- Write comprehensive tests first: spawn_workers, spawn_subtree, spawn_leaf_subtree, path resolution, config generation
- Extract modules guided by test coverage:
  - `services/agent_paths.rs`
  - `services/agent_config.rs`
  - `services/zellij_layout.rs`
- Tests verify no behavior change

---

## Wave 3: Popup via Continuations (Claude subtree, ~4 hours)

freer-simple reifies continuations as data. This is the architecture working as designed.

### Design

**User-facing API (pure, no IO):**
```haskell
toolHandler args = do
  result <- popup myFormSpec  -- yields Popup effect, suspends until user responds
  pure $ successResult result  -- resumes here after submission
```

Tool authors never see IO, IORef, or parking mechanics. The effect stack stays pure.

**Runtime internals (IO contained behind the curtain):**

The runtime (`Tool/Runtime.hs`) manages suspension:
1. Tool handler yields `Popup` effect
2. Runtime captures the freer-simple continuation (pure `Eff` value, reified by the library)
3. Runtime parks the continuation in a module-level IORef/MVar keyed by request_id
4. `handle_mcp_call` returns `Suspended { request_id }` to Rust
5. Plugin lock released. Other MCP calls work.

Resume path:
1. User interacts with popup in dedicated Zellij tab
2. `exomonad reply --id <id> --payload <json>` → Rust server receives reply
3. Rust calls new WASM export: `resume_suspended(request_id, payload)`
4. WASM runtime looks up parked continuation from IORef, feeds in payload, runs to completion
5. Result returned to Rust → delivered to agent

**Key properties:**
- Continuation stays in WASM memory (no serialization across FFI boundary)
- IO is an implementation detail of the runtime, not visible in tool handler signatures
- extism plugin instances persist between calls (we already depend on this)
- New WASM export `resume_suspended` alongside existing `handle_mcp_call`

**Verification:** MCP tool call triggers popup → returns immediately with request_id → other tools work concurrently → user submits in popup tab → result flows back to original caller's context.

---

## Wave 4: Speculative Polish Swarm (leaf subtrees, after 2B merges)

**Hard sequenced after Wave 2B** to avoid file overlap conflicts.

6 leaf subtrees (Gemini), each files a PR:

| Target | Spec focus |
|--------|-----------|
| `rust/exomonad-core/src/effects/` | Error handling, idiomatic Rust patterns |
| `rust/exomonad-core/src/services/external/` | External service clients: github, anthropic, ollama |
| `haskell/dsl/core/` | Unused exports, type safety, partial functions |
| `haskell/effects/` | Interpreter-by-interpreter review |
| `proto/effects/` | Field naming, documentation, consistency |
| Panic/unwrap audit (all Rust) | `panic!`/`unreachable!`/`unwrap()` in non-test code |

**Anti-patterns (in every spec):**
- DO NOT add dependencies
- DO NOT change public interfaces or type signatures
- DO NOT rename types/variants
- DO NOT restructure modules
- ONLY modify files in your assigned directory
- `cargo test --workspace` must pass

**Additional swarm targets:**
- Error message quality — audit each handler's error paths, make messages actionable (not "Wasm error: call failed")
- Agent startup failure detection — if a child agent dies before its first MCP call (bad --resume, crash on init), parent never finds out. Fix: session-end/stop hook notifies parent on unexpected exit, or heartbeat timeout injects `[AGENT TIMEOUT: X]` into parent pane.

**Merge policy:** TL reviews diff. Clean improvement → merge. Noise/hallucinated cleanup → close.

---

## Wave 5: Open Source Polish (parallel, ~2 hours)

### 5A. README + docs (worker)

- `README.md` — elevator pitch, architecture, quickstart
- `CONTRIBUTING.md` — build, test, extend
- Screen recording

### 5B. WASM boundary tests (Claude subtree, after 2A)

- Tool dispatch per role
- Hook dispatch (SessionStart, PreToolUse, Stop)
- Effect yield/resume round-trip

---

## Execution Order

```
PARALLEL: Wave 0A (direct) + 0B (worker) + 0C (worker)
THEN:     Wave 1 (messaging subtree) — Phase 2 is a gate
PARALLEL: Wave 2A (test harness) alongside Wave 1
THEN:     Wave 2B (agent_control refactor, needs 2A)
THEN:     Wave 4 (polish swarm, needs 2B merged)
PARALLEL: Wave 3 (popup) — independent, can run alongside 2A/2B
PARALLEL: Wave 5A (docs) + 5B (WASM tests, needs 2A)
```

**Critical path:** Wave 0 → Wave 1 (messaging gate) → Waves 2-5 fan out
**Parallel capacity:** 3-4 subtrees + workers simultaneously
