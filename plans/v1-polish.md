# ExoMonad v1 Polish Plan

Target: open-source-ready. Wall clock: 1-2 nights of agent swarms.

## Principles

1. **Claude Teams IS the messaging layer.** No custom inbox system. All agent→TL messaging flows through Teams inbox files. Zellij injection is the fallback for non-Teams agents (Gemini workers).
2. **Continuations are the architecture.** freer-simple was chosen for this. Popup proves it works.
3. **Speculative polish swarm.** Spawn workers broadly, merge good PRs, close bad ones.
4. **Plan for success.** Don't hedge — build toward features working. If something is blocked, research the blocker, don't descope.
5. **Maximize exomonad utilization.** Every wave exercises spawn tools.

---

## Wave 0: Baseline — DONE

- ~~0A. cargo fmt + clippy~~ — done
- ~~0B. Init sequencing~~ — done (a9dc31da: fixed zellij flag, tokio panic, empty tab)
- ~~0C. Small fixes~~ — done (ad221f61: docker.rs→command.rs, egregore.proto→experimental/)

---

---

## Wave 1: Messaging Unification — DONE

**Goal:** Claude Teams inbox is the single messaging channel. Kill the `.exo/messages/` custom inbox system entirely.

### What exists now

Three delivery paths, unified:
| Path | Current channel | Teams? |
|------|----------------|--------|
| `notify_parent` (events.rs) | Teams-first, Zellij fallback | Done |
| `github_poller` Copilot reviews | Teams-first, Zellij fallback | Done |
| `send_note` / `send_question` (messaging.rs) | **DELETED** | Done |

### Phase 1: Extract shared delivery helper — DONE

Create `services/delivery.rs` with a `deliver_to_agent()` function that:
1. Looks up target in `TeamRegistry`
2. If found → write to Teams inbox (`teams_mailbox::write_to_inbox`)
3. If not found → fall back to Zellij injection (`zellij_events::inject_input`)

Refactor `events.rs` (notify_parent) and `github_poller.rs` to use this helper instead of inline Teams+Zellij logic.

**Verification:** `cargo test --workspace`, existing notify_parent + Copilot review delivery still works.

### Phase 2: Kill messaging system — DONE

Delete entirely:
- `handlers/messaging.rs` — the messaging effect handler
- `services/inbox.rs` — the `.exo/messages/` JSON inbox
- `services/questions.rs` — the QuestionRegistry (sync blocking questions)
- `messaging.proto` effects — proto definitions
- Generated Haskell messaging effect types
- Role registrations for SendNote, SendQuestion, GetAgentMessages, AnswerQuestion
- All references in handler registration, RuntimeBuilder, etc.

**Rationale:** `send_note` is unused in practice. `send_question` was a sync blocking pattern that doesn't fit the push-based architecture. Claude Teams provides native messaging (SendMessage tool) that's richer than anything we'd build. notify_parent is the only agent→TL effect that matters.

**Verification:** `cargo test --workspace`, `cargo build -p exomonad`, `just wasm-all`. No messaging effects remain.

### Phase 3: Clean up delivery in notify_parent — DONE

After Phase 1 lands, notify_parent should use `delivery::deliver_to_agent()` instead of its inline Teams+Zellij code. Single code path for all agent→parent delivery.

**Verification:** `exomonad init`, spawn a worker, `notify_parent` delivers to TL via Teams inbox.

---

## Wave 2: Test Infrastructure (2 subtrees)

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

## Wave 3: Popup via Continuations (Claude subtree)

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

## Wave 5: Open Source Polish (parallel)

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
DONE:     Wave 0 (baseline)
DONE:     Wave 1 (messaging unification)
PARALLEL: Wave 2A (test harness) — NEXT
THEN:     Wave 2B (agent_control refactor, needs 2A)
THEN:     Wave 4 (polish swarm, needs 2B merged)
PARALLEL: Wave 3 (popup) — independent, can run alongside 2A/2B
PARALLEL: Wave 5A (docs) + 5B (WASM tests, needs 2A)
```

**Critical path:** Wave 1 (messaging gate) → Wave 2 → Wave 4
**Parallel capacity:** 3-4 subtrees + workers simultaneously
