# Parallel Swarm Session: 2025-12-31

## Starting Point

The session began with the **record-based Graph DSL** established on main:

```haskell
data TestGraph mode = TestGraph
  { entry   :: mode :- Entry Int
  , compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Exit Int]
  , exit    :: mode :- Exit Int
  }
```

At this point we had:
- Type-level graph structure via mode-parameterized records
- Compile-time validation of graph structure
- `AsGraph` mode for type-level spec, `AsHandler` mode for value-level handlers
- No runtime execution
- No WASM support
- No effect capture/yield

## The Approach: Parallel LLM Swarm

Instead of sequential development, we spawned **7+ parallel Claude Code instances** in git worktrees, each tackling independent tasks. Coordination happened through this orchestrating session.

### Worktree Pattern
```bash
git worktree add /tmp/<task-name> -b feat/<task-name> main
tmux new-session -d -s <task-name> -c /tmp/<task-name>
tmux send-keys -t <task-name> 'claude "<detailed prompt>"' Enter
```

Tasks were designed to be **disjoint** - touching different files/modules to avoid conflicts.

## What We Built

### PRs Merged (22 total, ~13,000+ lines)

| PR | Lines | What |
|----|-------|------|
| **#8** | +1008/-3047 | Remove list-based DSL, pure record syntax |
| **#9** | +198 | GotoChoice return type for Logic handlers |
| **#10** | +279 | LLMHandler GADT (Before/After/Both) |
| **#11** | +44 | WireTypes: LlmComplete, HttpFetch, LogError |
| **#12** | +232 | tidepool-wasm test suite |
| **#13** | +25/-107 | Fix stale CLAUDE.md claims |
| **#15** | +466 | Protocol conformance tests (Haskell ↔ TS) |
| **#16** | +2220 | Habitica effect vertical slice |
| **#17** | +438 | WASM-serializable Memory effect |
| **#18** | +489 | OneOf GADT + DispatchGoto typed executor |
| **#19** | +474 | Effect capture interpreter (yield/resume) |
| **#20** | +1147/-1305 | Split tidepool-core for WASM |
| **#21** | +4668 | Anemone debug frontend (React/SolidJS) |
| **#22** | +357 | FFI integration tests |
| **#23** | +150 | runGraph entry point |
| **#24** | +328 | LLM node execution |
| **#25** | +53 | Type-level edge case fixes |
| **#26** | +203 | Make invalid states unrepresentable |

### Still Open
| PR | What |
|----|------|
| **#27** | Effect capture with freer-effects continuations |
| **#28** | Self-loop validation + better type errors |

## Architecture After Session

```
┌─────────────────────────────────────────────────────────────┐
│                        Graph DSL                            │
│  data MyGraph mode = MyGraph { ... }                        │
│  - Record-based, mode-parameterized                         │
│  - Compile-time validation via type families                │
│  - AsGraph (structure) / AsHandler (implementations)        │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                   Graph Executor                            │
│  - OneOf GADT: type-indexed sum (no Dynamic!)               │
│  - DispatchGoto: typed recursive dispatch                   │
│  - runGraph: Entry → handlers → Exit                        │
│  - LLM node execution: template → call → parse → route      │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                  Effect Capture                             │
│  - Yield effects at WASM boundary                           │
│  - Serialize to StepOutput (WireTypes)                      │
│  - Resume with EffectResult                                 │
│  - Continuation-based (freer-effects migration in progress) │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                    WASM FFI                                 │
│  - initialize(input) → StepOutput                           │
│  - step(result) → StepOutput                                │
│  - getGraphInfo() → metadata                                │
│  - Compiles via wasm32-wasi-ghc                             │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│               TypeScript Harness (deploy/)                  │
│  - Cloudflare Durable Object                                │
│  - WebSocket protocol                                       │
│  - Effect handlers (Habitica, LLM, HTTP)                    │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                 Anemone Frontend                            │
│  - React debug UI                                           │
│  - Graph visualization                                      │
│  - Effect timeline                                          │
│  - Chat interface                                           │
└─────────────────────────────────────────────────────────────┘
```

## Key Design Decisions Made

### 1. OneOf Instead of Dynamic
```haskell
-- Fully typed, position encodes which target
data OneOf ts where
  Here  :: t -> OneOf (t ': ts)
  There :: OneOf ts -> OneOf (t ': ts)

-- GotoChoice wraps OneOf
newtype GotoChoice targets = GotoChoice (OneOf (Payloads targets))
```

### 2. DispatchGoto Typeclass
```haskell
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType
```
Pattern matches on OneOf, uses HasField to get handlers, recursively dispatches.

### 3. Effect Capture at Boundary
Effects yield at WASM FFI boundary, not inside Haskell. The WASM instance stays alive between yield/resume - no continuation serialization needed.

### 4. Protocol Conformance Testing
Haskell JSON encoding tested against TypeScript expectations. Catches field name mismatches, null handling, sum type encoding.

### 5. Invalid States Unrepresentable
Review task found representable invalid states, PR #26 fixed them with smarter types.

## Parallel Task Categories

### Feature Tasks
- runGraph entry point
- LLM node execution
- Effect capture interpreter
- FFI wiring
- Habitica vertical slice
- Memory serialization
- WireTypes expansion

### Review/Bug-Finding Tasks
- Type-level edge cases → found issues, fixed in #25
- Validation gaps → found issues, PR #28
- Invalid representable states → found issues, fixed in #26

## Metrics

- **PRs merged:** 22
- **Lines added:** ~13,000+
- **Lines removed:** ~4,500+
- **Parallel instances:** 7+ at peak
- **Time:** Single session

## What's Next

1. **Merge #27/#28** - Continuation capture + validation improvements
2. **End-to-end test** - WASM → Anemone → real effects
3. **Build real graph** - Habitica task scorer or LLM chatbot
4. **Deploy to Cloudflare** - Test edge execution
5. **Wire existing agents** - Connect DM/Tidying agents to new executor
