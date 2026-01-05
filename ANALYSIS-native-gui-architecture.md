# Architectural Analysis: tidepool-native-gui

## Executive Summary

The `tidepool-native-gui` directory contains 12 executor packages that interpret tidepool effects natively (without WASM/CF Workers). While the **executor pattern is sound**, there are **significant architectural confusions** around:

1. **Inconsistent naming and structure** across executors
2. **Ambiguous responsibility boundaries** between Config and Env types
3. **No clear abstraction** for multi-instance HTTP clients (duplicated)
4. **Wire types tightly coupled** to UI effects
5. **Effect ordering dependencies** implicit in EffectRunner composition

This analysis identifies specific patterns that should be clarified or refactored for maintainability.

---

## 1. Executor Pattern Inconsistencies

### 1.1 Naming Confusion: Config vs. Env vs. Context

The codebase uses **three different names** for similar concepts with no clear distinction:

| Type | Used By | Purpose | Initialization |
|------|---------|---------|-----------------|
| **Config** | LLM, Habitica, GitHub, BD, Issue, ClaudeCode, DevLog | Static configuration (API keys, URLs) | Loaded once, reused |
| **Env** | LLM, Habitica, UI | Config + runtime resources (HTTP Manager) | Created per-session/per-program |
| **Context** | UI | Mutable runtime state (IORef accumulation) | Created per-session |

**Problem**: These distinctions are not documented, making it unclear when to use which pattern.

```haskell
-- Three different patterns for similar concerns:

-- 1. Config only (lightweight)
data BDConfig = BDConfig
  { bcBeadsDir :: Maybe FilePath
  , bcQuiet :: Bool
  }

-- 2. Config + Env (requires initialization)
data HabiticaConfig = HabiticaConfig { ... }  -- Static
data HabiticaEnv = HabiticaEnv { leConfig, leManager }  -- Runtime
mkHabiticaEnv :: HabiticaConfig -> IO HabiticaEnv

-- 3. Context (mutable accumulation)
data UIContext = UIContext
  { ucMessages  :: IORef (Seq ChatMessage)  -- Mutable
  , ucGraphNode :: IORef Text
  , ucThinking  :: IORef Bool
}
```

**Where**: `/home/inanna/dev/tidepool/tidepool-native-gui/*/src/Tidepool/*/Executor.hs`

### 1.2 Inconsistent Executor Entry Points

Each executor has a **different naming convention** for its main runner function:

```haskell
runUI          :: UIContext -> UICallback -> Eff (UI ': effs) a -> Eff effs a
runHabitica    :: HabiticaEnv -> Eff (Habitica ': effs) a -> Eff effs a
runLLMComplete :: LLMEnv -> Eff (LLMComplete ': effs) a -> Eff effs a
runBDIO        :: BDConfig -> Eff (BD ': effs) a -> Eff effs a
runGitHubIO    :: GitHubConfig -> Eff (GitHub ': effs) a -> Eff effs a
runObservabilityWithContext :: TraceContext -> LokiConfig -> ...
runClaudeCodeExecIO :: ClaudeCodeConfig -> Eff (ClaudeCodeExec ': effs) a -> Eff effs a
runLSP         :: LSPSession -> Eff (LSP ': effs) a -> Eff effs a
runIssue       :: (IssueReport -> Eff effs ()) -> Eff (Issue ': effs) a -> Eff effs a
```

**Patterns observed**:
- `run<Effect>` (most common): 8 executors
- `run<Effect>IO` (special marking): 3 executors (BD, GitHub, ClaudeCode)
- `run<Effect>WithContext`: 1 executor (Observability)
- Custom patterns: 2 executors (Issue uses handler, LSP uses session)

**Unclear**: Why do some need `IO` suffix? Why does LSP use session instead of config?

**Where**: `/home/inanna/dev/tidepool/tidepool-native-gui/*/src/Tidepool/*/Executor.hs`

---

## 2. Wire Types Coupling

### 2.1 Single Wire Types Module for UI Communication

The `wire-types` package defines **UI-only protocol**:

```haskell
module Tidepool.Wire.Types
  ( UIState(..)        -- Server → Client
  , UserAction(..)     -- Client → Server
  , ChatMessage(..)
  , ChoiceOption(..)
  , ...
  )
```

**Problem**: Called "wire-types" (generic) but only handles UI protocol. No abstraction for:
- LLM request/response serialization
- DevLog event serialization
- Observability trace serialization

Each executor **reimplements serialization privately**:
- `Tidepool.LLM.Executor`: Uses Anthropic/OpenAI JSON directly
- `Tidepool.Observability.Executor`: Uses Loki push request + OTLP format
- `Tidepool.Issue.Executor`: Uses GitHub API JSON

**Where**:
- Wire types: `/home/inanna/dev/tidepool/tidepool-native-gui/wire-types/src/Tidepool/Wire/Types.hs`
- Inconsistent: Each executor in `/tidepool-native-gui/*/src/Tidepool/*/Executor.hs`

---

## 3. Configuration Aggregation Pattern Issues

### 3.1 ExecutorConfig is a Monolithic Configuration Bag

The `EffectRunner.ExecutorConfig` aggregates **all executor configs** in one type:

```haskell
data ExecutorConfig = ExecutorConfig
  { ecLLMConfig :: LLMConfig
  , ecHabiticaConfig :: HabiticaConfig
  , ecLokiConfig :: LokiConfig
  , ecOTLPConfig :: Maybe OTLPConfig
  , ecServiceName :: Text
  , ecClaudeCodeConfig :: ClaudeCodeConfig
  , ecDevLogConfig :: DevLogConfig
  }
```

**Problem 1: Tight coupling**. Adding a new executor requires:
1. Create new executor package with `XyzConfig`
2. Add field to `ExecutorConfig`
3. Update `loadExecutorConfig` to parse from env
4. Update `runEffects` to initialize and compose
5. Update EffectRunner imports

**Problem 2: Implicit validation**. The configuration is loaded from env vars with no schema validation:

```haskell
loadExecutorConfig :: IO ExecutorConfig
loadExecutorConfig = do
  anthropicKey <- lookupEnv "ANTHROPIC_API_KEY"
  -- No validation! What if key is empty? Wrong format?
  -- Users discover runtime via API call failure.
```

**Problem 3: Cannot be optional**. Non-optional executors (habitica, observability) must have configs even if unused:

```haskell
ecHabiticaConfig :: HabiticaConfig  -- Always required
ecLokiConfig :: LokiConfig          -- Always required (defaults to localhost:3100)
```

**Where**: `/home/inanna/dev/tidepool/tidepool-native-gui/server/src/Tidepool/Server/EffectRunner.hs` (lines 96-111)

### 3.2 ExecutorEnv Holds Subset of Resources

While `ExecutorConfig` is monolithic, `ExecutorEnv` only holds HTTP clients:

```haskell
data ExecutorEnv = ExecutorEnv
  { eeConfig :: ExecutorConfig
  , eeLLMEnv :: LLMEnv        -- HTTP manager + config
  , eeHabiticaEnv :: HabiticaEnv  -- HTTP manager + config
  }
```

**Missing**: No trace context, no session state, no log handle.

These are **initialized in `runEffects`** instead:

```haskell
runEffects env ctx callback action = do
  traceCtx <- newTraceContext  -- Created here
  result <- runM
    . runObservabilityWithContext traceCtx (ecLokiConfig $ eeConfig env)
    -- ...
```

**Why**: Some effects need mutable initialization (trace context), others don't (HTTP manager). No clear pattern.

**Where**: `/home/inanna/dev/tidepool/tidepool-native-gui/server/src/Tidepool/Server/EffectRunner.hs` (lines 252-273)

---

## 4. HTTP Client Duplication

### 4.1 Multiple HTTP Client Patterns

Three different HTTP client libraries are used with **duplicated initialization**:

| Executor | Library | Client Type | Initialization |
|----------|---------|------------|-----------------|
| LLM | `servant-client` | `Manager` + `BaseUrl` | `mkLLMEnv` |
| Habitica | `http-client` | `Manager` | `mkHabiticaEnv` |
| GitHub | `System.Process` | CLI wrapper | None (shells out) |
| Issue | `http-client` | Custom `Manager` | `createIssueManager` |
| Observability | `Network.HTTP.Req` | `Req` effect | None (inline) |
| LSP | `Language.LSP.Client` | `LSPSession` | `withLSPSession` (bracket) |

**Problem**: Each creates its own HTTP manager:

```haskell
-- LLM Executor
data LLMEnv = LLMEnv
  { leConfig :: LLMConfig
  , leManager :: Manager  -- HTTP manager
  }

mkLLMEnv :: LLMConfig -> IO LLMEnv
mkLLMEnv config = do
  mgr <- newManager tlsManagerSettings
  pure LLMEnv { leConfig = config, leManager = mgr }

-- Issue Executor (separate manager)
createIssueManager :: IO Manager
createIssueManager = newTlsManager

-- Habitica Executor (same pattern)
mkHabiticaEnv :: HabiticaConfig -> IO HabiticaEnv
mkHabiticaEnv config = do
  mgr <- newManager tlsManagerSettings
  pure HabiticaEnv { ... , heManager = mgr }
```

**Better pattern**: Shared HTTP manager pool passed to all executors.

**Where**:
- LLM: `/home/inanna/dev/tidepool/tidepool-native-gui/llm-executor/src/Tidepool/LLM/Types.hs`
- Habitica: `/home/inanna/dev/tidepool/tidepool-native-gui/habitica-executor/src/Tidepool/Habitica/Executor.hs`
- Issue: `/home/inanna/dev/tidepool/tidepool-native-gui/issue-executor/src/Tidepool/Issue/Executor.hs`

---

## 5. Implicit Effect Ordering Dependencies

### 5.1 Effect Stack Order Matters But Not Validated

The `runEffects` function composes effects in a **specific order** with no type-level enforcement:

```haskell
runEffects env ctx callback action = do
  traceCtx <- newTraceContext
  result <- runM
    . runObservabilityWithContext traceCtx (ecLokiConfig $ eeConfig env)  -- LAST
    . runDevLog (ecDevLogConfig $ eeConfig env)
    . runClaudeCodeExecIO (ecClaudeCodeConfig $ eeConfig env)
    . runLLMComplete (eeLLMEnv env)
    . runHabitica (eeHabiticaEnv env)
    . runUI ctx callback  -- FIRST
    $ action
```

**Documentation states** (lines 295-305):
```
Effects are peeled from the outside in:
1. UI (first to peel) - handles user interaction
2. Habitica - makes Habitica API calls
3. LLMComplete - makes LLM API calls
4. ClaudeCodeExec - executes nodes via Claude Code subprocess
5. DevLog - session-scoped dev logging
6. Observability (last to peel) - records events and spans
```

**Problem 1: No types enforce this**. If you swap two `run*` calls, Haskell accepts it silently. Runtime behavior changes unpredictably.

**Problem 2: Dependencies are implicit**. Why must UI be first? Why must Observability be last?
- UI first: So it intercepts user input before other effects
- Observability last: So it wraps all traces
- Missing: Documentation explaining why each position matters

**Problem 3: Adding new effects is fragile**:
1. Where does it go in the stack?
2. What other effects does it depend on?
3. Will moving it break something?

**Where**: `/home/inanna/dev/tidepool/tidepool-native-gui/server/src/Tidepool/Server/EffectRunner.hs` (lines 318-345)

---

## 6. Session/Context Management Patterns

### 6.1 Multiple Context Types with Different Lifetimes

Three different context types with **unclear lifecycle**:

```haskell
-- 1. Per-WebSocket-connection (Server.Session)
data Session = Session
  { sId        :: SessionId
  , sActionVar :: TVar (Maybe UserAction)  -- Mutable
  , sStateVar  :: TVar (Maybe UIState)     -- Mutable
  , sCreatedAt :: UTCTime
  , sGraphNode :: TVar Text                -- Mutable
  }

-- 2. Per-request (EffectRunner)
data ExecutorEnv = ExecutorEnv
  { eeConfig :: ExecutorConfig      -- Per-program (static)
  , eeLLMEnv :: LLMEnv              -- Per-program (reused across requests)
  , eeHabiticaEnv :: HabiticaEnv    -- Per-program (reused)
  }

-- 3. Per-turn (runEffects)
data UIContext = UIContext
  { ucMessages  :: IORef (Seq ChatMessage)  -- Per-turn (accumulates)
  , ucGraphNode :: IORef Text               -- Per-turn
  , ucThinking  :: IORef Bool               -- Per-turn
}
```

**Problem**: It's unclear which context is responsible for what:
- Does `Session` have session state, or is that in `UIContext`?
- Can you reuse `ExecutorEnv` across multiple agent runs?
- What happens if you run two agents concurrently with same `UIContext`?

**Where**:
- Session: `/home/inanna/dev/tidepool/tidepool-native-gui/server/src/Tidepool/Server/Session.hs`
- ExecutorEnv: `/home/inanna/dev/tidepool/tidepool-native-gui/server/src/Tidepool/Server/EffectRunner.hs`
- UIContext: `/home/inanna/dev/tidepool/tidepool-native-gui/ui-executor/src/Tidepool/UI/Executor.hs`

---

## 7. DevLog vs. Observability Boundary

### 7.1 Two Logging Systems with Overlapping Concerns

The codebase has **two distinct logging approaches** with unclear separation:

#### DevLog (session-scoped file logging)
- **Purpose**: Human-readable, greppable local logs for debugging
- **Output**: Files in `DEVLOG_DIR` or stderr
- **Events**: Graph transitions, LLM calls, state changes
- **Format**: Human-readable (with timestamps)
- **Lifecycle**: Per-session (via `DevLogConfig`)

```haskell
module Tidepool.DevLog.Executor
  { runDevLog :: DevLogConfig -> Eff (DevLog ': effs) a -> Eff effs a }

data DevLogConfig = DevLogConfig
  { dcVerbosity :: Verbosity     -- quiet|normal|verbose|trace
  , dcOutput :: DevLogOutput     -- Stderr | File | Both
  , dcSymlinkLatest :: Bool      -- Create latest.log?
  , dcSessionId :: Maybe UUID    -- Auto-generated if Nothing
  , dcSessionName :: Maybe Text
  }
```

#### Observability (structured logs + traces)
- **Purpose**: Structured instrumentation for production monitoring
- **Output**: Loki (logs) + Tempo (traces) via HTTP
- **Events**: Same events as DevLog + custom spans
- **Format**: JSON (Loki format) + OTLP (OpenTelemetry)
- **Lifecycle**: Per-trace-context (via `TraceContext`)

```haskell
module Tidepool.Observability.Executor
  { runObservabilityWithContext :: TraceContext -> LokiConfig -> ... }

data TraceContext = TraceContext
  { tcTraceId :: Text
  , tcSpans :: IORef [Span]  -- Accumulated
  , tcLokiPushReq :: IORef LokiPushRequest
  }
```

**Problem 1: Event duplication**. Same agent events are logged to both systems:

```haskell
-- runEffects composes both:
result <- runM
  . runObservabilityWithContext traceCtx lokiConfig  -- Publishes events
  . runDevLog devLogConfig                           -- Also logs events
  . ...
  $ action
```

**Problem 2: Unclear responsibility**. Should an agent use `DevLog` or `Observability`?
- **DevLog**: Local development debugging
- **Observability**: Production monitoring
- **Both**: What if you want both? What if you want neither?

**Problem 3: No composition pattern**. Unlike executors (which are cleanly stacked), DevLog and Observability **both intercept the same effects** and compete for event handling.

**Where**:
- DevLog: `/home/inanna/dev/tidepool/tidepool-native-gui/devlog-executor/src/Tidepool/DevLog/Executor.hs`
- Observability: `/home/inanna/dev/tidepool/tidepool-native-gui/observability-executor/src/Tidepool/Observability/Executor.hs`

---

## 8. Module Organization Issues

### 8.1 Inconsistent Executor Module Nesting

Executors have **different module depths** with no clear pattern:

```
Simple (one level):
├── bd-executor/src/Tidepool/BD/Executor.hs              ← runBD, runBDIO
├── github-executor/src/Tidepool/GitHub/Executor.hs      ← runGitHubIO
├── habitica-executor/src/Tidepool/Habitica/Executor.hs  ← runHabitica
├── issue-executor/src/Tidepool/Issue/Executor.hs        ← runIssue
├── lsp-executor/src/Tidepool/LSP/Executor.hs            ← runLSP
├── ui-executor/src/Tidepool/UI/Executor.hs              ← runUI

Complex (multiple sub-modules):
├── llm-executor/src/Tidepool/LLM/
│   ├── Executor.hs                    ← runLLMComplete
│   ├── Types.hs                       ← LLMEnv, LLMConfig
│   ├── API/
│   │   ├── Anthropic.hs               ← Anthropic API
│   │   ├── OpenAI.hs                  ← OpenAI API
│
├── observability-executor/src/Tidepool/Observability/
│   ├── Executor.hs                    ← runObservabilityWithContext
│   ├── Types.hs                       ← LokiConfig, OTLPConfig, TraceContext
│
├── devlog-executor/src/Tidepool/DevLog/
│   ├── Executor.hs                    ← runDevLog
│   ├── Config.hs                      ← DevLogConfig
│   ├── Formatter.hs                   ← Event formatting
│
├── claude-code-executor/src/Tidepool/ClaudeCode/
│   ├── Executor.hs                    ← runClaudeCodeRequest
│   ├── Effect.hs                      ← runClaudeCodeExecIO
│   ├── Config.hs                      ← ClaudeCodeConfig
│   ├── Session.hs                     ← Session management
│
├── bd-executor/src/Tidepool/BD/
│   ├── Executor.hs                    ← runBD, runBDIO
│   ├── GitExecutor.hs                 ← Git integration
│   ├── Prime/
│   │   ├── Graph.hs                   ← Prime agent graph
│   │   ├── Runner.hs                  ← Prime runner
```

**Why** do simple executors have flat structure, but complex ones have sub-modules?
- No documented convention for when to split into sub-modules
- Makes it harder to understand what's in an executor package

**Where**: All executor packages under `/home/inanna/dev/tidepool/tidepool-native-gui/`

### 8.2 Executor Types Scattered Across Modules

Configuration/environment types are stored **inconsistently**:

```haskell
-- Some in Executor.hs:
module Tidepool.BD.Executor where
  data BDConfig = BDConfig { ... }
  runBDIO :: BDConfig -> ...

-- Some in Types.hs:
module Tidepool.LLM.Types where
  data LLMConfig = LLMConfig { ... }
  data LLMEnv = LLMEnv { ... }

module Tidepool.LLM.Executor where
  import Tidepool.LLM.Types
  runLLMComplete :: LLMEnv -> ...

-- Some in Config.hs:
module Tidepool.DevLog.Config where
  data DevLogConfig = DevLogConfig { ... }

module Tidepool.DevLog.Executor where
  import Tidepool.DevLog.Config
  runDevLog :: DevLogConfig -> ...

-- Some in dedicated modules:
module Tidepool.Observability.Types where
  data LokiConfig = LokiConfig { ... }
  data OTLPConfig = OTLPConfig { ... }
  data TraceContext = TraceContext { ... }

module Tidepool.Observability.Executor where
  import Tidepool.Observability.Types
  runObservabilityWithContext :: TraceContext -> LokiConfig -> ...
```

**Problem**: Users don't know where to import from. Is it `Types`, `Config`, or `Executor`?

**Where**:
- BD: `/home/inanna/dev/tidepool/tidepool-native-gui/bd-executor/src/Tidepool/BD/Executor.hs`
- LLM: `/home/inanna/dev/tidepool/tidepool-native-gui/llm-executor/src/Tidepool/LLM/Types.hs` and `Executor.hs`
- DevLog: `/home/inanna/dev/tidepool/tidepool-native-gui/devlog-executor/src/Tidepool/DevLog/Config.hs` and `Executor.hs`
- Observability: `/home/inanna/dev/tidepool/tidepool-native-gui/observability-executor/src/Tidepool/Observability/Types.hs` and `Executor.hs`

---

## 9. Missing Abstractions

### 9.1 No Executor Registry or Discovery

Executors are manually wired in `EffectRunner`:

```haskell
-- From EffectRunner.hs
import Tidepool.UI.Executor
import Tidepool.Habitica.Executor
import Tidepool.Observability.Executor
import Tidepool.LLM.Executor
import Tidepool.LLM.Types
import Tidepool.ClaudeCode.Effect
import Tidepool.ClaudeCode.Config
import Tidepool.DevLog.Executor
import Tidepool.Effect.DevLog
-- ... 13 imports

runEffects env ctx callback action = do
  -- ... manually composed
```

**Problem**: To add a new executor:
1. Create executor package
2. Update EffectRunner imports
3. Update ExecutorConfig
4. Update loadExecutorConfig
5. Update runEffects composition

No way to plugin executors without modifying EffectRunner.

**Better pattern**: Executor registry or typeclasses (like `Executor` typeclass with `mkEnv`, `run`).

**Where**: `/home/inanna/dev/tidepool/tidepool-native-gui/server/src/Tidepool/Server/EffectRunner.hs`

### 9.2 No Clear Testing/Mocking Story

Some executors export low-level functions for testing:

```haskell
module Tidepool.LLM.Executor
  ( runLLMComplete
  , LLMEnv
  , mkLLMEnv
  , buildAnthropicRequest  -- Exported for testing
  , buildOpenAIRequest     -- Exported for testing
  , parseBaseUrl           -- Exported for testing
  , clientErrorToLLMError  -- Exported for testing
  )
```

Others don't:

```haskell
module Tidepool.Habitica.Executor
  ( runHabitica
  , HabiticaConfig(..)
  , defaultHabiticaConfig
  -- No low-level helpers exported
  )
```

**Problem**: Inconsistent testing surface. How do you test an agent that uses Habitica? You can't easily mock it.

**Better pattern**: Separate `Mock` modules or `Testable` typeclasses.

**Where**:
- With test exports: `/home/inanna/dev/tidepool/tidepool-native-gui/llm-executor/src/Tidepool/LLM/Executor.hs`
- Without: `/home/inanna/dev/tidepool/tidepool-native-gui/habitica-executor/src/Tidepool/Habitica/Executor.hs`

---

## Summary of Key Issues

| Issue | Severity | Scope | Impact |
|-------|----------|-------|--------|
| **Naming confusion** (Config/Env/Context) | High | All executors | Harder to understand patterns |
| **Inconsistent executor entry points** | High | 12 executors | Hard to learn/use the pattern |
| **Wire types misnamed** | Medium | wire-types | Confusing about scope |
| **HTTP client duplication** | Medium | 4 executors | Resource waste, harder to profile |
| **Implicit effect ordering** | High | EffectRunner | Fragile, hard to extend |
| **Multiple context types** | High | Session, ExecutorEnv, UIContext | Confusing lifecycle |
| **DevLog vs. Observability overlap** | Medium | 2 executors | Event duplication, unclear responsibility |
| **Inconsistent module nesting** | Low | All executors | Navigation difficulty |
| **Scattered config types** | Medium | All executors | Import confusion |
| **No executor registry** | High | EffectRunner | Not extensible |
| **No testing/mocking story** | Medium | Some executors | Hard to test agents |

---

## Recommended Refactoring Strategy

1. **Standardize naming**:
   - Rename consistently: `Config` (static), `Env` (resources), `Context` (mutable per-turn)
   - Document which pattern each executor should use

2. **Create executor abstraction**:
   ```haskell
   class Executor e where
     type Config e :: Type
     type Env e :: Type
     mkEnv :: Config e -> IO (Env e)
     runEffect :: Env e -> Eff (e ': effs) a -> Eff effs a
   ```

3. **Consolidate HTTP client**:
   - Create `SharedHTTPManager` type
   - Pass to all executors needing HTTP
   - Reuse connection pools

4. **Make effect ordering explicit**:
   - Use type-level proof to enforce stack order
   - Or document with examples why each position matters

5. **Clarify logging responsibility**:
   - DevLog: Local debugging
   - Observability: Production monitoring
   - No overlap in event handling

6. **Standardize module organization**:
   - Simple rule: `<executor>/src/Tidepool/<Name>/Executor.hs`
   - Complex: Only split if >500 lines
   - Types always in `Types.hs` or re-exported from `Executor.hs`

