# Task 04: Integrate Teaching for JSONL Capture

**Epic:** [00-epic-docgen-graph.md](00-epic-docgen-graph.md)
**Depends On:** [03-wire-graph-execution.md](03-wire-graph-execution.md)
**Blocks:** 05

## Goal

Wire `exomonad-teaching` into the graph execution so every Haiku call during DocGen exploration is captured to JSONL training data.

## Context

### exomonad-teaching Architecture

```
runLLMWithTeaching env
    → intercepts RunTurnOp from LLM effect
    → extracts NodeMetadata (node name, graph name)
    → wraps system prompt with teacher guidance
    → calls Haiku via Anthropic API
    → records TeachingTurn to anthropic.jsonl
    → returns TurnOutcome to continue graph execution
```

### What We Have (from earlier work)

- `ContentBlock` parses thinking blocks
- `convertToOutcome` extracts thinking content
- `teaching-harness` executable proves the pipeline works

### What We Need

Replace `runLLMComplete` with `runLLMWithTeaching` when teaching mode is enabled:

```haskell
-- Before (Task 03)
runDocGenGraph lspSession query = do
  runM
    . runLLMComplete llmEnv  -- Direct to Haiku
    $ runGraph docGenHandlers query

-- After (Task 04)
runDocGenGraph teachingEnv lspSession query = do
  runM
    . runLLMWithTeaching teachingEnv  -- Intercepts and records
    $ runGraph docGenHandlers query
```

## Acceptance Criteria

- [ ] **Teaching mode activates via env var** - `TEACHING_ENABLED=true`
- [ ] **JSONL files created** in session directory
- [ ] **Each dgSelect call recorded** as TeachingTurn
- [ ] **NodeMetadata correct** - shows "dgSelect" and "DocGenGraph"
- [ ] **Thinking content captured** - trThinking not empty
- [ ] **Graph execution still works** - returns correct TeachingDoc

## Subtasks

### 4.1 Add Teaching Config to Server

```haskell
-- In Server.hs or Types.hs
data ServerConfig = ServerConfig
  { scProjectDir :: FilePath
  , scLSPSession :: LSPSession
  , scTeachingEnv :: Maybe TeachingEnv  -- Nothing = production mode
  }

initServer :: IO ServerConfig
initServer = do
  projectDir <- getProjectDir
  mTeachingConfig <- loadTeachingConfig
  mTeachingEnv <- case mTeachingConfig of
    Nothing -> pure Nothing
    Just config -> do
      let guidance = teacherGuidance @DocGenGraph  -- From Teacher.hs
      env <- initTeachingEnv config guidance
      pure (Just env)
  lspSession <- startLSPSession projectDir
  pure ServerConfig { .. }
```

### 4.2 Create Teacher Instance for DocGen

```haskell
-- In Scout/Graph/Teacher.hs
instance FineTrainingTeacher DocGenGraph where
  teacherGuidance = T.unlines
    [ "You are generating training data for FunctionGemma, a small function-calling model."
    , ""
    , "When selecting relevant symbols:"
    , "1. Think step-by-step about why each candidate is or isn't relevant"
    , "2. Consider the topic context when making selections"
    , "3. Prefer domain-specific types over generic ones"
    , "4. Be precise in your reasoning - this becomes training data"
    , ""
    , "Your thinking will be captured for training, so be explicit about your reasoning process."
    ]
```

### 4.3 Conditional Teaching in Runner

```haskell
-- In Graph/Runner.hs
runDocGenGraph
  :: Maybe TeachingEnv
  -> LSPSession
  -> TeachQuery
  -> IO (Either Text TeachingDoc)
runDocGenGraph mTeachingEnv lspSession query = do
  let graphAction = runGraph docGenHandlers query

  runM
    . runError @Text
    . runLog Debug
    . runMemory initialState
    . runLSP lspSession
    . interpretLLM mTeachingEnv  -- Conditional based on env
    $ graphAction

interpretLLM :: Maybe TeachingEnv -> Eff (LLM ': effs) a -> Eff effs a
interpretLLM Nothing = runLLMComplete defaultLLMEnv
interpretLLM (Just env) = runLLMWithTeaching env
```

### 4.4 Ensure NodeMeta is Set

The teaching interpreter needs `NodeMetadata` from the effect stack. Graph dispatch should set this:

```haskell
-- In graph dispatch (Interpret.hs or our runner)
-- When entering dgSelect node:
runNodeMeta (NodeMetadata "dgSelect" "DocGenGraph") $
  executeHandler (dgSelect handlers) input
```

Check if `runGraph` already does this, or if we need to add it.

### 4.5 Update MCP Handler

```haskell
-- In Handler/MCP.hs
handleTeachTool :: ServerConfig -> Value -> IO ControlResponse
handleTeachTool config args = do
  case parseTeachQuery args of
    Left err -> pure $ mcpToolError "teach" err
    Right query -> do
      result <- runDocGenGraph
        (scTeachingEnv config)
        (scLSPSession config)
        query
      case result of
        Left err -> pure $ mcpToolError "teach" err
        Right doc -> pure $ mcpToolSuccess "teach" (toJSON doc)
```

### 4.6 Server Shutdown Cleanup

```haskell
-- In Server.hs
shutdownServer :: ServerConfig -> IO ()
shutdownServer config = do
  -- Close teaching env (flushes JSONL)
  case scTeachingEnv config of
    Nothing -> pure ()
    Just env -> closeTeachingEnv env
  -- Close LSP session
  closeLSPSession (scLSPSession config)
```

## Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `Scout/Graph/Teacher.hs` | Create | FineTrainingTeacher instance |
| `Scout/Graph/Runner.hs` | Modify | Conditional teaching interpreter |
| `Server.hs` | Modify | Load teaching config, pass to handlers |
| `Handler/MCP.hs` | Modify | Accept ServerConfig with teaching env |
| `exomonad-control-server.cabal` | Modify | Add exomonad-teaching dependency |

## JSONL Output Location

```
.exomonad/training/
└── session-{uuid}/
    ├── anthropic.jsonl   # TeachingTurn records
    ├── gemma.jsonl       # Reserved for FunctionGemma format
    └── metadata.json     # Session config
```

## Verification

```bash
# Enable teaching mode
export TEACHING_ENABLED=true
export ANTHROPIC_API_KEY=sk-ant-...
export TEACHING_OUTPUT_DIR=.exomonad/training

# Start server
cabal run exomonad-control-server

# Make MCP call (creates training data)
# ... (via Claude Code or socat)

# Check output
ls .exomonad/training/session-*/
# Should see: anthropic.jsonl, metadata.json

# Verify content
cat .exomonad/training/session-*/anthropic.jsonl | jq .
# Should see TeachingTurn with:
#   ttNodeName: "dgSelect"
#   ttGraphName: "DocGenGraph"
#   ttThinking: (non-empty if Haiku used thinking)

# Verify thinking captured
cat .exomonad/training/session-*/anthropic.jsonl | jq '.ttResponse.content[] | select(.type=="thinking")'
```

## Notes

- Teaching mode adds latency (records to disk) - that's expected
- JSONL is append-only during session, flushed on shutdown
- Multiple MCP calls in one session all go to same JSONL file
