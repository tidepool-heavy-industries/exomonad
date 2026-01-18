# Task 03: Wire Graph Execution into Control-Server

**Epic:** [00-epic-docgen-graph.md](00-epic-docgen-graph.md)
**Depends On:** [01-docgen-graph-design.md](01-docgen-graph-design.md), [02-templates-and-schemas.md](02-templates-and-schemas.md)
**Blocks:** 04

## Goal

Integrate the DocGen graph into control-server's MCP handler so the `teach` tool executes the graph instead of the hand-rolled BFS loop.

## Context

### Current Flow

```
Handler/MCP.hs:handleTeachTool
    → runScoutGemmaHTTP endpoint $
        runLSP lspSession $
        scout defaultTeachConfig query
    → scout calls exploreLoop (hand-rolled BFS)
    → ScoutGemma effect makes direct HTTP calls
```

### Target Flow

```
Handler/MCP.hs:handleTeachTool
    → runDocGenGraph lspSession $
        runGraph docGenHandlers query
    → Graph dispatches through nodes
    → dgSelect node uses LLM effect (interceptable by teaching)
```

## Acceptance Criteria

- [ ] **`teach` MCP tool executes graph** instead of exploreLoop
- [ ] **LSP session is available** to graph handlers
- [ ] **LLM effect is in stack** for dgSelect node
- [ ] **Graph returns TeachingDoc** same as before
- [ ] **Existing tests pass** (if any)
- [ ] **Logs show node transitions** for debugging

## Subtasks

### 3.1 Create Graph Handlers

```haskell
-- In Graph/Handlers.hs
docGenHandlers :: DocGenGraph (AsHandler DocGenEffects)
docGenHandlers = DocGenGraph
  { dgEntry = Proxy @TeachQuery

  , dgInit = \query -> do
      let state = initExploreState query
      updateMem @ExploreState (const state)
      case esFrontier state of
        [] -> pure $ gotoChoice @"dgFinalize" (FinalizeInput FrontierEmpty)
        ((sym, depth):_) -> pure $ gotoChoice @"dgProcess" (ProcessInput sym depth)

  , dgProcess = \input -> do
      state <- getMem @ExploreState
      -- LSP lookup
      symbol <- lspHover (piSymbolKey input)
      let candidates = extractCandidates (lsSignature symbol)
      if null candidates
        then do
          -- No candidates, skip to next in frontier
          advanceFrontier
          nextOrFinalize
        else
          pure $ gotoChoice @"dgSelect" SelectInput
            { siTopic = esTopic state
            , siSymbol = symbol
            , siCandidates = candidates
            }

  , dgSelect = LLMHandler
      { llmSystem = Nothing
      , llmUser = templateCompiled @SelectTpl
      , llmBefore = \input -> pure SelectContext
          { scTopic = siTopic input
          , scSymbolName = lsName (siSymbol input)
          , scSignature = lsSignature (siSymbol input)
          , scDocComment = lsDocComment (siSymbol input)
          , scCandidates = siCandidates input
          }
      , llmAfter = \output -> do
          state <- getMem @ExploreState
          pure $ gotoChoice @"dgExpand" ExpandInput
            { eiSelected = soSelected output
            , eiCurrentDepth = currentDepth state
            }
      }

  , dgExpand = \input -> do
      -- Resolve selected symbols via LSP
      resolved <- resolveSymbols (eiSelected input)
      -- Add to frontier at depth+1
      let newEntries = [(k, eiCurrentDepth input + 1) | k <- resolved]
      updateMem @ExploreState $ \s -> s
        { esFrontier = esFrontier s ++ newEntries
        , esBudget = esBudget s - 1
        }
      advanceFrontier
      nextOrFinalize

  , dgFinalize = \input -> do
      state <- getMem @ExploreState
      let doc = buildTeachingDoc state
      pure $ gotoExit doc

  , dgExit = Proxy @TeachingDoc
  }
```

### 3.2 Create Graph Runner

```haskell
-- In Graph/Runner.hs
runDocGenGraph
  :: LSPSession
  -> TeachQuery
  -> IO (Either Text TeachingDoc)
runDocGenGraph lspSession query = do
  let initialState = ExploreState
        { esGraph = Map.empty
        , esFrontier = []
        , esVisited = Set.empty
        , esBudget = tqBudget query
        , esTopic = tqTopic query
        }

  runM
    . runError @Text
    . runLog Debug
    . runMemory initialState
    . runLSP lspSession
    . runLLMComplete llmEnv  -- Or runLLMWithTeaching when enabled
    $ runGraph docGenHandlers query
```

### 3.3 Update MCP Handler

```haskell
-- In Handler/MCP.hs
handleTeachTool :: LSPSession -> Value -> IO ControlResponse
handleTeachTool lspSession args = do
  case parseTeachQuery args of
    Left err -> pure $ mcpToolError "teach" err
    Right query -> do
      result <- runDocGenGraph lspSession query
      case result of
        Left err -> pure $ mcpToolError "teach" err
        Right doc -> pure $ mcpToolSuccess "teach" (toJSON doc)
```

### 3.4 Wire LSP Effect

The LSP effect needs access to the session. Options:

**Option A: Reader-style**
```haskell
data LSPEnv = LSPEnv { leSession :: LSPSession }

runLSP :: LSPEnv -> Eff (LSP ': effs) a -> Eff effs a
```

**Option B: Pass session through handlers**
```haskell
-- Handlers close over session
mkDocGenHandlers :: LSPSession -> DocGenGraph (AsHandler effs)
```

**Option C: Use existing LSP effect from lsp-interpreter**
Check what `tidepool-lsp-interpreter` provides.

### 3.5 Handle Effect Stack

The full effect stack needs:
```haskell
type DocGenEffects =
  '[ Memory ExploreState
   , LSP
   , LLM
   , Log
   , Error Text
   , IO
   ]
```

Ensure all effects have interpreters wired in `runDocGenGraph`.

## Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `Scout/Graph/Handlers.hs` | Create | Node handler implementations |
| `Scout/Graph/Runner.hs` | Create | runDocGenGraph entry point |
| `Handler/MCP.hs` | Modify | Call runDocGenGraph instead of scout |
| `Server.hs` | Modify | Pass LSP session to MCP handler |

## Verification

```bash
# Build succeeds
cabal build tidepool-control-server

# Start server
ANTHROPIC_API_KEY=sk-ant-... cabal run tidepool-control-server

# Test MCP call (via socat or Claude Code)
echo '{"type":"McpToolCall","mcpId":"1","toolName":"teach","arguments":{"topic":"scoring","seeds":["compositeScore"],"budget":5}}' | socat - UNIX-CONNECT:.tidepool/control.sock

# Should return TeachingDoc JSON
# Logs should show node transitions: dgInit -> dgProcess -> dgSelect -> dgExpand -> ...
```

## Notes

- Start with LLM calls going to Haiku directly (not teaching mode yet)
- Teaching integration is Task 04
- Keep the old `scout` function around temporarily for comparison
