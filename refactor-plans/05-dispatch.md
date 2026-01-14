# Plan 05: Dispatch & Interpretation

## Scope

Update dispatch logic to route to specific entry points and invoke exit handlers.

## Files Owned

- `haskell/dsl/core/src/Tidepool/Graph/Interpret.hs`

## Dependencies

- **01-core-types**: Entry/Exit annotations
- **04-handler-generation**: `LLMNodeHandler`, handler lookup functions

## Implementation

### 1. Entry-Point-Aware Dispatch

```haskell
-- Old dispatch: just payload
dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType

-- New dispatch: payload + entry point name
dispatchGotoWithEntry
  :: graph (AsHandler es)
  -> Text              -- target node name
  -> Text              -- target entry point name
  -> Value             -- JSON payload
  -> Eff es exitType
```

### 2. LLM Node Execution Pipeline

```haskell
-- Execute LLM node with entry-point routing
executeLLMNode
  :: forall entries exits ctx targets es.
     ( KnownSymbol entryName
     , FromJSON (LookupEntry entryName entries)
     , ToGVal ctx
     , HasJSONSchema exitSchema
     , FromJSON exitSchema
     )
  => LLMNodeHandler entries exits ctx targets es
  -> Text              -- entry point name (runtime)
  -> Value             -- entry payload (JSON)
  -> Eff (LLM ': es) (GotoChoice targets)
executeLLMNode handler entryName payload = do
  -- 1. Parse payload and get entry handler dynamically
  ctx <- invokeEntryHandler handler entryName payload

  -- 2. Render template with context
  let userPrompt = runTypedTemplate ctx (lnhTemplate handler)
  let systemPrompt = fmap (runTypedTemplate ctx) (lnhSystemTemplate handler)

  -- 3. Call LLM with exits as tools
  let exitTools = exitsToTools @exits
  (exitName, exitPayload) <- callLLMWithTools systemPrompt userPrompt exitTools

  -- 4. Invoke exit handler
  invokeExitHandler handler exitName exitPayload
```

### 3. Dynamic Entry Handler Invocation

```haskell
-- Invoke entry handler by name (runtime string)
class InvokeEntryHandler entries ctx es where
  invokeEntryHandler
    :: HList (EntryHandlersFor entries ctx es)
    -> Text          -- entry name (runtime)
    -> Value         -- payload (JSON)
    -> Eff es ctx

instance InvokeEntryHandler '[] ctx es where
  invokeEntryHandler _ name _ =
    error $ "Unknown entry point: " <> T.unpack name

instance
  ( KnownSymbol name
  , FromJSON payload
  , InvokeEntryHandler rest ctx es
  )
  => InvokeEntryHandler ('(name, payload) ': rest) ctx es where
  invokeEntryHandler (HCons handler rest) entryName jsonPayload
    | entryName == T.pack (symbolVal (Proxy @name)) =
        case parseEither parseJSON jsonPayload of
          Left err -> error $ "Failed to parse entry payload: " <> err
          Right payload -> handler payload
    | otherwise =
        invokeEntryHandler @rest rest entryName jsonPayload
```

### 4. Dynamic Exit Handler Invocation

```haskell
-- Invoke exit handler by name (runtime string from LLM tool call)
class InvokeExitHandler exits targets es where
  invokeExitHandler
    :: HList (ExitHandlersFor exits targets es)
    -> Text          -- exit name (from LLM)
    -> Value         -- payload (from LLM tool call)
    -> Eff es (GotoChoice targets)

instance InvokeExitHandler '[] targets es where
  invokeExitHandler _ name _ =
    error $ "Unknown exit: " <> T.unpack name

instance
  ( KnownSymbol name
  , FromJSON payload
  , InvokeExitHandler rest targets es
  )
  => InvokeExitHandler ('(name, payload) ': rest) targets es where
  invokeExitHandler (HCons handler rest) exitName jsonPayload
    | exitName == T.pack (symbolVal (Proxy @name)) =
        case parseEither parseJSON jsonPayload of
          Left err -> error $ "Failed to parse exit payload: " <> err
          Right payload -> handler payload
    | otherwise =
        invokeExitHandler @rest rest exitName jsonPayload
```

### 5. Exits to Tools Conversion

```haskell
-- Convert Exits annotation to LLM tool definitions
class ExitsToTools (exits :: [(Symbol, Type)]) where
  exitsToTools :: [ToolDefinition]

instance ExitsToTools '[] where
  exitsToTools = []

instance
  ( KnownSymbol name
  , HasJSONSchema payload
  , ExitsToTools rest
  )
  => ExitsToTools ('(name, payload) ': rest) where
  exitsToTools =
    ToolDefinition
      { toolName = toSnakeCase (T.pack (symbolVal (Proxy @name)))
      , toolDescription = ""  -- Could add via separate annotation
      , toolInputSchema = jsonSchema @payload
      }
    : exitsToTools @rest

-- Convert PascalCase/camelCase to snake_case for tool names
toSnakeCase :: Text -> Text
toSnakeCase = ... -- "completeWork" -> "complete_work"
```

### 6. Graph-Level Dispatch

```haskell
-- Updated DispatchGoto to handle entry points
class DispatchGotoWithEntry graph targets es exitType where
  dispatchGotoWithEntry
    :: graph (AsHandler es)
    -> GotoChoice targets
    -> Eff es exitType

-- Terminal: Exit
instance (FromJSON exitType) => DispatchGotoWithEntry graph '[To Exit exitType] es exitType where
  dispatchGotoWithEntry _ (GotoChoice (Here payload)) = pure payload

-- Recursive: ToEntry
instance
  ( KnownSymbol node
  , KnownSymbol entry
  , HasField node (graph (AsHandler es)) (LLMNodeHandler entries exits ctx targets' es)
  , InvokeEntryHandler entries ctx es
  , LookupEntry entry entries ~ 'Just payload
  , ToJSON payload
  , DispatchGotoWithEntry graph rest es exitType
  )
  => DispatchGotoWithEntry graph (ToEntry node entry payload ': rest) es exitType where
  dispatchGotoWithEntry graph (GotoChoice (Here payload)) = do
    let handler = getField @node graph
    let jsonPayload = toJSON payload
    let entryName = T.pack (symbolVal (Proxy @entry))
    -- Execute node starting from entry point
    nextChoice <- executeLLMNode handler entryName jsonPayload
    dispatchGotoWithEntry graph nextChoice
  dispatchGotoWithEntry graph (GotoChoice (There rest)) =
    dispatchGotoWithEntry @graph @rest graph (GotoChoice rest)
```

### 7. runGraph Update

```haskell
-- Updated to take entry point name
runGraphFromEntry
  :: forall graph entryType targets exitType es entryNodeName entryPointName.
     ( ... constraints ...
     )
  => graph (AsHandler es)
  -> Proxy entryPointName   -- which entry point on entry node
  -> entryType              -- payload for that entry
  -> Eff es exitType
runGraphFromEntry graph _ input = do
  let entryHandler = getField @entryNodeName graph
  let entryName = T.pack (symbolVal (Proxy @entryPointName))
  firstChoice <- executeLLMNode entryHandler entryName (toJSON input)
  dispatchGotoWithEntry graph firstChoice
```

## Tests

Update `test/Graph/InterpretSpec.hs`:

```haskell
-- Multi-entry dispatch
testMultiEntryDispatch :: IO ()
testMultiEntryDispatch = do
  let graph = TestGraph { work = workHandler, ... }

  -- Route to "fresh" entry
  result1 <- runGraphFromEntry graph (Proxy @"fresh") taskSpec
  result1 `shouldBe` ...

  -- Route to "retry" entry
  result2 <- runGraphFromEntry graph (Proxy @"retry") retryInfo
  result2 `shouldBe` ...

-- Exit handler invocation
testExitHandlerInvocation :: IO ()
testExitHandlerInvocation = do
  -- Mock LLM returning "complete" tool
  result <- runWithMockLLM (toolCall "complete" payload) ...
  -- Verify complete handler was called
```

## PR Criteria

- [ ] `executeLLMNode` handles entry point dispatch
- [ ] `invokeEntryHandler` / `invokeExitHandler` work dynamically
- [ ] `exitsToTools` generates tool definitions from Exits
- [ ] `DispatchGotoWithEntry` handles ToEntry targets
- [ ] Tests with mock LLM for tool call flow

## Branch

`refactor/record-nodes-05-dispatch`
