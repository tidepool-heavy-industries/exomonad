# Plan 06: Actor Runtime

## Scope

Update actor runtime to pass entry point names through the router.

## Files Owned

- `haskell/runtime/actor/src/Tidepool/Actor/Graph.hs`
- `haskell/runtime/actor/src/Tidepool/Actor/Router.hs` (new, extract from Graph.hs)

## Dependencies

- **02-goto-refactor**: `extractChoiceWithEntry` returns `(node, entry, payload)` triple

## Implementation

### 1. Router Message Format

```haskell
-- Old message format: just node + payload
data RouterMessage = RouterMessage
  { rmTarget  :: !Text   -- node name
  , rmPayload :: !Value  -- JSON payload
  }

-- New message format: node + entry + payload
data RouterMessage = RouterMessage
  { rmTarget     :: !Text   -- node name
  , rmEntryPoint :: !Text   -- entry point name (empty for exit)
  , rmPayload    :: !Value  -- JSON payload
  }

-- Construct from GotoChoice extraction
mkRouterMessage :: (Text, Text, Value) -> RouterMessage
mkRouterMessage (target, entry, payload) = RouterMessage target entry payload
```

### 2. Handler Wrapper Update

```haskell
-- Old wrapper: router takes (Text, Value)
type Router = Text -> Value -> IO ()

-- New wrapper: router takes full message
type Router = RouterMessage -> IO ()

-- Update wrapEffHandler signature
wrapEffHandler
  :: (FromJSON payload, ExtractChoiceWithEntry targets)
  => (payload -> Eff '[IO] (GotoChoice targets))
  -> NodeHandler
wrapEffHandler handler = NodeHandler $ \router jsonPayload entryPoint -> do
  case parseEither parseJSON jsonPayload of
    Left err -> error $ "Payload parse failed: " <> err
    Right payload -> do
      choice <- runM (handler payload)
      let msg = mkRouterMessage (extractChoiceWithEntry choice)
      router msg
```

### 3. Actor Mailbox Update

```haskell
-- Old mailbox: receives just payload
type ActorMailbox = TQueue Value

-- New mailbox: receives entry point + payload
data ActorMessage = ActorMessage
  { amEntryPoint :: !Text
  , amPayload    :: !Value
  }

type ActorMailbox = TQueue ActorMessage
```

### 4. Entry Point Dispatch in Actor

```haskell
-- Actor now receives entry point and dispatches accordingly
spawnActor
  :: Ki.Scope
  -> Text                               -- actor name
  -> (Text -> Value -> Router -> IO ()) -- handler (entry -> payload -> router -> IO)
  -> IO Actor
spawnActor scope name handler = do
  mailbox <- newTQueueIO
  thread <- Ki.fork scope $ actorLoop mailbox
  pure Actor { actorName = name, actorMailbox = mailbox, actorThread = thread }
  where
    actorLoop mailbox = forever $ do
      ActorMessage entry payload <- atomically (readTQueue mailbox)
      handler entry payload router
```

### 5. Router Dispatch Logic

```haskell
-- Extract to Router.hs for clarity
module Tidepool.Actor.Router where

-- Main router dispatch
routerDispatch
  :: IORef (Map Text Actor)  -- actors map
  -> MVar result             -- exit channel
  -> RouterMessage
  -> IO ()
routerDispatch actorsRef exitChan msg
  | rmTarget msg == "exit" = do
      -- Exit: put result in MVar
      case parseEither parseJSON (rmPayload msg) of
        Left err -> error $ "Exit payload parse failed: " <> err
        Right result -> putMVar exitChan result

  | rmTarget msg == "arrive" = do
      -- Barrier arrive (unchanged, entry point ignored)
      dispatchToBarrier actorsRef msg

  | otherwise = do
      -- Normal dispatch to actor with entry point
      actors <- readIORef actorsRef
      case Map.lookup (rmTarget msg) actors of
        Nothing -> error $ "Unknown actor: " <> T.unpack (rmTarget msg)
        Just actor -> atomically $
          writeTQueue (actorMailbox actor) ActorMessage
            { amEntryPoint = rmEntryPoint msg
            , amPayload = rmPayload msg
            }
```

### 6. Graph Runner Update

```haskell
-- Update runGraphAsActors to use new message format
runGraphAsActors
  :: FromJSON result
  => Text                       -- entry node name
  -> Text                       -- entry point name on entry node
  -> Map Text HandlerBuilder    -- node handlers
  -> Value                      -- initial payload
  -> IO result
runGraphAsActors entryNode entryPoint handlerBuilders initialPayload = do
  exitChan <- newEmptyMVar
  actorsRef <- newIORef Map.empty

  Ki.scoped $ \scope -> do
    -- Build router
    let router = routerDispatch actorsRef exitChan

    -- Spawn actors
    actors <- flip Map.traverseWithKey handlerBuilders $ \name mkHandler -> do
      (NodeHandler handler) <- mkHandler
      spawnActor scope name $ \entry payload ->
        handler entry payload router

    writeIORef actorsRef actors

    -- Send initial message to entry node with entry point
    case Map.lookup entryNode actors of
      Nothing -> error $ "No entry node: " <> T.unpack entryNode
      Just entryActor -> atomically $
        writeTQueue (actorMailbox entryActor) ActorMessage
          { amEntryPoint = entryPoint
          , amPayload = initialPayload
          }

    -- Wait for exit
    takeMVar exitChan
```

### 7. Backward Compatibility Shim (Temporary)

```haskell
-- For migration: wrapper that defaults entry point to "default"
runGraphAsActorsCompat
  :: FromJSON result
  => Map Text HandlerBuilder
  -> Value
  -> IO result
runGraphAsActorsCompat = runGraphAsActors "entry" "default"
```

## Tests

Update `test/Actor/GraphSpec.hs`:

```haskell
-- Multi-entry routing
testMultiEntryRouting :: IO ()
testMultiEntryRouting = do
  let handlers = Map.fromList
        [ ("work", mkWorkHandler)
        ]

  -- Route to "fresh" entry
  result1 <- runGraphAsActors "work" "fresh" handlers (toJSON taskSpec)
  result1 `shouldBe` ...

  -- Route to "retry" entry
  result2 <- runGraphAsActors "work" "retry" handlers (toJSON retryInfo)
  result2 `shouldBe` ...

-- Entry point in router message
testRouterMessage :: IO ()
testRouterMessage = do
  let choice = gotoChoice @"work" @"retry" retryInfo
  let (node, entry, payload) = extractChoiceWithEntry choice
  node `shouldBe` "work"
  entry `shouldBe` "retry"
```

## PR Criteria

- [ ] `RouterMessage` includes entry point
- [ ] `ActorMessage` includes entry point
- [ ] Actor handler receives entry point parameter
- [ ] `runGraphAsActors` takes entry node + entry point
- [ ] Tests for multi-entry routing
- [ ] Backward compat shim for migration

## Branch

`refactor/record-nodes-06-actor-runtime`
