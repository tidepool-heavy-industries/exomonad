# Plan 04: Handler Type Generation

## Scope

Generate handler record types from Entries/Exits annotations.

## Files Owned

- `haskell/dsl/core/src/Tidepool/Graph/Generic.hs`
- `haskell/dsl/core/src/Tidepool/Graph/Handler.hs` (new)

## Dependencies

- **01-core-types**: Need `Entries`, `Exits`, extraction type families

## Implementation

### 1. New Handler Record Types (Handler.hs)

```haskell
module Tidepool.Graph.Handler where

-- Handler for a single entry point
-- Converts entry payload to template context
type EntryHandler :: Type -> Type -> [Effect] -> Type
type EntryHandler payload ctx es = payload -> Eff es ctx

-- Handler for a single exit point
-- Converts exit payload (from LLM tool call) to routing decision
type ExitHandler :: Type -> [Type] -> [Effect] -> Type
type ExitHandler payload targets es = payload -> Eff es (GotoChoice targets)

-- Full LLM node handler record
data LLMNodeHandler entries exits ctx targets es = LLMNodeHandler
  { lnhEntryHandlers :: !(HList (EntryHandlersFor entries ctx es))
  , lnhTemplate      :: !(TypedTemplate ctx SourcePos)
  , lnhSystemTemplate :: !(Maybe (TypedTemplate ctx SourcePos))
  , lnhExitHandlers  :: !(HList (ExitHandlersFor exits targets es))
  }

-- Generate entry handler types from Entries annotation
type EntryHandlersFor :: [(Symbol, Type)] -> Type -> [Effect] -> [Type]
type family EntryHandlersFor entries ctx es where
  EntryHandlersFor '[] _ _ = '[]
  EntryHandlersFor ('(_, payload) ': rest) ctx es =
    EntryHandler payload ctx es ': EntryHandlersFor rest ctx es

-- Generate exit handler types from Exits annotation
type ExitHandlersFor :: [(Symbol, Type)] -> [Type] -> [Effect] -> [Type]
type family ExitHandlersFor exits targets es where
  ExitHandlersFor '[] _ _ = '[]
  ExitHandlersFor ('(_, payload) ': rest) targets es =
    ExitHandler payload targets es ': ExitHandlersFor rest targets es
```

### 2. HList for Handler Records

```haskell
-- Heterogeneous list for handler records
data HList (ts :: [Type]) where
  HNil  :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

-- Index into HList by position
class HListIndex (n :: Nat) (ts :: [Type]) where
  type HListAt n ts :: Type
  hlistIndex :: HList ts -> HListAt n ts

instance HListIndex 0 (t ': ts) where
  type HListAt 0 (t ': ts) = t
  hlistIndex (HCons x _) = x

instance HListIndex n ts => HListIndex (n + 1) (t ': ts) where
  type HListAt (n + 1) (t ': ts) = HListAt n ts
  hlistIndex (HCons _ xs) = hlistIndex @n xs

-- Lookup handler by name (finds position, then indexes)
type EntryPosition :: Symbol -> [(Symbol, Type)] -> Nat
type family EntryPosition name entries where
  EntryPosition name ('(name, _) ': _) = 0
  EntryPosition name (_ ': rest) = 1 + EntryPosition name rest
  EntryPosition name '[] = TypeError ('Text "Entry point '" ':<>: 'Text name ':<>: 'Text "' not found")
```

### 3. Update NodeHandler Type Family (Generic.hs)

```haskell
-- New handler type for LLM nodes with Entries/Exits
type NodeHandler :: Type -> [Effect] -> Type
type family NodeHandler nodeDef es where
  -- Entry/Exit markers unchanged
  NodeHandler (Entry a) es = Proxy a
  NodeHandler (Exit a) es = Proxy a

  -- Logic nodes unchanged (still use UsesEffects)
  NodeHandler (LogicNode :@ ann) es =
    NodeHandlerLogic (LogicNode :@ ann) es

  -- LLM nodes with new Entries/Exits
  NodeHandler (LLMNode :@ ann) es =
    NodeHandlerLLM (LLMNode :@ ann) es

-- Generate LLM handler type
type NodeHandlerLLM :: Type -> [Effect] -> Type
type family NodeHandlerLLM node es where
  NodeHandlerLLM node es =
    LLMNodeHandler
      (FromJust (GetEntries node))      -- entries
      (FromJust (GetExits node))        -- exits
      (TemplateContext (FromJust (GetTemplate node)))  -- ctx type
      (ExitsToTargets (FromJust (GetExits node)))      -- routing targets
      es

-- Convert Exits to GotoChoice targets
type ExitsToTargets :: [(Symbol, Type)] -> [Type]
type family ExitsToTargets exits where
  ExitsToTargets '[] = '[]
  ExitsToTargets ('(name, payload) ': rest) =
    -- Each exit can route to any target (determined by handler)
    -- The actual targets come from the handler's return type
    ExitsToTargets rest
```

### 4. Handler Construction Helpers

```haskell
-- Build entry handlers HList from individual handlers
class BuildEntryHandlers (entries :: [(Symbol, Type)]) ctx es where
  type EntryHandlerArgs entries ctx es :: [Type]
  buildEntryHandlers :: HList (EntryHandlerArgs entries ctx es) -> HList (EntryHandlersFor entries ctx es)

-- Build exit handlers HList
class BuildExitHandlers (exits :: [(Symbol, Type)]) targets es where
  type ExitHandlerArgs exits targets es :: [Type]
  buildExitHandlers :: HList (ExitHandlerArgs exits targets es) -> HList (ExitHandlersFor exits targets es)

-- Smart constructor for full LLM handler
mkLLMHandler
  :: forall entries exits ctx targets es.
     ( BuildEntryHandlers entries ctx es
     , BuildExitHandlers exits targets es
     )
  => TypedTemplate ctx SourcePos                    -- user template
  -> Maybe (TypedTemplate ctx SourcePos)            -- system template
  -> HList (EntryHandlerArgs entries ctx es)        -- entry handlers
  -> HList (ExitHandlerArgs exits targets es)       -- exit handlers
  -> LLMNodeHandler entries exits ctx targets es
```

### 5. Runtime Handler Lookup

```haskell
-- Get entry handler by name at runtime
getEntryHandler
  :: forall (name :: Symbol) entries ctx es.
     ( KnownSymbol name
     , HListIndex (EntryPosition name entries) (EntryHandlersFor entries ctx es)
     )
  => LLMNodeHandler entries exits ctx targets es
  -> EntryHandler (LookupEntry name entries) ctx es
getEntryHandler handler =
  hlistIndex @(EntryPosition name entries) (lnhEntryHandlers handler)

-- Get exit handler by name at runtime
getExitHandler
  :: forall (name :: Symbol) exits targets es.
     ( KnownSymbol name
     , HListIndex (ExitPosition name exits) (ExitHandlersFor exits targets es)
     )
  => LLMNodeHandler entries exits ctx targets es
  -> ExitHandler (LookupExit name exits) targets es
getExitHandler handler =
  hlistIndex @(ExitPosition name exits) (lnhExitHandlers handler)
```

## Example Usage

```haskell
-- Node definition
gWork :: mode :- LLMNode
    :@ Entries '[ "fresh" ::: TaskSpec, "retry" ::: RetryInfo ]
    :@ Template WorkTpl
    :@ Exits '[ "complete" ::: CompletePayload, "blocked" ::: BlockedPayload ]

-- Handler implementation
workHandler :: LLMNodeHandler
    '[ '("fresh", TaskSpec), '("retry", RetryInfo) ]
    '[ '("complete", CompletePayload), '("blocked", BlockedPayload) ]
    WorkCtx
    '[ToEntry "verify" "fromWork" VerifyInput, To Exit FinalResult]
    es
workHandler = mkLLMHandler
    workTemplate
    (Just workSystemTemplate)
    -- Entry handlers (all produce WorkCtx)
    (HCons freshToCtx (HCons retryToCtx HNil))
    -- Exit handlers (each routes to next node)
    (HCons handleComplete (HCons handleBlocked HNil))
  where
    freshToCtx :: TaskSpec -> Eff es WorkCtx
    freshToCtx spec = pure WorkCtx { ... }

    retryToCtx :: RetryInfo -> Eff es WorkCtx
    retryToCtx info = pure WorkCtx { critique = Just info.critique, ... }

    handleComplete :: CompletePayload -> Eff es (GotoChoice '[...])
    handleComplete p = pure $ gotoChoice @"verify" @"fromWork" (mkVerifyInput p)

    handleBlocked :: BlockedPayload -> Eff es (GotoChoice '[...])
    handleBlocked p = pure $ gotoExit (mkBlockedResult p)
```

## Tests

Create `test/Graph/HandlerSpec.hs`:

```haskell
-- Handler type computation
testHandlerType ::
  NodeHandler (LLMNode :@ Entries '["a" ::: Int] :@ Template T :@ Exits '["x" ::: Bool]) '[IO]
  :~:
  LLMNodeHandler '[ '("a", Int)] '[ '("x", Bool)] (TemplateContext T) ... '[IO]

-- Entry handler lookup
testEntryLookup :: ...
```

## PR Criteria

- [ ] `LLMNodeHandler` record type defined
- [ ] `NodeHandler` type family updated for Entries/Exits
- [ ] HList-based handler storage
- [ ] Name-based handler lookup at runtime
- [ ] No changes to dispatch (that's plan 05)

## Branch

`refactor/record-nodes-04-handler-generation`
