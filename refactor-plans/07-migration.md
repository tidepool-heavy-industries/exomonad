# Plan 07: Migration & Integration

## Scope

Migrate all existing code to new record-based DSL. Clean break - no backward compatibility.

## Files Owned

- All test files in `haskell/dsl/core/test/`
- All test files in `haskell/runtime/actor/test/`
- `types-first-dev/` (if still using old DSL)
- Any example graphs in tidepool itself

## Dependencies

- **ALL previous plans** must be merged first

## Implementation

### 1. Migration Pattern

**Old syntax**:
```haskell
data MyGraph mode = MyGraph
  { entry    :: mode :- Entry Message
  , classify :: mode :- LLMNode :@ Input Message :@ Template ClassifyTpl :@ Schema Intent
  , route    :: mode :- LogicNode :@ Input Intent :@ UsesEffects '[Goto "handle" Msg, Goto Exit Done]
  , handle   :: mode :- LLMNode :@ Input Msg :@ Template HandleTpl :@ Schema Response
  , exit     :: mode :- Exit Response
  }
```

**New syntax**:
```haskell
data MyGraph mode = MyGraph
  { entry    :: mode :- Entry Message
  , classify :: mode :- LLMNode
      :@ Entries '[ "fromEntry" ::: Message ]
      :@ Template ClassifyTpl
      :@ Exits '[ "toRoute" ::: Intent ]
  , route    :: mode :- LogicNode
      :@ Input Intent
      :@ UsesEffects '[GotoEntry "handle" "fromRoute" Msg, Goto Exit Done]
  , handle   :: mode :- LLMNode
      :@ Entries '[ "fromRoute" ::: Msg ]
      :@ Template HandleTpl
      :@ Exits '[ "complete" ::: Response ]
  , exit     :: mode :- Exit Response
  }
```

### 2. Handler Migration Pattern

**Old handler**:
```haskell
classifyHandler :: Message -> Eff es (LLMHandler ClassifyTpl Intent es)
classifyHandler msg = do
  let ctx = ClassifyCtx { message = msg }
  pure LLMHandler
    { llmUser = classifyTemplate
    , llmSystem = Nothing
    , llmBefore = \_ -> pure ctx
    , llmAfter = \intent -> pure $ gotoChoice @"route" intent
    }
```

**New handler**:
```haskell
classifyHandler :: LLMNodeHandler
    '[ '("fromEntry", Message) ]
    '[ '("toRoute", Intent) ]
    ClassifyCtx
    '[ToEntry "route" "fromClassify" Intent]
    es
classifyHandler = mkLLMHandler
    classifyTemplate
    Nothing
    -- Entry handlers
    (HCons fromEntryHandler HNil)
    -- Exit handlers
    (HCons toRouteHandler HNil)
  where
    fromEntryHandler :: Message -> Eff es ClassifyCtx
    fromEntryHandler msg = pure ClassifyCtx { message = msg }

    toRouteHandler :: Intent -> Eff es (GotoChoice '[ToEntry "route" "fromClassify" Intent])
    toRouteHandler intent = pure $ gotoChoice @"route" @"fromClassify" intent
```

### 3. Test File Migration

Files to migrate in `haskell/dsl/core/test/`:
- [ ] `CallHandlerSpec.hs`
- [ ] `LLMNodeInterpretSpec.hs`
- [ ] `Graph/GotoSpec.hs`
- [ ] `Graph/ValidationSpec.hs`
- [ ] Any other graph-related tests

Files to migrate in `haskell/runtime/actor/test/`:
- [ ] `GraphSpec.hs`
- [ ] `ForkBarrierSpec.hs`
- [ ] Any other actor tests

### 4. types-first-dev Migration

Key files:
- [ ] `src/TypesFirstDev/Graph.hs` - Graph definition
- [ ] `src/TypesFirstDev/WorkHandlers.hs` - Handler implementations
- [ ] `src/TypesFirstDev/ScaffoldHandlers.hs`
- [ ] `src/TypesFirstDev/ImplHandlers.hs`
- [ ] `src/TypesFirstDev/ReviewHandlers.hs`

### 5. Remove Deprecated Code

After migration, remove:
- [ ] Old `Input` annotation (replaced by `Entries`)
- [ ] Old `Goto` effect for named targets (replaced by `GotoEntry`)
- [ ] Old `LLMHandler` type (replaced by `LLMNodeHandler`)
- [ ] Old single-target `gotoChoice` overloads
- [ ] Backward compat shims from plan 06

### 6. Documentation Update

- [ ] Update `haskell/dsl/core/CLAUDE.md` with new DSL reference
- [ ] Update top-level `CLAUDE.md` with new patterns
- [ ] Add migration guide section

### 7. Migration Script (Optional)

```bash
#!/bin/bash
# Semi-automated migration helper

# Find all graph definitions
rg "mode :- LLMNode :@ Input" --files-with-matches

# Find all old-style handlers
rg "LLMHandler \{" --files-with-matches

# Find all old-style gotos
rg 'gotoChoice @"[^"]*"[^@]' --files-with-matches
```

## Verification

### Build Verification
```bash
cd ~/dev/tidepool
cabal build all
cabal test all
```

### Runtime Verification
```bash
# Run native server with test graph
just native

# Test via curl/WebSocket
curl localhost:8080/api/status
```

### Integration Verification
```bash
# types-first-dev (if applicable)
cd ~/dev/tidepool/types-first-dev
cabal build
cabal test
```

## PR Criteria

- [ ] All graphs migrated to new syntax
- [ ] All handlers migrated to new format
- [ ] All tests pass
- [ ] Old deprecated code removed
- [ ] Documentation updated
- [ ] No backward compatibility shims remaining

## Branch

`refactor/record-nodes-07-migration`

## Notes

This is the final integration plan. Should only be executed after ALL other plans are merged. Consider doing this in a single focused session to avoid merge conflicts with active development.
