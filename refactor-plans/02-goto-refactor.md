# Plan 02: Goto Refactor

## Scope

Refactor `gotoChoice` to target both node AND entry point.

## Files Owned

- `haskell/dsl/core/src/Tidepool/Graph/Goto.hs`
- `haskell/dsl/core/src/Tidepool/Graph/Goto/Internal.hs`

## Dependencies

- **01-core-types**: Need `Entries`, `LookupEntry` type families

## Implementation

### 1. New Target Type (Internal.hs)

```haskell
-- Old: To target payload (target = node name or Exit)
-- New: ToEntry target entry payload (target = node, entry = entry point name)

-- Target with entry point
type ToEntry :: k -> Symbol -> Type -> Type
data ToEntry target entry payload

-- Keep To for Exit (no entry point on exit)
type To :: k -> Type -> Type
data To target payload

-- Extract payloads from mixed target list
type Payloads :: [Type] -> [Type]
type family Payloads targets where
  Payloads '[] = '[]
  Payloads (ToEntry _ _ payload ': rest) = payload ': Payloads rest
  Payloads (To _ payload ': rest) = payload ': Payloads rest
```

### 2. New GotoChoice Smart Constructors (Goto.hs)

```haskell
-- Target node + entry point
gotoChoice
  :: forall (node :: Symbol) (entry :: Symbol) payload targets.
     ( NonEmptyList targets
     , InjectTarget (ToEntry node entry payload) targets
     , GotoElemC (ToEntry node entry payload) targets
     )
  => payload -> GotoChoice targets

-- Exit graph (no entry point)
gotoExit
  :: forall payload targets.
     ( NonEmptyList targets
     , InjectTarget (To Exit payload) targets
     , GotoElemC (To Exit payload) targets
     )
  => payload -> GotoChoice targets

-- Self-loop with entry point
gotoSelf
  :: forall (entry :: Symbol) payload targets.
     ( NonEmptyList targets
     , InjectTarget (ToEntry Self entry payload) targets
     , GotoElemC (ToEntry Self entry payload) targets
     )
  => payload -> GotoChoice targets
```

### 3. Usage Effects Annotation Update

```haskell
-- Old: UsesEffects '[Goto "nextNode" Payload]
-- New: UsesEffects '[GotoEntry "nextNode" "entryPoint" Payload]

-- New effect marker for entry-aware goto
data GotoEntry (node :: Symbol) (entry :: Symbol) (payload :: Type)

-- Keep Goto for Exit (backward compat during migration)
data Goto (target :: k) (payload :: Type)

-- Convert effects to targets
type GotoEffectsToTargets :: [Type] -> [Type]
type family GotoEffectsToTargets effs where
  GotoEffectsToTargets '[] = '[]
  GotoEffectsToTargets (GotoEntry node entry payload ': rest) =
    ToEntry node entry payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (Goto Exit payload ': rest) =
    To Exit payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (_ ': rest) = GotoEffectsToTargets rest
```

### 4. Target Extraction for Dispatch

```haskell
-- Extract routing info from GotoChoice
class ExtractChoiceWithEntry targets where
  extractChoiceWithEntry :: GotoChoice targets -> (Text, Text, Value)
  -- Returns: (nodeName, entryName, jsonPayload)

instance (KnownSymbol node, KnownSymbol entry, ToJSON payload, ExtractChoiceWithEntry rest)
  => ExtractChoiceWithEntry (ToEntry node entry payload ': rest) where
  extractChoiceWithEntry (GotoChoice (Here payload)) =
    ( T.pack (symbolVal (Proxy @node))
    , T.pack (symbolVal (Proxy @entry))
    , toJSON payload
    )
  extractChoiceWithEntry (GotoChoice (There rest)) =
    extractChoiceWithEntry @rest (GotoChoice rest)

-- Exit has no entry point
instance (ToJSON payload, ExtractChoiceWithEntry rest)
  => ExtractChoiceWithEntry (To Exit payload ': rest) where
  extractChoiceWithEntry (GotoChoice (Here payload)) =
    ("exit", "", toJSON payload)  -- Empty entry for exit
  extractChoiceWithEntry (GotoChoice (There rest)) =
    extractChoiceWithEntry @rest (GotoChoice rest)
```

### 5. Example Usage

```haskell
-- Handler for exit that routes to another node's entry point
handleComplete :: CompletePayload -> Eff es (GotoChoice '[ToEntry "verify" "fromWork" VerifyInput, To Exit FinalResult])
handleComplete payload = case payload.status of
  NeedsVerify -> pure $ gotoChoice @"verify" @"fromWork" (mkVerifyInput payload)
  Done -> pure $ gotoExit (mkFinalResult payload)
```

## Tests

Create/update `test/Graph/GotoSpec.hs`:

```haskell
-- Type-level: gotoChoice produces correct target
testGotoChoice ::
  let choice = gotoChoice @"work" @"retry" ("info" :: Text)
  in extractChoiceWithEntry choice == ("work", "retry", toJSON "info")

-- Exit has empty entry
testGotoExit ::
  let choice = gotoExit @Int 42
  in extractChoiceWithEntry choice == ("exit", "", toJSON (42 :: Int))
```

## PR Criteria

- [ ] New `gotoChoice` signature with two type params
- [ ] `extractChoiceWithEntry` returns triple (node, entry, payload)
- [ ] Old `Goto` effect still exists for Exit
- [ ] Tests pass
- [ ] No changes to dispatch (that's plan 05/06)

## Branch

`refactor/record-nodes-02-goto-refactor`
