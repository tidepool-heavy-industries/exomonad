# TODO: Improve ToGVal Derivation Ergonomics

## Problem

Consuming repos write 200+ lines of mechanical `ToGVal` instances:

```haskell
instance ToGVal GingerM FunctionSpec where
  toGVal fn = dict
    [ ("name", toGVal fn.name)
    , ("signature", toGVal fn.signature)
    , ("brief", toGVal fn.brief)
    -- ... 4 more fields, purely mechanical
    ]
```

Generic derivation (`genericToGVal`) exists but isn't being used. Why?

## Current State

1. **`genericToGVal` works** — empty instance bodies use `DefaultSignatures`
2. **Docs exist** in `Text.Ginger.GVal.Generic` but are minimal
3. **Tests exist** in `test/Text/Ginger/TH/Tests.hs` showing usage

## Why Consumers Don't Use It

1. **Discovery** — not obvious that empty instances work
2. **Sum type handling** — generic produces `{"Blocked": "reason"}` style, might not match template expectations
3. **Field name control** — no way to rename fields without manual instance
4. **List handling** — need `list (toGVal <$> xs)` pattern, unsure if generic handles it

## Proposed Work

### Option A: Better Docs + Examples (Low effort)

Add to `Text.Ginger.GVal.Generic`:

```haskell
-- | Derive ToGVal for records with an empty instance body:
--
-- @
-- data User = User { name :: Text, age :: Int }
--   deriving (Generic)
--
-- instance ToGVal m User  -- That's it! Uses genericToGVal
-- @
--
-- For sum types:
--
-- @
-- data Status = Active | Inactive | Pending
--   deriving (Generic)
--
-- instance ToGVal m Status  -- Produces: "Active", "Inactive", "Pending"
-- @
--
-- For sum types with data:
--
-- @
-- data Result = Success Text | Failure Error
--   deriving (Generic)
--
-- instance ToGVal m Result
-- -- Success "ok" -> {"Success": "ok"}
-- -- Failure e    -> {"Failure": <e as gval>}
-- @
```

### Option B: TH Derivation (Medium effort)

Add `deriveToGVal` TH function:

```haskell
-- In Text.Ginger.GVal.TH
deriveToGVal ''FunctionSpec
-- Generates the instance automatically

-- With options:
deriveToGValWith defaultOptions { fieldLabelModifier = dropPrefix } ''FunctionSpec
```

Benefits:
- Explicit derivation (no magic)
- Field name customization
- Clear error messages at compile time

### Option C: DerivingVia (Low effort, GHC 8.6+)

```haskell
newtype GenericGVal a = GenericGVal a

instance (Generic a, GToGVal m (Rep a)) => ToGVal m (GenericGVal a) where
  toGVal (GenericGVal a) = genericToGVal a

-- Usage:
data FunctionSpec = FunctionSpec { ... }
  deriving (Generic)
  deriving ToGVal via (GenericGVal FunctionSpec)
```

## Recommendation

1. **Immediate**: Option A — improve docs, add examples to module header
2. **Next**: Option C — add `DerivingVia` newtype for explicit opt-in
3. **Maybe later**: Option B — TH if field renaming becomes needed

## Validation

After changes, this should work in consuming repos:

```haskell
-- Before: 15 lines
instance ToGVal GingerM FunctionSpec where
  toGVal fn = dict
    [ ("name", toGVal fn.name)
    , ("signature", toGVal fn.signature)
    , ("brief", toGVal fn.brief)
    , ("behavior", toGVal fn.behavior)
    , ("examples", list (toGVal <$> fn.examples))
    , ("properties", list (toGVal <$> fn.properties))
    ]

-- After: 1 line
instance ToGVal GingerM FunctionSpec
```

## Files to Touch

- `src/Text/Ginger/GVal.hs` — add DerivingVia newtype
- `src/Text/Ginger/GVal/Generic.hs` — expand documentation
- `test/Text/Ginger/GVal/GenericSpec.hs` — add comprehensive tests
- `ginger.cabal` — expose new module if TH route chosen
