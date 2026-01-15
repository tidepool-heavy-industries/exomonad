# Work Item 03: Graph Entry/Exit Extraction

**Priority**: Medium
**Depends on**: 01 (GraphNode marker)
**Parallelizable with**: 04, 05
**Blocks**: 06, 07

## Goal

Add type families to extract Entry and Exit types from graph definitions. This enables `GraphNode` to know the input/output types of child graphs.

## Files to Modify

- `haskell/dsl/core/src/Tidepool/Graph/Edges.hs`

## Implementation

### 1. Add to exports (after GetClaudeCode ~line 35)

```haskell
    -- * Graph Entry/Exit Extraction
  , GetGraphEntry
  , GetGraphExit
```

### 2. Add imports (if not present)

```haskell
import GHC.Generics (Rep, M1, D, C, S, K1, (:*:), Meta(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))
```

### 3. Add type families (new section after ClaudeCode extraction)

```haskell
-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH ENTRY/EXIT EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract the Entry type from a graph definition.
--
-- Given a graph type constructor, returns the input type of its Entry node.
--
-- @
-- GetGraphEntry SemanticScoutGraph = SemanticOrder
-- @
type GetGraphEntry :: (Type -> Type) -> Type
type family GetGraphEntry graph where
  GetGraphEntry g = FindEntryType (Rep (g AsGraph))

-- | Extract the Exit type from a graph definition.
--
-- Given a graph type constructor, returns the output type of its Exit node.
--
-- @
-- GetGraphExit SemanticScoutGraph = CompressedIntel
-- @
type GetGraphExit :: (Type -> Type) -> Type
type family GetGraphExit graph where
  GetGraphExit g = FindExitType (Rep (g AsGraph))

-- | Walk Generic Rep to find Entry type.
type FindEntryType :: (Type -> Type) -> Type
type family FindEntryType rep where
  -- Unwrap metadata layers
  FindEntryType (M1 D _ inner) = FindEntryType inner
  FindEntryType (M1 C _ inner) = FindEntryType inner
  -- Found Entry node (with or without annotations)
  FindEntryType (M1 S _ (K1 _ (_ :- Entry t))) = t
  FindEntryType (M1 S _ (K1 _ (_ :- Entry t :@ _))) = t
  FindEntryType (M1 S _ (K1 _ (_ :- Entry t :@ _ :@ _))) = t
  -- Skip non-Entry fields
  FindEntryType (M1 S _ _) = TypeError ('Text "No Entry node found")
  -- Search product (Entry typically first, but check both)
  FindEntryType (left :*: right) = FindEntryTypeInProduct (FindEntryType left) right

-- | Helper for product search - if left found Entry, use it; else check right.
type FindEntryTypeInProduct :: Type -> (Type -> Type) -> Type
type family FindEntryTypeInProduct leftResult right where
  FindEntryTypeInProduct (TypeError _) right = FindEntryType right
  FindEntryTypeInProduct found _ = found

-- | Walk Generic Rep to find Exit type.
type FindExitType :: (Type -> Type) -> Type
type family FindExitType rep where
  FindExitType (M1 D _ inner) = FindExitType inner
  FindExitType (M1 C _ inner) = FindExitType inner
  FindExitType (M1 S _ (K1 _ (_ :- Exit t))) = t
  FindExitType (M1 S _ (K1 _ (_ :- Exit t :@ _))) = t
  FindExitType (M1 S _ _) = TypeError ('Text "No Exit node found")
  FindExitType (left :*: right) = FindExitTypeInProduct (FindExitType left) right

type FindExitTypeInProduct :: Type -> (Type -> Type) -> Type
type family FindExitTypeInProduct leftResult right where
  FindExitTypeInProduct (TypeError _) right = FindExitType right
  FindExitTypeInProduct found _ = found
```

### 4. Import AsGraph from Generic.Core

```haskell
import Tidepool.Graph.Generic.Core (AsGraph)
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core

# Test with existing graph
cabal repl tidepool-core
> :kind! GetGraphEntry SimpleGraph
-- Should reduce to the Entry input type
> :kind! GetGraphExit SimpleGraph
-- Should reduce to the Exit output type
```

## Success Criteria

- [ ] `GetGraphEntry` extracts Entry input type from graph
- [ ] `GetGraphExit` extracts Exit output type from graph
- [ ] Works with graphs that have annotations on Entry/Exit
- [ ] Clear TypeError if Entry/Exit missing
