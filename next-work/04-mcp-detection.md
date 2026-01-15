# Work Item 04: MCPExport Detection Type Families

**Priority**: Medium
**Depends on**: 02 (MCPExport types)
**Parallelizable with**: 01, 03
**Blocks**: 05, 07

## Goal

Add type families to detect MCPExport annotations and extract ToolMeta from Entry nodes.

## Files to Modify

- `haskell/dsl/core/src/Tidepool/Graph/Edges.hs`

## Implementation

### 1. Add to exports

```haskell
    -- * MCP Export Detection
  , HasMCPExport
  , GetToolMeta
  , GetMCPEntries
```

### 2. Add imports for Types.hs additions

```haskell
import Tidepool.Graph.Types
  ( -- ...existing...
  , MCPExport
  , ToolMeta
  )
```

### 3. Add type families (new section)

```haskell
-- ════════════════════════════════════════════════════════════════════════════
-- MCP EXPORT DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if a node definition has MCPExport annotation.
--
-- @
-- HasMCPExport (Entry SearchInput :@ MCPExport) = 'True
-- HasMCPExport (Entry SearchInput) = 'False
-- @
type HasMCPExport :: Type -> Bool
type family HasMCPExport node where
  HasMCPExport (node :@ MCPExport) = 'True
  HasMCPExport (node :@ MCPExport :@ _) = 'True
  HasMCPExport (node :@ _ :@ MCPExport) = 'True
  HasMCPExport (node :@ _ :@ MCPExport :@ _) = 'True
  HasMCPExport (node :@ _ :@ _ :@ MCPExport) = 'True
  HasMCPExport (node :@ _) = HasMCPExport node
  HasMCPExport _ = 'False

-- | Extract ToolMeta annotation if present.
--
-- @
-- GetToolMeta (Entry X :@ MCPExport :@ ToolMeta '("name", "desc"))
--   = 'Just '("name", "desc")
-- @
type GetToolMeta :: Type -> Maybe (Symbol, Symbol)
type family GetToolMeta node where
  GetToolMeta (node :@ ToolMeta meta) = 'Just meta
  GetToolMeta (node :@ _ :@ ToolMeta meta) = 'Just meta
  GetToolMeta (node :@ _ :@ _ :@ ToolMeta meta) = 'Just meta
  GetToolMeta (node :@ _) = GetToolMeta node
  GetToolMeta _ = 'Nothing

-- | Collect all MCP-exported Entry field names from a graph.
--
-- Returns a type-level list of field names that have MCPExport.
-- Used by ReifyMCPTools to generate tool definitions.
type GetMCPEntries :: (Type -> Type) -> [(Symbol, Type)]
type family GetMCPEntries graph where
  GetMCPEntries g = CollectMCPEntries (Rep (g AsGraph))

-- | Walk Generic Rep collecting MCPExport entries.
type CollectMCPEntries :: (Type -> Type) -> [(Symbol, Type)]
type family CollectMCPEntries rep where
  CollectMCPEntries (M1 D _ inner) = CollectMCPEntries inner
  CollectMCPEntries (M1 C _ inner) = CollectMCPEntries inner
  -- Check if field has MCPExport
  CollectMCPEntries (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ (_ :- Entry t :@ rest))) =
    IfMCPExport (HasMCPExport (Entry t :@ rest)) name t
  CollectMCPEntries (M1 S _ _) = '[]
  CollectMCPEntries (left :*: right) =
    CollectMCPEntries left ++ CollectMCPEntries right
  CollectMCPEntries _ = '[]

-- | Conditional inclusion based on MCPExport presence.
type IfMCPExport :: Bool -> Symbol -> Type -> [(Symbol, Type)]
type family IfMCPExport hasMCP name inputType where
  IfMCPExport 'True name t = '[ '(name, t) ]
  IfMCPExport 'False _ _ = '[]

-- | Type-level list append.
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core

# Test detection
cabal repl tidepool-core
> :kind! HasMCPExport (Entry Int :@ MCPExport)
-- = 'True
> :kind! HasMCPExport (Entry Int)
-- = 'False
> :kind! GetToolMeta (Entry Int :@ MCPExport :@ ToolMeta '("test", "desc"))
-- = 'Just '("test", "desc")
```

## Success Criteria

- [ ] `HasMCPExport` correctly detects MCPExport at any annotation position
- [ ] `GetToolMeta` extracts ToolMeta tuple
- [ ] `GetMCPEntries` collects all MCP-exported entries from a graph
- [ ] Works with multiple annotation orderings
