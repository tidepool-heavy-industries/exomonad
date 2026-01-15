# Work Item 02: MCPExport and ToolMeta Types

**Priority**: High (foundation)
**Parallelizable with**: 01, 04
**Blocks**: 05, 07

## Goal

Add `MCPExport` marker and `ToolMeta` annotation to Types.hs for marking Entry points as MCP-exposed tools.

## Files to Modify

- `haskell/dsl/core/src/Tidepool/Graph/Types.hs`

## Implementation

### 1. Add to exports (after line ~68, near FunctionGemma)

```haskell
    -- * MCP Export Annotations
  , MCPExport
  , ToolMeta
```

### 2. Extend NodeKind (line ~94)

```haskell
data NodeKind
  = LLM    -- ^ Node that invokes the LLM
  | Logic  -- ^ Node with effect stack
  | Graph  -- ^ Nested graph execution
```

### 3. Add MCPExport marker (new section after FunctionGemma ~line 400)

```haskell
-- ════════════════════════════════════════════════════════════════════════════
-- MCP EXPORT ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Mark an Entry point for MCP server exposure.
--
-- When a graph Entry has MCPExport, it becomes an MCP tool that external
-- clients can invoke. The input type becomes the tool's parameter schema.
--
-- @
-- data MyGraph mode = MyGraph
--   { search :: mode :- Entry SearchInput :@ MCPExport
--       :@ ToolMeta '("search", "Search the codebase")
--   }
-- @
--
-- Use with 'ToolMeta' to provide tool name and description.
data MCPExport

-- | Provide name and description for an MCP-exported Entry.
--
-- @
-- :@ ToolMeta '("tool_name", "Tool description for LLM")
-- @
--
-- The name should be snake_case. The description appears in the MCP
-- tool listing and helps the LLM understand when to use the tool.
type ToolMeta :: (Symbol, Symbol) -> Type
data ToolMeta nameAndDesc
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core

# Verify exports
cabal repl tidepool-core
> :info MCPExport
> :kind ToolMeta
-- ToolMeta :: (Symbol, Symbol) -> Type
```

## Success Criteria

- [ ] `MCPExport` data type compiles
- [ ] `ToolMeta` has kind `(Symbol, Symbol) -> Type`
- [ ] `NodeKind` extended with `Graph` constructor
- [ ] Both exported from Types module
