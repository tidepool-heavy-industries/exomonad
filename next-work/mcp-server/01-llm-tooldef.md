# 01: ToolDef Annotation for LLMNode

## Goal

Allow LLMNode to carry MCP tool metadata via `ToolDef` annotation, reusing the same pattern as tool definitions.

## Current State

- `ToolDef` typeclass exists for defining tools (name, description, schema, execute)
- `ToolMeta` annotation exists (from MCPExport work) but was for graph entries
- LLMNode can have `Tools` annotation but not `ToolDef`

## Target State

```haskell
-- LLMNode annotated as MCP tool
myTool :: LLMNode
    :@ Input MyInput
    :@ Schema MyOutput
    :@ ToolDef "tool_name" "Tool description"

-- Extraction
type GetToolDef :: Type -> Maybe (Symbol, Symbol)
type family GetToolDef node where
  GetToolDef (node :@ ToolDef name desc) = 'Just '(name, desc)
  GetToolDef (node :@ _) = GetToolDef node
  GetToolDef _ = 'Nothing
```

## Files to Modify

### `haskell/dsl/core/src/Tidepool/Graph/Types.hs`

Ensure `ToolDef` annotation works on LLMNode (may already work since it's just a type-level tag).

### `haskell/dsl/core/src/Tidepool/Graph/Edges.hs`

Add extraction type family:

```haskell
-- | Extract ToolDef annotation from an LLMNode.
type GetToolDef :: Type -> Maybe (Symbol, Symbol)
type family GetToolDef node where
  GetToolDef (node :@ ToolDef name desc) = 'Just '(name, desc)
  GetToolDef (node :@ _) = GetToolDef node
  GetToolDef _ = 'Nothing

-- | Check if node has ToolDef.
type HasToolDef :: Type -> Bool
type family HasToolDef node where
  HasToolDef node = IsJust (GetToolDef node)
```

## Verification

```haskell
-- In test or REPL:
:kind! GetToolDef (LLMNode :@ Input Query :@ Schema Response :@ ToolDef "scout" "desc")
-- Should be: 'Just '("scout", "desc")

:kind! HasToolDef (LLMNode :@ Input Query :@ Schema Response)
-- Should be: 'False
```

## Notes

- Reuse existing `ToolDef` type if it exists, or rename `ToolMeta` to `ToolDef` for consistency
- Input type's JSON schema derivation already exists for tool defs - reuse it
