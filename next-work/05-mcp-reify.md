# Work Item 05: MCP Tool Reification

**Priority**: Medium
**Depends on**: 02 (MCPExport types), 04 (detection families)
**Parallelizable with**: 03, 06
**Blocks**: 07 (partial)

## Goal

Create `MCPReify.hs` module for runtime reification of MCP tool definitions from graph types. Similar to existing `DecisionTools` pattern.

## Files to Create

- `haskell/dsl/core/src/Tidepool/Graph/MCPReify.hs` (new)

## Files to Modify

- `haskell/dsl/core/tidepool-core.cabal` (add module)

## Implementation

### 1. Create MCPReify.hs

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Runtime reification of MCP tools from graph Entry points.
--
-- Graphs with MCPExport-annotated entries can be exposed as MCP servers.
-- This module provides the machinery to extract tool definitions at runtime.
--
-- @
-- tools <- reifyMCPTools (Proxy \@SemanticScoutGraph)
-- -- [MCPToolDef "map_influence" "Find all types..." {...}, ...]
-- @
module Tidepool.Graph.MCPReify
  ( -- * Tool Definition
    MCPToolDef(..)

    -- * Reification
  , ReifyMCPTools(..)
  , GReifyMCPEntries(..)
  ) where

import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import Tidepool.Schema (HasJSONSchema(..), schemaToValue)
import Tidepool.Graph.Types (type (:@), MCPExport, ToolMeta)
import Tidepool.Graph.Generic.Core (AsGraph, Entry, type (:-))
import Tidepool.Graph.Edges (HasMCPExport, GetToolMeta)

-- | An MCP tool definition extracted from a graph Entry.
data MCPToolDef = MCPToolDef
  { mtdName :: Text
    -- ^ Tool name (snake_case, from ToolMeta or field name)
  , mtdDescription :: Text
    -- ^ Tool description (from ToolMeta)
  , mtdInputSchema :: Value
    -- ^ JSON Schema for tool parameters (from HasJSONSchema)
  , mtdEntryName :: Text
    -- ^ Original field name in graph record (for dispatch)
  } deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- REIFICATION TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Types that can have their MCP tools reified at runtime.
--
-- @
-- tools <- reifyMCPTools (Proxy \@MyGraph)
-- @
class ReifyMCPTools (graph :: Type -> Type) where
  reifyMCPTools :: Proxy graph -> [MCPToolDef]

instance (Generic (graph AsGraph), GReifyMCPEntries (Rep (graph AsGraph)))
      => ReifyMCPTools graph where
  reifyMCPTools _ = gReifyMCPEntries (Proxy @(Rep (graph AsGraph)))

-- ════════════════════════════════════════════════════════════════════════════
-- GENERIC TRAVERSAL
-- ════════════════════════════════════════════════════════════════════════════

-- | Walk Generic Rep collecting MCPToolDefs.
class GReifyMCPEntries (rep :: Type -> Type) where
  gReifyMCPEntries :: Proxy rep -> [MCPToolDef]

-- Unwrap metadata
instance GReifyMCPEntries inner => GReifyMCPEntries (M1 D meta inner) where
  gReifyMCPEntries _ = gReifyMCPEntries (Proxy @inner)

instance GReifyMCPEntries inner => GReifyMCPEntries (M1 C meta inner) where
  gReifyMCPEntries _ = gReifyMCPEntries (Proxy @inner)

-- Product: combine both sides
instance (GReifyMCPEntries left, GReifyMCPEntries right)
      => GReifyMCPEntries (left :*: right) where
  gReifyMCPEntries _ =
    gReifyMCPEntries (Proxy @left) ++ gReifyMCPEntries (Proxy @right)

-- Empty: no fields
instance GReifyMCPEntries U1 where
  gReifyMCPEntries _ = []

-- Field: check for MCPExport
instance ( KnownSymbol name
         , ReifyMCPField (HasMCPExport fieldType) name fieldType
         )
      => GReifyMCPEntries (M1 S ('MetaSel ('Just name) su ss ds) (K1 i fieldType)) where
  gReifyMCPEntries _ = reifyMCPField @(HasMCPExport fieldType) @name @fieldType Proxy

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Conditionally reify a field based on MCPExport presence.
class ReifyMCPField (hasMCP :: Bool) (name :: Symbol) (fieldType :: Type) where
  reifyMCPField :: Proxy fieldType -> [MCPToolDef]

-- Not MCP-exported: skip
instance ReifyMCPField 'False name fieldType where
  reifyMCPField _ = []

-- MCP-exported: extract tool definition
instance ( KnownSymbol name
         , ExtractEntryInput fieldType
         , HasJSONSchema (EntryInputType fieldType)
         , ExtractToolMeta fieldType
         )
      => ReifyMCPField 'True name fieldType where
  reifyMCPField _ = [MCPToolDef
    { mtdName = getToolName @fieldType @name
    , mtdDescription = getToolDescription @fieldType
    , mtdInputSchema = schemaToValue (jsonSchema @(EntryInputType fieldType))
    , mtdEntryName = T.pack (symbolVal (Proxy @name))
    }]

-- ════════════════════════════════════════════════════════════════════════════
-- HELPER TYPE FAMILIES AND CLASSES
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract the input type from an Entry node definition.
type family EntryInputType (node :: Type) :: Type where
  EntryInputType (_ :- Entry t) = t
  EntryInputType (_ :- Entry t :@ _) = t
  EntryInputType (node :@ _) = EntryInputType node

class ExtractEntryInput (node :: Type) where
  type EntryInput node :: Type

class ExtractToolMeta (node :: Type) where
  getToolName :: forall name. KnownSymbol name => Text
  getToolDescription :: Text

-- Default: use field name as tool name
instance {-# OVERLAPPABLE #-} ExtractToolMeta node where
  getToolName @name = T.pack (camelToSnake (symbolVal (Proxy @name)))
  getToolDescription = "No description provided"

-- With ToolMeta: use provided name/description
instance (KnownSymbol toolName, KnownSymbol toolDesc)
      => ExtractToolMeta (node :@ ToolMeta '(toolName, toolDesc)) where
  getToolName = T.pack (symbolVal (Proxy @toolName))
  getToolDescription = T.pack (symbolVal (Proxy @toolDesc))

-- | Convert camelCase to snake_case.
camelToSnake :: String -> String
camelToSnake = concatMap go
  where
    go c
      | c >= 'A' && c <= 'Z' = ['_', toLower c]
      | otherwise = [c]
    toLower c = toEnum (fromEnum c + 32)
```

### 2. Add to cabal file

In `tidepool-core.cabal`, add to `exposed-modules`:

```
    Tidepool.Graph.MCPReify
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core

# Test reification (after golden test exists)
cabal repl tidepool-core
> import Tidepool.Graph.MCPReify
> import Data.Proxy
> -- reifyMCPTools (Proxy @SemanticScoutGraph)
```

## Success Criteria

- [ ] `MCPToolDef` data type with all fields
- [ ] `ReifyMCPTools` typeclass compiles
- [ ] Generic traversal extracts MCPExport entries
- [ ] ToolMeta name/description extracted correctly
- [ ] JSON Schema generated from HasJSONSchema
