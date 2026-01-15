# Work Item 07: SemanticScoutGraph Golden Test

**Priority**: Medium (integration validation)
**Depends on**: 01, 02, 03, 04 (all core types)
**Parallelizable with**: None (integration)
**Blocks**: None (final validation)

## Goal

Create a golden test that validates the complete GraphNode + MCPExport design compiles correctly.

## Files to Create

- `haskell/dsl/core/test/golden/valid/SemanticScoutGraph.hs`

## Files to Modify

- `haskell/dsl/core/tidepool-core.cabal` (add to test modules)

## Implementation

### 1. Create SemanticScoutGraph.hs

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Golden test: SemanticScoutGraph with MCPExport and multiple entries.
--
-- Validates:
-- 1. MCPExport annotation on Entry nodes
-- 2. ToolMeta annotation for tool name/description
-- 3. Multiple Entry points in one graph
-- 4. LLMNode 'Local for FunctionGemma execution
-- 5. GetGraphEntry/GetGraphExit extraction
module SemanticScoutGraph where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@), Input, Schema, LLMKind(..)
  , MCPExport, ToolMeta
  )
import Tidepool.Graph.Generic
  ( GraphMode(..), Entry, Exit, LLMNode, ValidGraphRecord
  )
import Tidepool.Schema (HasJSONSchema)

-- ════════════════════════════════════════════════════════════════════════════
-- DOMAIN TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Input for map_influence command.
data MapInfluenceInput = MapInfluenceInput
  { miiSymbol :: Text     -- Symbol to analyze
  , miiFile :: FilePath   -- File containing symbol
  , miiDepth :: Int       -- Max recursion depth
  } deriving (Generic, ToJSON, FromJSON, HasJSONSchema)

-- | Input for find_dead_ends command.
data FindDeadEndsInput = FindDeadEndsInput
  { fdeiFile :: FilePath
  } deriving (Generic, ToJSON, FromJSON, HasJSONSchema)

-- | Input for summarize_file command.
data SummarizeFileInput = SummarizeFileInput
  { sfiFile :: FilePath
  } deriving (Generic, ToJSON, FromJSON, HasJSONSchema)

-- | Unified request to the Local LLM.
data UnifiedRequest = UnifiedRequest
  { urCommand :: Text
  , urContext :: Text
  , urTarget :: Text
  } deriving (Generic, ToJSON, FromJSON, HasJSONSchema)

-- | Compressed intelligence output.
data CompressedIntel = CompressedIntel
  { ciSummary :: Text
  , ciReferences :: [FilePath]
  , ciConfidence :: Double
  } deriving (Generic, ToJSON, FromJSON, HasJSONSchema)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Semantic Scout graph with multiple MCP-exported entry points.
--
-- This graph can be:
-- 1. Embedded in a parent graph via GraphNode
-- 2. Exposed as an MCP server via ReifyMCPTools
-- 3. Executed standalone via runGraph
data SemanticScoutGraph mode = SemanticScoutGraph
  { -- Multiple entry points, each MCP-exported
    mapInfluence :: mode :- Entry MapInfluenceInput
        :@ MCPExport
        :@ ToolMeta '("map_influence", "Find all types and functions influenced by changes to a symbol")

  , findDeadEnds :: mode :- Entry FindDeadEndsInput
        :@ MCPExport
        :@ ToolMeta '("find_dead_ends", "Find unreachable code paths and unused exports")

  , summarizeFile :: mode :- Entry SummarizeFileInput
        :@ MCPExport
        :@ ToolMeta '("summarize_file", "Generate compressed summary of file purpose and API")

    -- Shared processing via Local LLM (FunctionGemma)
  , process :: mode :- LLMNode 'Local
        :@ Input UnifiedRequest
        :@ Schema CompressedIntel

    -- Single exit
  , exit :: mode :- Exit CompressedIntel
  }
  deriving Generic

-- ════════════════════════════════════════════════════════════════════════════
-- VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile-time validation.
_validateGraph :: ValidGraphRecord SemanticScoutGraph => ()
_validateGraph = ()

-- | Verify entry type extraction (used by GraphNode).
-- type _EntryType = GetGraphEntry SemanticScoutGraph
-- Should equal MapInfluenceInput (first entry)

-- | Verify exit type extraction (used by GraphNode).
-- type _ExitType = GetGraphExit SemanticScoutGraph
-- Should equal CompressedIntel
```

### 2. Add to cabal test modules

In `tidepool-core.cabal`, add to `other-modules` in test-suite:

```
        SemanticScoutGraph
```

### 3. Create parent graph test (optional, if 06 complete)

```haskell
-- | Parent graph embedding SemanticScout via GraphNode.
data MainAgentGraph mode = MainAgentGraph
  { maEntry :: mode :- Entry UserQuery
  , maScout :: mode :- GraphNode SemanticScoutGraph
      :@ Input SemanticOrder  -- Transformed before child entry
  , maProcess :: mode :- LLMNode 'API
      :@ Input ProcessInput
      :@ Schema AgentResponse
  , maExit :: mode :- Exit AgentResponse
  }
  deriving Generic
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core

# Run tests (should compile without errors)
cabal test tidepool-core:test:graph-tests

# Verify MCP reification
cabal repl tidepool-core
> import SemanticScoutGraph
> import Tidepool.Graph.MCPReify
> reifyMCPTools (Proxy @SemanticScoutGraph)
-- Should list 3 tools: map_influence, find_dead_ends, summarize_file
```

## Success Criteria

- [ ] `SemanticScoutGraph` compiles with all annotations
- [ ] Multiple Entry nodes with MCPExport work
- [ ] ToolMeta provides custom names and descriptions
- [ ] `LLMNode 'Local` compiles (FunctionGemma placeholder)
- [ ] `ValidGraphRecord` constraint satisfied
- [ ] MCP tools can be reified at runtime
