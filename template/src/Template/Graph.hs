{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Minimal graph definition demonstrating Entry -> LLM -> Logic -> Exit pattern.
--
-- This template uses the record-based Graph DSL from tidepool-core.
-- Modify this file to define your agent's state machine.
module Template.Graph
  ( -- * Graph Definition
    SimpleGraph(..)

    -- * Types (re-exported from Template.Types)
  , Input(..)
  , Output(..)
  , Result(..)

    -- * Schemas
  , inputSchema
  , outputSchema
  , resultSchema
  ) where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, Schema, Template, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)
import Tidepool.Graph.Reify (ReifyRecordGraph(..), makeGraphInfo)
import Tidepool.Schema (JSONSchema, deriveJSONSchema)

import Template.Templates (ProcessTpl)
import Template.Types (Input(..), Output(..), Result(..))

-- | JSON Schema for the Input type (what enters the graph).
inputSchema :: JSONSchema
inputSchema = $(deriveJSONSchema ''Input)

-- | JSON Schema for the Output type (used by LLM structured output).
outputSchema :: JSONSchema
outputSchema = $(deriveJSONSchema ''Output)

-- | JSON Schema for the Result type (what exits the graph).
resultSchema :: JSONSchema
resultSchema = $(deriveJSONSchema ''Result)

-- | A minimal graph: Entry -> process (LLM) -> route (Logic) -> Exit
--
-- This demonstrates the core pattern with chat history integration.
-- See tidepool-core/src/Tidepool/Graph/CLAUDE.md for DSL documentation.
--
-- Pattern:
-- - LLM node uses Template to render prompt, produces structured output (Schema)
-- - Logic node routes based on that output (UsesEffects with Goto)
data SimpleGraph mode = SimpleGraph
  { sgEntry   :: mode :- G.Entry Input
  , sgProcess :: mode :- G.LLMNode
      :@ Needs '[Input]
      :@ Template ProcessTpl
      :@ Schema Output
  , sgRoute   :: mode :- G.LogicNode :@ Needs '[Output] :@ UsesEffects '[Goto Exit Result]
  , sgExit    :: mode :- G.Exit Result
  }
  deriving Generic

-- | Enable Mermaid diagram generation from the graph type.
instance ReifyRecordGraph SimpleGraph where
  reifyRecordGraph = makeGraphInfo
