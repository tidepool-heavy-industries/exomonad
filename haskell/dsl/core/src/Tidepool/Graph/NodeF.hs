{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Functor for Free-based graph composition.
--
-- This module defines NodeF, the base functor for composing graphs using Free monads.
-- Simple single-logic-node graphs use Identity directly; complex multi-node graphs
-- use Free NodeF for monadic composition.
--
-- = Design
--
-- NodeF represents the algebra of graph nodes:
-- - EntryF: Entry point with input type
-- - LogicNodeF: Pure/effect computation with goto targets
-- - LLMNodeF: LLM call with template and schema
-- - ExitF: Terminal node with output type
--
-- The 'next' parameter enables Functor instance for Free composition.
module Tidepool.Graph.NodeF
  ( NodeF(..)
  ) where

import Data.Text (Text)
import Data.Typeable (TypeRep)
import GHC.Generics (Generic)

-- | Base functor for graph node composition.
--
-- Each constructor represents a node kind with its type-level metadata:
--
-- @
-- EntryF typeRep next       -- Entry point
-- LogicNodeF input targets next  -- Logic with goto targets
-- LLMNodeF input tpl schema next -- LLM call
-- ExitF typeRep             -- Terminal (no continuation)
-- @
--
-- The Functor instance enables Free monad composition:
--
-- @
-- type GraphF = Free NodeF
--
-- myGraph :: GraphF ()
-- myGraph = do
--   liftF $ EntryF (typeRep @Message) ()
--   liftF $ LogicNodeF (typeRep @Message) ["nextNode"] ()
--   liftF $ ExitF (typeRep @Response)
-- @
data NodeF next
  = EntryF TypeRep next
    -- ^ Entry point with input type token.
    --
    -- The TypeRep carries the input type for type-safe dispatch.

  | LogicNodeF
      { logicInput :: TypeRep
        -- ^ Input type for this logic node
      , logicTargets :: [Text]
        -- ^ List of goto target names (field names in graph record)
      , logicNext :: next
        -- ^ Continuation
      }
    -- ^ Logic node with typed input and explicit goto targets.
    --
    -- Targets are validated at compile time to ensure all referenced
    -- field names exist in the graph record.

  | LLMNodeF
      { llmInput :: TypeRep
        -- ^ Input type for this LLM node
      , llmTemplate :: TypeRep
        -- ^ Template type (TemplateDef instance)
      , llmSchema :: TypeRep
        -- ^ Output schema type (HasJSONSchema instance)
      , llmNext :: next
        -- ^ Continuation
      }
    -- ^ LLM node with template rendering and structured output.
    --
    -- Template context is built from input via handler's 'before' function.
    -- Schema output flows implicitly to downstream nodes.

  | ExitF TypeRep
    -- ^ Exit node with output type token.
    --
    -- Terminal node - no continuation. The TypeRep carries the final
    -- result type for the graph.

  deriving (Functor, Generic)
