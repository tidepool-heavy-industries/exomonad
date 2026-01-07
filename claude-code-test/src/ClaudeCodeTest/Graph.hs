{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Graph definition for the ClaudeCode test.
--
-- This graph demonstrates ClaudeCode integration with:
-- - Entry → explore (ClaudeCode) → route (Logic) → act (ClaudeCode) → Exit
--
-- Both explore and act nodes execute via Claude Code subprocess, not the LLM API.
module ClaudeCodeTest.Graph
  ( ClaudeCodeTestGraph(..)
  ) where

import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@)
  , Schema
  , Template
  , UsesEffects
  , Exit
  , ClaudeCode
  , ModelChoice(..)
  )
import qualified Tidepool.Graph.Types as Types (Input)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

import ClaudeCodeTest.Types (ExploreInput, Findings, ActionInput, ActionResult)
import ClaudeCodeTest.Templates (ExploreTpl, ActionTpl)


-- | ClaudeCode test graph with exploration and action phases.
--
-- Graph structure:
-- @
-- Entry(ExploreInput)
--   ↓
-- explore (ClaudeCode 'Sonnet) - explores files, returns Findings
--   ↓
-- route (LogicNode) - decides: files found? → act, else → exit
--   ↓
-- act (ClaudeCode 'Sonnet) - takes action, returns ActionResult
--   ↓
-- Exit(ActionResult)
-- @
--
-- Notes:
-- - Both LLM nodes use ClaudeCode annotation for subprocess execution
-- - 'Nothing for cwd means inherit working directory from runtime
-- - Templates define prompts, Schema defines structured output
data ClaudeCodeTestGraph mode = ClaudeCodeTestGraph
  { -- | Entry point - receives directory and objective
    entry :: mode :- G.Entry ExploreInput

    -- | Explore node - uses Claude Code to explore files in directory
    --
    -- ClaudeCode 'Sonnet 'Nothing means:
    -- - Use Sonnet model
    -- - CWD not specified at type level (passed dynamically)
  , explore :: mode :- G.LLMNode
      :@ Types.Input ExploreInput
      :@ Template ExploreTpl
      :@ Schema Findings
      :@ UsesEffects '[Goto "route" Findings]
      :@ ClaudeCode 'Sonnet 'Nothing

    -- | Route node - decides whether to take action or exit
    --
    -- If files were found, proceed to action.
    -- If nothing found, exit early with "nothing found" result.
  , route :: mode :- G.LogicNode
      :@ Types.Input Findings
      :@ UsesEffects '[Goto "act" ActionInput, Goto Exit ActionResult]

    -- | Action node - takes follow-up action based on findings
  , act :: mode :- G.LLMNode
      :@ Types.Input ActionInput
      :@ Template ActionTpl
      :@ Schema ActionResult
      :@ UsesEffects '[Goto Exit ActionResult]
      :@ ClaudeCode 'Sonnet 'Nothing

    -- | Exit point - returns the final action result
  , exit :: mode :- G.Exit ActionResult
  }
  deriving Generic
