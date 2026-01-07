{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the ClaudeCode test graph.
--
-- Each node in the graph has a corresponding handler here.
-- The explore and act nodes use ClaudeCodeLLMHandler for subprocess execution.
module ClaudeCodeTest.Handlers
  ( testHandlers
  ) where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T

import Control.Monad.Freer (Eff)

import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (GotoChoice, To, ClaudeCodeLLMHandler(..), ClaudeCodeResult(..), gotoChoice, gotoExit)
import Tidepool.Graph.Template (templateCompiled)
import Tidepool.Graph.Types (ModelChoice(..), Exit)

import ClaudeCodeTest.Context (ExploreContext(..), ActionContext(..))
import ClaudeCodeTest.Graph (ClaudeCodeTestGraph(..))
import ClaudeCodeTest.Types (ExploreInput(..), Findings(..), ActionInput(..), ActionResult(..))
import ClaudeCodeTest.Templates (ExploreTpl, ActionTpl)


-- | Handlers for ClaudeCodeTestGraph.
--
-- Effect stack: ClaudeCodeExec (for subprocess execution) + IO
--
-- - explore: ClaudeCodeLLMHandler that explores files
-- - route: Logic handler that decides next step based on findings
-- - act: ClaudeCodeLLMHandler that takes follow-up action
testHandlers :: ClaudeCodeTestGraph (AsHandler '[ClaudeCodeExec, IO])
testHandlers = ClaudeCodeTestGraph
  { entry = Proxy @ExploreInput

    -- Explore handler: Uses Claude Code to explore files in directory
  , explore = ClaudeCodeLLMHandler @'Sonnet @'Nothing
      Nothing                                  -- no system template
      (templateCompiled @ExploreTpl)           -- user template
      buildExploreContext                      -- before: builds context
      routeAfterExplore                        -- after: routes to 'route' node

    -- Route handler: Decides whether to proceed with action or exit early
  , route = routeHandler

    -- Action handler: Uses Claude Code to take follow-up action
  , act = ClaudeCodeLLMHandler @'Sonnet @'Nothing
      Nothing                                  -- no system template
      (templateCompiled @ActionTpl)            -- user template
      buildActionContext                       -- before: builds context
      routeAfterAction                         -- after: routes to exit

  , exit = Proxy @ActionResult
  }


-- ============================================================================
-- Explore Node Helpers
-- ============================================================================

-- | Build context for the explore template.
buildExploreContext
  :: ExploreInput
  -> Eff '[ClaudeCodeExec, IO] ExploreContext
buildExploreContext input = pure ExploreContext
  { directory = T.pack input.eiDirectory
  , objective = input.eiObjective
  }

-- | Route after exploration.
--
-- Always continues to the route node with findings.
routeAfterExplore
  :: ClaudeCodeResult Findings
  -> Eff '[ClaudeCodeExec, IO] (GotoChoice '[To "route" Findings])
routeAfterExplore result = pure $ gotoChoice @"route" result.ccrParsedOutput


-- ============================================================================
-- Route Node Handler
-- ============================================================================

-- | Route handler decides next step based on exploration findings.
--
-- If files were found, proceed to action.
-- Otherwise, exit with "nothing found" result.
routeHandler
  :: Findings
  -> Eff '[ClaudeCodeExec, IO] (GotoChoice '[To "act" ActionInput, To Exit ActionResult])
routeHandler findings
  | findings.fFileCount > 0 = pure $ gotoChoice @"act" ActionInput
      { aiFindings = findings
      , aiAction = findings.fRecommendation
      }
  | otherwise = pure $ gotoExit ActionResult
      { arSuccess = False
      , arDescription = "No files found in directory"
      , arArtifact = Nothing
      }


-- ============================================================================
-- Action Node Helpers
-- ============================================================================

-- | Build context for the action template.
buildActionContext
  :: ActionInput
  -> Eff '[ClaudeCodeExec, IO] ActionContext
buildActionContext input = pure ActionContext
  { summary = input.aiFindings.fSummary
  , fileCount = input.aiFindings.fFileCount
  , files = input.aiFindings.fFiles
  , recommendation = input.aiFindings.fRecommendation
  , action = input.aiAction
  }

-- | Route after action.
--
-- Always exits with the action result.
routeAfterAction
  :: ClaudeCodeResult ActionResult
  -> Eff '[ClaudeCodeExec, IO] (GotoChoice '[To Exit ActionResult])
routeAfterAction result = pure $ gotoExit result.ccrParsedOutput


