{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | MCP tool for optimal worktree assignment
module Tidepool.MCP.Tools.BdDispatch
  ( bdDispatchTool
  , BdDispatchInput(..)
  , BdDispatchOutput(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>), takeDirectory)

import Tidepool.Schema (TidepoolDefault(..))
import Tidepool.StructuredOutput () -- Import for generic instances
import Tidepool.StructuredOutput.Instances () -- Import for TidepoolDefault Aeson instances
import Tidepool.MCP.Types (McpTool)
import Tidepool.MCP.Server (makeMcpTool)
import Tidepool.BD.Interpreter (defaultBDConfig, bdListByStatus)
import Tidepool.BD.GitInterpreter (runGitIO) -- Need for detectWorktree
import Tidepool.Effects.Git (getWorktreeInfo, WorktreeInfo(..))
import Tidepool.Effects.BD (BeadInfo(..), BeadStatus(..), DependencyInfo(..))
import Control.Monad.Freer (runM)

-- | Input for bd_dispatch tool
newtype BdDispatchInput = BdDispatchInput
  { -- | Number of parallel agents/worktrees to prepare (default: 1, minimum: 1)
    agents :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Output for bd_dispatch tool
data BdDispatchOutput = BdDispatchOutput
  { commands :: [Text]
    -- ^ Shell commands to create worktrees
  , beads :: [Text]
    -- ^ IDs of beads assigned
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Tool definition for bd_dispatch
bdDispatchTool :: McpTool
bdDispatchTool = makeMcpTool
  (Proxy @(TidepoolDefault BdDispatchInput))
  "bd_dispatch"
  "Assign highest-priority unblocked beads to new git worktrees"
  (\(TidepoolDefault input) -> bdDispatchHandler input)

-- | Handler for bd_dispatch
--
-- Workflow:
-- 1. Validate inputs (must be in git repo, agents > 0)
-- 2. Query all open beads from BD
-- 3. Filter to unblocked beads (no open dependencies)
-- 4. Sort by priority descending (P0 first, P4 last)
-- 5. Generate git worktree add commands
--
-- Design decision: Uses defaultBDConfig instead of reading .bd/config.toml.
-- Rationale: Tool is invoked from Claude Code context, not user shell context.
-- User's BD config is in their environment, not necessarily visible to MCP process.
bdDispatchHandler :: BdDispatchInput -> IO BdDispatchOutput
bdDispatchHandler input = do
  -- Validate: agents parameter must be positive
  let n = max 1 (fromMaybe 1 input.agents)

  -- 1. Get repo root for worktree paths
  -- Fail explicitly if not in a git repository (no silent fallbacks)
  mWorktree <- runM $ runGitIO getWorktreeInfo
  (repoRoot, repoName) <- case mWorktree of
    Just wt -> pure (wt.wiRepoRoot, wt.wiName)
    Nothing -> fail "Not in a git repository - cannot determine worktree root"

  -- 2. Get unblocked open beads
  -- Note: Uses bd CLI via interpreter (subprocess-based)
  allOpen <- bdListByStatus defaultBDConfig StatusOpen
  let ready = filter isUnblocked allOpen

  -- 3. Sort by priority descending (P0=0 is highest, P4=4 is lowest)
  -- Down wrapper reverses sort order
  let sortedReady = sortOn (Down . (.biPriority)) ready

  -- 4. Take top N beads
  let assigned = take n sortedReady

  -- 5. Generate commands
  -- Path: <parent-of-repo>/<repo>-<beadId>
  -- Use takeDirectory instead of manual ".." to handle paths cleanly
  let parentDir = takeDirectory repoRoot

  let mkCommand bead =
        let path = parentDir </> (T.unpack repoName <> "-" <> T.unpack bead.biId)
            branch = bead.biId
        in "git worktree add " <> T.pack path <> " -b " <> branch

  pure $ BdDispatchOutput
    { commands = map mkCommand assigned
    , beads = map (.biId) assigned
    }

-- | Check if a bead is unblocked (no open dependencies)
isUnblocked :: BeadInfo -> Bool
isUnblocked bead =
  all depIsClosed bead.biDependencies
  where
    depIsClosed (DependencyInfo { diStatus = s }) = s == StatusClosed
