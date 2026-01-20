{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | MCP tool for optimal worktree assignment
module Tidepool.MCP.Tools.BdDispatch
  ( bdDispatchTool
  , BdDispatchInput(..)
  , BdDispatchOutput(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>))

import Tidepool.Schema (HasJSONSchema(..), TidepoolDefault(..))
import Tidepool.StructuredOutput () -- Import for generic instances
import Tidepool.StructuredOutput.Instances () -- Import for TidepoolDefault Aeson instances
import Tidepool.MCP.Types (McpTool)
import Tidepool.MCP.Server (makeMcpTool)
import Tidepool.BD.Interpreter (BDConfig(..), defaultBDConfig, bdListByStatus)
import Tidepool.BD.GitInterpreter (runGitIO) -- Need for detectWorktree
import Tidepool.Effects.Git (getWorktreeInfo, WorktreeInfo(..))
import Tidepool.Effects.BD (BeadInfo(..), BeadStatus(..), DependencyInfo(..))
import Control.Monad.Freer (runM)

-- | Input for bd_dispatch tool
data BdDispatchInput = BdDispatchInput
  { -- | Number of parallel agents/worktrees to prepare (default: 1)
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
bdDispatchHandler :: BdDispatchInput -> IO BdDispatchOutput
bdDispatchHandler input = do
  let n = fromMaybe 1 input.agents
  
  -- 1. Get repo root for worktree paths
  mWorktree <- runM $ runGitIO getWorktreeInfo
  let repoRoot = case mWorktree of
        Just wt -> wt.wiRepoRoot
        Nothing -> "." -- Fallback to current dir

  -- 2. Get unblocked open beads
  -- We use bd CLI directly via the interpreter's low-level functions
  allOpen <- bdListByStatus defaultBDConfig StatusOpen
  let ready = filter isUnblocked allOpen
  let sortedReady = sortOn (.biPriority) ready
  
  -- 3. Take top N beads
  let assigned = take n sortedReady
  
  -- 4. Generate commands
  -- Format: git worktree add ../<repo>-<beadId> -b <beadId>
  let repoName = case mWorktree of
        Just wt -> wt.wiName
        Nothing -> "tidepool"
        
  let mkCommand bead = 
        let path = repoRoot </> ".." </> (T.unpack repoName <> "-" <> T.unpack bead.biId)
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
