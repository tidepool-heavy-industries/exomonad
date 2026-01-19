{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | External Orchestration tools (Tier 1) as Graph DSL nodes.
--
-- Includes tools for interacting with beads (BD), git, and GitHub.
module Tidepool.Control.ExoTools
  ( -- * Exo Status
    ExoStatusGraph(..)
  , exoStatusHandlers
  , exoStatusLogic
  , ExoStatusArgs(..)
  , ExoStatusResult(..)

    -- * Exo Complete
  , ExoCompleteGraph(..)
  , exoCompleteHandlers
  , exoCompleteLogic
  , ExoCompleteArgs(..)
  , ExoCompleteResult(..)

    -- * Helpers
  , parseBeadId
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), getBead, closeBead)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles)
import Tidepool.Effects.GitHub (GitHub, PullRequest(..), listPullRequests, PRFilter(..), Repo(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

-- ════════════════════════════════════════════════════════════════════════════
-- EXO-STATUS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for exo_status tool.
data ExoStatusArgs = ExoStatusArgs
  { esaBeadId :: Maybe Text  -- ^ Optional bead ID. If not provided, inferred from branch.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ExoStatusArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    ]
    []

instance FromJSON ExoStatusArgs where
  parseJSON = withObject "ExoStatusArgs" $ \v ->
    ExoStatusArgs <$> v .:? "bead_id"

instance ToJSON ExoStatusArgs where
  toJSON args = object ["bead_id" .= esaBeadId args]

-- | Result of exo_status tool.
data ExoStatusResult = ExoStatusResult
  { esrBead :: Maybe BeadInfo
  , esrWorktree :: Maybe WorktreeInfo
  , esrDirtyFiles :: [FilePath]
  , esrPR :: Maybe PullRequest
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExoStatusResult where
  toJSON res = object
    [ "bead" .= esrBead res
    , "worktree" .= esrWorktree res
    , "dirty_files" .= esrDirtyFiles res
    , "pr" .= esrPR res
    ]

-- | Graph definition for exo_status tool.
data ExoStatusGraph mode = ExoStatusGraph
  { esEntry :: mode :- EntryNode ExoStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("exo_status", "Get current development context: bead details, git status, and PR info.")

  , esRun :: mode :- LogicNode
      :@ Input ExoStatusArgs
      :@ UsesEffects '[BD, Git, GitHub, Goto Exit ExoStatusResult]

  , esExit :: mode :- ExitNode ExoStatusResult
  }
  deriving Generic

-- | Handlers for exo_status graph.
exoStatusHandlers
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoStatusGraph (AsHandler es)
exoStatusHandlers = ExoStatusGraph
  { esEntry = ()
  , esRun = exoStatusLogic
  , esExit = ()
  }

-- | Core logic for exo_status.
exoStatusLogic
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoStatusArgs
  -> Eff es (GotoChoice '[To Exit ExoStatusResult])
exoStatusLogic args = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  -- 2. Determine Bead ID
  let branchBeadId = case mWt of
        Just wt -> parseBeadId wt.wiBranch
        Nothing -> Nothing
      targetBeadId = esaBeadId args <|> branchBeadId

  -- 3. Get Bead Info
  mBead <- case targetBeadId of
    Just bid -> getBead bid
    Nothing -> pure Nothing

  -- 4. Get PR Info
  -- We look for a PR where the head branch matches the current branch
  mPR <- case mWt of
    Just wt -> do
      -- Default to upstream repo
      let repo = Repo "tidepool-heavy-industries/tidepool"
      prs <- listPullRequests repo (PRFilter Nothing (Just "main") (Just 100))
      -- Filter PRs by head branch
      pure $ find (\pr -> pr.prHeadRefName == wt.wiBranch) prs
    Nothing -> pure Nothing

  pure $ gotoExit ExoStatusResult
    { esrBead = mBead
    , esrWorktree = mWt
    , esrDirtyFiles = dirtyFiles
    , esrPR = mPR
    }

-- ════════════════════════════════════════════════════════════════════════════
-- EXO-COMPLETE GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for exo_complete tool.
data ExoCompleteArgs = ExoCompleteArgs
  { ecaBeadId :: Maybe Text  -- ^ Optional bead ID. If not provided, inferred from branch.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ExoCompleteArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    ]
    []

instance FromJSON ExoCompleteArgs where
  parseJSON = withObject "ExoCompleteArgs" $ \v ->
    ExoCompleteArgs <$> v .:? "bead_id"

instance ToJSON ExoCompleteArgs where
  toJSON args = object
    [ "bead_id" .= ecaBeadId args
    ]

-- | Result of exo_complete tool.
data ExoCompleteResult = ExoCompleteResult
  { ecrBeadId :: Text
  , ecrStatus :: BeadStatus
  , ecrMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExoCompleteResult where
  toJSON res = object
    [ "bead_id" .= ecrBeadId res
    , "status" .= ecrStatus res
    , "message" .= ecrMessage res
    ]

-- | Graph definition for exo_complete tool.
data ExoCompleteGraph mode = ExoCompleteGraph
  { ecEntry :: mode :- EntryNode ExoCompleteArgs
      :@ MCPExport
      :@ MCPToolDef '("exo_complete", "Complete work on a bead: close the bead and verify git state.")

  , ecRun :: mode :- LogicNode
      :@ Input ExoCompleteArgs
      :@ UsesEffects '[BD, Git, Goto Exit ExoCompleteResult]

  , ecExit :: mode :- ExitNode ExoCompleteResult
  }
  deriving Generic

-- | Handlers for exo_complete graph.
exoCompleteHandlers
  :: (Member BD es, Member Git es)
  => ExoCompleteGraph (AsHandler es)
exoCompleteHandlers = ExoCompleteGraph
  { ecEntry = ()
  , ecRun = exoCompleteLogic
  , ecExit = ()
  }

-- | Core logic for exo_complete.
exoCompleteLogic
  :: (Member BD es, Member Git es)
  => ExoCompleteArgs
  -> Eff es (GotoChoice '[To Exit ExoCompleteResult])
exoCompleteLogic args = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  -- 2. Determine Bead ID
  let branchBeadId = case mWt of
        Just wt -> parseBeadId wt.wiBranch
        Nothing -> Nothing
      mTargetBeadId = ecaBeadId args <|> branchBeadId

  case mTargetBeadId of
    Nothing ->
      pure $ gotoExit ExoCompleteResult
        { ecrBeadId = ""
        , ecrStatus = StatusOpen
        , ecrMessage = "Could not determine bead ID. Please provide bead_id argument."
        }
    Just bid -> do
      -- 3. Get Bead Info to verify existence
      mBead <- getBead bid
      case mBead of
        Nothing ->
          pure $ gotoExit ExoCompleteResult
            { ecrBeadId = bid
            , ecrStatus = StatusOpen
            , ecrMessage = "Bead " <> bid <> " not found."
            }
        Just bead | bead.biStatus == StatusClosed ->
          pure $ gotoExit ExoCompleteResult
            { ecrBeadId = bid
            , ecrStatus = StatusClosed
            , ecrMessage = "Bead " <> bid <> " is already closed."
            }
        Just bead -> do
          -- 4. Check for dirty files
          if not (null dirtyFiles)
            then pure $ gotoExit ExoCompleteResult
              { ecrBeadId = bid
              , ecrStatus = bead.biStatus
              , ecrMessage = "Warning: You have uncommitted changes. Please commit before completing: " <> T.pack (show dirtyFiles)
              }
            else do
              -- 5. Close the bead
              closeBead bid
              pure $ gotoExit ExoCompleteResult
                { ecrBeadId = bid
                , ecrStatus = StatusClosed
                , ecrMessage = "Bead " <> bid <> " has been closed. Don't forget to push and create a PR if you haven't already!"
                }

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse bead ID from branch name (bd-{id}/* convention)
parseBeadId :: Text -> Maybe Text
parseBeadId branch =
  if "bd-" `T.isPrefixOf` branch
  then
    let content = T.drop 3 branch
        (beadId, rest) = T.break (== '/') content
    in if T.null beadId || T.null (T.drop 1 rest) -- Must have / and something after
       then Nothing
       else Just $ "tidepool-" <> beadId
  else Nothing
