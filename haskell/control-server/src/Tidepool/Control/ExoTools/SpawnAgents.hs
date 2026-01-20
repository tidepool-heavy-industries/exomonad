{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.SpawnAgents
  ( SpawnAgentsGraph(..)
  , spawnAgentsHandlers
  , spawnAgentsLogic
  , SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>))

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), getBead)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.Worktree (Worktree, WorktreeSpec(..), WorktreePath(..), createWorktree)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (slugify)

-- | Arguments for spawn_agents tool.
data SpawnAgentsArgs = SpawnAgentsArgs
  { saaBeadIds :: [Text]  -- ^ List of short form bead IDs (e.g. "wzi", "1b2").
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema SpawnAgentsArgs where
  jsonSchema = objectSchema
    [ ("bead_ids", describeField "bead_ids" "List of short-form bead IDs to spawn worktrees for." (arraySchema (emptySchema TString)))
    ]
    ["bead_ids"]

instance FromJSON SpawnAgentsArgs where
  parseJSON = withObject "SpawnAgentsArgs" $ \v ->
    SpawnAgentsArgs <$> v .: "bead_ids"

instance ToJSON SpawnAgentsArgs where
  toJSON args = object ["bead_ids" .= saaBeadIds args]

-- | Result of spawn_agents tool.
data SpawnAgentsResult = SpawnAgentsResult
  { sarWorktrees :: [(Text, FilePath)]
  , sarBootstrap :: [Text]
  , sarFailed    :: [(Text, Text)]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SpawnAgentsResult where
  toJSON res = object
    [ "worktrees" .= sarWorktrees res
    , "bootstrap" .= sarBootstrap res
    , "failed"     .= sarFailed res
    ]

-- | Graph definition for spawn_agents tool.
data SpawnAgentsGraph mode = SpawnAgentsGraph
  { saEntry :: mode :- EntryNode SpawnAgentsArgs
      :@ MCPExport
      :@ MCPToolDef '("spawn_agents", "Create worktrees and branches for parallel agent dispatch.")

  , saRun :: mode :- LogicNode
      :@ Input SpawnAgentsArgs
      :@ UsesEffects '[BD, Git, Worktree, Goto Exit SpawnAgentsResult]

  , saExit :: mode :- ExitNode SpawnAgentsResult
  }
  deriving Generic

-- | Handlers for spawn_agents graph.
spawnAgentsHandlers
  :: (Member BD es, Member Git es, Member Worktree es)
  => SpawnAgentsGraph (AsHandler es)
spawnAgentsHandlers = SpawnAgentsGraph
  { saEntry = ()
  , saRun = spawnAgentsLogic
  , saExit = ()
  }

-- | Core logic for spawn_agents.
spawnAgentsLogic
  :: (Member BD es, Member Git es, Member Worktree es)
  => SpawnAgentsArgs
  -> Eff es (GotoChoice '[To Exit SpawnAgentsResult])
spawnAgentsLogic args = do
  mWtInfo <- getWorktreeInfo
  let repoRoot = maybe "." (\wi -> wi.wiRepoRoot) mWtInfo

  results <- forM args.saaBeadIds $ \shortId -> do
    -- Normalize bead ID (ensure it starts with tidepool-)
    let fullId = if "tidepool-" `T.isPrefixOf` shortId
                 then shortId
                 else "tidepool-" <> shortId
    
    mBead <- getBead fullId
    case mBead of
      Nothing -> pure $ Left (shortId, "Bead not found: " <> fullId)
      Just bead -> do
        if bead.biStatus == StatusBlocked
          then pure $ Left (shortId, "Bead is blocked: " <> fullId)
          else do
            let slug = slugify bead.biTitle
                branchName = "bd-" <> shortId <> "/" <> slug
                -- Resolve absolute path based on repo root
                targetPath = repoRoot </> ".worktrees" </> "tidepool" </> "bd-" <> T.unpack shortId <> "-" <> T.unpack slug
                spec = WorktreeSpec
                  { wsBaseName = "bd-" <> shortId
                  , wsFromBranch = Just "origin/main"
                  , wsBranchName = Just branchName
                  , wsPath = Just targetPath
                  }
            
            res <- createWorktree spec
            case res of
              Left err -> pure $ Left (shortId, T.pack (show err))
              Right (WorktreePath path) -> pure $ Right (shortId, path)

  let (failed, succeeded) = partitionEithers results
      worktrees = succeeded
      bootstrap = map (\(_, path) -> "cd " <> T.pack path <> " && ./scripts/bead-context") succeeded

  pure $ gotoExit $ SpawnAgentsResult
    { sarWorktrees = worktrees
    , sarBootstrap = bootstrap
    , sarFailed    = failed
    }
