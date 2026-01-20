{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.Complete
  ( ExoCompleteGraph(..)
  , exoCompleteHandlers
  , exoCompleteLogic
  , ExoCompleteArgs(..)
  , ExoCompleteResult(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), getBead, closeBead)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (parseBeadId)

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
              -- Close the bead
              closeBead bid Nothing
              pure $ gotoExit ExoCompleteResult
                { ecrBeadId = bid
                , ecrStatus = StatusClosed
                , ecrMessage = "Successfully closed bead " <> bid
                }
