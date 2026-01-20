{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.FilePR
  ( FilePRGraph(..)
  , filePRHandlers
  , filePRLogic
  , FilePRArgs(..)
  , FilePRResult(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD, BeadInfo(..), getBead)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.GitHub (GitHub, Repo(..), PRCreateSpec(..), PRUrl(..), createPR)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (parseBeadId, slugify, formatPRBody)

-- | Arguments for file_pr tool.
data FilePRArgs = FilePRArgs
  { fpaBeadId :: Maybe Text  -- ^ Optional bead ID. If not provided, inferred from branch.
  , fpaTitle  :: Maybe Text  -- ^ Optional PR title. If not provided, derived from bead.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema FilePRArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    , ("title", describeField "title" "Optional PR title. If omitted, derived from bead title." (emptySchema TString))
    ]
    []

instance FromJSON FilePRArgs where
  parseJSON = withObject "FilePRArgs" $ \v ->
    FilePRArgs <$> v .:? "bead_id" <*> v .:? "title"

instance ToJSON FilePRArgs where
  toJSON args = object
    [ "bead_id" .= fpaBeadId args
    , "title" .= fpaTitle args
    ]

-- | Result of file_pr tool.
data FilePRResult = FilePRResult
  { fprUrl :: Maybe Text
  , fprError :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FilePRResult where
  toJSON res = object
    [ "url" .= fprUrl res
    , "error" .= fprError res
    ]

instance FromJSON FilePRResult where
  parseJSON = withObject "FilePRResult" $ \v ->
    FilePRResult
      <$> v .:? "url"
      <*> v .:? "error"

-- | Graph definition for file_pr tool.
data FilePRGraph mode = FilePRGraph
  { fpEntry :: mode :- EntryNode FilePRArgs
      :@ MCPExport
      :@ MCPToolDef '("file_pr", "File a pull request with full bead context in the body.")

  , fpRun :: mode :- LogicNode
      :@ Input FilePRArgs
      :@ UsesEffects '[BD, Git, GitHub, Goto Exit FilePRResult]

  , fpExit :: mode :- ExitNode FilePRResult
  }
  deriving Generic

-- | Handlers for file_pr graph.
filePRHandlers
  :: (Member BD es, Member Git es, Member GitHub es)
  => FilePRGraph (AsHandler es)
filePRHandlers = FilePRGraph
  { fpEntry = ()
  , fpRun = filePRLogic
  , fpExit = ()
  }

-- | Core logic for file_pr.
filePRLogic
  :: (Member BD es, Member Git es, Member GitHub es)
  => FilePRArgs
  -> Eff es (GotoChoice '[To Exit FilePRResult])
filePRLogic args = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo

  -- 2. Determine Bead ID
  let branchBeadId = case mWt of
        Just wt -> parseBeadId wt.wiBranch
        Nothing -> Nothing
      mTargetBeadId = args.fpaBeadId <|> branchBeadId

  case mTargetBeadId of
    Nothing ->
      pure $ gotoExit $ FilePRResult Nothing (Just "Could not determine bead ID. Please provide bead_id argument.")
    Just bid -> do
      -- 3. Get Bead Info
      mBead <- getBead bid
      case mBead of
        Nothing ->
          pure $ gotoExit $ FilePRResult Nothing (Just $ "Bead " <> bid <> " not found.")
        Just bead -> do
          -- 4. Prepare PR Spec
          -- Derive branch name if worktree info unavailable
          let derivedBranch = "bd-" <> bid <> "/" <> slugify bead.biTitle
              headBranch = case mWt of
                Just wt -> wt.wiBranch
                Nothing -> derivedBranch

          let title = fromMaybe ("[" <> bid <> "] " <> bead.biTitle) args.fpaTitle
              body = formatPRBody bead
              repo = Repo "tidepool-heavy-industries/tidepool"
              spec = PRCreateSpec
                { prcsRepo = repo
                , prcsHead = headBranch
                , prcsBase = "main"
                , prcsTitle = title
                , prcsBody = body
                }

          -- 5. Create PR
          PRUrl url <- createPR spec
          pure $ gotoExit $ FilePRResult (Just url) Nothing
