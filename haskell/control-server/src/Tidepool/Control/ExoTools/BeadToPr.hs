{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.BeadToPr
  ( BeadToPrGraph(..)
  , beadToPrHandlers
  , beadToPrLogic
  , BeadToPrArgs(..)
  , BeadToPrResult(..)
  , PRInfo(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.GitHub (GitHub, PullRequest(..), listPullRequests, Repo(..), PRFilter(..), defaultPRFilter)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

-- | Arguments for bead_to_pr tool.
data BeadToPrArgs = BeadToPrArgs
  { btpaBeadId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BeadToPrArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Bead ID (e.g. tidepool-huj)." (emptySchema TString))
    ]
    ["bead_id"]

instance FromJSON BeadToPrArgs where
  parseJSON = withObject "BeadToPrArgs" $ \v ->
    BeadToPrArgs <$> v .: "bead_id"

instance ToJSON BeadToPrArgs where
  toJSON args = object ["bead_id" .= btpaBeadId args]

-- | PR Info.
data PRInfo = PRInfo
  { priNumber :: Int
  , priUrl    :: Text
  , priStatus :: Text
  , priTitle  :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of bead_to_pr tool.
data BeadToPrResult = BeadToPrResult
  { btprPR :: Maybe PRInfo
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BeadToPrResult where
  toJSON res = object ["pr" .= btprPR res]

instance FromJSON BeadToPrResult where
  parseJSON = withObject "BeadToPrResult" $ \v ->
    BeadToPrResult <$> v .: "pr"

-- | Graph definition for bead_to_pr tool.
data BeadToPrGraph mode = BeadToPrGraph
  { btpEntry :: mode :- EntryNode BeadToPrArgs
      :@ MCPExport
      :@ MCPToolDef '("bead_to_pr", "Find the pull request associated with a bead ID.")

  , btpRun :: mode :- LogicNode
      :@ Input BeadToPrArgs
      :@ UsesEffects '[GitHub, Goto Exit BeadToPrResult]

  , btpExit :: mode :- ExitNode BeadToPrResult
  }
  deriving Generic

-- | Handlers for bead_to_pr graph.
beadToPrHandlers
  :: (Member GitHub es)
  => BeadToPrGraph (AsHandler es)
beadToPrHandlers = BeadToPrGraph
  { btpEntry = ()
  , btpRun = beadToPrLogic
  , btpExit = ()
  }

-- | Core logic for bead_to_pr.
beadToPrLogic
  :: Member GitHub es
  => BeadToPrArgs
  -> Eff es (GotoChoice '[To Exit BeadToPrResult])
beadToPrLogic args = do
  let repo = Repo "tidepool-heavy-industries/tidepool"
      searchStr = "[" <> args.btpaBeadId <> "]"
      filt = defaultPRFilter { pfSearch = Just searchStr, pfLimit = Just 1 }
  
  prs <- listPullRequests repo filt
  let mPR = listToMaybe prs
      info = fmap (\pr -> PRInfo
        { priNumber = pr.prNumber
        , priUrl = pr.prUrl
        , priStatus = T.pack (show pr.prState)
        , priTitle = pr.prTitle
        }) mPR

  pure $ gotoExit $ BeadToPrResult info
