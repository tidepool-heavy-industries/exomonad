{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.PrReviewStatus
  ( PrReviewStatusGraph(..)
  , prReviewStatusHandlers
  , prReviewStatusLogic
  , PrReviewStatusArgs(..)
  , PrReviewStatusResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.GitHub (GitHub, Repo(..), ReviewComment(..), getPullRequestReviews)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

-- | Arguments for pr_review_status tool.
data PrReviewStatusArgs = PrReviewStatusArgs
  { prsaPrNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PrReviewStatusArgs where
  jsonSchema = objectSchema
    [ ("pr_number", describeField "pr_number" "Pull Request number." (emptySchema TInteger))
    ]
    ["pr_number"]

instance FromJSON PrReviewStatusArgs where
  parseJSON = withObject "PrReviewStatusArgs" $ \v ->
    PrReviewStatusArgs <$> v .: "pr_number"

instance ToJSON PrReviewStatusArgs where
  toJSON args = object ["pr_number" .= prsaPrNumber args]

-- | Result of pr_review_status tool.
-- Returns comments grouped by author.
data PrReviewStatusResult = PrReviewStatusResult
  { prsrComments :: Map Text [ReviewComment]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrReviewStatusResult where
  toJSON res = toJSON (prsrComments res)

instance FromJSON PrReviewStatusResult where
  parseJSON v = PrReviewStatusResult <$> parseJSON v

-- | Graph definition for pr_review_status tool.
data PrReviewStatusGraph mode = PrReviewStatusGraph
  { prsEntry :: mode :- EntryNode PrReviewStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("pr_review_status", "Get PR review comments grouped by author (especially Copilot).")

  , prsRun :: mode :- LogicNode
      :@ Input PrReviewStatusArgs
      :@ UsesEffects '[GitHub, Goto Exit PrReviewStatusResult]

  , prsExit :: mode :- ExitNode PrReviewStatusResult
  }
  deriving Generic

-- | Handlers for pr_review_status graph.
prReviewStatusHandlers
  :: (Member GitHub es)
  => PrReviewStatusGraph (AsHandler es)
prReviewStatusHandlers = PrReviewStatusGraph
  { prsEntry = ()
  , prsRun = prReviewStatusLogic
  , prsExit = ()
  }

-- | Core logic for pr_review_status.
prReviewStatusLogic
  :: (Member GitHub es)
  => PrReviewStatusArgs
  -> Eff es (GotoChoice '[To Exit PrReviewStatusResult])
prReviewStatusLogic args = do
  let repo = Repo "tidepool-heavy-industries/tidepool"
  comments <- getPullRequestReviews repo args.prsaPrNumber
  
  -- Group by author, preserving chronological order
  let grouped = foldr (\c acc -> Map.insertWith (\new old -> old ++ new) c.rcAuthor [c] acc) Map.empty comments
  
  pure $ gotoExit PrReviewStatusResult
    { prsrComments = grouped
    }
