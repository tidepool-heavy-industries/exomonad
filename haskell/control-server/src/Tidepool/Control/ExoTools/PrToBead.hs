{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.PrToBead
  ( PrToBeadGraph(..)
  , prToBeadHandlers
  , prToBeadLogic
  , PrToBeadArgs(..)
  , PrToBeadResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.GitHub (GitHub, Repo(..), getPullRequest, PullRequest(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (extractBeadId)

-- | Arguments for pr_to_bead tool.
data PrToBeadArgs = PrToBeadArgs
  { ptbaPrNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PrToBeadArgs where
  jsonSchema = objectSchema
    [ ("pr_number", describeField "pr_number" "Pull Request number." (emptySchema TInteger))
    ]
    ["pr_number"]

instance FromJSON PrToBeadArgs where
  parseJSON = withObject "PrToBeadArgs" $ \v ->
    PrToBeadArgs <$> v .: "pr_number"

instance ToJSON PrToBeadArgs where
  toJSON args = object ["pr_number" .= ptbaPrNumber args]

-- | Result of pr_to_bead tool.
data PrToBeadResult = PrToBeadResult
  { ptbrBeadId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrToBeadResult where
  toJSON res = object ["bead_id" .= ptbrBeadId res]

instance FromJSON PrToBeadResult where
  parseJSON = withObject "PrToBeadResult" $ \v ->
    PrToBeadResult <$> v .: "bead_id"

-- | Graph definition for pr_to_bead tool.
data PrToBeadGraph mode = PrToBeadGraph
  { ptbEntry :: mode :- EntryNode PrToBeadArgs
      :@ MCPExport
      :@ MCPToolDef '("pr_to_bead", "Extract the bead ID from a pull request title.")

  , ptbRun :: mode :- LogicNode
      :@ Input PrToBeadArgs
      :@ UsesEffects '[GitHub, Goto Exit PrToBeadResult]

  , ptbExit :: mode :- ExitNode PrToBeadResult
  }
  deriving Generic

-- | Handlers for pr_to_bead graph.
prToBeadHandlers
  :: (Member GitHub es)
  => PrToBeadGraph (AsHandler es)
prToBeadHandlers = PrToBeadGraph
  { ptbEntry = ()
  , ptbRun = prToBeadLogic
  , ptbExit = ()
  }

-- | Core logic for pr_to_bead.
prToBeadLogic
  :: Member GitHub es
  => PrToBeadArgs
  -> Eff es (GotoChoice '[To Exit PrToBeadResult])
prToBeadLogic args = do
  let repo = Repo "tidepool-heavy-industries/tidepool"
  mPR <- getPullRequest repo args.ptbaPrNumber False
  
  let mBeadId = mPR >>= \pr -> extractBeadId pr.prTitle
  pure $ gotoExit $ PrToBeadResult mBeadId
