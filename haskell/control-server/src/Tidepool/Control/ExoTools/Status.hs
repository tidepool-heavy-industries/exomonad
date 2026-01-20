{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.Status
  ( ExoStatusGraph(..)
  , exoStatusHandlers
  , exoStatusLogic
  , ExoStatusArgs(..)
  , ExoStatusResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD)
import Tidepool.Effects.Git (Git)
import Tidepool.Effects.GitHub (GitHub)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (ExoStatusResult(..), getDevelopmentContext)

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
  result <- getDevelopmentContext args.esaBeadId
  pure $ gotoExit result
