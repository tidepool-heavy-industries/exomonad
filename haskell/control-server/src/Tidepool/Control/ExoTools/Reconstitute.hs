{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.Reconstitute
  ( ExoReconstituteGraph(..)
  , exoReconstituteHandlers
  , exoReconstituteLogic
  , ExoReconstituteArgs(..)
  , ExoReconstituteResult
  , ExoStatusResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD, sync)
import Tidepool.Effects.Git (Git)
import Tidepool.Effects.GitHub (GitHub)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (ExoStatusResult(..), getDevelopmentContext)

-- | Arguments for exo_reconstitute tool.
data ExoReconstituteArgs = ExoReconstituteArgs
  { eraBeadId :: Maybe Text  -- ^ Optional bead ID.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ExoReconstituteArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    ]
    []

instance FromJSON ExoReconstituteArgs where
  parseJSON = withObject "ExoReconstituteArgs" $ \v ->
    ExoReconstituteArgs <$> v .:? "bead_id"

instance ToJSON ExoReconstituteArgs where
  toJSON args = object ["bead_id" .= eraBeadId args]

-- | Result of exo_reconstitute tool.
type ExoReconstituteResult = ExoStatusResult

-- | Graph definition for exo_reconstitute tool.
data ExoReconstituteGraph mode = ExoReconstituteGraph
  { erEntry :: mode :- EntryNode ExoReconstituteArgs
      :@ MCPExport
      :@ MCPToolDef '("exo_reconstitute", "Synchronize beads from main and refresh development context.")

  , erRun :: mode :- LogicNode
      :@ Input ExoReconstituteArgs
      :@ UsesEffects '[BD, Git, GitHub, Goto Exit ExoReconstituteResult]

  , erExit :: mode :- ExitNode ExoReconstituteResult
  }
  deriving Generic

-- | Handlers for exo_reconstitute graph.
exoReconstituteHandlers
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoReconstituteGraph (AsHandler es)
exoReconstituteHandlers = ExoReconstituteGraph
  { erEntry = ()
  , erRun = exoReconstituteLogic
  , erExit = ()
  }

-- | Core logic for exo_reconstitute.
exoReconstituteLogic
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoReconstituteArgs
  -> Eff es (GotoChoice '[To Exit ExoReconstituteResult])
exoReconstituteLogic args = do
  -- 1. Run BD Sync (Primary difference from exo_status)
  sync

  -- 2. Determine context
  result <- getDevelopmentContext args.eraBeadId
  pure $ gotoExit result
