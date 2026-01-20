{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.PreCommitCheck
  ( PreCommitCheckGraph(..)
  , preCommitCheckHandlers
  , preCommitCheckLogic
  , PreCommitCheckArgs(..)
  , PreCommitCheckResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.Justfile (Justfile, runRecipe, JustResult(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

-- | Arguments for pre_commit_check tool.
data PreCommitCheckArgs = PreCommitCheckArgs
  { pccaRecipe :: Maybe Text  -- ^ Optional just recipe name.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PreCommitCheckArgs where
  jsonSchema = objectSchema
    [ ("recipe", describeField "recipe" "Optional just recipe to run. Defaults to 'pre-commit-fast'." (emptySchema TString))
    ]
    []

instance FromJSON PreCommitCheckArgs where
  parseJSON = withObject "PreCommitCheckArgs" $ \v ->
    PreCommitCheckArgs <$> v .:? "recipe"

instance ToJSON PreCommitCheckArgs where
  toJSON args = object ["recipe" .= pccaRecipe args]

-- | Result of pre_commit_check tool.
data PreCommitCheckResult = PreCommitCheckResult
  { pccrSuccess :: Bool
  , pccrStdout  :: Text
  , pccrStderr  :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PreCommitCheckResult where
  toJSON res = object
    [ "success" .= pccrSuccess res
    , "stdout"  .= pccrStdout res
    , "stderr"  .= pccrStderr res
    ]

instance FromJSON PreCommitCheckResult where
  parseJSON = withObject "PreCommitCheckResult" $ \v ->
    PreCommitCheckResult
      <$> v .: "success"
      <*> v .: "stdout"
      <*> v .: "stderr"

-- | Graph definition for pre_commit_check tool.
data PreCommitCheckGraph mode = PreCommitCheckGraph
  { pccEntry :: mode :- EntryNode PreCommitCheckArgs
      :@ MCPExport
      :@ MCPToolDef '("pre_commit_check", "Run pre-commit quality checks using 'just'.")

  , pccRun :: mode :- LogicNode
      :@ Input PreCommitCheckArgs
      :@ UsesEffects '[Justfile, Goto Exit PreCommitCheckResult]

  , pccExit :: mode :- ExitNode PreCommitCheckResult
  }
  deriving Generic

-- | Handlers for pre_commit_check graph.
preCommitCheckHandlers
  :: Member Justfile es
  => PreCommitCheckGraph (AsHandler es)
preCommitCheckHandlers = PreCommitCheckGraph
  { pccEntry = ()
  , pccRun = preCommitCheckLogic
  , pccExit = ()
  }

-- | Core logic for pre_commit_check.
preCommitCheckLogic
  :: Member Justfile es
  => PreCommitCheckArgs
  -> Eff es (GotoChoice '[To Exit PreCommitCheckResult])
preCommitCheckLogic args = do
  let recipe = fromMaybe "pre-commit-fast" args.pccaRecipe
  res <- runRecipe recipe []
  let success = res.exitCode == 0
  pure $ gotoExit PreCommitCheckResult
    { pccrSuccess = success
    , pccrStdout  = res.stdout
    , pccrStderr  = res.stderr
    }
