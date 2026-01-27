{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.ExoTools.Status.Types
  ( ExoStatusArgs(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..))

-- | Arguments for exo_status tool.
data ExoStatusArgs = ExoStatusArgs
  { esaBeadId :: Maybe Text  -- ^ Optional bead ID. If not provided, inferred from branch.
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "esa" } ''ExoStatusArgs
  [ 'esaBeadId ?? "Optional bead ID (e.g. exomonad-huj). If omitted, inferred from branch name."
  ])