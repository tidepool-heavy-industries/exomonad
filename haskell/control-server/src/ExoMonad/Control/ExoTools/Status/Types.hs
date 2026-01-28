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
  { esaVerbose :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "esa" } ''ExoStatusArgs
  [ 'esaVerbose ?? "Optional verbose output."
  ])