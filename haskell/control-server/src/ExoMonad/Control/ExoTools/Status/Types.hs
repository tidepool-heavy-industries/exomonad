{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.ExoTools.Status.Types
  ( ExoStatusArgs(..)
  ) where

import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), HasJSONSchema(..))
import Language.Haskell.TH (mkName)

-- | Arguments for exo_status tool.
data ExoStatusArgs = ExoStatusArgs
  { verbose :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''ExoStatusArgs
  [ mkName "verbose" ?? "Optional verbose output."
  ])