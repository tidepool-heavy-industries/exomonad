{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.ExoTools.Status.Types
  ( ExoStatusArgs (..),
  )
where

import ExoMonad.Schema (defaultMCPOptions, deriveMCPTypeWith, (??))
import GHC.Generics (Generic)
import Language.Haskell.TH (mkName)

-- | Arguments for exo_status tool.
data ExoStatusArgs = ExoStatusArgs
  { verbose :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''ExoStatusArgs
     [ mkName "verbose" ?? "Optional verbose output."
     ]
 )
