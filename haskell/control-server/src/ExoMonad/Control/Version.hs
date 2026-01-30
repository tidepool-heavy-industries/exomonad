{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Version information for exomonad-control-server.
--
-- The git SHA is embedded via Template Haskell at compile time from the
-- EXOMONAD_GIT_SHA environment variable.
module ExoMonad.Control.Version
  ( version,
    gitHash,
    versionString,
  )
where

import Data.Text qualified as T
import ExoMonad.Control.Version.TH (gitHashTH)

-- | Package version (from .cabal file)
version :: T.Text
version = "0.1.0.0"

-- | Git commit hash (embedded at compile time)
gitHash :: T.Text
gitHash = T.pack $(gitHashTH)

-- | Full version string for display
versionString :: T.Text
versionString = T.concat ["exomonad-control-server ", version, " (", gitHash, ")"]
