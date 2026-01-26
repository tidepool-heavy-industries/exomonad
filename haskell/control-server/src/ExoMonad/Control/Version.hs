{-# LANGUAGE OverloadedStrings #-}

-- | Version information for exomonad-control-server.
--
-- The git SHA is embedded via:
--   1. EXOMONAD_GIT_SHA environment variable (set at build time in Docker)
--   2. Fallback to "unknown" if not set
--
-- Docker builds set this via: ARG GIT_SHA / ENV EXOMONAD_GIT_SHA=${GIT_SHA}
module ExoMonad.Control.Version
  ( version
  , gitHash
  , versionString
  ) where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Package version (from .cabal file)
version :: T.Text
version = "0.1.0.0"

-- | Git commit hash (from environment, set at build time)
gitHash :: T.Text
gitHash = unsafePerformIO $ do
  envHash <- lookupEnv "EXOMONAD_GIT_SHA"
  pure $ T.pack $ maybe "unknown" id envHash
{-# NOINLINE gitHash #-}

-- | Full version string for display
versionString :: T.Text
versionString = T.concat ["exomonad-control-server ", version, " (", gitHash, ")"]
