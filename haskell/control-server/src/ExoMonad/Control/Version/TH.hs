{-# LANGUAGE TemplateHaskell #-}
module ExoMonad.Control.Version.TH (gitHashTH) where

import Language.Haskell.TH
import System.Environment (lookupEnv)

-- | Look up the EXOMONAD_GIT_SHA environment variable at compile time.
-- Defaults to "unknown" if not set.
--
-- Note: This uses runIO to read the environment during compilation.
-- In a Docker build, this is safe because the layer is rebuilt when the ARG changes.
gitHashTH :: Q Exp
gitHashTH = do
  mHash <- runIO $ lookupEnv "EXOMONAD_GIT_SHA"
  let hash = maybe "unknown" id mHash
  litE (stringL hash)
