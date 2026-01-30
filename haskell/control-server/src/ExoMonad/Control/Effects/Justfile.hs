{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.Justfile
  ( runJustfileRemote,
  )
where

import Control.Monad.Freer (Eff, Member, interpret)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import ExoMonad.Control.Effects.SshExec (ExecRequest (..), ExecResult (..), SshExec, execCommand)
import ExoMonad.Effects.Justfile (JustResult (..), Justfile (..))

-- | Interpreter: uses SshExec to run just commands remotely
-- Takes a container name (optional) and a working directory (relative to container root).
runJustfileRemote :: (Member SshExec effs) => Maybe Text -> FilePath -> Eff (Justfile ': effs) a -> Eff effs a
runJustfileRemote mContainer workDir = interpret $ \case
  RunRecipe recipe args -> do
    result <-
      execCommand $
        ExecRequest
          { container = mContainer,
            command = "just",
            args = recipe : args,
            workingDir = workDir,
            env = [],
            timeout = 300
          }
    pure $
      JustResult
        { stdout = result.stdout,
          stderr = result.stderr,
          exitCode = fromMaybe (-1) (result.exitCode)
        }
