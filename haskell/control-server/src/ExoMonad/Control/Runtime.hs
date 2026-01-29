{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Runtime environment for the Control Server.
--
-- This module defines the concrete effect stack used by the server
-- and provides the 'runApp' function to interpret it.
module ExoMonad.Control.Runtime
  ( AppEffects
  , runApp
  ) where

import Control.Monad.Freer (Eff, runM)
import OpenTelemetry.Trace (Tracer)

-- Effects
import ExoMonad.Effect.Types (Log, Time, runLog, LogLevel(Debug), runTime)
import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Effects.Worktree (Worktree)
import ExoMonad.Effects.FileSystem (FileSystem)
import ExoMonad.Effects.Env (Env)
import ExoMonad.Effects.Zellij (Zellij)
import ExoMonad.Effects.DockerSpawner (DockerSpawner)
import ExoMonad.Effect.Gemini (GeminiOp)
import ExoMonad.Effect.TUI (TUI)

import ExoMonad.Effects.Justfile (Justfile)
import ExoMonad.Control.Effects.Justfile (runJustfileRemote)
import ExoMonad.Control.Effects.SshExec (SshExec, runSshExec)
import Control.Monad.Freer.Reader (Reader, runReader)
import ExoMonad.Control.Effects.Effector (Effector, runEffectorIO)
import ExoMonad.Control.Effects.Cabal (Cabal, runCabalRemote)

-- Interpreters
import ExoMonad.Control.Logging (Logger)
import ExoMonad.Git.Interpreter (runGitIO)
import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import ExoMonad.Worktree.Interpreter (runWorktreeIO, defaultWorktreeConfig)
import ExoMonad.FileSystem.Interpreter (runFileSystemIO)
import ExoMonad.Env.Interpreter (runEnvIO)
import ExoMonad.Zellij.Interpreter (runZellijIO)
import ExoMonad.Control.Effects.DockerCtl (runDockerCtl)
import ExoMonad.Gemini.Interpreter (runGeminiIO)
import ExoMonad.Control.TUIInterpreter (runTUIFifo)
import qualified ExoMonad.Control.Runtime.Paths as Paths
import ExoMonad.Control.Hook.GitHubRetry (withRetry, defaultRetryConfig, RetryConfig(..))
import ExoMonad.Control.Types (ServerConfig(..))

-- | The concrete effect stack for the application.
--
-- Order matches interpretation order (head is interpreted first/innermost).
-- Dependencies (Log, Time, IO) must be at the end to be available to
-- outer interpreters.
type AppEffects =
  '[ GeminiOp
   , DockerSpawner
   , Justfile
   , Cabal
   , Effector
   , Worktree
   , Git
   , SshExec
   , Zellij
   , GitHub
   , FileSystem
   , Env
   , TUI
   , Reader ServerConfig
   , Reader Tracer
   , Reader CircuitBreakerMap
   , Time
   , Log
   , IO
   ]

-- | Run the application effect stack.
runApp :: ServerConfig -> Tracer -> CircuitBreakerMap -> Logger -> Eff AppEffects a -> IO a
runApp config tracer cbMap logger action = do
  -- Resolve paths
  binDir <- Paths.dockerBinDir
  let dockerCtlPath = Paths.dockerCtlBin binDir
  let repoRoot = "." -- Assume current directory is repo root

  -- GitHub config
  let ghConfig = case config.githubConfig of
        Just c -> c
        Nothing -> defaultGitHubConfig

  -- Retry config
  let retryCfg = defaultRetryConfig { tracer = Just tracer }

  runM
    $ runLog Debug
    $ runTime
    $ runReader cbMap
    $ runReader tracer
    $ runReader config
    $ runTUIFifo logger
    $ runEnvIO
    $ runFileSystemIO
    $ runGitHubIO ghConfig
    $ withRetry retryCfg
    $ runZellijIO
    $ runSshExec logger dockerCtlPath
    $ runGitIO -- Will use SshExec if GitRemote is used in other contexts? 
               -- Wait, runGitIO uses IO. runGitRemote uses SshExec.
               -- In runApp, we use runGitIO (local).
               -- But order matters for the list type equality.
    $ runWorktreeIO (defaultWorktreeConfig repoRoot)
    $ runEffectorIO logger
    $ runCabalRemote Nothing
    $ runJustfileRemote "" "" 
    $ runDockerCtl logger dockerCtlPath
    $ runGeminiIO
    $ action