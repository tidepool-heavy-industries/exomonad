{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Runtime environment for the Control Server.
--
-- This module defines the concrete effect stack used by the server
-- and provides the 'runApp' function to interpret it.
module ExoMonad.Control.Runtime
  ( AppEffects,
    runApp,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Reader (Reader, runReader)
import Data.Text (Text)
-- Effects

-- Interpreters
import ExoMonad.Control.Effects.DockerCtl (runDockerCtl)
import ExoMonad.Control.Effects.Effector (Effector, runEffectorIO)
import ExoMonad.Control.Effects.Git (runGitRemote)
import ExoMonad.Control.Effects.Justfile (runJustfileRemote)
import ExoMonad.Control.Effects.SshExec (SshExec, runSshExec)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap)
import ExoMonad.Control.Hook.GitHubRetry (RetryConfig (..), defaultRetryConfig, withRetry)
import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Protocol (AgentStatus)
import ExoMonad.Control.Runtime.Paths qualified as Paths
import ExoMonad.Control.TUIInterpreter (runTUIFifo)
import ExoMonad.Control.Types (ServerConfig (..))
import ExoMonad.Effect.Gemini (GeminiOp)
import ExoMonad.Effect.TUI (TUI)
import ExoMonad.Effect.Types (Log, LogLevel (Debug), Time, runLog, runTime)
import ExoMonad.Effects.DockerSpawner (DockerSpawner)
import ExoMonad.Effects.Env (Env)
import ExoMonad.Effects.FileSystem (FileSystem)
import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Effects.Justfile (Justfile)
import ExoMonad.Effects.Worktree (Worktree)
import ExoMonad.Effects.Zellij (Zellij)
import ExoMonad.Env.Interpreter (runEnvIO)
import ExoMonad.FileSystem.Interpreter (runFileSystemIO)
import ExoMonad.Gemini.Interpreter (runGeminiIO)
import ExoMonad.GitHub.Interpreter (defaultGitHubConfig, runGitHubIO)
import ExoMonad.Worktree.Interpreter (defaultWorktreeConfig, runWorktreeIO)
import ExoMonad.Zellij.Interpreter (runZellijIO)
import OpenTelemetry.Trace (Tracer)

-- | The concrete effect stack for the application.
--
-- Order matches interpretation order (head is interpreted first/innermost).
-- Dependencies (Log, Time, IO) must be at the end to be available to
-- outer interpreters.
type AppEffects =
  '[ GeminiOp,
     DockerSpawner,
     Justfile,
     Effector,
     Worktree,
     Git,
     SshExec,
     Zellij,
     GitHub,
     FileSystem,
     Env,
     TUI,
     Reader ServerConfig,
     Reader Tracer,
     Reader CircuitBreakerMap,
     Time,
     Log,
     IO
   ]

-- | Run the application effect stack.
--
-- REQUIRES a container ID for git operations. This is a deliberate design choice:
-- - MCP tools need to run git commands in the agent's container, not locally
-- - Without a container ID, we can't know where to run git commands
-- - Pass Nothing only for non-git operations (will fail if git is used)
runApp :: ServerConfig -> Tracer -> CircuitBreakerMap -> Logger -> TVar [AgentStatus] -> Maybe Text -> Eff AppEffects a -> IO a
runApp config tracer cbMap logger agentStore mContainerId action = do
  -- Resolve paths
  binDir <- Paths.dockerBinDir
  let dockerCtlPath = Paths.dockerCtlBin binDir
  let repoRoot = "." -- Assume current directory is repo root

  -- GitHub config
  let ghConfig = case config.githubConfig of
        Just c -> c
        Nothing -> defaultGitHubConfig

  -- Retry config
  let retryCfg = defaultRetryConfig {tracer = Just tracer}

  -- Container ID for remote execution
  -- If no container ID is provided, git operations will fail with a helpful error
  runM $
    runLog Debug $
      runTime $
        runReader cbMap $
          runReader tracer $
            runReader config $
              runTUIFifo logger $
                runEnvIO $
                  runFileSystemIO $
                    runGitHubIO ghConfig $
                      withRetry retryCfg $
                        runZellijIO $
                          runSshExec logger dockerCtlPath $
                            runGitRemote mContainerId "." $
                              runWorktreeIO (defaultWorktreeConfig repoRoot) $
                                runEffectorIO logger $
                                  runJustfileRemote mContainerId "" $
                                    runDockerCtl logger dockerCtlPath agentStore $
                                      runGeminiIO action
