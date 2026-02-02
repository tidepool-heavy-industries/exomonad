module ExoMonad.Context.Interpreter
  ( runContextIO,
    runContextStatic,
  )
where

import Polysemy (Sem, Member, interpret, embed)
import Polysemy.Embed (Embed)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effect.Context
import ExoMonad.Path (Path, Abs, Dir, parseAbsDirT)
import Path.IO (getCurrentDir)
import System.Environment (lookupEnv)

-- | Run Context effect by reading from the environment.
--
-- * Worktree: Current working directory
-- * AgentId: From AGENT_ID env var (or "unknown")
-- * UserConfig: Defaults (for now)
runContextIO :: (Member (Embed IO) r) => Sem (Context ': r) a -> Sem r a
runContextIO = interpret $ \case
  GetWorktreeRoot -> embed getCurrentDir
  GetUserConfig -> pure defaultUserConfig
  GetAgentId -> embed $ do
    maybeId <- lookupEnv "AGENT_ID"
    pure $ maybe "unknown" T.pack maybeId

-- | Run Context effect with static values (useful for testing).
runContextStatic ::
  Path Abs Dir ->
  UserConfig ->
  Text ->
  Sem (Context ': r) a ->
  Sem r a
runContextStatic root config agentId = interpret $ \case
  GetWorktreeRoot -> pure root
  GetUserConfig -> pure config
  GetAgentId -> pure agentId
