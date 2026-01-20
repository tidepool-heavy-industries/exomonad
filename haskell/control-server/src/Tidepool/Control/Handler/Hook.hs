-- | Hook event handler.
--
-- Initial implementation: passthrough (log and allow all).
-- This will be extended to run Tidepool effect logic.
module Tidepool.Control.Handler.Hook
  ( handleHook
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Control.Exception (SomeException, try)
import Control.Monad.Freer (runM)

import Tidepool.Control.Protocol
import Tidepool.Control.ExoTools (exoReconstituteLogic, ExoReconstituteArgs(..), ExoReconstituteResult)
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.Graph.Goto (unwrapSingleChoice)

-- | Handle a hook event.
--
-- Current behavior: passthrough (always allow).
-- TODO: Wire to Tidepool effect stack for real logic.
handleHook :: HookInput -> Runtime -> IO ControlResponse
handleHook input runtime = do
  TIO.putStrLn $ "  session=" <> input.sessionId
  TIO.putStrLn $ "  cwd=" <> input.cwd
  hFlush stdout

  -- On Stop, run reconstitute logic (sync beads)
  if input.hookEventName == "Stop"
    then do
      TIO.putStrLn "  [HOOK] Running post-stop reconstitute..."
      hFlush stdout
      result <- (try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ runGitIO
        $ runGitHubIO defaultGitHubConfig
        $ unwrapSingleChoice <$> exoReconstituteLogic (ExoReconstituteArgs Nothing)) :: IO (Either SomeException ExoReconstituteResult)
      case result of
        Left e -> do
          let errMsg = "Reconstitute failed: " <> T.pack (show e)
          TIO.putStrLn $ "  [HOOK] " <> errMsg
          hFlush stdout
          pure $ hookError runtime errMsg
        Right _ -> do
          TIO.putStrLn "  [HOOK] Reconstitute succeeded"
          hFlush stdout
          pure $ hookSuccess $ makeResponse input.hookEventName input
    else
      pure $ hookSuccess $ makeResponse input.hookEventName input

-- | Create appropriate response based on hook type.
makeResponse :: Text -> HookInput -> HookOutput
makeResponse eventName input = case eventName of
  "PreToolUse" -> allowPreToolUse (Just "Allowed by Tidepool") Nothing
  "PostToolUse" -> allowPostToolUse Nothing
  "PermissionRequest" -> defaultOutput
    { hookSpecificOutput = Just $ PermissionRequestOutput $ Allow Nothing
    }
  "UserPromptSubmit" -> defaultOutput
    { hookSpecificOutput = Just $ UserPromptSubmitOutput Nothing
    }
  "SessionStart" -> defaultOutput
    { hookSpecificOutput = Just $ SessionStartOutput $
        Just $ "Tidepool control server connected. Session: " <> input.sessionId
    }
  "SessionEnd" -> defaultOutput
    { hookSpecificOutput = Just SessionEndOutput
    }
  "Stop" -> defaultOutput
    { hookSpecificOutput = Just StopOutput
    }
  "SubagentStop" -> defaultOutput
    { hookSpecificOutput = Just SubagentStopOutput
    }
  "Notification" -> defaultOutput
    { hookSpecificOutput = Just NotificationOutput
    }
  "PreCompact" -> defaultOutput
    { hookSpecificOutput = Just PreCompactOutput
    }
  _ -> defaultOutput  -- Unknown hook type, just continue

-- | Default output (continue, no specific output)
defaultOutput :: HookOutput
defaultOutput = HookOutput
  { continue_ = True
  , stopReason = Nothing
  , suppressOutput = Nothing
  , systemMessage = Nothing
  , hookSpecificOutput = Nothing
  }
