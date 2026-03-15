{-# LANGUAGE OverloadedStrings #-}

-- | HTTP-native hook configuration for dev agents.
--
-- Hooks run server-side via WASM. The permission cascade validates
-- tool calls and guards against known Gemini failure modes.
module HttpDevHooks
  ( httpDevHooks,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (HookInput (..), HookOutput (..), Runtime (..), allowResponse, denyResponse, postToolUseResponse)
import ExoMonad.Permissions (PermissionCheck (..), checkAgentPermissions)
import ExoMonad.Types (HookConfig (..), Effects, defaultSessionStartHook)

-- ============================================================================
-- Gemini Tool Types
-- ============================================================================

-- | Parsed representation of Gemini CLI tool calls.
-- Add branches as we discover more tool schemas to guard against.
data GeminiTool
  = Replace
      { replaceFilePath :: Text,
        replaceOldString :: Text,
        replaceNewString :: Text
      }
  | Other Text Value

parseGeminiTool :: Text -> Value -> GeminiTool
parseGeminiTool "replace" (Object obj)
  | Just (String fp) <- KM.lookup "file_path" obj
  , Just (String old) <- KM.lookup "old_string" obj
  , Just (String new_) <- KM.lookup "new_string" obj
  = Replace fp old new_
parseGeminiTool name val = Other name val

-- ============================================================================
-- Pragma Guards
-- ============================================================================

-- | Check if a replace on a .hs file corrupts LANGUAGE pragma closings.
-- Gemini strips the dash from #-} producing #}, which breaks compilation.
checkPragmaCorruption :: GeminiTool -> Maybe Text
checkPragmaCorruption (Replace fp old new)
  | ".hs" `T.isSuffixOf` fp
  , "#-}" `T.isInfixOf` old
  , not ("#-}" `T.isInfixOf` new)
  , "#}" `T.isInfixOf` new
  = Just $ "BLOCKED: Your replacement corrupts Haskell LANGUAGE pragmas. "
        <> "The correct closing is `#-}`, NOT `#}`. "
        <> "Re-do this edit preserving all `{-# LANGUAGE ... #-}` pragma syntax exactly."
checkPragmaCorruption _ = Nothing

-- | Apply a check only when the agent is Gemini.
geminiOnly :: (GeminiTool -> Maybe Text) -> HookInput -> GeminiTool -> Maybe Text
geminiOnly check hookInput tool =
  case hiRuntime hookInput of
    Just Gemini -> check tool
    _ -> Nothing

-- ============================================================================
-- Hook Config
-- ============================================================================

httpDevHooks :: HookConfig
httpDevHooks =
  HookConfig
    { preToolUse = permissionCascade,
      postToolUse = \_ -> pure (postToolUseResponse Nothing),
      onStop = \_ -> runStopHookChecks,
      onSubagentStop = \_ -> runStopHookChecks,
      onSessionStart = defaultSessionStartHook
    }

-- | Permission cascade with tool-specific guards.
permissionCascade :: HookInput -> Eff Effects HookOutput
permissionCascade hookInput = do
  let tool = fromMaybe "" (hiToolName hookInput)
      args = fromMaybe (Aeson.Object mempty) (hiToolInput hookInput)
      argsJson = TLE.decodeUtf8 $ Aeson.encode args
  void $ suspendEffect_ @LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = "[BeforeTool] tool=" <> TL.fromStrict tool <> " input=" <> argsJson
    , Log.infoRequestFields = ""
    }
  -- Tool-specific guards
  let geminiTool = parseGeminiTool tool args
  case geminiOnly checkPragmaCorruption hookInput geminiTool of
    Just reason -> pure (denyResponse reason)
    Nothing ->
      case checkAgentPermissions "dev" tool args of
        Allowed -> pure (allowResponse Nothing)
        Escalate -> pure (allowResponse (Just "escalation-needed"))
        Denied reason -> pure (denyResponse reason)
