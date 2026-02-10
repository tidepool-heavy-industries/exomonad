{-# LANGUAGE OverloadedStrings #-}

-- | PostToolUse hook that captures team names from TeamCreate responses.
--
-- When Claude Code's TeamCreate tool completes, the response contains the
-- team name. This hook extracts it and persists to KV store so that
-- spawn_gemini_teammate can register agents in the correct team.
module PostToolUseHook
  ( teamCreateHook,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import ExoMonad.Effects.KV (kvSet, SetRequest (..))
import ExoMonad.Guest.Types (HookInput (..), HookOutput, postToolUseResponse)
import ExoMonad.Types (HookEffects)
import Polysemy (Sem, embed)

-- | Hook that captures team_name from TeamCreate tool responses.
teamCreateHook :: HookInput -> Sem HookEffects HookOutput
teamCreateHook hookInput =
  case hiToolName hookInput of
    Just "TeamCreate" -> embed $ do
      case extractTeamName (hiToolResponse hookInput) of
        Just name -> do
          _ <- kvSet (SetRequest {setRequestKey = "current_team", setRequestValue = TL.fromStrict name})
          pure (postToolUseResponse (Just $ "Team registered: " <> name))
        Nothing -> pure (postToolUseResponse Nothing)
    _ -> pure (postToolUseResponse Nothing)

-- | Extract team_name from a TeamCreate tool response JSON.
extractTeamName :: Maybe Aeson.Value -> Maybe Text
extractTeamName (Just (Aeson.Object obj)) = case KeyMap.lookup "team_name" obj of
  Just (Aeson.String name) -> Just name
  _ -> Nothing
extractTeamName _ = Nothing
