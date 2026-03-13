{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Telemetry hook emitting tool.called events to the event log.
module Telemetry
  ( telemetryPostToolUse,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson ((.=), object, Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (HookInput (..), HookOutput (..), HookEffects, postToolUseResponse)

-- | PostToolUse hook that emits a tool.called event to the event log.
-- Logs tool_name, agent_id, timestamp. For spawn tools, also extracts
-- branch_name/name and first 200 chars of task into spawn_metadata.
telemetryPostToolUse :: HookInput -> Eff HookEffects HookOutput
telemetryPostToolUse hookInput = do
  let toolName = fromMaybe "unknown" (hiToolName hookInput)
      agentId = fromMaybe "unknown" (hiAgentId hookInput)
      timestamp = fromMaybe "" (hiTimestamp hookInput)
      spawnMeta = extractSpawnMetadata toolName (hiToolInput hookInput)
      baseFields =
        [ "tool_name" .= toolName
        , "agent_id" .= agentId
        , "timestamp" .= timestamp
        ]
      fields = case spawnMeta of
        Nothing -> baseFields
        Just meta -> baseFields ++ ["spawn_metadata" .= meta]
      eventPayload = BSL.toStrict $ Aeson.encode $ object fields
  void $ suspendEffect_ @LogEmitEvent
    (Log.EmitEventRequest
      { Log.emitEventRequestEventType = "tool.called"
      , Log.emitEventRequestPayload = eventPayload
      , Log.emitEventRequestTimestamp = 0
      })
  pure (postToolUseResponse Nothing)

-- | For spawn tools, extract branch_name/name and truncated task.
extractSpawnMetadata :: Text -> Maybe Value -> Maybe Value
extractSpawnMetadata toolName (Just (Object obj))
  | isSpawnTool toolName =
      let branchOrName = case KM.lookup "branch_name" obj of
            Just (String b) -> Just ("branch_name" .= b)
            _ -> case KM.lookup "name" obj of
              Just (String n) -> Just ("name" .= n)
              _ -> Nothing
          taskSnippet = case KM.lookup "task" obj of
            Just (String t) -> Just ("task" .= T.take 200 t)
            _ -> Nothing
          pairs = [p | Just p <- [branchOrName, taskSnippet]]
      in if null pairs then Nothing else Just (object pairs)
extractSpawnMetadata _ _ = Nothing

isSpawnTool :: Text -> Bool
isSpawnTool t = t `elem`
  [ "mcp__exomonad__spawn_subtree"
  , "mcp__exomonad__spawn_leaf_subtree"
  , "mcp__exomonad__spawn_workers"
  , "mcp__exomonad__spawn_acp"
  ]
