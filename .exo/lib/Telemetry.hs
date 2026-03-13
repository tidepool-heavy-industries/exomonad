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
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (toList)
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
  | toolName == "mcp__exomonad__spawn_workers" =
      case KM.lookup "specs" obj of
        Just (Array specs) ->
          let specsList = toList specs
              count = length specsList
          in case specsList of
               (Object sObj : _) ->
                 let name = KM.lookup "name" sObj
                     task = KM.lookup "task" sObj
                     meta = [ "worker_count" .= count ]
                            ++ [ "first_worker_name" .= n | Just (String n) <- [name] ]
                            ++ [ "first_worker_task" .= T.take 200 t | Just (String t) <- [task] ]
                 in Just (object meta)
               (_ : _) -> Just (object ["worker_count" .= count])
               [] -> Nothing
        _ -> Nothing
  | isSpawnTool toolName =
      let nameVal = case KM.lookup "branch_name" obj of
            Just (String b) -> Just ("branch_name" .= b)
            _ -> case KM.lookup "name" obj of
              Just (String n) -> Just ("name" .= n)
              _ -> Nothing
          taskField = if toolName == "mcp__exomonad__spawn_acp" then "prompt" else "task"
          taskVal = case KM.lookup (Key.fromText taskField) obj of
            Just (String t) -> Just (Key.fromText taskField .= T.take 200 t)
            _ -> Nothing
          pairs = [p | Just p <- [nameVal, taskVal]]
      in if null pairs then Nothing else Just (object pairs)
extractSpawnMetadata _ _ = Nothing

isSpawnTool :: Text -> Bool
isSpawnTool t = t `elem`
  [ "mcp__exomonad__spawn_subtree"
  , "mcp__exomonad__spawn_leaf_subtree"
  , "mcp__exomonad__spawn_workers"
  , "mcp__exomonad__spawn_acp"
  ]
