{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Events
  ( WaitForEvent (..)
  , NotifyCompletion (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), genericParseJSON, genericToJSON, defaultOptions, fieldLabelModifier, camelTo2, Value)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text, pack)
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Data.Word (Word8)
import Effects.Events qualified as Proto
import ExoMonad.Effects.Events qualified as Events
import ExoMonad.Guest.Proto (fromText)
import ExoMonad.Guest.Tool.Class (MCPTool (..), liftEffect, errorResult, successResult, EffectRequest(..), MCPCallOutput, suspend)
import GHC.Generics (Generic)
import Proto3.Suite.Class (toLazyByteString, fromByteString)

-- | Wait for event tool
data WaitForEvent = WaitForEvent

data WaitForEventArgs = WaitForEventArgs
  { wfeTypes :: [Text]
  , wfeTimeoutSecs :: Int
  , wfeAfterEventId :: Maybe Int
  } deriving (Generic, Show)

instance FromJSON WaitForEventArgs where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }
instance ToJSON WaitForEventArgs where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }

instance MCPTool WaitForEvent where
  type ToolArgs WaitForEvent = WaitForEventArgs
  toolName = "wait_for_event"
  toolDescription = "Block until a matching event occurs or timeout expires. Returns worker completion events."
  toolSchema = object
    [
      "type" .= ("object" :: Text)
    , "properties" .= object
        [
          "types" .= object
            [
              "type" .= ("array" :: Text)
            , "items" .= object ["type" .= ("string" :: Text)]
            , "description" .= ("Event types to wait for (e.g. [\"worker_complete\"])" :: Text)
            ]
        , "timeout_secs" .= object
            [
              "type" .= ("number" :: Text)
            , "default" .= (300 :: Int)
            , "description" .= ("Timeout in seconds" :: Text)
            ]
        , "after_event_id" .= object
            [
              "type" .= ("number" :: Text)
            , "default" .= (0 :: Int)
            , "description" .= ("Only return events with ID greater than this value (cursor for skipping stale events)" :: Text)
            ]
        ]
    , "required" .= (["types"] :: [Text])
    ]
  toolHandler args = do
    -- Synchronous fallback (blocking)
    let cursor = maybe 0 fromIntegral (wfeAfterEventId args)
    result <- Events.waitForEvent (wfeTypes args) (wfeTimeoutSecs args) cursor
    case result of
      Left err -> pure $ errorResult $ pack (show err)
      Right resp -> case Proto.waitForEventResponseEvent resp of
        Just event -> pure $ successResult $ object
          [ "event" .= event
          , "event_id" .= Proto.eventEventId event
          ]
        Nothing -> pure $ errorResult "No event in response"

  toolHandlerEff args = do
    let cursor = maybe 0 fromIntegral (wfeAfterEventId args)
        req = Proto.WaitForEventRequest
          { Proto.waitForEventRequestTypes = V.fromList (map fromText (wfeTypes args))
          , Proto.waitForEventRequestTimeoutSecs = fromIntegral (wfeTimeoutSecs args)
          , Proto.waitForEventRequestAfterEventId = cursor
          }
        payloadBytes = BSL.unpack (toLazyByteString req)
    resultValue <- suspend (EffectRequest "events.wait_for_event" (Aeson.toJSON payloadBytes))
    case Aeson.fromJSON resultValue of
      Aeson.Success (bytes :: [Word8]) ->
        case fromByteString (BS.pack bytes) of
          Right (resp :: Proto.WaitForEventResponse) ->
            case Proto.waitForEventResponseEvent resp of
              Just event -> pure $ successResult $ object
                [ "event" .= event
                , "event_id" .= Proto.eventEventId event
                ]
              Nothing -> pure $ errorResult "No event in response"
          Left err -> pure $ errorResult $ "Failed to decode protobuf: " <> pack (show err)
      Aeson.Error err -> pure $ errorResult $ "Failed to parse result: " <> pack err

-- | Notify completion tool (for workers to call on exit)
data NotifyCompletion = NotifyCompletion

data NotifyCompletionArgs = NotifyCompletionArgs
  {
    ncSessionId :: Text
  , ncWorkerId :: Text
  , ncStatus :: Text
  , ncMessage :: Text
  } deriving (Generic, Show)

instance FromJSON NotifyCompletionArgs where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }
instance ToJSON NotifyCompletionArgs where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

instance MCPTool NotifyCompletion where
  type ToolArgs NotifyCompletion = NotifyCompletionArgs
  toolName = "notify_completion"
  toolDescription = "Notify TL that this worker has completed (called by workers on exit)"
  toolSchema = object
    [
      "type" .= ("object" :: Text)
    , "properties" .= object
        [
          "session_id" .= object
            [
              "type" .= ("string" :: Text)
            , "description" .= ("TL session ID" :: Text)
            ]
        , "worker_id" .= object
            [
              "type" .= ("string" :: Text)
            , "description" .= ("This worker's ID" :: Text)
            ]
        , "status" .= object
            [
              "type" .= ("string" :: Text)
            , "description" .= ("Completion status (success/failure)" :: Text)
            ]
        , "message" .= object
            [
              "type" .= ("string" :: Text)
            , "description" .= ("Completion message" :: Text)
            ]
        ]
    , "required" .= (["session_id", "worker_id", "status", "message"] :: [Text])
    ]
  toolHandler args = do
    let event = Proto.Event
          { Proto.eventEventId = 0
          , Proto.eventEventType = Just $ Proto.EventEventTypeWorkerComplete $ Proto.WorkerComplete
              {
                Proto.workerCompleteWorkerId = TL.fromStrict (ncWorkerId args)
              , Proto.workerCompleteStatus = TL.fromStrict (ncStatus args)
              , Proto.workerCompleteChanges = V.empty
              , Proto.workerCompleteMessage = TL.fromStrict (ncMessage args)
              }
          }
    liftEffect (Events.notifyEvent (ncSessionId args) event) $ \_ ->
      object ["success" .= True]
