{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Events
  ( WaitForEvent (..)
  , NotifyCompletion (..)
  ) where

import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Text (Text, pack)
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Events qualified as Proto
import ExoMonad.Effects.Events qualified as Events
import ExoMonad.Guest.Tool.Class (MCPTool (..), successResult, errorResult)
import GHC.Generics (Generic)

-- | Wait for event tool
data WaitForEvent = WaitForEvent

data WaitForEventArgs = WaitForEventArgs
  { wfeTypes :: [Text]
  , wfeTimeoutSecs :: Int
  } deriving (Generic, Show)

instance FromJSON WaitForEventArgs
instance ToJSON WaitForEventArgs

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
        ]
    , "required" .= (["types"] :: [Text])
    ]
  toolHandler args = do
    result <- Events.waitForEvent (wfeTypes args) (wfeTimeoutSecs args)
    case result of
      Left err -> pure $ errorResult $ pack (show err)
      Right resp -> case Proto.waitForEventResponseEvent resp of
        Just event -> pure $ successResult $ object ["event" .= event]
        Nothing -> pure $ errorResult "No event in response"

-- | Notify completion tool (for workers to call on exit)
data NotifyCompletion = NotifyCompletion

data NotifyCompletionArgs = NotifyCompletionArgs
  {
    ncSessionId :: Text
  , ncWorkerId :: Text
  , ncStatus :: Text
  , ncMessage :: Text
  } deriving (Generic, Show)

instance FromJSON NotifyCompletionArgs
instance ToJSON NotifyCompletionArgs

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
          {
            Proto.eventEventType = Just $ Proto.EventEventTypeWorkerComplete $ Proto.WorkerComplete
              {
                Proto.workerCompleteWorkerId = TL.fromStrict (ncWorkerId args)
              , Proto.workerCompleteStatus = TL.fromStrict (ncStatus args)
              , Proto.workerCompleteChanges = V.empty
              , Proto.workerCompleteMessage = TL.fromStrict (ncMessage args)
              }
          }
    result <- Events.notifyEvent (ncSessionId args) event
    case result of
      Left err -> pure $ errorResult $ pack (show err)
      Right _ -> pure $ successResult $ object ["success" .= True]
