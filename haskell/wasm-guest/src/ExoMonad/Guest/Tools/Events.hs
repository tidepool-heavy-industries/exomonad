{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Events
  ( NotifyParent (..),
  )
where

import Control.Monad (void)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Effects.Log qualified as Log
import ExoMonad.Effect.Class (runEffect_)
import ExoMonad.Effects.Events qualified as Events
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Proto (fromText)
import ExoMonad.Guest.Tool.Class (MCPTool (..), liftEffect)
import GHC.Generics (Generic)

-- | Notify parent tool (for workers/subtrees to call on completion)
data NotifyParent = NotifyParent

data NotifyParentArgs = NotifyParentArgs
  { npStatus :: Text,
    npMessage :: Text
  }
  deriving (Generic, Show)

instance FromJSON NotifyParentArgs where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}

instance ToJSON NotifyParentArgs where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}

instance MCPTool NotifyParent where
  type ToolArgs NotifyParent = NotifyParentArgs
  toolName = "notify_parent"
  toolDescription = "Notify your parent session that you have completed. Resolves worker ID and routing automatically."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "status"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["success", "failure"] :: [Text]),
                    "description" .= ("Completion status (success/failure)" :: Text)
                  ],
              "message"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Summary of work done (on success) or what went wrong (on failure)" :: Text)
                  ]
            ],
        "required" .= (["status", "message"] :: [Text])
      ]
  toolHandler args = do
    emitStructuredEvent "agent.completed" $
      object
        [ "status" .= npStatus args,
          "message" .= npMessage args
        ]
    liftEffect (Events.notifyParent "" (npStatus args) (npMessage args)) $ \_ ->
      object ["success" .= True]

emitStructuredEvent :: Text -> Aeson.Value -> IO ()
emitStructuredEvent eventType payload =
  void $
    runEffect_ @LogEmitEvent
      Log.EmitEventRequest
        { Log.emitEventRequestEventType = fromText eventType,
          Log.emitEventRequestPayload = BSL.toStrict (Aeson.encode payload),
          Log.emitEventRequestTimestamp = 0
        }
