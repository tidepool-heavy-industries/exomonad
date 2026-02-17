{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Events
  ( NotifyParent (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Events qualified as Events
import ExoMonad.Effects.Log (emitStructuredEvent)
import ExoMonad.Guest.Tool.Class (MCPTool (..), liftEffect)
import GHC.Generics (Generic)

-- | Notify parent tool (for workers/subtrees to call on completion)
data NotifyParent = NotifyParent

-- | Structured task report for enriched notifications.
data TaskReport = TaskReport
  { trWhat :: Text,
    trHow :: Text
  }
  deriving (Generic, Show)

instance FromJSON TaskReport where
  parseJSON = withObject "TaskReport" $ \v ->
    TaskReport <$> v .: "what" <*> v .: "how"

instance ToJSON TaskReport where
  toJSON (TaskReport w h) = object ["what" .= w, "how" .= h]

data NotifyParentArgs = NotifyParentArgs
  { npStatus :: Text,
    npMessage :: Text,
    npPrNumber :: Maybe Int,
    npTasksCompleted :: Maybe [TaskReport]
  }
  deriving (Generic, Show)

instance FromJSON NotifyParentArgs where
  parseJSON = withObject "NotifyParentArgs" $ \v ->
    NotifyParentArgs
      <$> v .: "status"
      <*> v .: "message"
      <*> v .:? "pr_number"
      <*> v .:? "tasks_completed"

instance ToJSON NotifyParentArgs where
  toJSON args =
    object
      [ "status" .= npStatus args,
        "message" .= npMessage args,
        "pr_number" .= npPrNumber args,
        "tasks_completed" .= npTasksCompleted args
      ]

instance MCPTool NotifyParent where
  type ToolArgs NotifyParent = NotifyParentArgs
  toolName = "notify_parent"
  toolDescription = "Signal to your parent that you are DONE. Call as your final action — after PR is filed, Copilot feedback addressed, and changes pushed. Status 'success' means work is review-clean. Status 'failure' means retries exhausted, escalating to parent."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "status"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["success", "failure"] :: [Text]),
                    "description" .= ("'success' = work is done and review-clean. 'failure' = exhausted retries, escalating to parent." :: Text)
                  ],
              "message"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("One-line summary. On success: what was accomplished. On failure: what went wrong." :: Text)
                  ],
              "pr_number"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("PR number if one was filed. Enables parent to immediately merge without searching." :: Text)
                  ],
              "tasks_completed"
                .= object
                  [ "type" .= ("array" :: Text),
                    "description" .= ("Array of {what, how} pairs. 'what' = task description, 'how' = verification command that was run." :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "properties"
                            .= object
                              [ "what" .= object ["type" .= ("string" :: Text)],
                                "how" .= object ["type" .= ("string" :: Text)]
                              ],
                          "required" .= (["what", "how"] :: [Text])
                        ]
                  ]
            ],
        "required" .= (["status", "message"] :: [Text])
      ]
  toolHandler args = do
    emitStructuredEvent "agent.completed" $
      object
        [ "status" .= npStatus args,
          "message" .= npMessage args,
          "pr_number" .= npPrNumber args,
          "tasks_completed" .= npTasksCompleted args
        ]
    let richMessage = composeNotifyMessage args
    liftEffect (Events.notifyParent "" (npStatus args) richMessage) $ \_ ->
      object ["success" .= True]

-- | Compose enriched notification message with PR number and task reports.
composeNotifyMessage :: NotifyParentArgs -> Text
composeNotifyMessage args =
  let base = npMessage args
      prSuffix = case npPrNumber args of
        Just n -> " (PR #" <> T.pack (show n) <> ")"
        Nothing -> ""
      taskLines = case npTasksCompleted args of
        Just tasks -> T.concat ["\n  - " <> trWhat t <> " (verified: " <> trHow t <> ")" | t <- tasks]
        Nothing -> ""
   in base <> prSuffix <> taskLines
