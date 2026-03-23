{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test runner tools: instruct (send message to root) and post_review (simulate Copilot).
module ExoMonad.Guest.Tools.TestRunner
  ( -- * Instruct
    instructCore,
    instructDescription,
    instructSchema,
    InstructArgs (..),

    -- * Post Review
    postReviewCore,
    postReviewDescription,
    postReviewSchema,
    PostReviewArgs (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Control.Monad.Freer (Eff)
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect)
import ExoMonad.Guest.Types (Effects)
import ExoMonad.Effects.Events qualified as ProtoEvents
import ExoMonad.Effects.Process (ProcessRun)
import Effects.Process qualified as Proc
import GHC.Generics (Generic)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Vector qualified as V

--------------------------------------------------------------------------------
-- Instruct
--------------------------------------------------------------------------------

-- | Args for the instruct tool — just content, recipient is hardcoded to "root".
data InstructArgs = InstructArgs
  { iaContent :: Text
  }
  deriving (Generic, Show)

instance FromJSON InstructArgs where
  parseJSON = withObject "InstructArgs" $ \v ->
    InstructArgs <$> v .: "content"

instance ToJSON InstructArgs where
  toJSON args = object ["content" .= iaContent args]

-- | Tool description for instruct.
instructDescription :: Text
instructDescription = "Send an instruction to the root agent under test. The recipient is always 'root' — you cannot message arbitrary agents."

-- | Tool schema for instruct.
instructSchema :: Aeson.Object
instructSchema =
  genericToolSchemaWith @InstructArgs
    [("content", "The instruction or message to send to the root agent")]

-- | Core instruct I/O: send message to root via events.send_message effect.
instructCore :: InstructArgs -> Eff Effects (Either Text Value)
instructCore args = do
  result <- suspendEffect @ProtoEvents.EventsSendMessage
              (ProtoEvents.SendMessageRequest
                { ProtoEvents.sendMessageRequestRecipient = "root",
                  ProtoEvents.sendMessageRequestContent = TL.fromStrict (iaContent args),
                  ProtoEvents.sendMessageRequestSummary = "test instruction"
                })
  case result of
    Left err -> pure $ Left ("instruct failed: " <> T.pack (show err))
    Right resp -> pure $ Right $ object
      [ "success" .= ProtoEvents.sendMessageResponseSuccess resp,
        "delivery_method" .= ProtoEvents.sendMessageResponseDeliveryMethod resp
      ]

--------------------------------------------------------------------------------
-- Post Review
--------------------------------------------------------------------------------

-- | Args for the post_review tool.
data PostReviewArgs = PostReviewArgs
  { praPrNumber :: Int
  , praState :: Text
  , praBody :: Text
  }
  deriving (Generic, Show)

instance FromJSON PostReviewArgs where
  parseJSON = withObject "PostReviewArgs" $ \v ->
    PostReviewArgs
      <$> v .: "pr_number"
      <*> v .: "state"
      <*> v .: "body"

instance ToJSON PostReviewArgs where
  toJSON args = object
    [ "pr_number" .= praPrNumber args
    , "state" .= praState args
    , "body" .= praBody args
    ]

postReviewDescription :: Text
postReviewDescription =
  "Post a simulated Copilot review to a PR via the mock GitHub API. \
  \Use this to simulate the Copilot review cycle: post CHANGES_REQUESTED \
  \with feedback (e.g. 'Add a docstring to the greet function'), then \
  \observe whether the agent addresses the feedback and pushes fixes."

postReviewSchema :: Aeson.Object
postReviewSchema =
  genericToolSchemaWith @PostReviewArgs
    [ ("pr_number", "The PR number to review")
    , ("state", "Review state: CHANGES_REQUESTED, APPROVED, or COMMENTED")
    , ("body", "Review body text — the feedback for the agent to address")
    ]

-- | Core post_review I/O: calls post_review.sh via process.run effect.
postReviewCore :: PostReviewArgs -> Eff Effects (Either Text Value)
postReviewCore args = do
  result <- suspendEffect @ProcessRun
              (Proc.RunRequest
                { Proc.runRequestCommand = "./post_review.sh"
                , Proc.runRequestArgs = V.fromList
                    [ TL.pack (show (praPrNumber args))
                    , TL.fromStrict (praState args)
                    , TL.fromStrict (praBody args)
                    ]
                , Proc.runRequestWorkingDir = "."
                , Proc.runRequestEnv = Map.empty
                , Proc.runRequestTimeoutMs = 10000
                })
  case result of
    Left err -> pure $ Left ("post_review failed: " <> T.pack (show err))
    Right resp
      | Proc.runResponseExitCode resp == 0 ->
          pure $ Right $ object
            [ "success" .= True
            , "output" .= TL.toStrict (Proc.runResponseStdout resp)
            ]
      | otherwise ->
          pure $ Left $ "post_review.sh failed (exit "
            <> T.pack (show (Proc.runResponseExitCode resp))
            <> "): " <> TL.toStrict (Proc.runResponseStderr resp)
