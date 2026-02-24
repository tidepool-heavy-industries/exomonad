{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.MergePR
  ( MergePR,
    MergePRArgs (..),
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff, sendM)
import Data.Aeson (FromJSON, Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import Effects.MergePr qualified as MP
import ExoMonad.Effects.Log (LogEmitEvent, LogError, LogInfo)
import ExoMonad.Effects.MergePR (MergePRMergePr)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, MCPTool (..), errorResult, successResult)
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import GHC.Generics (Generic)

data MergePR

data MergePRArgs = MergePRArgs
  { mprPrNumber :: Int,
    mprStrategy :: Maybe Text,
    mprWorkingDir :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON MergePRArgs where
  parseJSON = withObject "MergePRArgs" $ \v ->
    MergePRArgs
      <$> v .: "pr_number"
      <*> v .:? "strategy"
      <*> v .:? "working_dir"


data MergePROutput = MergePROutput
  { mpoSuccess :: Bool,
    mpoMessage :: Text,
    mpoGitFetched :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON MergePROutput where
  toJSON (MergePROutput s m g) =
    object
      [ "success" .= s,
        "message" .= m,
        "git_fetched" .= g
      ]

instance MCPTool MergePR where
  type ToolArgs MergePR = MergePRArgs
  toolName = "merge_pr"
  toolDescription = "Merge a GitHub pull request and fetch changes. After merging, verify the build — especially when merging multiple PRs in parallel, as changes may interact."
  toolSchema =
    genericToolSchemaWith @MergePRArgs
      [ ("pr_number", "PR number to merge"),
        ("strategy", "Merge strategy: squash (default), merge, or rebase"),
        ("working_dir", "Working directory for git operations")
      ]
  toolHandlerEff args = do
    let req =
          MP.MergePrRequest
            { MP.mergePrRequestPrNumber = fromIntegral (mprPrNumber args),
              MP.mergePrRequestStrategy = maybe "" TL.fromStrict (mprStrategy args),
              MP.mergePrRequestWorkingDir = maybe "" TL.fromStrict (mprWorkingDir args)
            }
    void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: Merging PR #" <> T.pack (show (mprPrNumber args))), Log.infoRequestFields = ""})
    result <- suspendEffect @MergePRMergePr req
    case result of
      Left err -> do
        void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("MergePR: failed: " <> T.pack (show err)), Log.errorRequestFields = ""})
        pure $ errorResult (T.pack (show err))
      Right resp -> do
        let output =
              MergePROutput
                { mpoSuccess = MP.mergePrResponseSuccess resp,
                  mpoMessage = TL.toStrict (MP.mergePrResponseMessage resp),
                  mpoGitFetched = MP.mergePrResponseGitFetched resp
                }
        void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: " <> mpoMessage output), Log.infoRequestFields = ""})
        let eventPayload = BSL.toStrict $ Aeson.encode $ object
              [ "pr_number" .= mprPrNumber args,
                "success" .= mpoSuccess output
              ]
        void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
          { Log.emitEventRequestEventType = "pr.merged",
            Log.emitEventRequestPayload = eventPayload,
            Log.emitEventRequestTimestamp = 0
          })
        pure $ successResult $
          object
            [ "success" .= mpoSuccess output,
              "message" .= mpoMessage output,
              "git_fetched" .= mpoGitFetched output,
              "next" .= ("Verify build: cargo check --workspace. Especially important after parallel merges — changes may interact." :: Text)
            ]
  