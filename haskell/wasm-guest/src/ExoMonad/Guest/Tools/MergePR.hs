{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.MergePR
  ( MergePR,
    MergePRArgs (..),
  )
where

import Data.Aeson (FromJSON, Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import Effects.MergePr qualified as MP
import ExoMonad.Effect.Class (runEffect, runEffect_)
import ExoMonad.Effects.Log (LogError, LogInfo, emitStructuredEvent)
import ExoMonad.Effects.MergePR (MergePRMergePr)
import ExoMonad.Guest.Tool.Class
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
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
  toolHandler args = do
    let req =
          MP.MergePrRequest
            { MP.mergePrRequestPrNumber = fromIntegral (mprPrNumber args),
              MP.mergePrRequestStrategy = maybe "" TL.fromStrict (mprStrategy args),
              MP.mergePrRequestWorkingDir = maybe "" TL.fromStrict (mprWorkingDir args)
            }
    _ <- runEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: Merging PR #" <> T.pack (show (mprPrNumber args))), Log.infoRequestFields = ""})
    result <- runEffect @MergePRMergePr req
    case result of
      Left err -> do
        _ <- runEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("MergePR: failed: " <> T.pack (show err)), Log.errorRequestFields = ""})
        pure $ errorResult (T.pack (show err))
      Right resp -> do
        let output =
              MergePROutput
                { mpoSuccess = MP.mergePrResponseSuccess resp,
                  mpoMessage = TL.toStrict (MP.mergePrResponseMessage resp),
                  mpoGitFetched = MP.mergePrResponseGitFetched resp
                }
        _ <- runEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: " <> mpoMessage output), Log.infoRequestFields = ""})
        emitStructuredEvent "pr.merged" $
          object
            [ "pr_number" .= mprPrNumber args,
              "success" .= mpoSuccess output
            ]
        pure $ successResult $
          object
            [ "success" .= mpoSuccess output,
              "message" .= mpoMessage output,
              "git_fetched" .= mpoGitFetched output,
              "next" .= ("Verify build: cargo check --workspace. Especially important after parallel merges — changes may interact." :: Text)
            ]