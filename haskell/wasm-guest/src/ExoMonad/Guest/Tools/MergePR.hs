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
import GHC.Generics (Generic)

data MergePR

data MergePRArgs = MergePRArgs
  { mprNumber :: Int,
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
    mpoJjFetched :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON MergePROutput where
  toJSON (MergePROutput s m j) =
    object
      [ "success" .= s,
        "message" .= m,
        "jj_fetched" .= j
      ]

instance MCPTool MergePR where
  type ToolArgs MergePR = MergePRArgs
  toolName = "merge_pr"
  toolDescription = "Merge a GitHub pull request and fetch changes via jj"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["pr_number"] :: [Text]),
        "properties"
          .= object
            [ "pr_number"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("PR number to merge" :: Text)
                  ],
              "strategy"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Merge strategy: squash (default), merge, or rebase" :: Text)
                  ],
              "working_dir"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Working directory for git/jj operations" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let req =
          MP.MergePrRequest
            { MP.mergePrRequestPrNumber = fromIntegral (mprNumber args),
              MP.mergePrRequestStrategy = maybe "" TL.fromStrict (mprStrategy args),
              MP.mergePrRequestWorkingDir = maybe "" TL.fromStrict (mprWorkingDir args)
            }
    _ <- runEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: Merging PR #" <> T.pack (show (mprNumber args))), Log.infoRequestFields = ""})
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
                  mpoJjFetched = MP.mergePrResponseJjFetched resp
                }
        _ <- runEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: " <> mpoMessage output), Log.infoRequestFields = ""})
        emitStructuredEvent "pr.merged" $
          object
            [ "pr_number" .= mprNumber args,
              "success" .= mpoSuccess output
            ]
        pure $ successResult (Aeson.toJSON output)
