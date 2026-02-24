{-# LANGUAGE TypeApplications #-}

-- | File PR tool - creates or updates a PR for the current branch.
module ExoMonad.Guest.Tools.FilePR
  ( FilePR,
    FilePRArgs (..),
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
import Effects.EffectError (EffectError (..))
import Effects.FilePr qualified as FP
import Effects.Log qualified as Log
import ExoMonad.Effects.FilePR (FilePRFilePr)
import ExoMonad.Effects.Log (LogEmitEvent, LogError, LogInfo)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, MCPTool (..), errorResult, successResult)
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import GHC.Generics (Generic)

-- | File PR tool marker type.
data FilePR

-- | Arguments for file_pr tool.
data FilePRArgs = FilePRArgs
  { fpTitle :: Text,
    fpBody :: Text,
    fpBaseBranch :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON FilePRArgs where
  parseJSON = withObject "FilePRArgs" $ \v ->
    FilePRArgs
      <$> v .: "title"
      <*> v .: "body"
      <*> v .:? "base_branch"

-- | Output type for formatting the response.
data FilePROutput = FilePROutput
  { fpoUrl :: Text,
    fpoNumber :: Int,
    fpoHeadBranch :: Text,
    fpoBaseBranch :: Text,
    fpoCreated :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON FilePROutput where
  toJSON (FilePROutput u n h b c) =
    object
      [ "pr_url" .= u,
        "pr_number" .= n,
        "head_branch" .= h,
        "base_branch" .= b,
        "created" .= c
      ]

instance MCPTool FilePR where
  type ToolArgs FilePR = FilePRArgs
  toolName = "file_pr"
  toolDescription = "Create or update a pull request for the current branch. Idempotent — safe to call multiple times (updates existing PR). Pushes the branch automatically. Base branch auto-detected from dot-separated naming (e.g. main.foo.bar targets main.foo)."
  toolSchema =
    genericToolSchemaWith @FilePRArgs
      [ ("title", "PR title"),
        ("body", "PR body/description"),
        ("base_branch", "Target branch. Auto-detected from dot-separated naming if omitted (main.foo.bar targets main.foo). Only set to override.")
      ]
  toolHandlerEff args = do

    let req =
          FP.FilePrRequest
            { FP.filePrRequestTitle = TL.fromStrict (fpTitle args),
              FP.filePrRequestBody = TL.fromStrict (fpBody args),
              FP.filePrRequestBaseBranch = maybe "" TL.fromStrict (fpBaseBranch args)
            }

    -- Log before calling host
    void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = "FilePR: Calling file_pr effect", Log.infoRequestFields = ""})

    result <- suspendEffect @FilePRFilePr req

    -- Log the result
    case result of
      Left err -> do
        void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("FilePR: effect failed: " <> T.pack (show err)), Log.errorRequestFields = ""})
        pure $ errorResult (T.pack (show err))
      Right resp -> do
        let output =
              FilePROutput
                { fpoUrl = TL.toStrict (FP.filePrResponsePrUrl resp),
                  fpoNumber = fromIntegral (FP.filePrResponsePrNumber resp),
                  fpoHeadBranch = TL.toStrict (FP.filePrResponseHeadBranch resp),
                  fpoBaseBranch = TL.toStrict (FP.filePrResponseBaseBranch resp),
                  fpoCreated = FP.filePrResponseCreated resp
                }
        void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("FilePR: Success - PR #" <> T.pack (show $ fpoNumber output)), Log.infoRequestFields = ""})
        let eventPayload = BSL.toStrict $ Aeson.encode $ object
              [ "pr_number" .= fpoNumber output,
                "pr_url" .= fpoUrl output,
                "head_branch" .= fpoHeadBranch output,
                "base_branch" .= fpoBaseBranch output,
                "created" .= fpoCreated output
              ]
        void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
          { Log.emitEventRequestEventType = "pr.filed",
            Log.emitEventRequestPayload = eventPayload,
            Log.emitEventRequestTimestamp = 0
          })
        pure $ successResult (Aeson.toJSON output)
