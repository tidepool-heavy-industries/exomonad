{-# LANGUAGE TypeApplications #-}

-- | File PR tool - creates or updates a PR for the current branch.
--
-- 'filePRCore' contains the shared I/O logic.
-- Role-specific MCP wrappers apply their own state transitions.
module ExoMonad.Guest.Tools.FilePR
  ( FilePR,
    FilePRArgs (..),
    FilePROutput (..),
    filePRCore,
    filePRDescription,
    filePRSchema,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.FilePr qualified as FP
import Effects.Log qualified as Log
import ExoMonad.Effects.FilePR (FilePRFilePr)
import ExoMonad.Effects.Log (LogError, LogInfo)
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (Effects)
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

-- | Shared tool description (reused by role-specific MCPTool instances).
filePRDescription :: Text
filePRDescription = "Create or update a pull request for the current branch. Idempotent — safe to call multiple times (updates existing PR). Pushes the branch automatically. Base branch auto-detected from dot-separated naming (e.g. main.foo.bar targets main.foo)."

-- | Shared tool schema (reused by role-specific MCPTool instances).
filePRSchema :: Aeson.Object
filePRSchema =
  genericToolSchemaWith @FilePRArgs
    [ ("title", "PR title"),
      ("body", "PR body/description"),
      ("base_branch", "Target branch. Auto-detected from dot-separated naming if omitted (main.foo.bar targets main.foo). Only set to override.")
    ]

-- | Shared I/O logic for filing a PR. Role-specific tools call this
-- and apply their own state transitions on the result.
filePRCore :: FilePRArgs -> Eff Effects (Either Text FilePROutput)
filePRCore args = do
  let req =
        FP.FilePrRequest
          { FP.filePrRequestTitle = TL.fromStrict (fpTitle args),
            FP.filePrRequestBody = TL.fromStrict (fpBody args),
            FP.filePrRequestBaseBranch = maybe "" TL.fromStrict (fpBaseBranch args)
          }

  void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = "FilePR: Calling file_pr effect", Log.infoRequestFields = ""})

  result <- suspendEffect @FilePRFilePr req

  case result of
    Left err -> do
      void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("FilePR: effect failed: " <> T.pack (show err)), Log.errorRequestFields = ""})
      pure $ Left (T.pack (show err))
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
      pure $ Right output
