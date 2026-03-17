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
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.EffectError (EffectError (..))
import Effects.FilePr qualified as FP
import Effects.Log qualified as Log
import ExoMonad.Effects.FilePR (FilePRFilePr)
import ExoMonad.Effects.Log (LogError, LogInfo)
import ExoMonad.Guest.Lifecycle.DevState (SomeDevState (..), DevState (..), describeDevPhase)
import ExoMonad.Guest.Lifecycle.DevTransitions (applyDevEvent, DevEvent (..))
import ExoMonad.Guest.Lifecycle.PhaseEffect (getDevPhase, setDevPhase, getTLPhase)
import ExoMonad.Guest.Lifecycle.TLTransitions (applyTLEvent, TLEvent (..))
import ExoMonad.Guest.Tool.Class (MCPCallOutput, MCPTool (..), errorResult, successResult)
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
    -- Phase check: log warning for unexpected phases, auto-init if no phase set
    mState <- getDevPhase
    mTLState <- getTLPhase
    case (mState, mTLState) of
      (_, Just _) -> pure () -- TL role, skip dev phase check
      (Just (SomeDevState SWorking), _) -> pure ()
      (Just (SomeDevState SSpawned), _) -> setDevPhase (SomeDevState SWorking)
      (Just other, _) ->
        void $ suspendEffect_ @LogInfo (Log.InfoRequest
          { Log.infoRequestMessage = TL.fromStrict $ "FilePR: unexpected phase " <> describeDevPhase other <> ", proceeding anyway"
          , Log.infoRequestFields = ""
          })
      (Nothing, Nothing) -> setDevPhase (SomeDevState SWorking)

    result <- filePRCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> do
        mTLStateAfter <- getTLPhase
        case mTLStateAfter of
          Just _ -> applyTLEvent (OwnPRFiled (fpoNumber output) (fpoUrl output) (fpoHeadBranch output))
          Nothing -> applyDevEvent (PRCreated (fpoNumber output) (fpoUrl output) (fpoHeadBranch output))
        pure $ successResult (Aeson.toJSON output)
