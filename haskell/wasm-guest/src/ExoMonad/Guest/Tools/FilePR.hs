-- | File PR tool - creates or updates a PR for the current branch.
module ExoMonad.Guest.Tools.FilePR
  ( FilePR,
    FilePRArgs (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall (LogLevel (..), LogPayload (..), callHost, callHostVoid, host_file_pr, host_log_error, host_log_info)
import ExoMonad.Guest.FFI (FFIBoundary)
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)

-- | File PR tool marker type.
data FilePR

-- | Arguments for file_pr tool.
data FilePRArgs = FilePRArgs
  { fpTitle :: Text,
    fpBody :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON FilePRArgs where
  parseJSON = withObject "FilePRArgs" $ \v ->
    FilePRArgs
      <$> v .: "title"
      <*> v .: "body"

-- | Input type for host function (matches Rust FilePRInput).
data FilePRInput = FilePRInput
  { fpiTitle :: Text,
    fpiBody :: Text
  }
  deriving (Show, Generic)

instance ToJSON FilePRInput where
  toJSON (FilePRInput t b) =
    object
      [ "title" .= t,
        "body" .= b
      ]

instance FromJSON FilePRInput where
  parseJSON = withObject "FilePRInput" $ \v ->
    FilePRInput
      <$> v .: "title"
      <*> v .: "body"

instance FFIBoundary FilePRInput

-- | Output type from host function (matches Rust FilePROutput).
data FilePROutput = FilePROutput
  { fpoUrl :: Text,
    fpoNumber :: Int,
    fpoHeadBranch :: Text,
    fpoBaseBranch :: Text,
    fpoCreated :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON FilePROutput where
  parseJSON = withObject "FilePROutput" $ \v ->
    FilePROutput
      <$> v .: "pr_url"
      <*> v .: "pr_number"
      <*> v .: "head_branch"
      <*> v .: "base_branch"
      <*> v .: "created"

instance ToJSON FilePROutput where
  toJSON (FilePROutput u n h b c) =
    object
      [ "pr_url" .= u,
        "pr_number" .= n,
        "head_branch" .= h,
        "base_branch" .= b,
        "created" .= c
      ]

instance FFIBoundary FilePROutput

instance MCPTool FilePR where
  type ToolArgs FilePR = FilePRArgs
  toolName = "file_pr"
  toolDescription = "Create or update a pull request for the current branch"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["title", "body"] :: [Text]),
        "properties"
          .= object
            [ "title"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("PR title" :: Text)
                  ],
              "body"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("PR body/description" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let input =
          FilePRInput
            { fpiTitle = fpTitle args,
              fpiBody = fpBody args
            }

    -- Log before calling host
    callHostVoid host_log_info (LogPayload Info "FilePR: Calling host_file_pr" Nothing)

    result <- callHost host_file_pr input

    -- Log the result
    case result of
      Left err -> do
        callHostVoid host_log_error (LogPayload Error ("FilePR: callHost failed: " <> err) Nothing)
        pure $ errorResult err
      Right output -> do
        callHostVoid host_log_info (LogPayload Info ("FilePR: Success - PR #" <> T.pack (show $ fpoNumber output)) Nothing)
        pure $ successResult (Aeson.toJSON output)
