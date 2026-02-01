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
import ExoMonad.Guest.HostCall (callHost, host_file_pr)
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

-- | Host result wrapper (matches Rust HostResult).
data HostResult a
  = Success a
  | HostError Text
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (HostResult a) where
  parseJSON = withObject "HostResult" $ \v -> do
    kind <- v .: "kind"
    case (kind :: Text) of
      "Success" -> Success <$> v .: "payload"
      "Error" -> do
        errObj <- v .: "payload"
        HostError <$> (errObj .: "message")
      _ -> fail "Unknown HostResult kind"

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
    result <- callHost host_file_pr input :: IO (Either String (HostResult FilePROutput))
    case result of
      Left err -> pure $ errorResult (T.pack err)
      Right (Success output) -> pure $ successResult (Aeson.toJSON output)
      Right (HostError msg) -> pure $ errorResult msg
