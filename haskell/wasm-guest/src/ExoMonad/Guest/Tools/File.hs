-- | File tool definitions and handlers.
module ExoMonad.Guest.Tools.File
  ( -- * Tool types
    ReadFile,
    WriteFile,

    -- * Argument types (exported for tests)
    ReadFileArgs (..),
    WriteFileArgs (..),
  )
where

import Control.Monad.Freer (runM)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Guest.Effects.FileSystem qualified as FS
import ExoMonad.Guest.Tool.Class

-- ============================================================================
-- ReadFile
-- ============================================================================

-- | Read contents of a file.
data ReadFile

data ReadFileArgs = ReadFileArgs
  { rfPath :: Text,
    rfMaxLines :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadFileArgs where
  parseJSON = Aeson.withObject "ReadFileArgs" $ \v ->
    ReadFileArgs
      <$> v .: "path"
      <*> v .:? "max_lines"

instance MCPTool ReadFile where
  type ToolArgs ReadFile = ReadFileArgs
  toolName = "read_file"
  toolDescription = "Read contents of a file"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["path"] :: [Text]),
        "properties"
          .= object
            [ "path"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("File path (relative to project root or absolute)" :: Text)
                  ],
              "max_lines"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum lines to read (default: unlimited)" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let maxBytes = maybe 0 (* 100) (rfMaxLines args) -- Rough estimate: 100 bytes/line
    result <- runM $ FS.runFileSystem $ FS.readFile (rfPath args) maxBytes
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult $ Aeson.toJSON output

-- ============================================================================
-- WriteFile
-- ============================================================================

-- | Write contents to a file.
data WriteFile

data WriteFileArgs = WriteFileArgs
  { wfPath :: Text,
    wfContent :: Text,
    wfCreateParents :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON WriteFileArgs where
  parseJSON = Aeson.withObject "WriteFileArgs" $ \v ->
    WriteFileArgs
      <$> v .: "path"
      <*> v .: "content"
      <*> v .:? "create_parents"

instance MCPTool WriteFile where
  type ToolArgs WriteFile = WriteFileArgs
  toolName = "write_file"
  toolDescription = "Write contents to a file"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["path", "content"] :: [Text]),
        "properties"
          .= object
            [ "path"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("File path (relative to project root or absolute)" :: Text)
                  ],
              "content"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Content to write to the file" :: Text)
                  ],
              "create_parents"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("Create parent directories if they don't exist (default: true)" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let createParents = maybe True id (wfCreateParents args)
    result <- runM $ FS.runFileSystem $ FS.writeFile (wfPath args) (wfContent args) createParents
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult $ Aeson.toJSON output
