{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Tools.FileSystem
  ( -- * Tool Types
    ReadFile,
    ReadFileArgs (..),
    
    -- * Tool Record
    FileTools (..),
    fileTools,
  )
where

import Polysemy (runM)
import Data.Aeson (FromJSON (..), ToJSON, object, (.=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import ExoMonad.Tool.Class (MCPTool (..), successResult, errorResult)
import ExoMonad.Tool.Mode (ToolMode (..), AsHandler, mkHandler)
import ExoMonad.Effects.FileSystem qualified as FS
import GHC.Generics (Generic)
import Prelude hiding (readFile)

-- ============================================================================
-- ReadFile
-- ============================================================================
data ReadFile

data ReadFileArgs = ReadFileArgs
  { path :: Text,
    maxBytes :: Maybe Int
  }
instance ToJSON ReadFileArgs where
  toJSON (ReadFileArgs p m) =
    object
      [ "path" .= p
      , "max_bytes" .= m
      ]

instance FromJSON ReadFileArgs where
  parseJSON = Aeson.withObject "ReadFileArgs" $ \v ->
    ReadFileArgs
      <$> v .: "path"
      <*> v .:? "max_bytes"

instance MCPTool ReadFile where
  type ToolArgs ReadFile = ReadFileArgs
  toolName = "read_file"
  toolDescription = "Read file content"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["path"] :: [Text]),
        "properties" .= object
          [ "path" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to file" :: Text)
              ]
          , "max_bytes" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Maximum bytes to read (0 for unlimited)" :: Text)
              ]
          ]
      ]
  toolHandler args = do
    res <- runM $ FS.runFileSystem $ FS.readFile (path args) (fromMaybe 0 (maxBytes args))
    case res of
      Left err -> pure $ errorResult err
      Right content -> pure $ successResult $ Aeson.toJSON content

-- ============================================================================
-- Tool Record
-- ============================================================================

data FileTools mode = FileTools
  { fsReadFile :: mode :- ReadFile
  }
  deriving (Generic)

-- | Pre-built value with handlers
fileTools :: FileTools AsHandler
fileTools =
  FileTools
    { fsReadFile = mkHandler @ReadFile
    }
