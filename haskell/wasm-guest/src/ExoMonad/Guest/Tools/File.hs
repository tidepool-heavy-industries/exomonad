{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.File
  ( ReadFile,
    ReadFileArgs (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import ExoMonad.Guest.Tool.Class (MCPTool (..), successResult)
import GHC.Generics (Generic)

-- ReadFile
data ReadFile

data ReadFileArgs = ReadFileArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ReadFileArgs where
  parseJSON = Aeson.withObject "ReadFileArgs" $ \_ -> pure ReadFileArgs

instance MCPTool ReadFile where
  type ToolArgs ReadFile = ReadFileArgs
  toolName = "read_file"
  toolDescription = "Read file content"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = pure $ successResult $ Aeson.String "" -- Stub