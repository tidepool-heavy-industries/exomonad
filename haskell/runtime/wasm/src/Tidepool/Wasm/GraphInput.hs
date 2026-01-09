{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Tidepool.Wasm.GraphInput
  ( GraphInput(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Options(..), SumEncoding(..), defaultOptions, genericToJSON, genericParseJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Tidepool.Anthropic.Types (ImageSource(..))

-- | Universal input type for WASM graphs that can handle text or photos.
--
-- Matches TypeScript GraphInput in protocol.ts:
-- @
-- type GraphInput =
--   | { type: "text"; text: string }
--   | { type: "photo"; caption?: string; image: { mediaType: string; data: string } }
-- @
--
-- Uses custom Aeson options to generate the correct JSON format:
-- - "type" field for discriminator (not "tag")
-- - camelCase field names (textContent → text, photoCaption → caption)
data GraphInput
  = TextInput
      { textContent :: Text
      }
  | PhotoInput
      { photoCaption :: Maybe Text
      , photoImage :: ImageSource  -- Reuses existing type from tidepool-core
      }
  deriving (Show, Eq, Generic)

-- | Custom JSON options for GraphInput to match TypeScript format.
graphInputOptions :: Options
graphInputOptions = defaultOptions
  { sumEncoding = TaggedObject
      { tagFieldName = "type"  -- Use "type" instead of "tag"
      , contentsFieldName = "contents"
      }
  , constructorTagModifier = \case
      "TextInput"  -> "text"
      "PhotoInput" -> "photo"
      other        -> other
  , fieldLabelModifier = \field ->
      case field of
        "textContent" -> "text"
        "photoCaption" -> "caption"
        "photoImage" -> "image"
        _ -> field
  , omitNothingFields = True  -- Omit caption field when Nothing
  }

instance ToJSON GraphInput where
  toJSON = genericToJSON graphInputOptions

instance FromJSON GraphInput where
  parseJSON = genericParseJSON graphInputOptions
