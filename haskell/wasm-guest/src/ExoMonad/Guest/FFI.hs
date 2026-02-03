{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.Guest.FFI where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=), (.!=), eitherDecode, encode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Word (Word64)
import Data.Aeson (Value)

-- | Error codes matching Rust side
data ErrorCode 
  = NotFound
  | NotAuthenticated
  | GitError
  | IoError
  | NetworkError
  | InvalidInput
  | InternalError
  | Timeout
  | AlreadyExists
  deriving (Show, Eq, Generic)

instance ToJSON ErrorCode where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.constructorTagModifier = Aeson.camelTo2 '_' }

instance FromJSON ErrorCode where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.constructorTagModifier = Aeson.camelTo2 '_' }

-- | Rich context for debugging errors.
data ErrorContext = ErrorContext
  { errorContextCommand :: Maybe Text
  , errorContextExitCode :: Maybe Int
  , errorContextStderr :: Maybe Text
  , errorContextStdout :: Maybe Text
  , errorContextFilePath :: Maybe Text
  , errorContextWorkingDir :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorContext where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 12, Aeson.omitNothingFields = True }

instance FromJSON ErrorContext where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 12 }

-- | Structured error returned to the WASM guest.
data FFIError = FFIError
  { feMessage :: Text
  , feCode :: ErrorCode
  , feContext :: Maybe ErrorContext
  , feSuggestion :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON FFIError where
  toJSON (FFIError msg code ctx sugg) = 
    object
      [ "message" .= msg
      , "code" .= code
      , "context" .= ctx
      , "suggestion" .= sugg
      ]

instance FromJSON FFIError where
  parseJSON = withObject "FFIError" $ \v ->
    FFIError
      <$> v .: "message"
      <*> v .:? "code" .!= InternalError
      <*> v .:? "context"
      <*> v .:? "suggestion"

-- | Standardized result envelope.
data FFIResult a = FFISuccess a | FFIErrorResult FFIError
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (FFIResult a) where
  toJSON (FFISuccess payload) = 
    object
      [ "kind" .= ("Success" :: Text)
      , "payload" .= payload
      ]
  toJSON (FFIErrorResult err) = 
    object
      [ "kind" .= ("Error" :: Text)
      , "payload" .= err
      ]

instance (FromJSON a) => FromJSON (FFIResult a) where
  parseJSON = withObject "FFIResult" $ \v -> do
    kind <- v .: "kind"
    case kind :: Text of
      "Success" -> FFISuccess <$> v .: "payload"
      "Error" -> FFIErrorResult <$> v .: "payload"
      _ -> fail "Unknown result kind"

-- | Typeclass for types crossing the FFI boundary
class (ToJSON a, FromJSON a) => FFIBoundary a where
  toFFI :: a -> ByteString
  toFFI = encode

  fromFFI :: ByteString -> Either FFIError a
  fromFFI bs = 
    case eitherDecode bs of
      Left err -> Left $ FFIError (T.pack err) InternalError Nothing (Just "JSON decode failed")
      Right res -> 
        case res of
          FFISuccess val -> Right val
          FFIErrorResult err -> Left err

instance (FFIBoundary a) => FFIBoundary [a]
instance (FFIBoundary a) => FFIBoundary (Maybe a)
instance (FFIBoundary a) => FFIBoundary (FFIResult a)
instance FFIBoundary Text
instance FFIBoundary Bool
instance FFIBoundary Int
instance FFIBoundary Word64
instance FFIBoundary ()
instance FFIBoundary Value
