-- | HTTP operations for Anthropic Messages API
-- This module contains the actual HTTP calls (requires networking)
module Tidepool.Anthropic.Http
  ( -- * HTTP Operations
    callMessages

    -- * Re-exports from Types (for convenience)
  , module Tidepool.Anthropic.Types
  ) where

import Tidepool.Anthropic.Types

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import Network.HTTP.Req

-- ══════════════════════════════════════════════════════════════
-- HTTP OPERATIONS
-- ══════════════════════════════════════════════════════════════

-- | Make a Messages API call
-- This is the core HTTP operation - no tool loop logic here
callMessages
  :: Text               -- API key
  -> MessagesRequest    -- Request
  -> IO (Either ApiError MessagesResponse)
callMessages apiKey request = runReq defaultHttpConfig $ do
  let url = https "api.anthropic.com" /: "v1" /: "messages"
      -- Include structured outputs beta header
      headers = header "x-api-key" (TE.encodeUtf8 apiKey)
             <> header "anthropic-version" ("2023-06-01" :: ByteString)
             <> header "anthropic-beta" ("structured-outputs-2025-11-13,interleaved-thinking-2025-05-14" :: ByteString)
             <> header "content-type" ("application/json" :: ByteString)

  response <- req POST url (ReqBodyJson request) jsonResponse headers

  let body = responseBody response :: Value
  case fromJSON body of
    Success resp -> pure $ Right resp
    Error err -> pure $ Left $ ParseError (T.pack err)
