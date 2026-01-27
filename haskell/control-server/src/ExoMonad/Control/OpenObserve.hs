{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | OpenObserve client for shipping session transcripts.
module ExoMonad.Control.OpenObserve
  ( OpenObserveConfig(..)
  , shipTranscript
  , loadOpenObserveConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64
import Network.HTTP.Simple
import Control.Exception (try, SomeException)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forM)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)

-- | Configuration for OpenObserve ingestion.
data OpenObserveConfig = OpenObserveConfig
  { ooBaseUrl  :: Text      -- ^ e.g. "http://localhost:5080"
  , ooOrg      :: Text      -- ^ e.g. "default"
  , ooStream   :: Text      -- ^ e.g. "claude_sessions"
  , ooEmail    :: Text      -- ^ Ingestion user email
  , ooPassword :: Text      -- ^ Ingestion user password
  } deriving (Eq, Generic)

instance Show OpenObserveConfig where
  show OpenObserveConfig{..} =
    "OpenObserveConfig"
      <> " { ooBaseUrl = "  <> show ooBaseUrl
      <> ", ooOrg = "       <> show ooOrg
      <> ", ooStream = "    <> show ooStream
      <> ", ooEmail = "     <> show ooEmail
      <> ", ooPassword = "  <> "<redacted>"
      <> " }"

-- | Load OpenObserve configuration from environment variables.
loadOpenObserveConfig :: IO (Maybe OpenObserveConfig)
loadOpenObserveConfig = do
  mUrl    <- lookupEnv "OPENOBSERVE_URL"
  mOrg    <- lookupEnv "OPENOBSERVE_ORG"
  mStream <- lookupEnv "OPENOBSERVE_STREAM"
  mEmail  <- lookupEnv "OPENOBSERVE_EMAIL"
  mPass   <- lookupEnv "OPENOBSERVE_PASSWORD"

  let baseUrl  = T.pack $ fromMaybe "http://localhost:5080" mUrl
      org      = T.pack $ fromMaybe "default" mOrg
      stream   = T.pack $ fromMaybe "claude_sessions" mStream
      email    = T.pack $ fromMaybe "admin@exomonad.local" mEmail
      password = T.pack $ fromMaybe "exomonad-dev" mPass

  pure $ Just OpenObserveConfig
    { ooBaseUrl  = baseUrl
    , ooOrg      = org
    , ooStream   = stream
    , ooEmail    = email
    , ooPassword = password
    }

-- | Ship session transcript to OpenObserve.
--
-- Performs enrichment and POSTs to the JSON ingestion API in a background thread.
-- Includes exponential backoff retry logic.
shipTranscript :: OpenObserveConfig -> [Value] -> IO ()
shipTranscript config events = void $ forkIO $ retry 3 1000000
  where
    retry :: Int -> Int -> IO ()
    retry 0 _ = hPutStrLn stderr "[OpenObserve] Failed to ship transcript after all attempts"
    retry n delay = do
      result <- try @SomeException $ do
        let url = T.unpack (ooBaseUrl config) 
               <> "/api/" <> T.unpack (ooOrg config) 
               <> "/" <> T.unpack (ooStream config) 
               <> "/_json"
            auth = B64.encode $ TE.encodeUtf8 $ ooEmail config <> ":" <> ooPassword config
            
            -- Prepare request
            request = setRequestBodyLBS (encode events)
                    $ setRequestHeader "Authorization" ["Basic " <> auth]
                    $ setRequestHeader "Content-Type" ["application/json"]
                    $ setRequestMethod "POST"
                    $ parseRequest_ url
        
        response <- httpLBS request
        let status = getResponseStatusCode response
        if status >= 200 && status < 300
          then pure ()
          else ioError $ userError $ "OpenObserve returned status " <> show status

      case result of
        Left err -> do
          hPutStrLn stderr $ "[OpenObserve] Attempt failed: " <> show err
          threadDelay delay
          retry (n - 1) (delay * 2)
        Right _ -> hPutStrLn stderr $ "[OpenObserve] Successfully shipped transcript (" <> show (length events) <> " events)"
