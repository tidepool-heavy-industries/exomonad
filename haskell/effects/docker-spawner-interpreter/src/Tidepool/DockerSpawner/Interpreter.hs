{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tidepool.DockerSpawner.Interpreter
  ( runDockerSpawner
  , DockerSpawnerConfig(..)
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, interpret, LastMember, sendM)
import Data.Aeson (encode, eitherDecode)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Text as T

import Tidepool.Effects.DockerSpawner

data DockerSpawnerConfig = DockerSpawnerConfig
  { dscBaseUrl :: String  -- e.g., "http://localhost:7435"
  , dscManager :: Manager
  }

runDockerSpawner
  :: LastMember IO es
  => DockerSpawnerConfig
  -> Eff (DockerSpawner ': es) a
  -> Eff es a
runDockerSpawner config = interpret $ \case
  SpawnContainer cfg -> sendM $ doSpawn config cfg
  StopContainer cid -> sendM $ doStop config cid
  GetContainerStatus cid -> sendM $ doStatus config cid

-- | Helper to wrap HTTP calls in try/catch and map to DockerError
withHttpError :: IO (Either DockerError a) -> IO (Either DockerError a)
withHttpError action = do
  result <- try @SomeException action
  case result of
    Right val -> pure val
    Left err -> pure $ Left $ DockerConnectionError (T.pack $ show err)

doSpawn :: DockerSpawnerConfig -> SpawnConfig -> IO (Either DockerError ContainerId)
doSpawn config cfg = withHttpError $ do
  let url = dscBaseUrl config <> "/spawn"
  req <- parseRequest url
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS (encode cfg)
        , requestHeaders = [("Content-Type", "application/json")]
        , responseTimeout = responseTimeoutMicro 5000000 -- 5 seconds
        }
  resp <- httpLbs req' (dscManager config)
  case statusCode (responseStatus resp) of
    200 -> case eitherDecode (responseBody resp) of
      Right cid -> pure $ Right cid
      Left err -> pure $ Left $ DockerApiError 200 (T.pack err)
    code -> pure $ Left $ DockerApiError code "Spawn failed"

doStop :: DockerSpawnerConfig -> ContainerId -> IO (Either DockerError ())
doStop config (ContainerId cid) = withHttpError $ do
  let url = dscBaseUrl config <> "/stop/" <> T.unpack cid
  req <- parseRequest url
  let req' = req 
        { method = "POST"
        , responseTimeout = responseTimeoutMicro 5000000 -- 5 seconds
        }
  resp <- httpLbs req' (dscManager config)
  case statusCode (responseStatus resp) of
    200 -> pure $ Right ()
    404 -> pure $ Left $ ContainerNotFound (ContainerId cid)
    code -> pure $ Left $ DockerApiError code "Stop failed"

doStatus :: DockerSpawnerConfig -> ContainerId -> IO (Either DockerError ContainerStatus)
doStatus config (ContainerId cid) = withHttpError $ do
  let url = dscBaseUrl config <> "/status/" <> T.unpack cid
  req <- parseRequest url
  let req' = req
        { responseTimeout = responseTimeoutMicro 5000000 -- 5 seconds
        }
  resp <- httpLbs req' (dscManager config)
  case statusCode (responseStatus resp) of
    200 -> case eitherDecode (responseBody resp) of
      Right status -> pure $ Right status
      Left err -> pure $ Left $ DockerApiError 200 (T.pack err)
    404 -> pure $ Right NotFound
    code -> pure $ Left $ DockerApiError code "Status check failed"