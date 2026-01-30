{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (SomeException, try)
import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text, pack)
import ExoMonad.Guest.HostCall
import Extism.PDK (input, output)
import Foreign.C.Types (CInt (..))
import GHC.Generics (Generic)

-- Define an Effect
data Git r where
  GetBranch :: Git String

-- Request/Response types for Host Call
data GetBranchReq = GetBranchReq
  deriving (Show, Generic)

instance ToJSON GetBranchReq

data GetBranchResp = GetBranchResp {branch :: String}
  deriving (Show, Generic)

instance FromJSON GetBranchResp

instance ToJSON GetBranchResp

runGit :: (LastMember IO effs) => Eff (Git ': effs) a -> Eff effs a
runGit = interpret $ \case
  GetBranch -> sendM $ do
    res <- callHost host_git_get_branch GetBranchReq
    case res of
      Left err -> error err
      Right (GetBranchResp b) -> pure b

foreign export ccall handle_mcp_call :: IO Int32

foreign export ccall handle_pre_tool_use :: IO Int32

handle_mcp_call :: IO Int32
handle_mcp_call = wrapHandler $ do
  -- Just echo for now
  inp <- input
  output inp
  pure 0

handle_pre_tool_use :: IO Int32
handle_pre_tool_use = wrapHandler $ do
  -- logic that uses Git
  br <- runM $ runGit $ getBranch
  let resp = object ["branch" .= br]
  output (BSL.toStrict $ Aeson.encode resp)
  pure 0

getBranch :: (Member Git effs) => Eff effs String
getBranch = send GetBranch

wrapHandler :: IO Int32 -> IO Int32
wrapHandler action = do
  res <- try @SomeException action
  case res of
    Right code -> pure code
    Left err -> do
      -- Write error to output
      let errJson = Aeson.encode $ object ["error" .= show err]
      output (BSL.toStrict errJson)
      pure 1

main :: IO ()
main = pure ()
