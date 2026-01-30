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
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import ExoMonad.Guest.HostCall
import Extism.PDK (input, output)
import Foreign.C.Types (CInt (..))
import GHC.Generics (Generic)

-- Define an Effect
-- NOTE: This duplicates ExoMonad.Effects.Git because that module is excluded
-- from WASM builds in exomonad-core.
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
      Left err -> pure ("Error: " ++ err)
      Right (GetBranchResp b) -> pure b

foreign export ccall handle_mcp_call :: IO CInt

foreign export ccall handle_pre_tool_use :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ do
  -- Just echo for now
  inp <- input @ByteString
  output inp
  pure 0

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler $ do
  -- logic that uses Git
  br <- runM $ runGit $ getBranch
  let resp = object ["branch" .= br]
  output (BSL.toStrict $ Aeson.encode resp)
  pure 0

getBranch :: (Member Git effs) => Eff effs String
getBranch = send GetBranch

wrapHandler :: IO CInt -> IO CInt
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
