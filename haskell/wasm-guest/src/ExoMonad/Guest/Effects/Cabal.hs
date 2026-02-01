{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.Cabal
  ( -- * Effect type
    Cabal (..),
    -- * Smart constructors
    cabalBuild,
    cabalTest,
    -- * Interpreter
    runCabal,
    -- * Types
    CabalInput (..),
    BuildResult (..),
    TestResult (..),
    GHCError (..),
    TestFailure (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), withObject)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall (callHost, host_cabal_build, host_cabal_test)
import ExoMonad.Guest.Effects.FileSystem (HostResult (..))
import GHC.Generics (Generic)

-- Types

data CabalInput = CabalInput
  { ciPath :: Text,
    ciContainerId :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CabalInput where
  toJSON (CabalInput p c) = object
    [ "path" .= p
    , "containerId" .= c
    ]
instance FromJSON CabalInput where
    parseJSON = withObject "CabalInput" $ \v -> CabalInput
        <$> v .: "path"
        <*> v .: "containerId"

data GHCError = GHCError
  { geFile :: Text,
    geLine :: Int,
    geMessage :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GHCError where
  toJSON (GHCError f l m) = object ["file" .= f, "line" .= l, "message" .= m]
instance FromJSON GHCError where
  parseJSON = withObject "GHCError" $ \v -> GHCError
    <$> v .: "file"
    <*> v .: "line"
    <*> v .: "message"

data BuildResult
  = BuildSuccess
  | BuildFailed [GHCError]
  deriving (Show, Eq, Generic)

instance ToJSON BuildResult
instance FromJSON BuildResult

data TestFailure = TestFailure
  { tfName :: Text,
    tfMessage :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON TestFailure where
  toJSON (TestFailure n m) = object ["name" .= n, "message" .= m]
instance FromJSON TestFailure where
  parseJSON = withObject "TestFailure" $ \v -> TestFailure
    <$> v .: "name"
    <*> v .: "message"

data TestResult
  = TestsPassed Int
  | TestsFailed [TestFailure]
  | NoTests
  deriving (Show, Eq, Generic)

instance ToJSON TestResult
instance FromJSON TestResult

-- Effect

data Cabal r where
  CabalBuild :: Text -> Text -> Cabal (Either Text BuildResult) -- path, containerId
  CabalTest :: Text -> Text -> Cabal (Either Text TestResult) -- path, containerId

-- Smart Constructors

cabalBuild :: (Member Cabal effs) => Text -> Text -> Eff effs (Either Text BuildResult)
cabalBuild path cid = send (CabalBuild path cid)

cabalTest :: (Member Cabal effs) => Text -> Text -> Eff effs (Either Text TestResult)
cabalTest path cid = send (CabalTest path cid)

-- Interpreter

runCabal :: (LastMember IO effs) => Eff (Cabal ': effs) a -> Eff effs a
runCabal = interpret $ \case
  CabalBuild path cid -> sendM $ do
    let input = CabalInput path cid
    res <- callHost host_cabal_build input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg
  CabalTest path cid -> sendM $ do
    let input = CabalInput path cid
    res <- callHost host_cabal_test input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg