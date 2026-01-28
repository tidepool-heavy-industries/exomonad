{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit tests for Memory effect serialization.
--
-- Tests round-tripping of MemoryStore through JSON serialization,
-- verifying that memory scopes survive WASM instance lifecycle.
module MemorySerializationSpec (spec) where

import Test.Hspec
import Data.Aeson (ToJSON(..), FromJSON(..), Value, encode, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Monad.Freer (run)
import Data.Generics.Labels ()

import ExoMonad.Graph.Memory


-- ════════════════════════════════════════════════════════════════════════════
-- TEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Example node memory: tracks URLs visited during exploration
data ExploreMem = ExploreMem
  { urlsVisited :: [Text]
  , searchCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Another node memory type: tracks classification results
data ClassifyMem = ClassifyMem
  { categories :: [Text]
  , confidence :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Global session state example
data SessionState = SessionState
  { userId :: Text
  , turnCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "MemoryStore serialization" $ do

    it "round-trips empty store" $ do
      let store = emptyMemoryStore
          json = serializeMemoryStore store
      case restoreMemoryStore json of
        Left err -> expectationFailure $ "Failed to restore: " ++ show err
        Right restored -> restored `shouldBe` emptyMemoryStore

    it "round-trips single scope" $ do
      let mem = ExploreMem { urlsVisited = ["https://example.com"], searchCount = 5 }
          store = setScope @ExploreMem "explore" mem emptyMemoryStore
          json = serializeMemoryStore store

      case restoreMemoryStore json of
        Left err -> expectationFailure $ "Failed to restore: " ++ show err
        Right restored -> do
          case getScope @ExploreMem "explore" restored of
            Left err -> expectationFailure $ "Failed to get scope: " ++ show err
            Right Nothing -> expectationFailure "Scope not found after restore"
            Right (Just val) -> val `shouldBe` mem

    it "round-trips multiple scopes with different types" $ do
      let exploreMem = ExploreMem { urlsVisited = ["a", "b"], searchCount = 10 }
          classifyMem = ClassifyMem { categories = ["tech", "news"], confidence = 0.95 }
          sessionMem = SessionState { userId = "user123", turnCount = 42 }
          store = setScope @ExploreMem "explore" exploreMem
                $ setScope @ClassifyMem "classify" classifyMem
                $ setScope @SessionState "session" sessionMem
                $ emptyMemoryStore
          json = serializeMemoryStore store

      case restoreMemoryStore json of
        Left err -> expectationFailure $ "Failed to restore: " ++ show err
        Right restored -> do
          getScope @ExploreMem "explore" restored `shouldBe` Right (Just exploreMem)
          getScope @ClassifyMem "classify" restored `shouldBe` Right (Just classifyMem)
          getScope @SessionState "session" restored `shouldBe` Right (Just sessionMem)

    it "handles missing scope gracefully" $ do
      let store = emptyMemoryStore
      getScope @ExploreMem "nonexistent" store `shouldBe` Right Nothing

    it "returns error for type mismatch" $ do
      -- Store ExploreMem but try to read as ClassifyMem
      let mem = ExploreMem { urlsVisited = ["url"], searchCount = 1 }
          store = setScope @ExploreMem "explore" mem emptyMemoryStore

      case getScope @ClassifyMem "explore" store of
        Left err -> T.unpack err `shouldSatisfy` ("Failed to deserialize" `isInfixOf`)
        Right _ -> expectationFailure "Should have failed with type mismatch"

    it "rejects unknown snapshot version" $ do
      -- Manually construct a snapshot with wrong version
      let badSnapshot = Aeson.object
            [ "version" Aeson..= (99 :: Int)
            , "scopes" Aeson..= Aeson.object []
            ]
      case restoreMemoryStore badSnapshot of
        Left err -> T.unpack err `shouldSatisfy` ("Unsupported snapshot version" `isInfixOf`)
        Right _ -> expectationFailure "Should have rejected version 99"

  describe "runMemoryScoped" $ do

    it "initializes from empty store with default value" $ do
      let defaultMem = ExploreMem { urlsVisited = [], searchCount = 0 }
          action = getMem @ExploreMem
          (result, _) = run $ runMemoryScoped "test" defaultMem emptyMemoryStore action
      result `shouldBe` defaultMem

    it "loads existing scope value" $ do
      let mem = ExploreMem { urlsVisited = ["stored"], searchCount = 99 }
          store = setScope @ExploreMem "test" mem emptyMemoryStore
          defaultMem = ExploreMem { urlsVisited = [], searchCount = 0 }
          action = getMem @ExploreMem
          (result, _) = run $ runMemoryScoped "test" defaultMem store action
      result `shouldBe` mem

    it "persists updates to store" $ do
      let defaultMem = ExploreMem { urlsVisited = [], searchCount = 0 }
          action = do
            modifyMem @ExploreMem #searchCount (+ 1)
            getMem @ExploreMem
          (result, finalStore) = run $ runMemoryScoped "test" defaultMem emptyMemoryStore action
      result.searchCount `shouldBe` 1
      -- Verify it's actually in the store
      case getScope @ExploreMem "test" finalStore of
        Right (Just val) -> val.searchCount `shouldBe` 1
        _ -> expectationFailure "Value not persisted to store"

    it "isolates different scope names" $ do
      let mem1 = ExploreMem { urlsVisited = ["a"], searchCount = 1 }
          mem2 = ExploreMem { urlsVisited = ["b"], searchCount = 2 }
          store = setScope @ExploreMem "node1" mem1
                $ setScope @ExploreMem "node2" mem2
                $ emptyMemoryStore
          defaultMem = ExploreMem { urlsVisited = [], searchCount = 0 }
          -- Read from node1
          (result1, _) = run $ runMemoryScoped "node1" defaultMem store (getMem @ExploreMem)
          -- Read from node2
          (result2, _) = run $ runMemoryScoped "node2" defaultMem store (getMem @ExploreMem)
      result1 `shouldBe` mem1
      result2 `shouldBe` mem2

  describe "MemorySnapshot JSON format" $ do

    it "produces expected JSON structure" $ do
      let mem = ExploreMem { urlsVisited = ["url1"], searchCount = 5 }
          store = setScope @ExploreMem "explore" mem emptyMemoryStore
          json = serializeMemoryStore store

      -- Check structure
      case json of
        Aeson.Object obj -> do
          KM.lookup "version" obj `shouldBe` Just (Aeson.Number 1)
          case KM.lookup "scopes" obj of
            Just (Aeson.Object scopes) -> do
              KM.lookup "explore" scopes `shouldNotBe` Nothing
            _ -> expectationFailure "Expected scopes object"
        _ -> expectationFailure "Expected object at top level"

    it "can be encoded/decoded as ByteString" $ do
      let mem = ExploreMem { urlsVisited = ["test"], searchCount = 42 }
          store = setScope @ExploreMem "mynode" mem emptyMemoryStore
          json = serializeMemoryStore store
          bs = encode json

      case decode @Value bs of
        Nothing -> expectationFailure "Failed to decode JSON ByteString"
        Just decoded -> case restoreMemoryStore decoded of
          Left err -> expectationFailure $ "Failed to restore: " ++ show err
          Right restored ->
            getScope @ExploreMem "mynode" restored `shouldBe` Right (Just mem)
