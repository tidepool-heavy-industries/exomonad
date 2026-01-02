{-# LANGUAGE OverloadedStrings #-}

-- | Tests that verify FFI output matches the unified Registry.
--
-- With the unified Registry (Phase 4), this test now:
-- 1. Uses the unified getGraphInfo FFI function (takes graphId)
-- 2. Compares against registryGraphSpecs (single source of truth)
--
-- The sync guarantee is now structural: both FFI and codegen derive from the same
-- Registry entries, so they can't get out of sync.
module CodegenSyncSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, Value(..), (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.List (sort)

import Tidepool.Wasm.Registry
  ( graphIds
  , getGraphInfo
  , registryGraphSpecs
  )
import Tidepool.Generated.GraphSpecs (GraphSpec(..))


spec :: Spec
spec = describe "Codegen Sync" $ do
  describe "Registry provides unified GraphSpecs" $ do
    it "registryGraphSpecs has entries for all graphIds" $ do
      let specIds = map (\s -> s.gsId) registryGraphSpecs
      sort specIds `shouldBe` sort graphIds

    it "graph IDs are unique" $ do
      let ids = map (\s -> s.gsId) registryGraphSpecs
          nub [] = []
          nub (x:xs) = x : nub (filter (/= x) xs)
      length ids `shouldBe` length (nub ids)

  describe "Unified FFI matches Registry specs" $ do
    it "test graph info matches spec" $ do
      json <- getGraphInfo "test"
      let spec = findSpec "test"
      verifyGraphInfo json spec

    it "example graph info matches spec" $ do
      json <- getGraphInfo "example"
      let spec = findSpec "example"
      verifyGraphInfo json spec

    it "habitica graph info matches spec" $ do
      json <- getGraphInfo "habitica"
      let spec = findSpec "habitica"
      verifyGraphInfo json spec


-- | Find a GraphSpec by ID.
findSpec :: Text -> GraphSpec
findSpec gid =
  case filter (\s -> s.gsId == gid) registryGraphSpecs of
    [s] -> s
    _ -> error $ "No spec found for: " ++ T.unpack gid


-- | Verify that FFI JSON output matches a GraphSpec.
verifyGraphInfo :: Text -> GraphSpec -> IO ()
verifyGraphInfo jsonText gspec = do
  let mValue = decode (BL.fromStrict $ encodeUtf8 jsonText) :: Maybe Value

  case mValue of
    Nothing -> expectationFailure $ "Failed to parse JSON: " ++ T.unpack jsonText
    Just val -> do
      -- Extract fields from JSON
      let mId = extractField val "id" :: Maybe Text
          mName = extractField val "name" :: Maybe Text
          mNodes = extractNodes val
          mEdges = extractEdges val

      -- Verify id
      mId `shouldBe` Just gspec.gsId

      -- Verify name
      mName `shouldBe` Just gspec.gsName

      -- Verify nodes (order may differ, so sort)
      case mNodes of
        Nothing -> expectationFailure "Missing 'nodes' field in JSON"
        Just nodes -> sort nodes `shouldBe` sort gspec.gsNodes

      -- Verify edges (order may differ, so sort)
      case mEdges of
        Nothing -> expectationFailure "Missing 'edges' field in JSON"
        Just edges -> sort edges `shouldBe` sort gspec.gsEdges


-- | Extract a text field from JSON.
extractField :: Value -> Text -> Maybe Text
extractField (Object obj) key = parseMaybe (.: Key.fromText key) obj
extractField _ _ = Nothing


-- | Extract nodes array from JSON.
extractNodes :: Value -> Maybe [Text]
extractNodes (Object obj) = parseMaybe (.: Key.fromText "nodes") obj
extractNodes _ = Nothing


-- | Extract edges as (from, to) pairs from JSON.
extractEdges :: Value -> Maybe [(Text, Text)]
extractEdges (Object obj) = do
  edges <- parseMaybe (.: Key.fromText "edges") obj :: Maybe [Value]
  mapM extractEdge edges
  where
    extractEdge (Object e) = do
      from <- parseMaybe (.: Key.fromText "from") e
      to <- parseMaybe (.: Key.fromText "to") e
      pure (from, to)
    extractEdge _ = Nothing
extractEdges _ = Nothing
