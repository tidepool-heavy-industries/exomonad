{-# LANGUAGE OverloadedStrings #-}

-- | Tests that verify FFI output matches the shared GraphSpecs.
--
-- This is a critical sync test: if someone adds a graph to Ffi.hs but forgets
-- to add it to GraphSpecs.hs, this test will fail.
--
-- The test works by:
-- 1. Calling each graph's getGraphInfo_* FFI function
-- 2. Parsing the JSON response
-- 3. Comparing nodes and edges against GraphSpecs.hs
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

import Tidepool.Wasm.Ffi
  ( getGraphInfo_test
  , getGraphInfo_example
  , getGraphInfo_habitica
  )
import Tidepool.Generated.GraphSpecs
  ( allGraphSpecs
  , testGraphSpec
  , exampleGraphSpec
  , habiticaGraphSpec
  , GraphSpec(..)
  )


spec :: Spec
spec = describe "Codegen Sync" $ do
  describe "GraphSpecs matches FFI output" $ do
    it "testGraphSpec matches getGraphInfo_test" $ do
      json <- getGraphInfo_test
      verifyGraphInfo json testGraphSpec

    it "exampleGraphSpec matches getGraphInfo_example" $ do
      json <- getGraphInfo_example
      verifyGraphInfo json exampleGraphSpec

    it "habiticaGraphSpec matches getGraphInfo_habitica" $ do
      json <- getGraphInfo_habitica
      verifyGraphInfo json habiticaGraphSpec

  describe "allGraphSpecs completeness" $ do
    it "has exactly 3 graphs (update this when adding graphs)" $ do
      length allGraphSpecs `shouldBe` 3

    it "graph IDs are unique" $ do
      let ids = map (\s -> s.gsId) allGraphSpecs
      length ids `shouldBe` length (nub ids)
      where
        nub [] = []
        nub (x:xs) = x : nub (filter (/= x) xs)


-- | Verify that FFI JSON output matches a GraphSpec.
verifyGraphInfo :: Text -> GraphSpec -> IO ()
verifyGraphInfo jsonText spec = do
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
      mId `shouldBe` Just spec.gsId

      -- Verify name
      mName `shouldBe` Just spec.gsName

      -- Verify nodes (order may differ, so sort)
      case mNodes of
        Nothing -> expectationFailure "Missing 'nodes' field in JSON"
        Just nodes -> sort nodes `shouldBe` sort spec.gsNodes

      -- Verify edges (order may differ, so sort)
      case mEdges of
        Nothing -> expectationFailure "Missing 'edges' field in JSON"
        Just edges -> sort edges `shouldBe` sort spec.gsEdges


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
