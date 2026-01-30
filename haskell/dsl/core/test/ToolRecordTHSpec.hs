{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for ToolRecord TH derivation.
module ToolRecordTHSpec (spec) where

import Control.Monad.Freer (Eff, run, runM)
import Data.Aeson (FromJSON, ToJSON, Value (..), toJSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import ExoMonad.LLM.Tools (ToolDispatchError (..), ToolRecord (..), ToolSchema (..), dispatchHandler)
import ExoMonad.LLM.Tools.TH (Tool (..), deriveToolRecord)
import ExoMonad.Schema (HasJSONSchema (..), deriveHasJSONSchema, schemaToValue)
import GHC.Generics (Generic)
import Test.Hspec

-- Define tool input types
data SearchArgs = SearchArgs {query :: Text}
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

deriveHasJSONSchema ''SearchArgs

data SearchResult = SearchResult {results :: [Text]}
  deriving (Generic, FromJSON, ToJSON)

data LookupArgs = LookupArgs {id :: Int}
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

deriveHasJSONSchema ''LookupArgs

data LookupResult = LookupResult
  { found :: Bool,
    name :: Maybe String
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- Tool record type
data TestTools es = TestTools
  { search :: SearchArgs -> Eff es SearchResult,
    lookupById :: LookupArgs -> Eff es LookupResult
  }

-- Derive the ToolRecord instance using TH
deriveToolRecord
  ''TestTools
  [ Tool "search" "Search for items by query string",
    Tool "lookupById" "Look up an item by its unique ID"
  ]

spec :: Spec
spec = describe "ToolRecord TH Derivation" $ do
  describe "toolSchemas" $ do
    it "generates correct tool names (camelCase to snake_case)" $ do
      let schemas = toolSchemas (Proxy @TestTools)
      length schemas `shouldBe` 2

      let names = map (\ts -> ts.tsName) schemas
      names `shouldContain` ["search"]
      names `shouldContain` ["lookup_by_id"]

    it "includes correct descriptions" $ do
      let schemas = toolSchemas (Proxy @TestTools)
      let searchSchema = head [s | s <- schemas, s.tsName == "search"]
      searchSchema.tsDescription `shouldBe` "Search for items by query string"

      let lookupSchema = head [s | s <- schemas, s.tsName == "lookup_by_id"]
      lookupSchema.tsDescription `shouldBe` "Look up an item by its unique ID"

  describe "dispatchTool" $ do
    it "dispatches to search handler correctly" $ do
      let tools =
            TestTools
              { search = \args -> pure $ SearchResult ["result1", "result2"],
                lookupById = \_ -> pure $ LookupResult True (Just "test")
              }

      result <- runM $ dispatchTool tools "search" (toJSON (SearchArgs "test query"))
      case result of
        Right val -> val `shouldBe` toJSON (SearchResult ["result1", "result2"])
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "dispatches to lookupById handler correctly" $ do
      let tools =
            TestTools
              { search = \_ -> pure $ SearchResult [],
                lookupById = \args -> pure $ LookupResult (args.id > 0) (Just "found item")
              }

      result <- runM $ dispatchTool tools "lookup_by_id" (toJSON (LookupArgs 42))
      case result of
        Right val -> val `shouldBe` toJSON (LookupResult True (Just "found item"))
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "returns ToolNotFound for unknown tools" $ do
      let tools =
            TestTools
              { search = \_ -> pure $ SearchResult [],
                lookupById = \_ -> pure $ LookupResult False Nothing
              }

      result <- runM $ dispatchTool tools "unknown_tool" (toJSON (SearchArgs "test"))
      case result of
        Left (ToolNotFound name) -> name `shouldBe` "unknown_tool"
        _ -> expectationFailure "Expected ToolNotFound error"

    it "returns parse error for invalid input" $ do
      let tools =
            TestTools
              { search = \_ -> pure $ SearchResult [],
                lookupById = \_ -> pure $ LookupResult False Nothing
              }

      -- Pass wrong JSON structure
      result <- runM $ dispatchTool tools "search" (Object mempty)
      case result of
        Left (ToolInputParseError name _) -> name `shouldBe` "search"
        _ -> expectationFailure "Expected ToolInputParseError"
