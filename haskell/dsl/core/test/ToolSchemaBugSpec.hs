{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ToolSchemaBugSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import ExoMonad.Schema (HasJSONSchema (..), JSONSchema (..), SchemaType (..), emptySchema, schemaToValue)
import ExoMonad.Tool.Wire (AnthropicTool (..), schemaToAnthropicTool)
import Test.Hspec

-- | A mock primitive type (Text) for testing
data MyPrimitive = MyPrimitive

instance HasJSONSchema MyPrimitive where
  jsonSchema = emptySchema TString

spec :: Spec
spec = describe "Tool Schema Generation Bug" $ do
  it "schemaToAnthropicTool should wrap primitive types in an object" $ do
    let schema = jsonSchema @MyPrimitive
        tool = schemaToAnthropicTool "my_tool" "desc" schema

    -- This test verifies that primitive types are wrapped in an object with a "value" property,
    -- producing a schema like {"type": "object", "properties": {"value": {"type": "string"}}, "required": ["value"]}.
    -- This ensures the tool input schema matches the expected wrapped representation.

    let expected =
          object
            [ "type" .= ("object" :: Text),
              "properties" .= object ["value" .= object ["type" .= ("string" :: Text)]],
              "required" .= (["value"] :: [Text]),
              "additionalProperties" .= False
            ]

    tool.atInputSchema `shouldBe` expected
