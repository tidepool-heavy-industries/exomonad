module Tidepool.Teaching.ConvertSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Teaching.Convert

spec :: Spec
spec = do
  describe "extractTeachingTurn" $ do
    it "extracts reasoning and tool use from Anthropic response" $ do
      let resp = [aesonQQ|
            { "content": [
                {"type": "text", "text": "Analyzing..."},
                {"type": "tool_use", "name": "select_symbols",
                 "input": {"selected": ["Foo", "Bar"]}}
              ]
            }
          |]
      let Right turn = extractTeachingTurn resp
      ttReasoning turn `shouldBe` "Analyzing..."
      ttToolName turn `shouldBe` "select_symbols"

    it "concatenates multiple text blocks with newlines" $ do
      let resp = [aesonQQ|
            { "content": [
                {"type": "text", "text": "First thought."},
                {"type": "text", "text": "Second thought."},
                {"type": "tool_use", "name": "select_symbols",
                 "input": {"selected": ["Foo"]}}
              ]
            }
          |]
      let Right turn = extractTeachingTurn resp
      ttReasoning turn `shouldBe` "First thought.\nSecond thought."

    it "handles empty text blocks" $ do
      let resp = [aesonQQ|
            { "content": [
                {"type": "tool_use", "name": "select_symbols",
                 "input": {"selected": ["Foo"]}}
              ]
            }
          |]
      let Right turn = extractTeachingTurn resp
      ttReasoning turn `shouldBe` ""

    it "returns error when no tool_use block found" $ do
      let resp = [aesonQQ|
            { "content": [
                {"type": "text", "text": "Just text"}
              ]
            }
          |]
      extractTeachingTurn resp `shouldSatisfy` isLeft

    it "returns error when content is missing" $ do
      let resp = object []
      extractTeachingTurn resp `shouldSatisfy` isLeft

  describe "formatFunctionCall" $ do
    it "formats array arguments with escape delimiters" $ do
      let args = object ["selected" .= (["A", "B", "C"] :: [Text])]
      formatFunctionCall "select_symbols" args `shouldBe`
        "select_symbols{selected:<escape>A,B,C<escape>}"

    it "formats string arguments" $ do
      let args = object ["name" .= ("foo" :: Text)]
      formatFunctionCall "get_info" args `shouldBe`
        "get_info{name:<escape>foo<escape>}"

    it "formats number arguments" $ do
      let args = object ["count" .= (42 :: Int)]
      T.unpack (formatFunctionCall "set_value" args) `shouldContain` "count:<escape>42"

    it "formats boolean arguments" $ do
      let args = object ["enabled" .= True]
      T.unpack (formatFunctionCall "toggle" args) `shouldContain` "enabled:<escape>true<escape>"

    it "formats multiple fields separated by commas" $ do
      let args = object
            [ "selected" .= (["A", "B"] :: [Text])
            , "count" .= (5 :: Int)
            ]
      let result = formatFunctionCall "multi_arg" args
      T.unpack result `shouldContain` "selected:<escape>A,B<escape>"
      -- Number might be formatted as "5" or "5.0" depending on aeson version
      T.unpack result `shouldContain` "count:<escape>5"

  describe "anthropicToFunctionGemma" $ do
    it "formats complete conversation with all turns" $ do
      let turn = TeachingTurn
            { ttReasoning = "Selecting symbols..."
            , ttToolName = "select_symbols"
            , ttToolArgs = object ["selected" .= (["Foo", "Bar"] :: [Text])]
            }
      let gemma = anthropicToFunctionGemma "Context here" turn
      T.unpack gemma `shouldContain` "/* Selecting symbols... */"
      T.unpack gemma `shouldContain` "call:select_symbols{selected:<escape>Foo,Bar<escape>}"
      T.unpack gemma `shouldContain` "<start_of_turn>model"
      T.unpack gemma `shouldContain` "<start_of_turn>user"
      T.unpack gemma `shouldContain` "<start_of_turn>developer"
      T.unpack gemma `shouldContain` "Context here"

    it "wraps result as valid JSONL" $ do
      let turn = TeachingTurn
            { ttReasoning = "Test"
            , ttToolName = "select_symbols"
            , ttToolArgs = object ["selected" .= ([] :: [Text])]
            }
      let gemma = anthropicToFunctionGemma "Query" turn
      T.unpack gemma `shouldContain` "{\"text\":"

    it "includes developer system prompt" $ do
      let turn = TeachingTurn
            { ttReasoning = ""
            , ttToolName = "select_symbols"
            , ttToolArgs = object ["selected" .= ([] :: [Text])]
            }
      let gemma = anthropicToFunctionGemma "Query" turn
      T.unpack gemma `shouldContain` "You are a semantic code analysis assistant."

    it "includes function declaration" $ do
      let turn = TeachingTurn
            { ttReasoning = ""
            , ttToolName = "select_symbols"
            , ttToolArgs = object ["selected" .= ([] :: [Text])]
            }
      let gemma = anthropicToFunctionGemma "Query" turn
      T.unpack gemma `shouldContain` "<start_function_declaration>select_symbols"
      T.unpack gemma `shouldContain` "<end_function_declaration>"

  describe "formatGemmaConversation" $ do
    it "formats 3-turn conversation structure" $ do
      let turn = TeachingTurn
            { ttReasoning = "Thinking..."
            , ttToolName = "select_symbols"
            , ttToolArgs = object ["selected" .= (["X"] :: [Text])]
            }
      let conv = formatGemmaConversation "User query" turn
      T.unpack conv `shouldContain` "<start_of_turn>developer"
      T.unpack conv `shouldContain` "<end_of_turn>"
      T.unpack conv `shouldContain` "<start_of_turn>user"
      T.unpack conv `shouldContain` "User query"
      T.unpack conv `shouldContain` "<start_of_turn>model"
      T.unpack conv `shouldContain` "/* Thinking... */"
      T.unpack conv `shouldContain` "<start_function_call>"
      T.unpack conv `shouldContain` "<end_function_call>"

    it "handles empty reasoning without comment markers" $ do
      let turn = TeachingTurn
            { ttReasoning = ""
            , ttToolName = "select_symbols"
            , ttToolArgs = object ["selected" .= ([] :: [Text])]
            }
      let conv = formatGemmaConversation "Query" turn
      T.unpack conv `shouldNotContain` "/*"
      T.unpack conv `shouldNotContain` "*/"

  describe "wrapAsJSONL" $ do
    it "wraps text in JSON object with text field" $ do
      let jsonl = wrapAsJSONL "Hello, world!"
      T.unpack jsonl `shouldContain` "{\"text\":"
      T.unpack jsonl `shouldContain` "Hello, world!"

-- Helper to check Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
