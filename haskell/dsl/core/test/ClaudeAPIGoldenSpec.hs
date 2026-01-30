{-# LANGUAGE TypeApplications #-}

module ClaudeAPIGoldenSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import ExoMonad.Anthropic.Types
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | Load a fixture from the project root's test/fixtures/claude-api directory
loadFixture :: FilePath -> IO BL.ByteString
loadFixture path = do
  cwd <- getCurrentDirectory
  -- Handle running from root or package dir
  let root =
        if "haskell/dsl/core" `isSuffixOf` cwd
          then "../../.."
          else "."
  let fullPath = root </> "test/fixtures/claude-api" </> path
  BL.readFile fullPath

spec :: Spec
spec = describe "Claude API Golden Tests" $ do
  describe "MessagesRequest Serialization" $ do
    it "matches request/simple_message.json" $ do
      fixture <- loadFixture "request/simple_message_expanded.json"
      let decoded = eitherDecode @MessagesRequest fixture
      decoded `shouldSatisfy` isRight
      let Right req = decoded
      let encoded = encode req
      -- Re-decode encoded to compare structure (handling potential whitespace diffs)
      let reDecoded = eitherDecode @MessagesRequest encoded
      reDecoded `shouldBe` Right req

      -- Verify key fields match strictly
      -- (Here we rely on the fact that if it decodes from the golden file,
      -- and re-encodes/decodes to the same thing, it's structurally compatible.
      -- To be 100% sure about snake_case keys, we should check Value equality)

      let valFixture :: Either String Data.Aeson.Value = eitherDecode fixture
      let valEncoded :: Either String Data.Aeson.Value = eitherDecode encoded
      valEncoded `shouldBe` valFixture

    it "matches request/with_tools.json" $ do
      fixture <- loadFixture "request/with_tools_expanded.json"
      let valFixture :: Either String Data.Aeson.Value = eitherDecode fixture

      -- We need to construct the Haskell object or decode it first.
      -- Since we don't have a partial FromJSON (it requires all fields),
      -- we rely on the fact that the fixture IS valid for our type.
      let decoded = eitherDecode @MessagesRequest fixture
      decoded `shouldSatisfy` isRight

      let Right req = decoded
      let encoded = encode req
      let valEncoded :: Either String Data.Aeson.Value = eitherDecode encoded

      valEncoded `shouldBe` valFixture

  describe "MessagesResponse Deserialization" $ do
    it "matches response/text_response.json" $ do
      fixture <- loadFixture "response/text_response.json"
      let decoded = eitherDecode @MessagesResponse fixture
      decoded `shouldSatisfy` isRight
      let Right resp = decoded

      -- Verify content
      case resp.content of
        (Text t :| []) -> t `shouldBe` "Hello!"
        _ -> expectationFailure "Expected single text block"

      -- Verify stop reason
      resp.stopReason `shouldBe` EndTurn

      -- Verify usage
      resp.usage.inputTokens `shouldBe` 10
      resp.usage.outputTokens `shouldBe` 5

    it "matches response/tool_use_response.json" $ do
      fixture <- loadFixture "response/tool_use_response.json"
      let decoded = eitherDecode @MessagesResponse fixture
      decoded `shouldSatisfy` isRight
      let Right resp = decoded

      -- Verify content
      let blocks = resp.content
      length blocks `shouldBe` 2

      case blocks of
        (Text t :| [ToolUse id' name' input']) -> do
          t `shouldBe` "I will check the weather."
          id'.unToolUseId `shouldBe` "toolu_01234"
          name' `shouldBe` "get_weather"
          -- Verify input value (simplified check)
          encode input' `shouldBe` "{\"location\":\"San Francisco\"}"
        _ -> expectationFailure "Expected Text then ToolUse"

      -- Verify stop reason
      resp.stopReason `shouldBe` ToolUseStop

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
