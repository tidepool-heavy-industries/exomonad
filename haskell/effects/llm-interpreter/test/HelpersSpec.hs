-- | Tests for helper functions in LLM interpreter.
--
-- Covers parseBaseUrl and clientErrorToLLMError.
module HelpersSpec (spec) where

import Test.Hspec
import ExoMonad.LLM.Types (ParsedBaseUrl(..), Scheme(..))

import ExoMonad.LLM.Interpreter (parseBaseUrl)


spec :: Spec
spec = do
  describe "parseBaseUrl" $ do
    describe "scheme detection" $ do
      it "detects https:// scheme" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        pbuScheme url `shouldBe` Https

      it "detects http:// scheme" $ do
        let url = parseBaseUrl "http://localhost:8080"
        pbuScheme url `shouldBe` Http

      it "defaults to https when no scheme" $ do
        let url = parseBaseUrl "api.anthropic.com"
        pbuScheme url `shouldBe` Https

    describe "host extraction" $ do
      it "extracts host from URL" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        pbuHost url `shouldBe` "api.anthropic.com"

      it "extracts host with port" $ do
        let url = parseBaseUrl "http://localhost:8080"
        pbuHost url `shouldBe` "localhost"

      it "extracts host with path" $ do
        let url = parseBaseUrl "https://api.anthropic.com/v1"
        pbuHost url `shouldBe` "api.anthropic.com"

    describe "port handling" $ do
      it "uses explicit port" $ do
        let url = parseBaseUrl "http://localhost:8080"
        pbuPort url `shouldBe` 8080

      it "defaults to 443 for https" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        pbuPort url `shouldBe` 443

      it "defaults to 80 for http" $ do
        let url = parseBaseUrl "http://example.com"
        pbuPort url `shouldBe` 80

      it "handles invalid port gracefully" $ do
        -- Invalid port should fall back to default
        let url = parseBaseUrl "https://example.com:invalid"
        pbuPort url `shouldBe` 443

    describe "path extraction" $ do
      it "extracts path from URL" $ do
        let url = parseBaseUrl "https://api.anthropic.com/v1"
        pbuPath url `shouldBe` "/v1"

      it "returns empty path when none" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        pbuPath url `shouldBe` ""

    describe "edge cases" $ do
      it "handles trailing slash" $ do
        let url = parseBaseUrl "https://api.anthropic.com/"
        pbuHost url `shouldBe` "api.anthropic.com"
        pbuPath url `shouldBe` ""

      it "handles empty string" $ do
        let url = parseBaseUrl ""
        pbuHost url `shouldBe` ""

      it "handles URL with all components" $ do
        let url = parseBaseUrl "https://api.example.com:9000/api/v2"
        pbuScheme url `shouldBe` Https
        pbuHost url `shouldBe` "api.example.com"
        pbuPort url `shouldBe` 9000
        pbuPath url `shouldBe` "/api/v2"
