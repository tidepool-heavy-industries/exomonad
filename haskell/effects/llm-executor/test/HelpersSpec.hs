-- | Tests for helper functions in LLM executor.
--
-- Covers parseBaseUrl and clientErrorToLLMError.
module HelpersSpec (spec) where

import Test.Hspec
import Servant.Client (BaseUrl(..), Scheme(..))

import Tidepool.LLM.Executor (parseBaseUrl)


spec :: Spec
spec = do
  describe "parseBaseUrl" $ do
    describe "scheme detection" $ do
      it "detects https:// scheme" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        baseUrlScheme url `shouldBe` Https

      it "detects http:// scheme" $ do
        let url = parseBaseUrl "http://localhost:8080"
        baseUrlScheme url `shouldBe` Http

      it "defaults to https when no scheme" $ do
        let url = parseBaseUrl "api.anthropic.com"
        baseUrlScheme url `shouldBe` Https

    describe "host extraction" $ do
      it "extracts host from URL" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        baseUrlHost url `shouldBe` "api.anthropic.com"

      it "extracts host with port" $ do
        let url = parseBaseUrl "http://localhost:8080"
        baseUrlHost url `shouldBe` "localhost"

      it "extracts host with path" $ do
        let url = parseBaseUrl "https://api.openai.com/v1"
        baseUrlHost url `shouldBe` "api.openai.com"

    describe "port handling" $ do
      it "uses explicit port" $ do
        let url = parseBaseUrl "http://localhost:8080"
        baseUrlPort url `shouldBe` 8080

      it "defaults to 443 for https" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        baseUrlPort url `shouldBe` 443

      it "defaults to 80 for http" $ do
        let url = parseBaseUrl "http://example.com"
        baseUrlPort url `shouldBe` 80

      it "handles invalid port gracefully" $ do
        -- Invalid port should fall back to default
        let url = parseBaseUrl "https://example.com:invalid"
        baseUrlPort url `shouldBe` 443

    describe "path extraction" $ do
      it "extracts path from URL" $ do
        let url = parseBaseUrl "https://api.openai.com/v1"
        baseUrlPath url `shouldBe` "/v1"

      it "returns empty path when none" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        baseUrlPath url `shouldBe` ""

    describe "edge cases" $ do
      it "handles trailing slash" $ do
        let url = parseBaseUrl "https://api.anthropic.com/"
        baseUrlHost url `shouldBe` "api.anthropic.com"
        baseUrlPath url `shouldBe` ""

      it "handles empty string" $ do
        let url = parseBaseUrl ""
        baseUrlHost url `shouldBe` ""

      it "handles URL with all components" $ do
        let url = parseBaseUrl "https://api.example.com:9000/api/v2"
        baseUrlScheme url `shouldBe` Https
        baseUrlHost url `shouldBe` "api.example.com"
        baseUrlPort url `shouldBe` 9000
        baseUrlPath url `shouldBe` "/api/v2"
