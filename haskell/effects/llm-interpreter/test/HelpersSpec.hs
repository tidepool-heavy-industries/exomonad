-- | Tests for helper functions in LLM interpreter.
--
-- Covers parseBaseUrl.
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
        url.pbuScheme `shouldBe` Https

      it "detects http:// scheme" $ do
        let url = parseBaseUrl "http://localhost:8080"
        url.pbuScheme `shouldBe` Http

      it "defaults to https when no scheme" $ do
        let url = parseBaseUrl "api.anthropic.com"
        url.pbuScheme `shouldBe` Https

    describe "host extraction" $ do
      it "extracts host from URL" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        url.pbuHost `shouldBe` "api.anthropic.com"

      it "extracts host with port" $ do
        let url = parseBaseUrl "http://localhost:8080"
        url.pbuHost `shouldBe` "localhost"

      it "extracts host with path" $ do
        let url = parseBaseUrl "https://api.anthropic.com/v1"
        url.pbuHost `shouldBe` "api.anthropic.com"

    describe "port handling" $ do
      it "uses explicit port" $ do
        let url = parseBaseUrl "http://localhost:8080"
        url.pbuPort `shouldBe` 8080

      it "defaults to 443 for https" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        url.pbuPort `shouldBe` 443

      it "defaults to 80 for http" $ do
        let url = parseBaseUrl "http://example.com"
        url.pbuPort `shouldBe` 80

      it "handles invalid port gracefully" $ do
        -- Invalid port should fall back to default
        let url = parseBaseUrl "https://example.com:invalid"
        url.pbuPort `shouldBe` 443

    describe "path extraction" $ do
      it "extracts path from URL" $ do
        let url = parseBaseUrl "https://api.anthropic.com/v1"
        url.pbuPath `shouldBe` "/v1"

      it "returns empty path when none" $ do
        let url = parseBaseUrl "https://api.anthropic.com"
        url.pbuPath `shouldBe` ""

    describe "edge cases" $ do
      it "handles trailing slash" $ do
        let url = parseBaseUrl "https://api.anthropic.com/"
        url.pbuHost `shouldBe` "api.anthropic.com"
        url.pbuPath `shouldBe` ""

      it "handles empty string" $ do
        let url = parseBaseUrl ""
        url.pbuHost `shouldBe` ""

      it "handles URL with all components" $ do
        let url = parseBaseUrl "https://api.example.com:9000/api/v2"
        url.pbuScheme `shouldBe` Https
        url.pbuHost `shouldBe` "api.example.com"
        url.pbuPort `shouldBe` 9000
        url.pbuPath `shouldBe` "/api/v2"
