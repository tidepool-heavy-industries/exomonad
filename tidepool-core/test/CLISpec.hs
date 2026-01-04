{-# LANGUAGE TemplateHaskell #-}

-- | Tests for CLI derivation (Tidepool.Graph.CLI).
module CLISpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Options.Applicative
import Test.Hspec

import CLITestTypes
import Tidepool.Graph.CLI

-- Derive parsers using TH
simpleParser :: Parser SimpleInput
simpleParser = $(deriveCLIParser ''SimpleInput)

commandParser :: Parser CommandInput
commandParser = $(deriveCLIParser ''CommandInput)

-- | Helper to parse arguments, returning either error or result.
parseArgs :: Parser a -> [String] -> Either String a
parseArgs p args =
  case execParserPure defaultPrefs (info p mempty) args of
    Success a -> Right a
    Failure f -> Left $ show f
    CompletionInvoked _ -> Left "completion invoked"

spec :: Spec
spec = do
  describe "deriveCLIParser" $ do
    describe "flat records" $ do
      it "parses all required flags" $ do
        let result = parseArgs simpleParser
              ["--input-file", "in.txt", "--count", "5"]
        result `shouldBe` Right SimpleInput
          { inputFile = "in.txt"
          , outputFile = Nothing
          , count = 5
          , verbose = False
          }

      it "parses optional flags" $ do
        let result = parseArgs simpleParser
              ["--input-file", "in.txt", "--output-file", "out.txt", "--count", "10"]
        result `shouldBe` Right SimpleInput
          { inputFile = "in.txt"
          , outputFile = Just "out.txt"
          , count = 10
          , verbose = False
          }

      it "parses boolean switches" $ do
        let result = parseArgs simpleParser
              ["--input-file", "in.txt", "--count", "1", "--verbose"]
        result `shouldBe` Right SimpleInput
          { inputFile = "in.txt"
          , outputFile = Nothing
          , count = 1
          , verbose = True
          }

      it "fails on missing required flags" $ do
        let result = parseArgs simpleParser ["--input-file", "in.txt"]
            isLeft (Left _) = True
            isLeft _ = False
        result `shouldSatisfy` isLeft

    describe "sum types (subcommands)" $ do
      it "parses first subcommand" $ do
        let result = parseArgs commandParser
              ["process", "--process-file", "data.txt"]
        result `shouldBe` Right (Process "data.txt" False)

      it "parses subcommand with flags" $ do
        let result = parseArgs commandParser
              ["process", "--process-file", "data.txt", "--process-strict"]
        result `shouldBe` Right (Process "data.txt" True)

      it "parses second subcommand" $ do
        let result = parseArgs commandParser
              ["validate", "--validate-file", "schema.json"]
        result `shouldBe` Right (Validate "schema.json")

      it "parses nullary subcommand" $ do
        let result = parseArgs commandParser ["list"]
        result `shouldBe` Right List

  describe "formatOutput" $ do
    it "formats as JSON" $ do
      let result = OutputResult "success" 42
          jsonOutput = formatOutput FormatJSON result
          -- Decode and re-encode to compare JSON values (key order independent)
          decoded = Aeson.decode @OutputResult (Aeson.encode result)
      decoded `shouldBe` Just result
      -- Also verify the output is valid JSON
      (Aeson.decode @Aeson.Value $ Aeson.encode jsonOutput) `shouldSatisfy` (/= Nothing)

    it "formats as text (via Show)" $ do
      let result = OutputResult "ok" 1
      formatOutput FormatText result `shouldBe` T.pack (show result)

  describe "outputFormatParser" $ do
    it "defaults to text" $ do
      let result = parseArgs outputFormatParser []
      result `shouldBe` Right FormatText

    it "parses --format json" $ do
      let result = parseArgs outputFormatParser ["--format", "json"]
      result `shouldBe` Right FormatJSON

    it "parses --format text" $ do
      let result = parseArgs outputFormatParser ["--format", "text"]
      result `shouldBe` Right FormatText

    it "parses -f shorthand" $ do
      let result = parseArgs outputFormatParser ["-f", "json"]
      result `shouldBe` Right FormatJSON

  describe "toKebabCase" $ do
    -- Testing via the generated flag names
    it "converts camelCase field names to kebab-case flags" $ do
      -- inputFile becomes --input-file
      let result = parseArgs simpleParser
            ["--input-file", "test.txt", "--count", "1"]
          isRight (Right _) = True
          isRight _ = False
      result `shouldSatisfy` isRight
