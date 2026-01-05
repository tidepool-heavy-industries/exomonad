-- | CLI entry point for impact-analysis tool
--
-- Usage:
--   impact-analysis --file src/Foo.hs --line 42 --col 10
--   impact-analysis --file src/Foo.hs --line 42 --col 10 --format json
--
module Main where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative

import ImpactAnalysis.Analyze (runAnalysis)
import ImpactAnalysis.Types


-- ════════════════════════════════════════════════════════════════════════════
-- CLI PARSING
-- ════════════════════════════════════════════════════════════════════════════

data OutputFormat = TextFormat | JSONFormat
  deriving (Eq, Show)

data CLIOptions = CLIOptions
  { optFile   :: FilePath
  , optLine   :: Int
  , optCol    :: Int
  , optFormat :: OutputFormat
  }

parseOptions :: Parser CLIOptions
parseOptions = CLIOptions
  <$> strOption
      ( long "file"
      <> short 'f'
      <> metavar "PATH"
      <> help "Haskell source file to analyze"
      )
  <*> option auto
      ( long "line"
      <> short 'l'
      <> metavar "N"
      <> help "Line number (1-indexed)"
      )
  <*> option auto
      ( long "col"
      <> short 'c'
      <> metavar "N"
      <> help "Column number (1-indexed)"
      )
  <*> flag TextFormat JSONFormat
      ( long "json"
      <> short 'j'
      <> help "Output as JSON"
      )

opts :: ParserInfo CLIOptions
opts = info (parseOptions <**> helper)
  ( fullDesc
  <> progDesc "Analyze impact of changing a symbol at a given position"
  <> header "impact-analysis - LSP-powered code impact analyzer"
  )


-- ════════════════════════════════════════════════════════════════════════════
-- MAIN
-- ════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  options <- execParser opts

  let input = ImpactInput
        { file = optFile options
        , line = optLine options
        , col = optCol options
        }

  result <- runAnalysis input

  case result of
    Left err -> do
      TIO.putStrLn $ "Error: " <> err
    Right output ->
      case optFormat options of
        JSONFormat -> BL.putStrLn $ encodePretty output
        TextFormat -> TIO.putStrLn $ formatText output


-- ════════════════════════════════════════════════════════════════════════════
-- TEXT FORMATTING
-- ════════════════════════════════════════════════════════════════════════════

formatText :: ImpactOutput -> Text
formatText output = T.unlines $
  -- Symbol info
  [ case symbol output of
      Nothing -> "Symbol: (not found)"
      Just info ->
        T.unlines
          [ "Symbol: " <> symbolName info
          , "Type: " <> typeSignature info
          ]
  , ""
  -- Definition locations
  , "Defined at:"
  ] ++
  (if null (definedAt output)
    then ["  (not found)"]
    else map formatLocation (definedAt output)
  ) ++
  [ ""
  -- References
  , "References (" <> T.pack (show $ totalReferences $ stats output)
      <> " in " <> T.pack (show $ filesAffected $ stats output) <> " files):"
  ] ++
  (if null (references output)
    then ["  (none)"]
    else map formatLocation (references output)
  ) ++
  [ ""
  -- File breakdown
  , "Impact by file:"
  ] ++
  map formatBreakdown (fileBreakdown $ stats output)

formatLocation :: LocationInfo -> Text
formatLocation loc =
  "  " <> locFile loc <> ":" <> T.pack (show $ locLine loc)
    <> ":" <> T.pack (show $ locCol loc)

formatBreakdown :: (Text, Int) -> Text
formatBreakdown (file', count) =
  "  " <> file' <> ": " <> T.pack (show count)
