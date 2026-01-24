-- | Cabal interpreter - interprets the Cabal effect to IO.
--
-- Runs cabal commands in subprocesses and parses test output into
-- structured failure information for LLM consumption.
--
-- = Usage
--
-- @
-- import Tidepool.Cabal.Interpreter
--
-- main :: IO ()
-- main = do
--   let action = do
--         result <- cabalBuild projectPath
--         case result of
--           CabalSuccess -> cabalTest projectPath
--           failure -> pure failure
--   result <- runM . runCabalIO defaultCabalConfig $ action
--   print result
-- @
module Tidepool.Cabal.Interpreter
  ( -- * Interpreter
    runCabalIO

    -- * Configuration
  , CabalConfig(..)
  , defaultCabalConfig

    -- * Test Output Parsing
  , parseQuickCheckOutput
  , parseHSpecOutput
  , parseTestOutput

    -- * Re-exports
  , Cabal(..)
  , CabalResult(..)
  , TestFailure(..)
  , cabalBuild
  , cabalTest
  , cabalClean
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Text.Regex.TDFA ((=~))

import Tidepool.Effects.Cabal


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for the Cabal interpreter.
data CabalConfig = CabalConfig
  { ccVerbosity :: Int
    -- ^ Verbosity level: 0 = quiet, 1 = normal, 2 = verbose
  , ccShowTestDetails :: Bool
    -- ^ Whether to show detailed test output (--test-show-details=always)
  , ccTimeout :: Maybe Int
    -- ^ Optional timeout in seconds for commands
  }
  deriving (Show, Eq)

-- | Default configuration with quiet builds and detailed test output.
defaultCabalConfig :: CabalConfig
defaultCabalConfig = CabalConfig
  { ccVerbosity = 0
  , ccShowTestDetails = True
  , ccTimeout = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the Cabal effect to IO.
--
-- Interprets 'CabalBuild', 'CabalTest', and 'CabalClean' by spawning
-- cabal subprocesses and parsing the output.
--
-- @
-- result <- runM . runCabalIO cfg $ do
--   cabalBuild path >>= \\case
--     CabalSuccess -> cabalTest path
--     failure -> pure failure
-- @
runCabalIO
  :: LastMember IO effs
  => CabalConfig
  -> Eff (Cabal ': effs) a
  -> Eff effs a
runCabalIO cfg = interpret $ \case
  CabalBuild path -> sendM $ runCabalBuild cfg path
  CabalTest path -> sendM $ runCabalTest cfg path
  CabalClean path -> sendM $ runCabalClean cfg path


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run cabal build in the given directory.
runCabalBuild :: CabalConfig -> FilePath -> IO CabalResult
runCabalBuild cfg path = do
  let args = ["build", "all"] ++ verbosityArgs cfg
  (exitCode, stdout, stderr) <- withCurrentDirectory path $
    readProcessWithExitCode "cabal" args ""
  pure $ case exitCode of
    ExitSuccess -> CabalSuccess
    ExitFailure code -> CabalBuildFailure
      { cbfExitCode = code
      , cbfStderr = T.pack stderr
      , cbfStdout = T.pack stdout
      , cbfParsedErrors = []
      }

-- | Run cabal test in the given directory.
runCabalTest :: CabalConfig -> FilePath -> IO CabalResult
runCabalTest cfg path = do
  let detailsArg = if ccShowTestDetails cfg
        then ["--test-show-details=always"]
        else []
      args = ["test"] ++ verbosityArgs cfg ++ detailsArg
  (exitCode, stdout, stderr) <- withCurrentDirectory path $
    readProcessWithExitCode "cabal" args ""
  let output = T.pack $ stdout ++ stderr
  pure $ case exitCode of
    ExitSuccess -> CabalTestSuccess { ctsOutput = output }
    ExitFailure code ->
      -- Check if it's a build failure vs test failure
      if isBuildFailure (T.pack stderr)
        then CabalBuildFailure
          { cbfExitCode = code
          , cbfStderr = T.pack stderr
          , cbfStdout = T.pack stdout
          , cbfParsedErrors = []
          }
        else CabalTestFailure
          { ctfParsedFailures = parseTestOutput output
          , ctfRawOutput = output
          }

-- | Run cabal clean in the given directory.
runCabalClean :: CabalConfig -> FilePath -> IO CabalResult
runCabalClean _cfg path = do
  (exitCode, stdout, stderr) <- withCurrentDirectory path $
    readProcessWithExitCode "cabal" ["clean"] ""
  pure $ case exitCode of
    ExitSuccess -> CabalSuccess
    ExitFailure code -> CabalBuildFailure
      { cbfExitCode = code
      , cbfStderr = T.pack stderr
      , cbfStdout = T.pack stdout
      , cbfParsedErrors = []
      }

-- | Generate verbosity arguments for cabal.
verbosityArgs :: CabalConfig -> [String]
verbosityArgs cfg = case ccVerbosity cfg of
  0 -> ["-v0"]
  1 -> []
  _ -> ["-v2"]

-- | Check if output indicates a build failure (vs test failure).
isBuildFailure :: Text -> Bool
isBuildFailure output =
  "error:" `T.isInfixOf` output &&
  ("Failed to build" `T.isInfixOf` output ||
   "Build failed" `T.isInfixOf` output ||
   "cannot satisfy" `T.isInfixOf` output)


-- ════════════════════════════════════════════════════════════════════════════
-- TEST OUTPUT PARSING
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse test output from any supported framework.
--
-- Tries QuickCheck first, then HSpec. Returns all failures found.
parseTestOutput :: Text -> [TestFailure]
parseTestOutput output =
  parseQuickCheckOutput output ++ parseHSpecOutput output

-- | Parse QuickCheck test output.
--
-- Looks for patterns like:
--
-- @
-- *** Failed! Falsified (after 5 tests):
-- [1,2,3]
-- prop_pushPop:                             FAIL
-- @
parseQuickCheckOutput :: Text -> [TestFailure]
parseQuickCheckOutput output = mapMaybe (parseQCFailure output) chunks
  where
    -- Split on "*** Failed!" to find failure blocks
    chunks = drop 1 $ T.splitOn "*** Failed!" output

-- | Parse a single QuickCheck failure block.
--
-- Takes the full output for context extraction.
parseQCFailure :: Text -> Text -> Maybe TestFailure
parseQCFailure fullOutput chunk = do
  -- Extract the failure message (e.g., "Falsified (after 5 tests):")
  let lines' = T.lines chunk
  case lines' of
    [] -> Nothing
    (firstLine:rest) -> do
      let message = "Failed!" <> T.strip firstLine

      -- Extract counterexample (usually the next few lines before another test)
      let counterexample = extractCounterexample rest

      -- Try to extract the property name from context
      let propName = extractPropertyName chunk fullOutput

      -- Try to extract seed
      let seed = extractSeed chunk

      Just TestFailure
        { tfPropertyName = propName
        , tfMessage = message
        , tfCounterexample = counterexample
        , tfSeed = seed
        , tfLocation = Nothing
        }

-- | Extract counterexample from lines following "Failed!".
extractCounterexample :: [Text] -> Maybe Text
extractCounterexample lines' =
  let relevant = takeWhile isCounterexampleLine lines'
  in if null relevant
     then Nothing
     else Just $ T.strip $ T.unlines relevant
  where
    isCounterexampleLine line =
      not (T.null (T.strip line)) &&
      not ("prop_" `T.isPrefixOf` T.strip line) &&
      not ("***" `T.isPrefixOf` T.strip line) &&
      not ("====" `T.isPrefixOf` T.strip line) &&
      not ("FAIL" `T.isSuffixOf` T.strip line) &&
      not ("PASS" `T.isSuffixOf` T.strip line)

-- | Extract property name from failure context.
--
-- Looks for patterns like "prop_name: FAIL" or "Testing prop_name..."
extractPropertyName :: Text -> Text -> Text
extractPropertyName chunk fullOutput =
  -- First look backwards in full output for context (more reliable)
  case findPropBeforeChunk chunk fullOutput of
    Just name -> name
    Nothing ->
      -- Fall back to finding prop_name in the chunk itself
      case findPropInChunk chunk of
        Just name -> name
        Nothing -> "unknown_property"
  where
    findPropInChunk c =
      let matches = (T.unpack c :: String) =~ ("prop_[a-zA-Z0-9_]+" :: String) :: [[String]]
      in case matches of
           ((m:_):_) -> Just (T.pack m)
           _ -> Nothing

    findPropBeforeChunk _c full =
      -- Find where chunk appears in full output, look before it
      let (before, _) = T.breakOn "*** Failed!" full
          -- Get last prop_ mention before the failure
          matches = (T.unpack before :: String) =~ ("prop_[a-zA-Z0-9_]+" :: String) :: [[String]]
      in case reverse matches of
           ((m:_):_) -> Just (T.pack m)
           _ -> Nothing

-- | Extract QuickCheck seed from output.
--
-- Looks for patterns like "Use --seed 12345 to reproduce"
extractSeed :: Text -> Maybe Int
extractSeed chunk =
  let matches = (T.unpack chunk :: String) =~ ("--seed[= ]([0-9]+)" :: String) :: [[String]]
  in case matches of
       ((_:seedStr:_):_) -> case reads seedStr of
         [(n, "")] -> Just n
         _ -> Nothing
       _ -> Nothing


-- | Parse HSpec test output.
--
-- Looks for patterns like:
--
-- @
-- 1) MyModule.functionName should do something
--    expected: True
--     but got: False
-- @
parseHSpecOutput :: Text -> [TestFailure]
parseHSpecOutput output = mapMaybe parseHSpecFailure chunks
  where
    -- HSpec numbers failures: "1)", "2)", etc.
    chunks = drop 1 $ T.splitOn "\n  " filtered
    -- Filter to lines that look like HSpec failures
    filtered = output

-- | Parse a single HSpec failure.
parseHSpecFailure :: Text -> Maybe TestFailure
parseHSpecFailure chunk = do
  let lines' = T.lines chunk
  case lines' of
    [] -> Nothing
    (firstLine:rest) ->
      -- Check if this looks like a numbered failure
      if isHSpecFailureHeader firstLine
        then do
          let name = extractHSpecTestName firstLine
              message = T.unlines $ take 5 rest  -- Include context
              location = extractHSpecLocation (T.unlines rest)
          Just TestFailure
            { tfPropertyName = name
            , tfMessage = T.strip message
            , tfCounterexample = Nothing
            , tfSeed = Nothing
            , tfLocation = location
            }
        else Nothing

-- | Check if line is an HSpec failure header (starts with number and paren).
isHSpecFailureHeader :: Text -> Bool
isHSpecFailureHeader line =
  let stripped = T.strip line
  in case T.uncons stripped of
       Just (c, rest) -> c >= '1' && c <= '9' && ")" `T.isPrefixOf` T.dropWhile (/= ')') rest
       Nothing -> False

-- | Extract test name from HSpec failure header.
extractHSpecTestName :: Text -> Text
extractHSpecTestName line =
  -- "1) MyModule.functionName should do something" -> "functionName should do something"
  let afterNum = T.dropWhile (/= ')') line
      afterParen = T.drop 1 afterNum
  in T.strip afterParen

-- | Extract source location from HSpec output.
extractHSpecLocation :: Text -> Maybe Text
extractHSpecLocation output =
  let matches = (T.unpack output :: String) =~ ("([a-zA-Z0-9_/]+\\.hs):([0-9]+)" :: String) :: [[String]]
  in case matches of
       ((m:_):_) -> Just (T.pack m)
       _ -> Nothing
