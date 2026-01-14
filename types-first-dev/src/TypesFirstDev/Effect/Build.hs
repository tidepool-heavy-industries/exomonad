{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | Build effect for running cabal commands.
--
-- Abstracts over cabal build/test operations with proper error handling.
-- This makes builds:
-- - Mockable for testing
-- - Traceable via effect interpretation
-- - Consistent in error handling
module TypesFirstDev.Effect.Build
  ( -- * Effect Type
    Build(..)

    -- * Operations
  , build
  , buildAll
  , test
  , testWithDetails

    -- * Result Types
  , BuildResult(..)
  , TestResult(..)
  , TestFailure(..)

    -- * Interpreter
  , runBuildIO
  ) where

import Control.Monad.Freer (Eff, Member, LastMember, send, interpret, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Build effect for cabal operations.
data Build r where
  -- | Build a specific target
  Build :: FilePath -> Text -> Build BuildResult

  -- | Build all targets
  BuildAll :: FilePath -> Build BuildResult

  -- | Run tests
  Test :: FilePath -> Build TestResult

  -- | Run tests with detailed output
  TestWithDetails :: FilePath -> Build TestResult


-- ════════════════════════════════════════════════════════════════════════════
-- RESULT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of a build operation.
data BuildResult = BuildResult
  { brSuccess :: Bool
  , brOutput :: Text
  , brErrors :: Text
  }
  deriving (Show, Eq)

-- | Result of a test operation.
data TestResult = TestResult
  { trSuccess :: Bool
  , trOutput :: Text
  , trErrors :: Text
  , trPassedCount :: Int
  , trFailedCount :: Int
  , trFailures :: [TestFailure]
  }
  deriving (Show, Eq)

-- | A single test failure.
data TestFailure = TestFailure
  { tfName :: Text
  , tfMessage :: Text
  , tfCounterexample :: Maybe Text
  }
  deriving (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Build a specific target in a directory.
build :: Member Build effs => FilePath -> Text -> Eff effs BuildResult
build dir target = send (Build dir target)

-- | Build all targets in a directory.
buildAll :: Member Build effs => FilePath -> Eff effs BuildResult
buildAll dir = send (BuildAll dir)

-- | Run tests in a directory.
test :: Member Build effs => FilePath -> Eff effs TestResult
test dir = send (Test dir)

-- | Run tests with detailed output.
testWithDetails :: Member Build effs => FilePath -> Eff effs TestResult
testWithDetails dir = send (TestWithDetails dir)


-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTOR (IO)
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Build effect in IO using cabal.
runBuildIO :: LastMember IO effs => Eff (Build ': effs) a -> Eff effs a
runBuildIO = interpret $ \case
  Build dir target -> sendM $ runCabalBuild dir (Just target)
  BuildAll dir -> sendM $ runCabalBuild dir Nothing
  Test dir -> sendM $ runCabalTest dir False
  TestWithDetails dir -> sendM $ runCabalTest dir True


-- | Run cabal build.
runCabalBuild :: FilePath -> Maybe Text -> IO BuildResult
runCabalBuild dir mTarget = do
  let args = case mTarget of
        Nothing -> ["build", "-v0", "all"]
        Just t -> ["build", "-v0", T.unpack t]

  (exitCode, stdout, stderr) <- withCurrentDirectory dir $
    readProcessWithExitCode "cabal" args ""

  pure BuildResult
    { brSuccess = exitCode == ExitSuccess
    , brOutput = T.pack stdout
    , brErrors = T.pack stderr
    }


-- | Run cabal test.
runCabalTest :: FilePath -> Bool -> IO TestResult
runCabalTest dir withDetails = do
  let args = if withDetails
        then ["test", "--test-show-details=always"]
        else ["test"]

  (exitCode, stdout, stderr) <- withCurrentDirectory dir $
    readProcessWithExitCode "cabal" args ""

  let output = T.pack stdout
      errors = T.pack stderr
      allOutput = output <> errors
      failures = parseTestFailures allOutput
      passedCount = countPassed allOutput
      failedCount = length failures

  pure TestResult
    { trSuccess = exitCode == ExitSuccess
    , trOutput = output
    , trErrors = errors
    , trPassedCount = passedCount
    , trFailedCount = failedCount
    , trFailures = failures
    }


-- | Parse test failures from output.
-- Looks for QuickCheck "*** Failed!" blocks.
parseTestFailures :: Text -> [TestFailure]
parseTestFailures output =
  let blocks = T.splitOn "*** Failed!" output
      failureBlocks = drop 1 blocks  -- Skip text before first failure
  in map parseFailureBlock failureBlocks
  where
    parseFailureBlock :: Text -> TestFailure
    parseFailureBlock block =
      let ls = T.lines block
          msg = maybe "" T.strip (listToMaybe ls)
          propName = extractPropName block
          counterex = extractCounterexample (drop 1 ls)
      in TestFailure
           { tfName = propName
           , tfMessage = T.take 200 msg
           , tfCounterexample = counterex
           }

    extractPropName :: Text -> Text
    extractPropName block =
      case T.breakOn "prop_" block of
        (_, rest) | not (T.null rest) ->
          T.takeWhile isAlphaNumOrUnderscore rest
        _ -> "unknown"

    isAlphaNumOrUnderscore c = c `elem` ("_" :: String) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    extractCounterexample :: [Text] -> Maybe Text
    extractCounterexample ls =
      let relevant = filter isCounterexLine ls
          combined = T.intercalate "\n" (take 3 relevant)
      in if T.null combined then Nothing else Just combined

    isCounterexLine line =
      let s = T.strip line
      in not (T.null s) &&
         not ("prop_" `T.isPrefixOf` s) &&
         not ("***" `T.isPrefixOf` s) &&
         not ("FAIL" `T.isSuffixOf` s)

    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x


-- | Count passed tests from output.
countPassed :: Text -> Int
countPassed output =
  let ls = T.lines output
      passedLines = filter hasPassedMarker ls
  in length passedLines
  where
    hasPassedMarker line =
      "passed" `T.isInfixOf` T.toLower line ||
      "OK" `T.isInfixOf` line ||
      "PASS" `T.isInfixOf` line
